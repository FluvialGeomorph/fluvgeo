# Source DEM downloads: local receipt contract v1

Implemented in fluvgeo 2026.09.19.9038 for FG Studio's local analyst preview.
The owner approved study-local storage and byte-integrity verification on
2026-09-19. This is additive; existing selection, context and Event schemas are
unchanged. See sibling Studio's `dev/features/dem-download-proposal.md` for the
approved workflow and scope. This does not define an Enterprise asset layout.

## Directory and evidence model

The caller supplies an existing parent and a `source-dem` destination. Resolve
existing filesystem links before writes and reject paths outside that destination.
Create `assets/` and `attempts/<32 lowercase hex characters>/`. Under each attempt:

- `selection.gpkg`: byte-exact copy of the saved STREAM_DEM_SELECTION_1 snapshot,
  including all source, Stream, query and file metadata; only its selected IDs run.
- `request.json`: immutable STREAM_DEM_DOWNLOAD_1 object with `id`, UTC
  `created_at`, original `selection_file` basename, `selection_sha256`,
  `context_revision`, `stream_id`, `candidate_key` and numeric `limits`.
- `started.json`: immutable start time; exclusive publication makes execution
  one-shot. A retry creates another attempt.
- `receipts/000001.json` onward: immutable STREAM_DEM_RECEIPT_1 objects for the
  ordered selected IDs. Each contains selection checksum, context/Stream/collection
  identity, `file_id`, `source_key`, `url`, `source_filename`, `reported_bytes`,
  `started_at`, `finished_at`, `outcome`, `message` and observed `bytes`.
- Successful receipts additionally include `asset` (relative path), `sha256`,
  selected response `headers` and a `verification` object. Fresh downloads also
  record `http_status`; reused assets retain original transfer headers.
- `progress.json`: mutable, advisory current index/title, received/total bytes,
  completed/total file counts and timestamp. Missing/partial reads are ignored.
- `incomplete/000001.part` onward: unregistered payloads, never completed assets.
- `finished.json`: immutable terminal time, transferred byte count and attempt
  outcome FINISHED, CANCELLED or LIMIT_REACHED. FINISHED is worker completion,
  not a claim that every file succeeded.
- `cancelled.json`: cancellation timestamp, written after the caller stops and
  joins the worker. Cleanup touches only this attempt's numbered `.part` files.

All JSON numeric byte fields use exact integer-valued doubles within the configured
limits; unknown metadata are JSON null. GeoPackage remains the spatial evidence
format; JSON records operational attempts and transfer receipts, not FGDB tables.
Asset filenames are `<attempt-id>-<six-digit-index>-<sha256>.tif`. The original
filename is evidence only. Paths stay relative when a whole study is relocated.

## Verification and state

Require HTTP 200, nonempty data, known HTTP/catalog lengths matching the local
file, and TIFF/BigTIFF signature. Missing lengths remain unknown. Request identity
encoding and reject encoded responses. Compute local SHA-256; this is content
identity, not a provider authenticity assertion. Record only Content-Length,
Content-Type, Content-Encoding, ETag, Last-Modified and explicitly named S3 checksum
headers. ETag is not interpreted as a checksum; provider checksum headers are
recorded evidence and are not claimed verified.

Publish the original payload with a non-replacing hard link, then its immutable
receipt. An orphan payload after receipt failure is retained and never reused
without a completed source/content receipt. A same-source match uses a SHA-256
key over R serialization version 2 of the saved collection/file metadata. Rehash
local bytes before reuse; changed evidence or corrupt assets require a fresh
attempt and never overwrite the earlier payload. No remote-freshness claim follows
from reuse. Unchanged source bytes can be associated with additional saved
selections without inferring Survey Event identity.

Receipt outcomes are DOWNLOADED, REUSED, FAILED and CANCELLED. The reader derives
NOT_STARTED for absent receipts and INTERRUPTED for the last active file in an
unfinished attempt (CANCELLED when the cancellation marker exists). It derives
UNAVAILABLE when receipt association, file existence, length or checksum fails.
`verify=FALSE` deliberately labels successful receipts RECORDED: existence and
length were checked, but the checksum has not been rechecked in that read.
The app uses background `verify=TRUE` reads on reopening, keeping Shiny responsive.

## Transport and ownership

The adapter accepts direct HTTPS TIFF objects under the exact supported public
USGS directory derived from the saved collection. Percent-encoded/ambiguous paths,
query strings, redirects, archives and other providers/formats are unsupported.
Default limits: 10 GiB/file, 50 GiB/attempt, 30 s connect, 120 s idle, 2 h/file,
8 h/attempt. Known excess is rejected before preparing an attempt. Streaming
enforces the remaining byte budget and checks cancellation. One worker downloads
files sequentially; prior successes survive later failures. Limits can be supplied
to `prepare_stream_dem_download`, with named positive finite numeric overrides.

The transport uses curl's streaming and progress callbacks; see the
[curl reference](https://jeroen.r-universe.dev/curl/doc/manual.html).
FG Studio owns current-revision/source guards, destination choice, worker
cancellation/join and display. Backend APIs are `prepare_stream_dem_download`,
`run_stream_dem_download`, `read_stream_dem_download` and
`cancel_stream_dem_download`. No API assigns CRS, clips/mosaics terrain, validates
scientific suitability, or creates Events. Raster readability beyond the signature
is a subsequent inspection step. Other clients and shared libraries are not upgraded.

## Qualification

On 2026-09-19, 74 focused DEM assertions passed, including a loopback HTTP server
for streaming/byte/cancellation/idle/file-timeout behavior. A directory-symlink
test skipped because the account cannot create symbolic directory links. Studio's
opt-in synthetic Omaha check acquired and verified one 10,295,641-byte USGS source,
then rehashed and reused it; no analyst selections were executed.

The limited package check completed with one non-ASCII warning in unchanged
`R/survey_collections.R` and two notes concerning existing methods/global bindings.
Unavailable optional fluvgeodata/gt required `_R_CHECK_FORCE_SUGGESTS_=false`.
Tests/examples were excluded from that check because legacy report tests can
delete and rewrite HOME outputs. No full legacy suite qualification is claimed.
App-side evidence and runtime handoff are maintained in sibling Studio's
`dev/features/stream-dem-files.md` and `dev/checkpoints/current/terrain-acquisition.md`.
