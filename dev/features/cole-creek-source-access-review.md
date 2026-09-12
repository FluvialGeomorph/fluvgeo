# Cole Creek: from catalog leads to source access

## Outcome (2026-09-12)

The 2013 and 2022 leads have accessible public metadata and representative data
endpoints. They remain candidates, not accepted Survey Events or scientifically
qualified inputs. This read-only follow-up to the
[survey-opportunity report](survey-opportunity-report.md) downloaded metadata only,
queried a public tile index and checked HTTP headers. No terrain payloads,
point-cloud processing, raster conversions or FGDB mutations occurred.

## 2013 NGA Omaha / USIEI 39542

**Verified:** the [USGS project directory](https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/Non_Standard_Contributed/NGA_US_Cities/Omaha_NE/Omaha_20130419-25/)
publishes bare-earth, point-cloud and other product directories. The
[Woolpert project report](https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/Non_Standard_Contributed/NGA_US_Cities/Omaha_NE/Omaha_20130419-25/not_loaded/additional_metadata/LiDAR%20Report_OmahaNE_June2013.pdf),
Table 2.2 (PDF page 6), lists missions on April 19, 20, 24 and 25, 2013. June is
the report date, not the flight interval. Page 4 specifies NAD83 / UTM 14N meters,
NAVD88 meters and GEOID12A; page 14 describes 1 m bare-earth products and classified
LAS 1.2 deliverables. This resolves the catalog's June label at project level,
not the exact flight date of each Cole Creek point.

**Access evidence:** `BE/omaha_bare_earth_02.img` answered HTTP HEAD 200
(14,282 bytes); the directory also lists external `.ige` raster storage and
sidecars. This small `.img` is not a complete self-contained terrain download.
Its [XML metadata](https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/Non_Standard_Contributed/NGA_US_Cities/Omaha_NE/Omaha_20130419-25/BE/omaha_bare_earth_02.img.xml)
bounding box contains the retained R1 flowline envelope. This is metadata screening,
not a check of valid terrain support along the Reach.

**Unresolved:** delivered XML contains conflicting CRS assertions (including a
WGS84 / UTM 15N name versus the project report's NAD83 / UTM 14N) and apparent
inherited metadata artifacts. Do not select an EPSG code by majority vote or copy
the report CRS onto a raster. Inspect the actual asset/header and transformations
before analysis. Access/use fields say none, but security-handling text refers to
distribution limited by the point of contact. Public hosting does not resolve
that conflict or authorize redistribution. Clarify with the steward before FG
redistribution/loading decisions.

## 2022 Douglas County / USIEI 47476

**Verified:** the [county download application](https://experience.arcgis.com/experience/8bed1358b2534a0f8e66fd2d3196b2d5/page/2022-Apr/)
uses [web map 89fb1f4c4d2b452eb65052d8bbb88e7b](https://www.arcgis.com/sharing/rest/content/items/89fb1f4c4d2b452eb65052d8bbb88e7b/data?f=json)
and [Douglas County tile-index layer 4](https://dcgis.org/server/rest/services/Hosted/Douglas_County_NE_LiDAR_Tiles_view/FeatureServer/4).
A spatial-envelope query for retained `y2010_R1.gdb/flowline`, transformed to
EPSG:4326 (`-96.03586853,41.26128966,-96.03154991,41.27781366`), returned 11 tile
records with `exceededTransferLimit=false`. This is a bounding-envelope selection,
not an assertion that all eleven intersect the line or that the whole AOI is usable.

Representative public HTTP HEAD checks succeeded:

- [DTM 133500_110500.tif](https://dcgis-lidar.s3.amazonaws.com/2022/DTM/133500_110500.tif):
  200, image/tiff, 7,046,746 bytes.
- [Classified point cloud 133500_110500.las](https://dcgis-lidar.s3.amazonaws.com/2022/Point_Cloud/Classified/133500_110500.las):
  200, 118,145,130 bytes.

The [metadata bundle](https://dcgis-lidar.s3.amazonaws.com/2022/2022_Douglas_County_LiDAR_Metadata.zip)
contains product metadata, CRS, tile/extent geometry, acquisition and accuracy
reports. `Product_Metadata/DTM_Metadata.xml` describes a hydro-flattened GeoTIFF
product: OPPD utility-network lidar, reprocessed for resale to Douglas County.
That source-acquisition versus derived-product distinction must survive FG ingestion.

**Unresolved assertions to retain separately:**

- USIEI: March 29-April 14, 2022; county web-map popup: April 4-17;
  DTM XML ground-condition interval: March 28-June 22. These may describe different
  scopes, but that explanation is not verified. Do not automatically replace one
  with another or manufacture a precise Reach-level acquisition date.
- The horizontal `.prj` explicitly uses international feet (0.3048 m) and a custom
  NAD83(2011) Transverse Mercator definition. The portal says vertical international
  feet/NAVD88; DTM XML says NAVD88 and simply `feet`. Inspect actual assets and
  acquisition documentation before setting vertical unit/geoid transformation.
  FG's owner has since clarified that the historic requirement was simply feet,
  not specifically US survey feet. Do not infer either foot variant from that
  convention; see the [ArcGIS unit investigation](../../../FGDB/dev/architecture/legacy-esri-elevation-feet.md).
- DTM XML access/use constraints both refer to a license agreement. That agreement
  has not been identified or interpreted. Public download access is verified;
  unrestricted reuse/redistribution is not.

## Header and small-sample qualification (2026-09-12 follow-up)

The subsequent read-only probe retrieved two public 2022 GeoTIFF tiles and the
2013 `.img` header into ignored `dev/outputs/terrain-development/source-header-v1/`.
This extends the earlier metadata-only check; it does not resolve source-use
conditions or authorize redistribution. Nothing was added to fluvgeodata,
accepted intake folders or enterprise storage.

**Verified 2022 declarations:** both `133500_110500.tif` and
`135000_113500.tif` contain vertical datum GeoKey 4098 = 5103 and vertical unit
GeoKey 4099 = 9002. Embedded compound-CRS text explicitly states
`NAVD88_height_(ftIntl)` with vertical `Foot` = 0.3048 m. These samples therefore
declare NAVD88/international-foot elevations in the TIFF itself. Geoid realization,
acquisition timing and independent accuracy remain unresolved.

Both files are Float32, 1501 x 1501, with one-international-foot grid spacing,
`AREA_OR_POINT=Point` and NoData -32767. The metadata's 1500-row/column description
does not exactly describe them. Preserve actual registration and dimensions;
do not silently crop or shift to fit a catalog description.

**Reader boundary:** GDAL 3.12.1 through sf exposes band unit `foot` but only the
horizontal CRS in this inspection. An independent Pillow raw-tag read exposes
the stored vertical declarations. Unexposed reader metadata is not necessarily
missing source metadata. This is not a reader-defect or full GeoTIFF-conformance
finding. Current fluvgeo exposed-band/WKT observation is not a full GeoKey inventory.

**Supported-reader follow-up (2026-09-12):** GDAL's documented
`GTIFF_REPORT_COMPD_CS=TRUE` option exposes the full compound CRS for these samples.
They use GeoTIFF 1.0, for which GDAL defaults to stripping the vertical component
on read. This resolves the reader-exposure question without a custom TIFF parser
or rewriting the files. The additive
[`inspect_terrain_vertical_reference()` interface](../schemas/vertical-reference-observation.md)
retains both ordinary and internal-only compound observations. The internal
declaration is NAVD88 with a vertical foot conversion factor of exactly 0.3048 m;
it does not recover a geoid model or establish correct source processing.
Existing report/intake acceptance is unchanged. This is the first implementation
of [ADR-0026](../../../FGDB/dev/decisions/adr-0026-vertical-reference-recovery-and-preservation.md),
which treats legacy metadata reconstruction as a required workflow.
Both samples passed the additive API probe with unchanged SHA-256; the paired
observations are retained as `source-header-v1/vertical-reference-observations-v1.json`.

Source declarations apply to these source products, not automatically to a
historic FG analysis DEM. The owner confirms that analysts could deliberately
choose different horizontal and vertical analysis references, prepare project
DEMs in that common framework and derive FG products from them. Recovery must
link the source, chosen analysis reference and preparation operations. A legitimate
source-to-analysis transformation is distinct from conflicting declarations
about the same source artifact described above.

**Spatial/read checks:** `133500_110500` does not intersect R1 despite its inclusion
in the earlier county index envelope response; its retrieved index polygon is also
outside R1. The broad server response's cause was not diagnosed. `135000_113500`
intersects part of R1, not the entire Reach; a midpoint sample on the clipped line
reads approximately 1036.88 in declared units. This checks readability, not full
terrain support, accuracy, raster-rectangle occupancy or geomorphic change.

**Verified 2013 header:** `omaha_bare_earth_02.img` declares WGS84 / UTM 14N
(EPSG:32614), one-meter spacing and a rectangle containing R1; no elevation-band
unit is exposed. This does not resolve the conflict with the report's NAD83 or
XML's inconsistent CRS names. The external `.ige` payload was not downloaded and
2013 elevations were not read. Header NoData near -3.4028235e38 also differs from
the XML's -999 sentinel. Do not silently remap values or interpret stored statistics
as verified elevation support. No CRS redefinition or reprojection was performed.

**Reproduction:** from the workspace root run
`dev/scripts/cole-creek-source-header-probe.R` with the sample directory and a new
JSON output. Run `dev/scripts/inspect-geotiff-unit-keys.py` with a new JSON output
and the two TIFF paths (Pillow required). Saved evidence: `header-review.json`,
`raw-geotiff-keys.json`, `douglas-2022-index-3012.geojson`. Both probes completed;
expected spatial/read checks and byte-preservation assertions passed. These are
development diagnostics, not a production API or deployed client capability.

Sample SHA-256 fingerprints:

- `133500_110500.tif`: `06A6ECCA5CF2E38A86570FD5E1383A4243D70B09ECFC041E4441739B25871D4C`
- `135000_113500.tif`: `9EDDBF267F4BC99F7EB75E1290BBD66AE694A04D06A0C02015E96C1D04B63B3F`
- `omaha_bare_earth_02.img`: `D99273DF35D01B5482F0B2F2FE2D84D220D28B8C8909E324303C32B7D978E1D1`

## Remaining bounded step

The sample-header/readability portion above is complete. Obtain steward clarification
of reuse/distribution conditions and unresolved source assertions before qualifying
a complete analysis subset and its processing history. Preserve a reviewable evidence link rather than
editing the original catalog snapshot. Do not commission a broad point-cloud-to-DEM
pipeline or add candidate Survey Events just because the URLs respond.

The initial HTML remains a frozen catalog-snapshot report; this follow-up supersedes
its unresolved 2013 project-date question, not its preserved raw catalog label.
Provider-aware source-review evidence should be exposed in a later report increment
before these candidates are presented as analysis-ready.

## Retained evidence and verification boundary

Metadata is under ignored local `dev/outputs/terrain-development/source-access-v1/`:
`omaha-2013-report.pdf`, `omaha-bare-earth-01.xml`, `omaha-bare-earth-02.xml`,
`douglas-2022-metadata.zip`, `douglas-2022-webmap.json`,
`douglas-2022-focus-tiles.json`. The public PDF's relevant pages were read and pages
4 and 6 visually checked. Source SHA-256 fingerprints:

- 2013 PDF: `4294BB97F7ED3DA10B2D28C2B0600C283AF03FDE771A3F412937DC299E193B91`
- 2022 ZIP: `6EA7330B9B70F8717C9399F0C24BB0C61B56CDC9458BC06CBB6C1B28085D14C4`

No inference of legal permission, sub-Reach collection dates, full download
integrity or scientific comparability follows from these checks. No contacts were
sent, accounts used, credentials stored or production package code changed.
