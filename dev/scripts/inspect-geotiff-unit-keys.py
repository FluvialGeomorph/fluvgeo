"""Read raw TIFF georeferencing tags without decoding pixels or editing inputs.

Usage: python inspect-geotiff-unit-keys.py <new-output.json> <input.tif> ...
Requires Pillow. Retains raw IDs/values; does not assign a replacement CRS.
"""
import hashlib
import json
import sys
from pathlib import Path

from PIL import Image

output = Path(sys.argv[1])
if output.exists():
    raise FileExistsError(output)
records = []
for arg in sys.argv[2:]:
    path = Path(arg)
    before = hashlib.sha256(path.read_bytes()).hexdigest()
    with Image.open(path) as image:
        raw = {str(key): image.tag_v2[key]
               for key in (34735, 34736, 34737, 42112, 42113)
               if key in image.tag_v2}
    directory = raw.get('34735', ())
    entries = [directory[i:i + 4] for i in range(4, len(directory), 4)]
    assert all(len(entry) == 4 for entry in entries)
    after = hashlib.sha256(path.read_bytes()).hexdigest()
    assert before == after
    records.append(dict(file=path.name, sha256=before,
                        directory_entries=entries, raw_tags=raw, unchanged=True))
with output.open('x', encoding='utf-8') as handle:
    json.dump(records, handle, indent=2)
print(f'Read {len(records)} TIFF headers; source bytes unchanged.')
