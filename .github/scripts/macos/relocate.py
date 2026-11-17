"""Make native dependency paths portable in a disposable compiler cache."""
from pathlib import Path
import json, os, shutil, subprocess, sys

cache, app, out = map(lambda x: Path(x).resolve(), sys.argv[1:4])
bundled = {}
for path in app.rglob('*.dylib'):
    bundled.setdefault(path.name, path)
records = []
queue = list(cache.rglob('*.so')) + list(cache.rglob('*.dylib'))
seen = set()
copies = {}
while queue:
    binary = queue.pop(0)
    if binary in seen:
        continue
    seen.add(binary)
    changed = False
    if binary.suffix == '.dylib':
        subprocess.run(['install_name_tool', '-id', '@rpath/' + binary.name, str(binary)], check=True)
        changed = True
    listing = subprocess.check_output(['otool', '-L', str(binary)], text=True)
    for line in listing.splitlines()[1:]:
        dep = line.strip().split(' (compatibility', 1)[0]
        if not dep.startswith('/') or dep.startswith(('/usr/lib/', '/System/Library/')):
            continue
        source = Path(dep)
        if source.name in bundled:
            replacement = '@executable_path/../' + bundled[source.name].relative_to(app).as_posix()
        else:
            if not source.is_file():
                raise SystemExit('Missing native dependency: ' + dep)
            target = binary.parent / source.name
            if target.resolve() != source.resolve():
                if not target.exists():
                    shutil.copy2(source, target)
                    copies[target] = source.resolve()
                elif copies.get(target) != source.resolve() and target.read_bytes() != source.read_bytes():
                    raise SystemExit('Conflicting native dependency: ' + str(target))
            queue.append(target)
            replacement = '@loader_path/' + target.name
            if target.name.startswith('libgsl'):
                license_source = Path(os.environ['JAMOVI_R_HOME']) / 'share/licenses/GPL-3'
                if not license_source.is_file():
                    raise SystemExit('GPL-3 license required for bundled GSL libraries.')
                shutil.copyfile(license_source, target.parent / 'GSL-COPYING')
        subprocess.run(['install_name_tool', '-change', dep, replacement, str(binary)], check=True)
        records.append({'file': str(binary.relative_to(cache)), 'from': dep, 'to': replacement})
        changed = True
    if changed:
        subprocess.run(['codesign', '--force', '--sign', '-', str(binary)], check=True)
(out / 'NATIVE_RELOCATION.json').write_text(json.dumps(records, indent=2) + '\n')
print('Native dependency relocation complete:', len(records), 'paths')
