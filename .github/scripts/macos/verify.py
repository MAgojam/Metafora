"""Verify the compiler-produced package, resources, datasets and Mach-O targets."""
from pathlib import Path
import hashlib, json, os, re, subprocess, sys, tempfile, zipfile

root, out = map(Path, sys.argv[1:3]); arch = sys.argv[3]
packages = list(out.glob('*.jmo')); assert len(packages) == 1
artifact = packages[0]
commit = subprocess.check_output(['git', '-C', str(root), 'rev-parse', 'HEAD'], text=True).strip()
assert commit == os.environ['SOURCE_COMMIT']
with zipfile.ZipFile(artifact) as z, tempfile.TemporaryDirectory() as temporary:
    assert z.testzip() is None
    assert re.search(r'minApp:\s*28\.18\.0', z.read('Metafora/jamovi.yaml').decode())
    description = z.read('Metafora/R/Metafora/DESCRIPTION').decode()
    assert 'Version: 2.0.1' in description and 'Built: R 4.6.' in description
    assert 'Version: 5.3-4' in z.read('Metafora/R/metafor/DESCRIPTION').decode()
    for source in (root / 'inst').rglob('*'):
        if source.is_file():
            assert z.read('Metafora/R/Metafora/' + source.relative_to(root / 'inst').as_posix()) == source.read_bytes()
    datasets = list((root / 'data').glob('*.omv')); assert len(datasets) == 6
    for source in datasets:
        matches = [name for name in z.namelist() if name.endswith('/' + source.name)]
        assert len(matches) == 2
        assert all(z.read(name) == source.read_bytes() for name in matches)
    forbidden = re.compile(r'^Metafora/(?:tests|md|srcjs|tools|\.github)/')
    assert not any(forbidden.search(name) for name in z.namelist())
    natives = []
    for name in z.namelist():
        if not name.endswith(('.so', '.dylib')):
            continue
        path = Path(temporary) / name
        path.parent.mkdir(parents=True, exist_ok=True); path.write_bytes(z.read(name))
        subprocess.run(['lipo', str(path), '-verify_arch', arch], check=True)
        dependencies = subprocess.check_output(['otool', '-L', str(path)], text=True)
        assert not re.search(r'^\s+/(?:opt|usr/local|Library/Frameworks)/', dependencies, re.M), (name, dependencies)
        natives.append({'path': name, 'dependencies': dependencies})
report = {'module':'Metafora','version':'2.0.1','sourceCommit':commit,'jamovi':'28.18.0.0',
          'architecture':arch,'file':artifact.name,'sha256':hashlib.sha256(artifact.read_bytes()).hexdigest(),
          'bytes':artifact.stat().st_size,'nativeLibraries':natives,'packageChecks':'PASSED','desktopGui':'Not tested'}
(out / 'BUILD.json').write_text(json.dumps(report, indent=2) + '\n')
print('Commit, resources, six datasets, architecture and dependency paths PASSED')
