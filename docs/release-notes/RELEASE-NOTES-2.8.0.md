# ocicl 2.8.0 Release Notes

I'm pleased to announce ocicl 2.8.0, a feature release adding native Linux package support.

## What's New

### Native Linux Package Distribution

This release adds comprehensive support for distributing ocicl as native RPM and DEB packages:

**RPM Packages (Fedora, RHEL, CentOS, etc.)**
- Source RPM (SRPM) and binary RPM packages
- Build from source using standard `rpmbuild` tooling
- Spec file included in repository (`ocicl.spec`)

**DEB Packages (Debian, Ubuntu, etc.)**
- Source and binary DEB packages
- Build from source using standard `dpkg-buildpackage` tooling
- Debian packaging files in `debian/` directory

**Package Features:**
- Installs `ocicl` binary to `/usr/bin/ocicl`
- Builds from source during package creation using existing `setup.lisp`
- Compatible with system package managers (`dnf`, `apt`)
- All packages disable debug package generation (Lisp binaries don't produce C-style debug symbols)

### Vendored License Collection

Automatic collection and bundling of all vendored dependency licenses:

- New `collect-licenses.sh` script extracts licenses from 57 vendored Common Lisp libraries
- Handles multiple license file formats (LICENSE, LICENCE, COPYING, COPYRIGHT)
- Extracts license text from `.asd` file headers when no separate license file exists
- Extracts `:license` and `:licence` fields from ASDF system definitions
- All licenses included in packages under `/usr/share/doc/ocicl/LICENSES/`

**License compatibility verified:** All vendored libraries use MIT, BSD, BSD-style, LLGPL, or Public Domain licenses - all compatible with ocicl's MIT license.

### CI/CD Improvements

**Release Workflow Updates:**
- Automated building of RPM and DEB packages on GitHub releases
- Source packages (SRPM, .dsc, .tar.gz) automatically published
- Builds run in proper containers (Fedora for RPM, Debian for DEB)

**Testing Workflow:**
- New `test-packages` workflow for manual testing of package builds
- Tests both RPM and DEB builds including installation verification
- Runs `ocicl version` to verify successful installation

### Documentation

- New `docs/RELEASING.md` with complete release process documentation
- Release notes organized in `docs/release-notes/` directory
- Moved `HACKING.md` to `docs/` for better organization

## Installation

### Via Package Manager (NEW!)

**Fedora/RHEL/CentOS:**
```bash
wget https://github.com/ocicl/ocicl/releases/download/v2.8.0/ocicl-2.8.0-1.*.x86_64.rpm
sudo dnf install ./ocicl-2.8.0-1.*.x86_64.rpm
ocicl setup
```

**Debian/Ubuntu:**
```bash
wget https://github.com/ocicl/ocicl/releases/download/v2.8.0/ocicl_2.8.0-1_amd64.deb
sudo apt install ./ocicl_2.8.0-1_amd64.deb
ocicl setup
```

### Via Homebrew
```bash
brew install ocicl
# or
brew upgrade ocicl
```

### From Source
```bash
git clone https://github.com/ocicl/ocicl.git
cd ocicl
git checkout v2.8.0
sbcl --load setup.lisp
```

## Breaking Changes

None. This release is fully backward compatible with 2.7.9.

## Contributors

Thanks to all contributors who helped make this release possible!

---

For more information, visit the [ocicl repository](https://github.com/ocicl/ocicl) or read the [documentation](https://github.com/ocicl/ocicl/blob/main/README.md).
