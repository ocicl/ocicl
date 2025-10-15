# Release Process for ocicl

This document describes how to create a new release of ocicl with binary, RPM, and DEB packages.

## Prerequisites

- Push access to the ocicl repository
- Git configured with your credentials

## Release Steps

### 1. Update Version Numbers

Update the version in the following files:

- `ocicl.asd` - Update the `:version` field
- `README.md` - Update version references if needed
- `ocicl.spec` - Version will be automatically updated by CI
- `debian/changelog` - Version will be automatically updated by CI

### 2. Create Release Notes

Create a new release notes file in `docs/release-notes/` named `RELEASE-NOTES-X.Y.Z.md` documenting:
- Summary of changes
- Bug fixes
- New features
- Breaking changes (if any)

See existing files in `docs/release-notes/` for format examples.

### 3. Commit Changes

```bash
git add ocicl.asd README.md docs/release-notes/RELEASE-NOTES-X.Y.Z.md
git commit -m "Boost version to X.Y.Z"
```

### 4. Create and Push Tag

```bash
git tag vX.Y.Z
git push origin main
git push origin vX.Y.Z
```

### 5. GitHub Actions Builds Packages

Once you push the tag, GitHub Actions will automatically:

1. **Build standalone binary** (`ocicl-linux-x86_64`)
   - Built using Homebrew SBCL on Ubuntu

2. **Build RPM package** (`ocicl-X.Y.Z-1.fc*.x86_64.rpm`)
   - Built from source in Fedora container
   - Uses `ocicl.spec` file

3. **Build DEB package** (`ocicl_X.Y.Z-1_amd64.deb`)
   - Built from source in Debian container
   - Uses `debian/` packaging files

4. **Create GitHub Release**
   - Attaches all three artifacts to the release
   - Release can be found at: `https://github.com/ocicl/ocicl/releases/tag/vX.Y.Z`

### 6. Verify Release

Check the GitHub Actions workflow at: `https://github.com/ocicl/ocicl/actions`

Verify the release includes:
- `ocicl-linux-x86_64` - Standalone binary
- `ocicl-X.Y.Z-1.*.x86_64.rpm` - RPM package
- `ocicl_X.Y.Z-1_amd64.deb` - DEB package

### 7. Test Packages (Optional but Recommended)

#### Test RPM on Fedora:
```bash
wget https://github.com/ocicl/ocicl/releases/download/vX.Y.Z/ocicl-X.Y.Z-1.*.x86_64.rpm
sudo dnf install ./ocicl-X.Y.Z-1.*.x86_64.rpm
ocicl version
ocicl setup
```

#### Test DEB on Debian/Ubuntu:
```bash
wget https://github.com/ocicl/ocicl/releases/download/vX.Y.Z/ocicl_X.Y.Z-1_amd64.deb
sudo apt install ./ocicl_X.Y.Z-1_amd64.deb
ocicl version
ocicl setup
```

## Package Details

### RPM Package (`ocicl.spec`)
- Builds from source using SBCL
- Dependencies: sbcl, libfixposix-devel, gcc
- Installs binary to `/usr/bin/ocicl`
- Users run `ocicl setup` after installation

### DEB Package (`debian/`)
- Builds from source using SBCL
- Dependencies: sbcl, libfixposix-dev, gcc
- Installs binary to `/usr/bin/ocicl`
- Users run `ocicl setup` after installation

## Troubleshooting

### Build fails in GitHub Actions
- Check the Actions tab for error logs
- Common issues:
  - SBCL build errors (check dependencies)
  - Version string mismatches
  - Missing files in tarball

### Package installation fails
- Verify dependencies are installed
- Check architecture (packages are x86_64 only currently)
- Ensure sufficient disk space for SBCL build

## Future Improvements

- [ ] Add ARM64 builds
- [ ] Add macOS packages
- [ ] Add Windows packages
- [ ] Automate homebrew formula updates
- [ ] Sign RPM packages with GPG
- [ ] Sign DEB packages with GPG
