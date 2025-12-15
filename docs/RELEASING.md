# Release Process for ocicl

This document describes how to create a new release of ocicl with packages for Linux, Windows, and macOS.

## Prerequisites

- Push access to the ocicl repository
- Git configured with your credentials

## Release Steps

### 1. Update Version Numbers

Update the version in the following files:

- `ocicl.asd` - Update the `:version` field
- `README.md` - Update version references if needed
- `releng/ocicl.spec` - Version will be automatically updated by CI
- `releng/debian/changelog` - Version will be automatically updated by CI
- `releng/chocolatey/ocicl.nuspec` - Version will be automatically updated by CI

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

Once you push the tag, GitHub Actions will automatically build packages for all platforms:

#### Linux Packages

1. **RPM package** (`ocicl-X.Y.Z-1.fc*.x86_64.rpm`)
   - Built from source in Fedora container
   - Uses `releng/ocicl.spec` file

2. **DEB package** (`ocicl_X.Y.Z-1_amd64.deb`)
   - Built from source in Debian container
   - Uses `releng/debian/` packaging files

#### Windows Packages

3. **Windows ZIP** (`ocicl-X.Y.Z-windows-amd64.zip`)
   - Standalone binary with LICENSE and README

4. **NSIS Installer** (`ocicl-X.Y.Z-setup.exe`)
   - GUI installer with optional PATH configuration
   - Uses `releng/installer/nsis/ocicl.nsi`

5. **MSI Installer** (`ocicl-X.Y.Z.msi`)
   - Windows Installer package
   - Uses `releng/installer/wix/ocicl.wxs`

6. **Chocolatey Package** (`ocicl.X.Y.Z.nupkg`)
   - For installation via `choco install ocicl`
   - Uses `releng/chocolatey/` files

#### macOS Packages

7. **macOS ARM64** (`ocicl-X.Y.Z-macos-arm64.tar.gz`)
   - For Apple Silicon Macs (M1/M2/M3)

8. **macOS x64** (`ocicl-X.Y.Z-macos-x64.tar.gz`)
   - For Intel Macs

#### GitHub Release

9. **Create GitHub Release**
   - Attaches all artifacts to the release
   - Release can be found at: `https://github.com/ocicl/ocicl/releases/tag/vX.Y.Z`

### 6. Verify Release

Check the GitHub Actions workflow at: `https://github.com/ocicl/ocicl/actions`

Verify the release includes all packages:
- Linux: `.rpm`, `.deb`
- Windows: `.zip`, `-setup.exe`, `.msi`, `.nupkg`
- macOS: `-macos-arm64.tar.gz`, `-macos-x64.tar.gz`

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

#### Test on Windows:
```powershell
# Using Chocolatey (after package is published)
choco install ocicl

# Or download and run the installer
# Download ocicl-X.Y.Z-setup.exe from GitHub releases
# Run the installer and optionally add to PATH
ocicl version
ocicl setup
```

#### Test on macOS:
```bash
# ARM64 (Apple Silicon)
curl -LO https://github.com/ocicl/ocicl/releases/download/vX.Y.Z/ocicl-X.Y.Z-macos-arm64.tar.gz
tar xzf ocicl-X.Y.Z-macos-arm64.tar.gz
./ocicl version
./ocicl setup

# x64 (Intel)
curl -LO https://github.com/ocicl/ocicl/releases/download/vX.Y.Z/ocicl-X.Y.Z-macos-x64.tar.gz
tar xzf ocicl-X.Y.Z-macos-x64.tar.gz
./ocicl version
./ocicl setup
```

## Package Details

### RPM Package (`releng/ocicl.spec`)
- Builds from source using SBCL
- Dependencies: sbcl, libfixposix-devel, gcc
- Installs binary to `/usr/bin/ocicl`
- Users run `ocicl setup` after installation

### DEB Package (`releng/debian/`)
- Builds from source using SBCL
- Dependencies: sbcl, libfixposix-dev, gcc
- Installs binary to `/usr/bin/ocicl`
- Users run `ocicl setup` after installation

### Windows Packages (`releng/installer/`, `releng/chocolatey/`)
- NSIS installer: GUI with PATH configuration option
- MSI installer: Standard Windows Installer
- Chocolatey: Package manager installation
- ZIP: Manual extraction
- All install to `C:\Program Files\ocicl\` (installers) or user-chosen location

### macOS Packages
- Tarballs with binary, LICENSE, README, and THIRD-PARTY-LICENSES
- Users extract and run `ocicl setup` to configure

## Testing Workflows

Before creating a release, you can test the package building using:

```bash
# Trigger the test workflow manually from GitHub Actions
# Go to Actions > Test Package Building > Run workflow
```

This builds all packages without creating a release, useful for verifying changes to packaging.

## Troubleshooting

### Build fails in GitHub Actions
- Check the Actions tab for error logs
- Common issues:
  - SBCL build errors (check dependencies)
  - Version string mismatches
  - Missing files in tarball
  - Windows: NSIS or WiX installation issues
  - macOS: Homebrew SBCL issues

### Package installation fails
- Verify dependencies are installed
- Check architecture (packages are platform-specific)
- Ensure sufficient disk space for SBCL build
- Windows: Run installer as Administrator if needed

### Chocolatey package issues
- Package must be submitted to Chocolatey community repository
- Checksum must match the ZIP file exactly
- Version in nuspec must match release version

## Directory Structure

```
releng/
├── chocolatey/           # Chocolatey package files
│   ├── ocicl.nuspec
│   └── tools/
│       ├── chocolateyInstall.ps1
│       ├── chocolateyUninstall.ps1
│       └── VERIFICATION.txt
├── debian/               # Debian package files
│   ├── changelog
│   ├── control
│   ├── copyright
│   └── rules
├── installer/
│   ├── nsis/
│   │   └── ocicl.nsi    # NSIS installer script
│   └── wix/
│       ├── ocicl.wxs    # WiX MSI definition
│       └── License.rtf   # License for MSI UI
└── ocicl.spec           # RPM spec file
```

## Future Improvements

- [ ] Add Linux ARM64 builds
- [ ] Automate Homebrew formula updates
- [ ] Sign RPM packages with GPG
- [ ] Sign DEB packages with GPG
- [ ] Sign Windows packages
- [ ] Publish Chocolatey package to community repository
