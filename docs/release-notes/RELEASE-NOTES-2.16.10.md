# ocicl 2.16.10 Release Notes

## Changes

### GPG-signed DEB packages and APT repository

DEB packages are now GPG-signed. A new APT repository is hosted via
GitHub Pages, allowing Debian/Ubuntu users to install ocicl with:

```bash
curl -fsSL https://ocicl.github.io/ocicl/deb-repo/ocicl-archive-keyring.gpg | sudo tee /usr/share/keyrings/ocicl-archive-keyring.asc > /dev/null
echo "deb [signed-by=/usr/share/keyrings/ocicl-archive-keyring.asc] https://ocicl.github.io/ocicl/deb-repo stable main" | sudo tee /etc/apt/sources.list.d/ocicl.list
sudo apt update
sudo apt install ocicl
```

### Clean version strings in all release builds

Removed `.git` directory before building in all CI jobs (RPM, DEB,
Linux tarball, Windows) to prevent `+dirty` suffixes in version
strings. Added verification steps that fail the build if a dirty
version is detected.

## Breaking Changes

None. This release is fully backward compatible with 2.16.9.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.10):

**Linux:**
- **Fedora/RHEL/CentOS**: `sudo dnf install ocicl` (via repo) or download RPM
- **Debian/Ubuntu**: `sudo apt install ocicl` (via repo) or download DEB

**Windows:**
- **Installer**: `ocicl-2.16.10-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.10.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.16.10-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.10-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.10-macos-x64.tar.gz`
