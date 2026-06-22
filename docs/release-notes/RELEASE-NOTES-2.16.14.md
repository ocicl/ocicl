# ocicl 2.16.14 Release Notes

**Release Date:** June 2026

## Summary

Bug fix release: dependencies that ASDF resolves outside ocicl-managed directories are now downloaded, and SBOM package URLs use the correct `pkg:ocicl` scheme.

## Bug Fixes

- **Download dependencies that resolve outside ocicl-managed directories**: `ocicl install <system>` relied on `asdf:find-system` to pull in dependencies as a side effect. Because ocicl's downloader runs last among ASDF's search functions, any dependency ASDF could already resolve elsewhere (a sibling project, a globally installed system, or an `.asd` file in the current directory) was never downloaded into the local systems directory. ocicl now explicitly downloads any dependency whose resolved `.asd` doesn't already live in an ocicl-managed systems directory, while skipping builtin/`:require` modules and deduplicating diamond dependencies.
- **SBOM package URLs use the `pkg:ocicl` scheme**: `ocicl create-sbom` previously emitted CycloneDX purls with the unregistered `pkg:oci/` prefix. Purls now use `pkg:ocicl/`.

## New Features

None.

## Breaking Changes

None. This release is fully backward compatible with 2.16.13.

## Upgrade Notes

Drop-in replacement for 2.16.13.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.14):

**Linux:**
- **Fedora/RHEL/CentOS**: `sudo dnf install ocicl` (via repo) or download RPM
- **Debian/Ubuntu**: `sudo apt install ocicl` (via repo) or download DEB

**Windows:**
- **Installer**: `ocicl-2.16.14-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.14.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.16.14-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.14-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.14-macos-x64.tar.gz`
