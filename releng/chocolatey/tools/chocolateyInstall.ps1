$ErrorActionPreference = 'Stop'

# ocicl - OCI-based ASDF system distribution
# Chocolatey Install Script
#
# SPDX-License-Identifier: MIT
# Copyright (C) 2023-2025 Anthony Green <green@moxielogic.com>

$toolsDir = "$(Split-Path -Parent $MyInvocation.MyCommand.Definition)"

# Package parameters - version and checksum are replaced during CI build
$packageArgs = @{
  packageName    = $env:ChocolateyPackageName
  unzipLocation  = $toolsDir
  url64bit       = 'https://github.com/ocicl/ocicl/releases/download/v__VERSION__/ocicl-__VERSION__-windows-amd64.zip'
  checksum64     = '__CHECKSUM__'
  checksumType64 = 'sha256'
}

Install-ChocolateyZipPackage @packageArgs

# Create shim for ocicl.exe
# The exe is extracted directly to tools directory
$ocicl = Join-Path $toolsDir 'ocicl.exe'
if (Test-Path $ocicl) {
  Write-Host "ocicl installed successfully to: $ocicl"
} else {
  throw "Installation failed: ocicl.exe not found at $ocicl"
}
