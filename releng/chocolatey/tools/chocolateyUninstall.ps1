$ErrorActionPreference = 'Stop'

# ocicl - OCI-based ASDF system distribution
# Chocolatey Uninstall Script
#
# SPDX-License-Identifier: MIT
# Copyright (C) 2023-2025 Anthony Green <green@moxielogic.com>

$toolsDir = "$(Split-Path -Parent $MyInvocation.MyCommand.Definition)"

# Remove the executable
$ocicl = Join-Path $toolsDir 'ocicl.exe'
if (Test-Path $ocicl) {
  Remove-Item $ocicl -Force
  Write-Host "Removed: $ocicl"
}

# Remove LICENSE if present
$license = Join-Path $toolsDir 'LICENSE'
if (Test-Path $license) {
  Remove-Item $license -Force
}

# Remove README if present
$readme = Join-Path $toolsDir 'README.md'
if (Test-Path $readme) {
  Remove-Item $readme -Force
}

# Remove THIRD-PARTY-LICENSES if present
$thirdParty = Join-Path $toolsDir 'THIRD-PARTY-LICENSES.txt'
if (Test-Path $thirdParty) {
  Remove-Item $thirdParty -Force
}

Write-Host "ocicl has been uninstalled."
