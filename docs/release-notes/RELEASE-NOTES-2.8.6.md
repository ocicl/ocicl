# ocicl 2.8.6 Release Notes

## What's New

### Libyear Calculation Fixes

The `ocicl libyear` command now correctly calculates dependency staleness:

- **Fixed version ordering**: Version tags from the registry are now sorted chronologically before determining newer versions. Previously, the arbitrary order returned by the registry could cause incorrect comparisons.

- **Fixed OCI image name lookup**: The command now correctly extracts the OCI image name from the registry URL rather than relying on hash table iteration order, which could return secondary system names (like `foo-test`) that don't exist as separate OCI images.

- **Removed problematic pagination parameter**: Removed `&last=latest` from the tags list query which was causing empty results in some cases.

- **Improved error handling**: Now catches all errors during version lookups, not just network errors, preventing crashes when packages return unexpected responses.

- **Cleaner output**: Packages with 0 days of staleness (same-day updates with different commits) are no longer displayed, as they don't contribute to the libyear metric.

### Dependency Updates

- Updated named-readtables to 20251210-6eea566

## Breaking Changes

None. This release is fully backward compatible with 2.8.5.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.8.6):

- **Standalone binary**: `ocicl-linux-x86_64`
- **Fedora/RHEL/CentOS**: `ocicl-2.8.6-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.8.6-1_amd64.deb`
