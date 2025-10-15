# ocicl 2.8.1 Release Notes

## What's New

### SBOM Generation

New `create-sbom` command generates Software Bill of Materials (SBOM) documents for your projects:

**Features:**
- Supports CycloneDX and SPDX formats
- Catalogs all vendored dependencies with versions, licenses, and checksums
- Output to file or stdout
- Default format is CycloneDX

**Usage:**
```bash
ocicl create-sbom                    # CycloneDX to stdout
ocicl create-sbom cyclonedx sbom.json
ocicl create-sbom spdx sbom.spdx.json
```

### License Collection

New `collect-licenses` command collects license information from all vendored dependencies:

**License Sources:**
The command checks five different locations in priority order:
1. Dedicated LICENSE/COPYING files
2. README License sections - supports both markdown-style (`#`) and underline-style (`---`) headers
3. `.asd` file header comments
4. `.asd` `:license` field
5. Source file footers - checks last ~50 lines for copyright/license text

**Output Format:**
- Table of contents listing all dependencies
- Full license text for each dependency
- Source attribution (which file provided the license)
- OCI URL for each vendored system
- List of systems without license information

**Results:**
Testing on a real project with 78 vendored dependencies found licenses for 77 systems (98.7% success rate).

### Build System Updates

**License Collection Integration:**
- Removed `collect-licenses.sh` bash script
- Integrated license collection directly into ocicl binary
- RPM and DEB packages now use `./ocicl collect-licenses` command
- Output saved as single `VENDORED-LICENSES.txt` file instead of LICENSES directory
- RPM: `/usr/share/licenses/ocicl/VENDORED-LICENSES.txt`
- DEB: `/usr/share/doc/ocicl/VENDORED-LICENSES.txt`

## Breaking Changes

None. This release is fully backward compatible with 2.8.0.
