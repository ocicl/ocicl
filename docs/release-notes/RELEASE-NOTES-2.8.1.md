# ocicl 2.8.1 Release Notes (PRE-RELEASE)

**Note:** These are pre-release notes for an upcoming version.

## What's New

### Enhanced License Collection

The `collect-licenses` command has been significantly enhanced with better heuristics for finding license information:

**New License Sources:**
- README License sections - supports both markdown-style (`#`) and underline-style (`---`) headers
- Source file footers - checks last ~50 lines for copyright/license text
- Fixed `.asd` `:license` field extraction (was broken due to empty string handling)

**Multiple Source Priority:**
The command now checks five different locations in priority order:
1. Dedicated LICENSE/COPYING files (existing)
2. README License sections (new)
3. `.asd` file header comments (existing)
4. `.asd` `:license` field (existing, now fixed)
5. Source file footers (new)

**Results:**
Testing on a real project with 78 vendored dependencies showed the enhanced heuristics reduce missing licenses from 17 to just 1, successfully finding licenses in:
- README files (cl-utilities, slime, sly, dexador)
- Source file footers (trivial-utf-8)
- `.asd` :license fields (uuid - previously broken)

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

## Contributors

Thanks to all contributors who helped make this release possible!
