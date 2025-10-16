# ocicl 2.8.2 Release Notes

## What's New

### License Collection Enhancements

The `collect-licenses` command now automatically follows file references in license text:

**File Reference Detection:**
When license information contains references to other files (e.g., "see file LICENSE.text" or "See [LICENSE.md](LICENSE.md)"), the command now:
- Detects the file reference using pattern matching
- Locates the referenced file in the system directory
- Automatically includes the full contents of the referenced file in the output

**Supported Reference Patterns:**
1. **"see file X"** pattern (case insensitive)
2. **Markdown links**: "See [X](X)"
3. **"see LICENSE*"** where the filename looks like a license file

**Impact:**
This enhancement ensures that users get complete license information instead of unhelpful references to files they don't have access to. For example:
- **Before**: "BSD 2-clause, see file LICENSE.text"
- **After**: Shows the reference text followed by the complete BSD license from LICENSE.text

The output clearly separates the original text from the referenced file contents with visual separators.

## Breaking Changes

None. This release is fully backward compatible with 2.8.1.
