#!/bin/bash
# Script to collect licenses from vendored dependencies in systems/

set -e

LICENSES_DIR="LICENSES"
rm -rf "$LICENSES_DIR"
mkdir -p "$LICENSES_DIR"

echo "Collecting licenses from vendored dependencies..."

for system_dir in systems/*/; do
    system_name=$(basename "$system_dir")

    # Look for license files (case insensitive)
    license_file=$(find "$system_dir" -maxdepth 1 -type f \( -iname "license*" -o -iname "copying*" -o -iname "copyright*" \) | head -1)

    if [ -n "$license_file" ]; then
        cp "$license_file" "$LICENSES_DIR/${system_name}-$(basename "$license_file")"
        echo "  ✓ $system_name"
    else
        echo "  ⚠ $system_name (no license file found)"
    fi
done

echo ""
echo "Collected $(ls -1 "$LICENSES_DIR" | wc -l) license files in $LICENSES_DIR/"
