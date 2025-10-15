#!/bin/bash
# Script to collect licenses from vendored dependencies in systems/

set -e

LICENSES_DIR="LICENSES"
rm -rf "$LICENSES_DIR"
mkdir -p "$LICENSES_DIR"

echo "Collecting licenses from vendored dependencies..."

for system_dir in systems/*/; do
    system_name=$(basename "$system_dir")

    # Look for license files (case insensitive, both spellings)
    license_file=$(find "$system_dir" -maxdepth 1 -type f \( -iname "licen[cs]e*" -o -iname "copying*" -o -iname "copyright*" \) | head -1)

    if [ -n "$license_file" ]; then
        cp "$license_file" "$LICENSES_DIR/${system_name}-$(basename "$license_file")"
        echo "  ✓ $system_name"
    else
        # Try to extract license from .asd file header
        asd_file=$(find "$system_dir" -maxdepth 1 -type f -name "*.asd" ! -name "*test*.asd" | head -1)
        if [ -n "$asd_file" ]; then
            # Extract license-related comments from top of file (look for copyright, license, public domain, warranty mentions)
            awk 'BEGIN { found=0 }
                 /^;;;.*([Cc]opyright|[Ll]icen[cs]e|[Pp]ublic [Dd]omain|warranty|[Pp]ermission)/ { found=1 }
                 found && /^;;;/ { print }
                 found && !/^;;;/ { exit }
            ' "$asd_file" > "$LICENSES_DIR/${system_name}-LICENSE-from-asd.txt"

            # Check if we extracted anything meaningful
            if [ -s "$LICENSES_DIR/${system_name}-LICENSE-from-asd.txt" ]; then
                echo "  ✓ $system_name (extracted from .asd)"
            else
                # Try to find :license field in defsystem
                license_line=$(grep -i ':license' "$asd_file" | head -1)
                if [ -n "$license_line" ]; then
                    echo ";;; License information from $system_name.asd:" > "$LICENSES_DIR/${system_name}-LICENSE-from-asd.txt"
                    echo "$license_line" >> "$LICENSES_DIR/${system_name}-LICENSE-from-asd.txt"
                    echo "  ✓ $system_name (license field from .asd)"
                else
                    rm "$LICENSES_DIR/${system_name}-LICENSE-from-asd.txt"
                    echo "  ⚠ $system_name (no license found)"
                fi
            fi
        else
            echo "  ⚠ $system_name (no license found)"
        fi
    fi
done

echo ""
echo "Collected $(ls -1 "$LICENSES_DIR" | wc -l) license files in $LICENSES_DIR/"
