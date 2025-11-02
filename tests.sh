#!/bin/bash
set -e

script_dir="$(dirname "$0")"

directory="$(realpath "$script_dir/listings/perfaware/part1/")"


process_file() {
    asm_file="$1"
    base_name="$2"
    asm_filename=$(basename $asm_file)
    base_filename=$(basename $base_name) 
    
    $script_dir/bin/decode_instructions -i $base_name -o /tmp/$asm_filename

    nasm /tmp/$asm_filename
    diff /tmp/$base_filename $base_name
}

echo "Testing computer enhance listing ..."
echo

find "$directory" -name "*.asm" | sort | while read -r asm_file; do
    base_name="${asm_file%.asm}"
    process_file "$asm_file" "$base_name"
done
