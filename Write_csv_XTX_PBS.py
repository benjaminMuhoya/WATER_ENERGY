#!/bin/bash

# Define paths
GENOTYPE_DATA="./GEMMA_files"  # Directory where PLINK files (.bed, .bim, .fam) are stored
OUTPUT_DIR="./output_snp_regions"
PLINK_PATH="./plink"  # Ensure PLINK is accessible and executable

# List of files to process
FILES=("filtered_data_XtX.csv" "filtered_data_PBS.csv")

# Ensure the output directory exists
mkdir -p "$OUTPUT_DIR"

# Loop through each file in the FILES array
for FILE in "${FILES[@]}"; do
    # Extract filename for output differentiation
    BASENAME=$(basename "$FILE" .csv)

    # Read the regions from the file and loop through each, skipping the header
    tail -n +2 "$FILE" | while IFS=, read -r REGION rest; do
        # Remove any unwanted characters like quotes
        REGION=$(echo "$REGION" | tr -d '"')

        # Parse region details
        IFS='_' read -ra ADDR <<< "$REGION"
        CHR="${ADDR[0]}"
        FROM_BP="${ADDR[1]}"
        TO_BP="${ADDR[2]}"

        # Extend the region by 50kb upstream and downstream
        FROM_BP=$((FROM_BP - 50000))
        TO_BP=$((TO_BP + 50000))

        # Ensure that the start position is not negative
        if [ "$FROM_BP" -lt 0 ]; then
            FROM_BP=0
        fi

        # Define output path to avoid complex file names
        OUTPUT_PATH="${OUTPUT_DIR}/${BASENAME}_${CHR}_${FROM_BP}_${TO_BP}"

        # Subset data for the specified region with PLINK and recode into a text file with space delimiters
        ${PLINK_PATH} --bfile ${GENOTYPE_DATA} --chr $CHR --from-bp $FROM_BP --to-bp $TO_BP \
                      --recode A --out "${OUTPUT_PATH}"

        # Get the headers from the .raw file and format them as CSV
        HEADERS=$(head -n 1 "${OUTPUT_PATH}.raw" | tr ' ' ',')

        # Create a new CSV file and write the headers
        echo "$HEADERS" > "${OUTPUT_PATH}.csv"

        # Skip the header line in the .raw file, then append the rest to the CSV
        tail -n +2 "${OUTPUT_PATH}.raw" | tr ' ' ',' >> "${OUTPUT_PATH}.csv"

        # Optionally, remove the intermediate .raw file
        rm "${OUTPUT_PATH}.raw"

        echo "Extracted SNPs for extended region: ${CHR}:${FROM_BP}-${TO_BP} in CSV format for file: ${BASENAME}"
    done

    echo "All regions processed for file: ${BASENAME}. SNP data saved in CSV format in the directory: ${OUTPUT_DIR}"
done

