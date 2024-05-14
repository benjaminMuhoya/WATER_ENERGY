#!/usr/bin/env python3

import pandas as pd
import sys

def main(input_file, full_file):
    try:
        # Load the data from the specified input files
        snp_data = pd.read_csv(input_file)
        full_data = pd.read_csv(full_file, delimiter="\t")

        # Convert relevant columns in full_data to strings
        columns_to_check = ['SNPS', 'MERGED', 'SNP_ID_CURRENT']
        for col in columns_to_check:
            full_data[col] = full_data[col].astype(str)

        # Convert snpID in snp_data to string
        snp_data['rsid'] = snp_data['rsid'].astype(str)

        # Create a mask for any rows in full_data where any of the specified columns match any snpID
        mask = full_data[columns_to_check].apply(lambda x: x.isin(snp_data['rsid'])).any(axis=1)
        
        # Filter full_data based on the mask
        matched_data = full_data[mask]

        # Check if there are any matches
        if matched_data.empty:
            print("none found")
        else:
            # Write matches to a text file
            matched_data.to_csv("Those_that_match_GWAS_Catalog_PBS.txt", sep='\t', index=False)
            print("Matches found and written to 'Those_that_match_GWAS_CatalogPBS.txt'.")

    except Exception as e:
        print(f"An error occurred: {e}")

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python script.py input_file full_file")
    else:
        main(sys.argv[1], sys.argv[2])

