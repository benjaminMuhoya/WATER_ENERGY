#! /usr/bin/env python3
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import sys

# Check if a file name is provided as a command-line argument
if len(sys.argv) < 2:
    print("Usage: python my_script.py <path_to_file>")
    sys.exit(1)

# Load the GWAS results from the command-line argument
file_path = sys.argv[1]
gwas_results = pd.read_csv(file_path, sep="\s+", header=0)

# Print column names to debug
print(gwas_results.columns)

# The rest of your script...


# Ensure all p-values are numeric
gwas_results['P'] = pd.to_numeric(gwas_results['P'], errors='coerce')

# Drop rows with NA p-values
gwas_results.dropna(subset=['P'], inplace=True)

# Create a new column for the negative log10 of the p-values
gwas_results['neg_log10_p_value'] = -np.log10(gwas_results['P'])

# Create a Manhattan plot
plt.figure(figsize=(12, 6))

# Plotting all the chromosomes as different colors
chromosomes = gwas_results['CHR'].unique()
colors = ['red' if (i % 2 == 0) else 'blue' for i in range(len(chromosomes))]

for chrom, color in zip(chromosomes, colors):
    data = gwas_results[gwas_results['CHR'] == chrom]
    plt.scatter(data['BP'], data['neg_log10_p_value'], c=color, label=str(chrom), alpha=0.6)

# Add a horizontal line for the genome-wide significance threshold
significance_threshold = -np.log10(5e-8)
plt.axhline(y=significance_threshold, color='grey', linestyle='--')

# Label SNPs above the significance threshold
significant_snps = gwas_results[gwas_results['neg_log10_p_value'] > significance_threshold]
for _, row in significant_snps.iterrows():
    plt.text(row['BP'], row['neg_log10_p_value'], row['SNP'], fontsize=8, verticalalignment='bottom')

# Adjusting the plot
plt.xlabel('Base Pair Position')
plt.ylabel('-log10(p-value)')
plt.title('Manhattan Plot for HDL GWAS')
plt.legend(title='Chromosome', bbox_to_anchor=(1.05, 1), loc='upper left')
plt.tight_layout()

# Show the plot
plt.show()
