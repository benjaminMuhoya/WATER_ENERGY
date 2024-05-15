#!/usr/bin/env python3
import pandas as pd

# Load the Excel file
file_path = "./BAG_BOX.xlsx"
df = pd.read_excel(file_path, sheet_name='Sheet1')  # Adjust the sheet name as needed

# Initialize empty lists to store results
unique_ids = []
bag_numbers = []

# Iterate over each column to process the data
for col in df.columns:
    bag_number = col
    unique_ids.extend(df[col].dropna().tolist())
    bag_numbers.extend([bag_number] * df[col].dropna().shape[0])

# Create a DataFrame from the results
transformed_data = pd.DataFrame({
    'Unique_ID': unique_ids,
    'Bag': bag_numbers
})

# Display the transformed data
print(transformed_data)

# Save the transformed data to a new CSV file
output_filename = "BAG_BOX.csv"
transformed_data.to_csv(output_filename, index=False)

