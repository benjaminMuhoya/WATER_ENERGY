#!/usr/bin/env python3
import pandas as pd

# Function to process each sheet
def process_sheet(sheet_df):
    unique_ids = []
    box_ids = []
    current_box = None

    # Iterate over each row to process the data
    for index, row in sheet_df.iterrows():
        # Check if the row contains a box header by detecting non-NA value in the first cell only
        if pd.notna(row[0]) and row[1:].isna().all():
            current_box = row[0]
        else:
            # Append the non-NA entries with the current box identifier
            if current_box is not None:
                for cell in row.dropna():
                    unique_ids.append(cell)
                    box_ids.append(current_box)
    
    return unique_ids, box_ids

# Initialize lists to store combined results
combined_unique_ids = []
combined_box_ids = []

# Process the first Excel sheet
file_path1 = "./TRAIN_GTP4O.xlsx"
sheet1 = pd.read_excel(file_path1, sheet_name=0, header=None)
unique_ids1, box_ids1 = process_sheet(sheet1)
combined_unique_ids.extend(unique_ids1)
combined_box_ids.extend(box_ids1)

# Process the second Excel sheet
file_path2 = "./cm_bdna_2.xlsx"
sheet2 = pd.read_excel(file_path2, sheet_name=0, header=None)
unique_ids2, box_ids2 = process_sheet(sheet2)
combined_unique_ids.extend(unique_ids2)
combined_box_ids.extend(box_ids2)

# Create a DataFrame from the combined results
transformed_data = pd.DataFrame({
    'Unique_ID': combined_unique_ids,
    'Box': combined_box_ids
})

# Display the transformed data
print(transformed_data)

# Save the transformed data to a new CSV file
output_filename = "B_DNA_transformed_data_combined.csv"
transformed_data.to_csv(output_filename, index=False)

