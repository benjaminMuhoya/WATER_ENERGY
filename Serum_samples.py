#!/usr/bin/env python3
import pandas as pd
import sys
import re

def safe_read_csv(file_path, delimiter=','):
    try:
        return pd.read_csv(file_path, delimiter=delimiter, dtype=str), None
    except Exception as e:
        return None, str(e)

def extract_ids_from_string(s):
    parts = re.findall(r'\d+', s)
    if len(parts) >= 2:
        return "_".join(parts[-2:])  # Extract last two numeric parts
    return None

def prepare_all_data(df):
    # Convert certain fields to numeric for calculations, handle missing or non-numeric issues
    numeric_fields = ['Number.of.rooms.in.the.household']
    for field in numeric_fields:
        df[field] = pd.to_numeric(df[field], errors='coerce')

    # Calculate the household style of life index
    conditions = {
        'Number.of.rooms.in.the.household': df['Number.of.rooms.in.the.household'] > 1,
        'Presence.of.a.finished.floor.': df['Presence.of.a.finished.floor.'] == "Yes",
        'Presence.of.an.iron.concrete.or.slate.roof.': df['Presence.of.an.iron.concrete.or.slate.roof.'] == "Yes",
        'Presence.of.electricity.': df['Presence.of.electricity.'] == "Yes",
        'Presence.of.flush.toilet.': df['Presence.of.flush.toilet.'] == "Yes",
        'Does.the.household.have.indoor.tap.water.': df['Does.the.household.have.indoor.tap.water.'] == "Yes"
    }
    df['h_sol'] = sum([df[col] for col in conditions.values()]).astype(int)

    # Convert Unique.ID into a Clean_Bar_ID if necessary
    df['Clean_Bar_ID'] = df['Unique.ID'].apply(extract_ids_from_string)
    return df[['Age', 'Sex', 'Sampling.location', 'Unique.ID', 'Tribe', 'h_sol', 'Clean_Bar_ID']]

def prepare_dataframe(df, barcode_field, id_field, new_id_field):
    # Create Clean_Bar_ID from barcode and id fields
    df['Clean_Bar_ID'] = df[barcode_field].str.extract('(\d+)') + "_" + df[id_field].apply(lambda x: f"{int(x):d}")
    return df

def main(file_paths):
    All_data, error = safe_read_csv(file_paths[0], delimiter='\t')  # Assuming tab-delimited
    if error:
        print(f"Error reading All_data: {error}")
        return
    
    # Prepare All_data
    Meta_ONLY = prepare_all_data(All_data)

    Freezer_wally, _ = safe_read_csv(file_paths[1])
    Freezer_wally = prepare_dataframe(Freezer_wally, 'Sample_Barcode', 'Sample_ID', 'Wally_boxes_Bar_ID')

    p5ml_tubes, _ = safe_read_csv(file_paths[2])
    p5ml_tubes = prepare_dataframe(p5ml_tubes, 'Barcode_p5ml', 'ID_p5ml', 'Bar_ID_p5ml')

    two_ml_tubes, _ = safe_read_csv(file_paths[3])
    two_ml_tubes = prepare_dataframe(two_ml_tubes, 'Sample_Barcode', 'Sample_ID', 'LN_boxes_Bar_ID')

    Top_Freeze, _ = safe_read_csv(file_paths[4])
    Top_Freeze = prepare_dataframe(Top_Freeze, 'Barcode', 'ID', 'TopFre_Barcode_ID')

    # Perform sequential merging
    Meta_Wally = pd.merge(Meta_ONLY, Freezer_wally, on='Clean_Bar_ID', how='outer')
    Meta_Wally_LN = pd.merge(Meta_Wally, two_ml_tubes, on='Clean_Bar_ID', how='outer')
    Meta_Wally_LN_p5ml = pd.merge(Meta_Wally_LN, p5ml_tubes, on='Clean_Bar_ID', how='outer')
    Meta_Wally_LN_p5ml_Top = pd.merge(Meta_Wally_LN_p5ml, Top_Freeze, on='Clean_Bar_ID', how='outer')

    # Save the final merged data to a CSV file
    Meta_Wally_LN_p5ml_Top.to_csv("final_merged_data.csv", index=False)
    print("Merged data saved successfully.")

if __name__ == "__main__":
    if len(sys.argv) != 6:
        print("Usage: python script.py All_data.txt Freezer_Wally.csv p5ml_tubes.csv two_ml_tubes.csv Top_Freeze.csv")
    else:
        main(sys.argv[1:])

