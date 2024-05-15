# Set working directory
setwd("/Users/Bm0211/RegEx/Water_Energy/")
getwd()
# Load necessary libraries
library(tidyverse)  # Includes dplyr, tidyr, readr, ggplot2
library(stringr)
library(rio)
library(openxlsx)
library(lubridate)
library(readr)
library(readxl)
# Get the current date
current_date <- Sys.Date()
# Format the date as 'MAY_1' (for example, 'May 1' becomes 'MAY_1')
formatted_date <- format(current_date, "%b_%d")  # %b - Abbreviated month name, %d - Day of the month
formatted_date <- toupper(formatted_date)  # Convert to uppercase

# Import data
All_data <- import("THGP_database_full_corrected_merged_2024-05-06.txt")
# Calculate the household style of life index and prepare the data
colnames(All_data)
All_data <- All_data %>%
  mutate(
    h_sol = ifelse(Number.of.rooms.in.the.household > 1, 1, 0) +
      ifelse(Presence.of.a.finished.floor. == "Yes", 1, 0) +
      ifelse(Presence.of.an.iron.concrete.or.slate.roof. == "Yes", 1, 0) +
      ifelse(Presence.of.electricity. == "Yes", 1, 0) +
      ifelse(Presence.of.flush.toilet. == "Yes", 1, 0) +
      ifelse(Does.the.household.have.indoor.tap.water. == "Yes", 1, 0),
    Clean_Bar_ID = str_c(str_extract(Unique.ID, "\\d+(?=_[0-9]+$)"), 
                         str_extract(Unique.ID, "\\d+$"), 
                         sep = "_")
  ) %>%
  dplyr::select(Age, Sex, Sampling.location, Unique.ID, Tribe, h_sol, Clean_Bar_ID)
# Load Data from sample maps
Freezer_wally <- read_csv("Freezer_Wally.csv") %>%
  mutate(
    Sample_Barcode = str_remove_all(Sample_Barcode, "\\D"),  # Remove non-digit characters
    Sample_ID = as.character(as.numeric(Sample_ID)),  # Convert to numeric to remove leading zeros, then back to character
    Sample_ID = if_else(Sample_ID == "0", "0", gsub("^0+", "", Sample_ID)),  # Remove leading zeros,
    Clean_Bar_ID = paste(Sample_Barcode, Sample_ID, sep = "_"),
    NOTES = coalesce(Sample_Notes, ""),
    FREEZER = "WALLY_rack_C1|B1|D1"  ##This can me easily changed to match FREEZER LOCATION
  ) %>%
  rename(WALLY_2ML = Box_map_ID) %>%
  dplyr::select(WALLY_2ML, NOTES, Clean_Bar_ID, FREEZER)

# Import p5ml tubes data
p5ml_tubes <- read_csv("p5mlCombinedsheet.csv") %>%
  separate(Barcode_ID_p5ml, into = c("Barcode_p5ml", "ID_p5ml"), sep = "_", convert = TRUE, extra = "drop", fill = "right") %>%
  mutate(
    Barcode_p5ml = str_remove_all(Barcode_p5ml, "\\D"),
    ID_p5ml = coalesce(as.character(ID_p5ml), "unknown"),  
    ID_p5ml = if_else(ID_p5ml == "0", "0", gsub("^0+", "", ID_p5ml)),
    Clean_Bar_ID = str_c(Barcode_p5ml, ID_p5ml, sep = "_"),
    FREEZER = "WALLY_Middle_Shelf"
  ) %>%
  rename(NOTES = p5mltube_notes)  %>%
  rename(p5ML = Box_map_ID)  %>%
  dplyr::select(p5ML, NOTES, Clean_Bar_ID, FREEZER)

# Import and process two_ml_tubes data
two_ml_tubes <- read_csv("Liq_Nit_Boxes_Combined_sheet.csv") %>%
  dplyr::select(Box_map_ID, Sample_Barcode, Sample_ID, Barcode_ID_LN) %>%
  mutate(
    Sample_Barcode = str_remove_all(Sample_Barcode, "\\D"),
    Sample_ID = coalesce(as.character(Sample_ID), "unknown"),  
    Sample_ID = if_else(Sample_ID == "0", "0", gsub("^0+", "", Sample_ID)), 
    Clean_Bar_ID = paste(Sample_Barcode, Sample_ID, sep = "_"),
    FREEZER = "LN_racks_UNKNOWN"
  )  %>%
  rename(LN_2ML = Box_map_ID) %>%
  dplyr::select(LN_2ML, Clean_Bar_ID, FREEZER)
# Import and process Top_Freeze data
Top_Freeze <- read_csv("Top_Freezer.csv") %>%
  mutate(
    Barcode = str_remove_all(Barcode, "\\D"),
    ID = coalesce(as.character(ID), "unknown"),  
    ID = if_else(ID == "0", "0", gsub("^0+", "", ID)),
    Clean_Bar_ID = paste(Barcode, ID, sep = "_"),
    FREEZER = "WALLY_rack_b1|d2",
    NOTES = coalesce(Notes, "")
  )  %>%
  rename(top2ML = Box_map_ID) %>%
  dplyr::select(top2ML, NOTES, Clean_Bar_ID, FREEZER)

##Import and process FECAL Manifest
FECAL_CM_May13 <- import("FECAL_CM_May13.csv")

colnames(FECAL_CM_May13)[9] <- "FREEZER_FECAL"
colnames(FECAL_CM_May13)[6] <- "NAP_fecal"
colnames(FECAL_CM_May13)[7] <- "Formalin_fecal"
colnames(FECAL_CM_May13)[8] <- "ETOH_fecal"
colnames(FECAL_CM_May13)[2] <- "DATE_FECAL"

FECAL_CM_May13 <- FECAL_CM_May13 %>%
  mutate(
    DATE = as.Date(str_replace(DATE_FECAL, "th", ""), format = "%d %B %Y"),
    Barcode = str_remove_all(BARCODE, "\\D"),
    ID = coalesce(as.character(ID), "unknown") %>% gsub("^0+", "", .),
    Clean_Bar_ID = paste(Barcode, ID, sep = "_"),
    Combined_Fecal = paste(NAP_fecal, Formalin_fecal, ETOH_fecal, sep = " ")
  ) %>%
  select(Combined_Fecal, Clean_Bar_ID, FREEZER_FECAL)
##
head(FECAL_CM_May13)
dim(FECAL_CM_May13)

##Samples SELECTED for NMR
NMR_BOXES <- import("NMR_BOXES.csv")
head(NMR_BOXES)
#Merge BARCODE ID into Clean_Bar_ID
NMR_BOXES <- NMR_BOXES %>%
  mutate(
    Barcode = str_remove_all(BARCODE, "\\D"),
    ID = coalesce(as.character(ID), "unknown") %>% gsub("^0+", "", .),
    Clean_Bar_ID = paste(Barcode, ID, sep = "_"),
    FREEZER = "Wally_rack_b2"
  ) %>%
  dplyr::select(NMR_BOX_MAP_ID, Clean_Bar_ID, Previous_Box_Position)
# Function to merge dataframes and handle duplicated columns from merging
merge_and_handle_duplicates <- function(df1, df2) {
  merged_df <- merge(df1, df2, by = "Clean_Bar_ID", all = TRUE, suffixes = c(".x", ".y"))
  cols <- grep("\\.x$|\\.y$", names(merged_df), value = TRUE)
  base_cols <- unique(sub("\\.x$|\\.y$", "", cols))
  for (base_col in base_cols) {
    col_x <- paste0(base_col, ".x")
    col_y <- paste0(base_col, ".y")
    if (col_x %in% names(merged_df) && col_y %in% names(merged_df)) {
      merged_df[[base_col]] <- ifelse(is.na(merged_df[[col_x]]), merged_df[[col_y]], 
                                      ifelse(is.na(merged_df[[col_y]]), merged_df[[col_x]], 
                                             paste(merged_df[[col_x]], merged_df[[col_y]], sep = "|")))
      merged_df <- dplyr::select(merged_df, -c(col_x, col_y))
    }
  }
  return(merged_df)
}
# Sequential merging using the refined function
Meta_Wally <- merge_and_handle_duplicates(All_data, Freezer_wally)
Meta_Wally_LN <- merge_and_handle_duplicates(Meta_Wally, two_ml_tubes)
Meta_Wally_LN_p5ml <- merge_and_handle_duplicates(Meta_Wally_LN, p5ml_tubes)
Meta_Wally_LN_p5ml_Top <- merge_and_handle_duplicates(Meta_Wally_LN_p5ml, Top_Freeze)

##add Mpala samples
Meta_Wally_LN_p5ml_Top <- merge_and_handle_duplicates(Meta_Wally_LN_p5ml_Top, FECAL_CM_May13)
##Add NMR samples
Meta_Wally_LN_p5ml_Top <- merge_and_handle_duplicates(Meta_Wally_LN_p5ml_Top, NMR_BOXES)
# Final summarization and data clean-up
Meta_Wally_LN_p5ml_Top <- Meta_Wally_LN_p5ml_Top %>%
  group_by(Clean_Bar_ID) %>%
  summarise(across(everything(), ~paste(unique(.), collapse = ","), .names = "{.col}"),
            .groups = "drop")

# Import non-matching data
all_non_matching_MANUAL <- import("all_non_matching_MANUAL.csv")
##Merge to Meta_Wally_LN_p5ml using Clean_Bar_ID and NEW_label columns
all_non_matching_MANUAL$Box_map_ID <- paste(all_non_matching_MANUAL$Box_map_ID_Top_Freeze, 
                                            all_non_matching_MANUAL$Boxmap_ID_WALLY,
                                            all_non_matching_MANUAL$Box_map_ID_LN,
                                            all_non_matching_MANUAL$Box_map_ID_p5ml, sep = "|")
all_non_matching_MANUAL <- dplyr::select(all_non_matching_MANUAL, c(2, 7, 8))
names(all_non_matching_MANUAL)[2] <- "NOTES"
names(all_non_matching_MANUAL)[1] <- "Clean_Bar_ID"
all_non_matching_MANUALadded <- merge_and_handle_duplicates(Meta_Wally_LN_p5ml_Top, all_non_matching_MANUAL)
colnames(all_non_matching_MANUALadded)
dim(all_non_matching_MANUALadded)
all_non_matching_MANUALadded <- all_non_matching_MANUALadded %>%
  group_by(Clean_Bar_ID) %>%
  slice(1) %>%
  ungroup()
dim(all_non_matching_MANUALadded)
##Load other sample information
Vanderbilt_data <- import("Clean_samples_concatenateedited.xlsx", which = "Vanderbilt_samples")
Vanderbilt_data <- dplyr::select(Vanderbilt_data, c(1,3))
names(Vanderbilt_data)[2] <- "Vanderbilt"
Metabolomics_STUFF <- import("batchMap_with_Meta.csv")
Metabolomics_STUFF <- dplyr::select(Metabolomics_STUFF, c(1,2))

all_non_matching_MANUALadded <- merge(all_non_matching_MANUALadded, Metabolomics_STUFF, by = "Unique.ID", all.x = TRUE)
all_non_matching_MANUALadded <- merge(all_non_matching_MANUALadded, Vanderbilt_data, by = "Unique.ID", all.x = TRUE)
# Write the final merged and cleaned data to a CSV file
dim(all_non_matching_MANUALadded)
#write.csv(all_non_matching_MANUALadded, "Serum_Samples_Apr_26.csv", row.names = FALSE)

##read in the PBMC Sep 
PBMC_Sep <- import("FRPsamples_Sep_TurkanaOnly_forManifest.txt")
names(PBMC_Sep)[2] <- "Sep_Processed"
names(PBMC_Sep)[3] <- "Sep_QC"
PBMC_Cryo <- import("FRPsamples_CPT_TurkanaOnly_forManifest.txt")
names(PBMC_Cryo)[2] <- "CPT_Processed"
names(PBMC_Cryo)[3] <- "CPT_QC"
##Merge the two dataframes by Unique.ID
PBMC_all <- merge(PBMC_Sep, PBMC_Cryo, by = "Unique.ID", all = TRUE)
##Merge to Larger Data
PBMC_X_SERUM <- merge(PBMC_all, all_non_matching_MANUALadded, by = "Unique.ID", all = TRUE)
##Drop NA in the Unique.ID column
PBMC_X_SERUM <- PBMC_X_SERUM %>% drop_na(Unique.ID)
##Select relevnt columns for Manifest
PBMC_X_SERUM <- dplyr::select(PBMC_X_SERUM, c("Unique.ID", "NMR_BOX_MAP_ID", "Previous_Box_Position","Sep_Processed", "Sep_QC", "CPT_Processed", "CPT_QC", "WALLY_2ML", "LN_2ML", "p5ML", "top2ML","Combined_Fecal", "FREEZER", "FREEZER_FECAL","NOTES","Age", "Sex", "Sampling.location","metabRunNum", "Vanderbilt"))

dim(PBMC_X_SERUM)
colnames(PBMC_X_SERUM)

##

##

##

##

##ADD BLOOD DNA DATA
#./CHARLESDATA.py AND ./BDNA_BAGS.py python script to organize the data accordingly
# Import the first sheet from the Excel file
input_filename <- "bDNA_CM_MAY_15.xlsx"
# Import the first sheet from the Excel file
Bdna <- read_excel(input_filename, sheet = 3)
head(Bdna)
long_data <- data %>%
  pivot_longer(cols = everything(), names_to = "Bag", values_to = "Unique_ID") %>%
  filter(!is.na(Unique_ID))  # Remove rows with NA
# Display the transformed data
print(transformed_data)



# Construct the filename with the current date
filename <- paste0("PBMC_X_SERUM_", formatted_date, ".csv")
# Write the DataFrame to a CSV file with the dynamic filename
write.csv(PBMC_X_SERUM, filename, row.names = FALSE)

# Writing Box Maps with p5ml tubes as example
input_filename <- paste0("/Users/Bm0211/RegEx/Water_Energy/PBMC_X_SERUM_", formatted_date, ".csv")
output_filename <- paste0("Sample_Position_Grids_p5Ml_", formatted_date, ".xlsx")
# Reading Sample Manifest
Sample_manifest <- read_csv(input_filename)
# Select necessary columns
sample_map_meta <- select(Sample_manifest, Unique.ID, p5ML)  # Change to "p5ML", "top2ML", "LN_2ML", "WALLY_2ML"
# Preprocess the data to handle multiple identifiers
sample_map_meta <- sample_map_meta %>%
  separate_rows(p5ML, sep = ",") %>%
  mutate(
    # Extract the box type and number, ensuring any trailing underscores are handled
    Box_Number = gsub("(.*_)[0-9]+$", "\\1", p5ML),
    # Extract the last numeric sequence for position
    Position = as.numeric(gsub("^.*_([0-9]+)$", "\\1", p5ML))
  ) %>%
  drop_na(Position)
# Initialize a workbook
wb <- createWorkbook()
# Process each unique box
unique_boxes <- unique(sample_map_meta$Box_Number)
for (box in unique_boxes) {
  box_data <- filter(sample_map_meta, Box_Number == box)
  # Define grid size based on maximum position (assuming a 9x9 grid but check for anomalies)
  grid_size <- 9
  max_position <- max(box_data$Position, na.rm = TRUE)
  if (max_position > 81) {
    grid_size <- 10  # Use a 10x10 grid if positions exceed 81
  }
  # Create a matrix filled with NA for the grid
  grid <- matrix(NA, nrow = grid_size, ncol = grid_size)
  # Populate the grid with Unique.ID based on their Position
  for (i in seq_len(nrow(box_data))) {
    position <- box_data$Position[i]
    unique_id <- box_data$`Unique.ID`[i]
    if (!is.na(position) && position <= grid_size^2) {
      row_index <- ceiling(position / grid_size)
      col_index <- position %% grid_size
      col_index <- ifelse(col_index == 0, grid_size, col_index)
      # Assign the unique ID to the grid cell
      grid[row_index, col_index] <- unique_id
    }
  }
  # Convert the matrix to a data frame for easier handling/display
  grid_df <- as.data.frame(grid)
  # Add the grid to the workbook, named by the box number
  sheet_name <- gsub("_$", "", box)  # Clean up the box number for the sheet name
  addWorksheet(wb, sheetName = sheet_name)
  writeData(wb, sheet = sheet_name, grid_df)
}
# Save workbook with the dynamic filename
saveWorkbook(wb, output_filename, overwrite = TRUE)

##Read in the Manifest and remove those that were chosen for NMR
Manifest_2 <- import(input_filename)
# Function to remove NMR_BOX_MAP_IDs from Manifest
remove_ids <- function(column, ids_to_remove) {
  sapply(column, function(cell) {
    if (is.na(cell)) return(cell)
    items <- unlist(str_split(cell, ","))
    items <- items[!items %in% ids_to_remove]
    paste(items, collapse = ",")
  })
}
# Get the unique identifiers to remove
ids_to_remove <- Manifest_2$Previous_Box_Position ##Flexible to remove anything
# Apply the function to the specified columns
Manifest_2 <- Manifest_2 %>%
  mutate(
    WALLY_2ML = remove_ids(WALLY_2ML, ids_to_remove),
    LN_2ML = remove_ids(LN_2ML, ids_to_remove),
    p5ML = remove_ids(p5ML, ids_to_remove),
    top2ML = remove_ids(top2ML, ids_to_remove)
  )
colnames(Manifest_2)
##Drop column "Previous_Box_Position", "Age", "Sex"
Manifest_2 <- dplyr::select(Manifest_2, -c("Previous_Box_Position", "Age", "Sex"))

# Construct the filename with the current date
filename <- paste0("MANIFEST_NMR_REMOVED_", formatted_date, ".csv")
# Write the DataFrame to a CSV file with the dynamic filename
write.csv(Manifest_2, filename, row.names = FALSE)
