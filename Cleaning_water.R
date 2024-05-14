
# Load necessary libraries
library(tidyverse)  # Includes dplyr, tidyr, and more
library(rio)        # For import functionality
library(stringr)    # For string operations

# Import data
All_data <- import("/Users/Bm0211/RegEx/Water_Energy/THGP_database_full_corrected_merged_2024-04-02.txt")

# Household style of life index calculation
All_data <- All_data %>%
  mutate(
    h_sol = ifelse(Number.of.rooms.in.the.household > 1, 1, 0) +
            ifelse(Presence.of.a.finished.floor. == "Yes", 1, 0) +
            ifelse(Presence.of.an.iron.concrete.or.slate.roof. == "Yes", 1, 0) +
            ifelse(Presence.of.electricity. == "Yes", 1, 0) +
            ifelse(Presence.of.flush.toilet. == "Yes", 1, 0) +
            ifelse(Does.the.household.have.indoor.tap.water. == "Yes", 1, 0),
    Clean_Bar_ID = str_extract(Unique.ID, "(?<=_)[0-9]+(?=_)") %>%
                   str_extract(Unique.ID, "(?<=_)[0-9]+$") %>%
                   str_c(., sep = "_")
  ) %>%
  select(Age, Sex, Sampling.location, Unique.ID, Tribe, h_sol, Clean_Bar_ID)

# Load additional datasets and prepare them
Freezer_wally <- read.csv("/users/bm0211/RegEx/Water_Energy/Freezer_Wally.csv") %>%
  mutate(
    Sample_Barcode = str_remove_all(Sample_Barcode, "\\D"),
    Sample_ID = as.character(as.numeric(Sample_ID)),
    Wally_boxes_Bar_ID = str_c(Sample_Barcode, Sample_ID, sep = "_")
  ) %>%
  select(Wally_boxes_Bar_ID, NOTES = 5, FREEZER = 8, 9)

p5ml_tubes <- import("/Users/Bm0211/RegEx/Water_Energy/p5mlCombinedsheet.csv") %>%
  separate(Barcode_ID_p5ml, into = c("Barcode_p5ml", "ID_p5ml"), sep = "_") %>%
  mutate(
    ID_p5ml = as.character(as.numeric(ID_p5ml)),
    Bar_ID_p5ml = str_c(Barcode_p5ml, ID_p5ml, sep = "_"),
    FREEZER = "WALLY"
  ) %>%
  select(Bar_ID_p5ml, NOTES = 5)

two_ml_tubes <- import("/Users/Bm0211/RegEx/Water_Energy/Liq_Nit_Boxes_Combined_sheet.csv") %>%
  mutate(
    LN_boxes_Bar_ID = str_c(Sample_Barcode, as.character(as.numeric(Sample_ID)), sep = "_"),
    FREEZER = "LN"
  ) %>%
  select(LN_boxes_Bar_ID, FREEZER, 5:7)

Top_Freeze <- import("/Users/Bm0211/RegEx/Water_Energy/Top_Freezer.csv") %>%
  mutate(
    TopFre_Barcode_ID = str_c(Barcode, ID, sep = "_"),
    FREEZER = "WALLY"
  ) %>%
  select(TopFre_Barcode_ID, NOTES = 4, 5, 6)

# Merge datasets and handle duplicates
merge_and_handle_duplicates <- function(df1, df2) {
  merge(df1, df2, by = "Clean_Bar_ID", all = TRUE) %>%
    unite_everything(sep = ", ")  # Combining all columns with possible duplicates
}

# Sequential merging
Meta_Wally <- merge_and_handle_duplicates(Meta_ONLY, Freezer_wally)
Meta_Wally_LN <- merge_and_handle_duplicates(Meta_Wally, two_ml_tubes)
Meta_Wally_LN_p5ml <- merge_and_handle_duplicates(Meta_Wally_LN, p5ml_tubes)
Meta_Wally_LN_p5ml_Top <- merge_and_handle_duplicates(Meta_Wally_LN_p5ml, Top_Freeze)

# Output the final merged dataset
write.csv(Meta_Wally_LN_p5ml_Top, "/users/bm0211/RegEx/Water_Energy/final_merged_data.csv", row.names = FALSE)

## install.packages()
library(tidyverse)
library(rio)
library(here)
library(reshape2)
library(dplyr)
library(tidyr)
All_data <- import("/Users/Bm0211/RegEx/Water_Energy/THGP_database_full_corrected_merged_2024-04-02.txt")
dim(All_data)
colnames(All_data$)
table(All_data$Presence.of.an.iron.concrete.or.slate.roof.)
## Household style of life index from Gildner 2020: Market integration and soil-transmitted helminth infection among the Shuar of Amazonian Ecuador
All_data$h_sol <- NA
All_data$h_sol <- ifelse(All_data$Number.of.rooms.in.the.household > 1, 1, 0) +
  ifelse(All_data$Presence.of.a.finished.floor. == "Yes", 1, 0) +
  ifelse(All_data$Presence.of.an.iron.concrete.or.slate.roof. == "Yes", 1, 0) +
  ifelse(All_data$Presence.of.electricity. == "Yes", 1, 0) +
  ifelse(All_data$Presence.of.flush.toilet. == "Yes", 1, 0) +
  ifelse(All_data$Does.the.household.have.indoor.tap.water. == "Yes", 1, 0)
table(All_data$h_sol)
Meta_ONLY <- All_data %>% dplyr::select(c("Age", "Sex", "Sampling.location", "Unique.ID", "Tribe", "h_sol",
                                             "Amount.of.water.you.drink.per.day.in.cups.", "What.is.your.drinking.water.source.", 
                                             "How.often.do.you.retrieve.water.", "Does.the.household.have.indoor.tap.water.",
                                             "Number.of.children"))
Meta_ONLY <- Meta_ONLY %>%
  mutate(
    Clean_Bar_ID = paste(
      str_extract(Unique.ID, "(?<=_)[0-9]+(?=_)"),  # Extract the barcode
      str_extract(Unique.ID, "(?<=_)[0-9]+$"),      # Extract the ID
      sep = "_"
    )
  )
dim(Meta_ONLY)
colnames(Meta_ONLY)
table(Meta_ONLY$What.is.your.drinking.water.source.)
##########################################
##These will be recoded as Reliable water (Tap water, Tank, Springs|Tap water, Tank, Springs, Outside Tap, Piped Water)
##Unreliable water (Piped Water ,Rain Water, River, Lugger, Stream, Pond,
## Valley|Well, Well, Borehole|Well, Borehole|Valley|Well, Borehole|River/Stream, Borehole|Outside Tap
## Borehole|Lugger, Borehole|Lake, )
##Recoding the water source
Meta_ONLY$What.is.your.drinking.water.source. <- recode(Meta_ONLY$What.is.your.drinking.water.source.,
                                                       "Tap water" = "Reliable_water",
                                                       "Tank" = "Reliable_water",
                                                       "Springs" = "Reliable_water",
                                                       "Outside Tap" = "Reliable_water",
                                                       "Piped Water" = "Reliable_water",
                                                       "Springs|Tap water" = "Reliable_water",
                                                       "Borehole|Outside Tap" = "Reliable_water",
                                                       "Rain Water" = "Unreliable_water",
                                                       "Lake|Outside Tap" = "Reliable_water",
                                                       "River" = "Unreliable_water",
                                                       "County Govt Water" = "Reliable_water",
                                                       "Buy Water" = "Reliable_water",
                                                       "Lugger" = "Unreliable_water",
                                                       "Stream" = "Unreliable_water",
                                                       "Pond" = "Unreliable_water",
                                                       "Valley" = "Unreliable_water",
                                                       "Well" = "Unreliable_water",
                                                       "Valley|Well" = "Unreliable_water",
                                                       "Borehole|Well" = "Unreliable_water",
                                                       "Borehole|Valley|Well" = "Unreliable_water",
                                                       "Borehole|River/Stream" = "Unreliable_water",
                                                       "Borehole|Lugger" = "Unreliable_water",
                                                       "Borehole|Lake" = "Unreliable_water",
                                                       "River/Stream|Well" = "Unreliable_water",
                                                       "Oasis" = "Reliable_water",
                                                       " " = "NA",
                                                       "None" = "NA",
                                                       "Not Answered|Other" = "NA")
## Load SAMPLE MAP
table(Meta_ONLY$Number.of.children)
dim(Meta_ONLY)
# Filter out entries in 'number of children' that contain non-digit characters
cleaned_meta <- Meta_ONLY %>%
  filter(grepl("^[0-9]+$", `Number.of.children`))  # This regex matches strings that are entirely numeric
# View the cleaned dataframe
dim(cleaned_meta)
cleaned_meta <- cleaned_meta %>%
  mutate(`Number.of.children` = as.character(`Number.of.children`)) %>%
  filter(grepl("^[0-9]+$", `Number.of.children`))
##Boxplot with Water source (x - axis) and number of children
ggplot(cleaned_meta, aes(x = `What.is.your.drinking.water.source.`, y = `Number.of.children`)) +
  geom_boxplot() +
  labs(x = "Water Source", y = "Number of Children") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##table of the sampling locations of those places where the water source IS NA
table()
table(cleaned_meta$Sampling.location, cleaned_meta$`What.is.your.drinking.water.source.`)
##Impute the drinking water source based on the sampling location whereby
##If the sampling location is "Rumuruti", "Kambi Robert-Rumuruti" the drinking water source is "Reliable_water"
##If the sampling location is "Ikoree-Katilu" the drinking water source is "Unreliable_water"
cleaned_meta$`What.is.your.drinking.water.source.` <- ifelse(cleaned_meta$Sampling.location %in% c("Rumuruti", "Kitale", "Segera", "Endana","Juakali", "Kariunga"),
                                                             "Reliable_water",
                                                             ifelse(cleaned_meta$Sampling.location == c("Ikoree-Katilu", "Kambi Robert-Rumuruti"), "Borehole", cleaned_meta$`What.is.your.drinking.water.source.`))

library(dplyr)

# Assuming cleaned_meta is already loaded and contains the 'Sampling.location' column


# Optionally print out a summary or the head of the dataframe to verify changes
print(head(cleaned_meta))
table(cleaned_meta$`What.is.your.drinking.water.source.`)
##Freezer wally QR Codes have THGP in them that needs to be removed
Freezer_wally <- read.csv("/users/bm0211/RegEx/Water_Energy/Freezer_Wally.csv")
# Clean the Barcode column to only have digits
Freezer_wally <- Freezer_wally %>%
  mutate(Sample_Barcode = str_remove_all(Sample_Barcode, "\\D")) %>%
  mutate(Sample_ID = sprintf("%1d", as.numeric(Sample_ID)))
##Join the Sample_Barcode Sample_ID columns into one column called LN_boxes_Bar_ID seperated by "_"
Freezer_wally <- Freezer_wally %>%
  mutate(Wally_boxes_Bar_ID = paste(Sample_Barcode, Sample_ID, sep = "_"))
names(Freezer_wally)[5] <- "NOTES"
Freezer_wally$FREEZER <- "WALLY"
head(Freezer_wally)
colnames(Freezer_wally)
Freezer_wally <- Freezer_wally %>% select(c(1, 5, 8, 9))
colnames(Freezer_wally)
names(Freezer_wally)[3] <- "Clean_Bar_ID"
##0.5ml tubes
p5ml_tubes <- import("/Users/Bm0211/RegEx/Water_Energy/p5mlCombinedsheet.csv")
##Seperate the Barcode_ID_p5ml by "_" label it Barcode_p5ml and ID_p5ml
p5ml_tubes <- p5ml_tubes %>%
  separate(Barcode_ID_p5ml, into = c("Barcode_p5ml", "ID_p5ml"), sep = "_", remove = FALSE)
##Remove 0 preceeding single digits in ID_p5ml column
p5ml_tubes <- p5ml_tubes %>%
  mutate(ID_p5ml = sprintf("%1d", as.numeric(ID_p5ml)))
##Join the Barcode_p5ml ID_p5ml columns into Bar_ID_p5ml seperated by "_"
p5ml_tubes <- p5ml_tubes %>%
  mutate(Bar_ID_p5ml = paste(Barcode_p5ml, ID_p5ml, sep = "_"))
colnames(p5ml_tubes)
p5ml_tubes$FREEZER <- "WALLY"
names(p5ml_tubes)[5] <- "NOTES"
p5ml_tubes <- p5ml_tubes %>% select(c(4:7))
colnames(p5ml_tubes)
names(p5ml_tubes)[3] <- "Clean_Bar_ID"
##2ml tubes in LN ...Here the ID has two digits eg, 09 instead of 9
##Use Barcode_ID_LN to merge with Meta_ONLY ....###...#####
two_ml_tubes <- import("/Users/Bm0211/RegEx/Water_Energy/Liq_Nit_Boxes_Combined_sheet.csv", header = TRUE)
two_ml_tubes <- dplyr::select(two_ml_tubes, c(1, 2, 3, 4, 5))
##Join the Sample_Barcode Sample_ID columns into one column called LN_boxes_Bar_ID seperated by "_"
two_ml_tubes <- two_ml_tubes %>%
  mutate(LN_boxes_Bar_ID = paste(Sample_Barcode, Sample_ID, sep = "_"))
colnames(two_ml_tubes)
two_ml_tubes$FREEZER <- "LN"
two_ml_tubes <- two_ml_tubes %>% select(c(1, 5:7))
colnames(two_ml_tubes)
names(two_ml_tubes)[2] <- "Clean_Bar_ID"
##Load the Topfreeze data
Top_Freeze <- import("/Users/Bm0211/RegEx/Water_Energy/Top_Freezer.csv")
##merge Barcode and ID into TopFre_Barcode_ID 
Top_Freeze <- Top_Freeze %>%
  mutate(TopFre_Barcode_ID = paste(Barcode, ID, sep = "_"))
Top_Freeze$FREEZER <- "WALLY"
names(Top_Freeze)[4] <- "NOTES"
colnames(Top_Freeze)
Top_Freeze <- Top_Freeze %>% select(c(1, 4, 5, 6))

##Files to merge
##0.5ml tubes
##2ml tubes in LN
##Freezer wally
##Top_Freeze_MovedApril2024
# head(Meta_ONLY)




colnames(Meta_ONLY)

# Function to merge dataframes and handle duplicated columns from merging
merge_and_handle_duplicates <- function(df1, df2) {
  merged_df <- merge(df1, df2, by = "Clean_Bar_ID", all = TRUE, suffixes = c(".x", ".y"))
  # Find columns that are duplicated as a result of the merge
  duplicated_cols <- names(merged_df)[duplicated(sub("\\.x$|\\.y$", "", names(merged_df)))]
  original_cols <- unique(gsub("\\.x$|\\.y$", "", duplicated_cols))
  # Handle duplicated columns
  for (col in original_cols) {
    col_x <- paste0(col, ".x")
    col_y <- paste0(col, ".y")
    if (col_x %in% names(merged_df) && col_y %in% names(merged_df)) {
      # Combine the information from both columns into one and remove NAs
      merged_df[col] <- apply(merged_df[c(col_x, col_y)], 1, function(x) paste(na.omit(unique(x)), collapse = " , "))
      # Drop the original .x and .y columns
      merged_df <- merged_df %>% select(-one_of(c(col_x, col_y)))
    } else if (col_x %in% names(merged_df)) {
      # Rename back to original if only one exists
      names(merged_df)[names(merged_df) == col_x] <- col
    } else if (col_y %in% names(merged_df)) {
      names(merged_df)[names(merged_df) == col_y] <- col
    }
  }
  return(merged_df)
}

# Sequential merging
Meta_Wally <- merge_and_handle_duplicates(Meta_ONLY, Freezer_wally)
Meta_Wally_LN <- merge_and_handle_duplicates(Meta_Wally, two_ml_tubes)
Meta_Wally_LN_p5ml <- merge_and_handle_duplicates(Meta_Wally_LN, p5ml_tubes)
Meta_Wally_LN_p5ml_Top <- merge_and_handle_duplicates(Meta_Wally_LN_p5ml, Top_Freeze)

# Inspect the results
head(Meta_Wally_LN_p5ml_Top)
colnames(Meta_Wally_LN_p5ml_Top)

##Concatenate Duplicate Unique.ID 
Meta_Wally_LN_p5ml_Top$Unique.ID <- as.character(Meta_Wally_LN_p5ml_Top$Unique.ID)
Meta_Wally_LN_p5ml_Top <- Meta_Wally_LN_p5ml_Top %>%
  group_by(Unique.ID) %>%
  summarise(across(everything(), ~paste(unique(.), collapse = ","), .names = "{.col}"),
            .groups = "drop")
##Remove duplicate entries in the cells of the Box_map_ID column e.g. "Box_10_2024_11 , TopFrez_Box1_60,Box_10_2024_11 ,TopFrez_Box2_40" has a duplicate entry
Meta_Wally_LN_p5ml_Top$Box_map_ID <- sapply(strsplit(Meta_Wally_LN_p5ml_Top$Box_map_ID, "\\,"), function(x) paste(unique(x), collapse = "|"))
# Write the final merged data to a CSV file
write.csv(Meta_Wally_LN_p5ml_Top, "/users/bm0211/RegEx/Water_Energy/final_merged_data.csv", row.names = FALSE)


# Perform merges and capture non-matching entries, followed by cleaning
Meta_Wally <- merge(Meta_ONLY, Freezer_wally, by.x = "Clean_Bar_ID", by.y = "Wally_boxes_Bar_ID", all = TRUE)
non_matching_wally <- Meta_Wally %>% 
  filter(is.na(Unique.ID)) %>%
  mutate(Clean_Bar_ID = ifelse(Clean_Bar_ID == "_NA", NA, Clean_Bar_ID)) %>%
  drop_na(Clean_Bar_ID)
##Merge 1 with LN boxes
Meta_Wally_LN <- merge(Meta_Wally, two_ml_tubes, by.x = "Clean_Bar_ID", by.y = "LN_boxes_Bar_ID", all = TRUE)
non_matching_ln <- Meta_Wally_LN %>% 
  filter(is.na(Unique.ID)) %>%
  mutate(Clean_Bar_ID = ifelse(Clean_Bar_ID == "_NA", NA, Clean_Bar_ID)) %>%
  drop_na(Clean_Bar_ID)
##Merge 2 with 0.5ml tubes
Meta_Wally_LN_p5ml <- merge(Meta_Wally_LN, p5ml_tubes, by.x = "Clean_Bar_ID", by.y = "Bar_ID_p5ml", all = TRUE)
non_matching_p5ml <- Meta_Wally_LN_p5ml %>% 
  filter(is.na(Unique.ID)) %>%
  mutate(Clean_Bar_ID = ifelse(Clean_Bar_ID == "_NA", NA, Clean_Bar_ID)) %>%
  drop_na(Clean_Bar_ID)
##Merge 3 with Top Freeze
Meta_Wally_LN_p5ml_Top <- merge(Meta_Wally_LN_p5ml, Top_Freeze, by.x = "Clean_Bar_ID", by.y = "TopFre_Barcode_ID", all = TRUE)
non_matching_top <- Meta_Wally_LN_p5ml_Top %>% 
  filter(is.na(Unique.ID)) %>%
  mutate(Clean_Bar_ID = ifelse(Clean_Bar_ID == "_NA", NA, Clean_Bar_ID)) %>%
  drop_na(Clean_Bar_ID)
# Combine all non-matching data frames into one by merging them based on Clean_Bar_ID
all_non_matching <- reduce(list(non_matching_wally, non_matching_ln, non_matching_p5ml, non_matching_top), full_join, by = "Clean_Bar_ID")
# Group by Clean_Bar_ID and concatenate information
all_non_matching <- all_non_matching %>%
  group_by(Clean_Bar_ID) %>%
  summarise(across(everything(), ~paste(unique(na.omit(.)), collapse = "|"), .names = "{.col}_concatenated"),
            .groups = "drop")
dim(all_non_matching)
# Write the combined non-matching data to a single file
write.csv(all_non_matching, "/users/bm0211/RegEx/Water_Energy/all_non_matching.csv", row.names = FALSE)
#final dataset
colnames(Meta_Wally_LN_p5ml_Top)
##Concatenate Box_map_ID_p5ml, Box_map_ID_LN and Wally_Box_Map_ID into one column separated by "|"
Meta_Wally_LN_p5ml_Top$Box_map_ID_All <- paste(Meta_Wally_LN_p5ml_Top$Boxmap_ID_WALLY, Meta_Wally_LN_p5ml_Top$Box_map_ID_LN, Meta_Wally_LN_p5ml_Top$Box_map_ID_p5ml, Meta_Wally_LN_p5ml_Top$Box_map_ID_Top_Freeze, sep = ",")
##Concatenate Duplicate Unique.ID rows into one row separated by "|"
Meta_Wally_LN_p5ml_Top$Unique.ID <- as.character(Meta_Wally_LN_p5ml_Top$Unique.ID)
Meta_Wally_LN_p5ml_Top <- Meta_Wally_LN_p5ml_Top %>%
  group_by(Unique.ID) %>%
  summarise(across(everything(), ~paste(unique(.), collapse = ","), .names = "{.col}_concatenated"),
            .groups = "drop")
dim(Meta_Wally_LN_p5ml_Top)
##  Load all_non_matching_MANUAL.csv after adding "NEW_label" column WITH MANUAL FIX
all_non_matching_MANUAL <- read.csv("/users/bm0211/RegEx/Water_Energy/all_non_matching_MANUAL.csv")
head(all_non_matching_MANUAL)
##Merge to Meta_Wally_LN_p5ml using Clean_Bar_ID and NEW_label columns
Meta_Wally_LN_p5ml_non_matching <- merge(Meta_Wally_LN_p5ml_Top, all_non_matching_MANUAL, by.x = "Clean_Bar_ID_concatenated", by.y = "NEW_label", all.x = TRUE)
##Write the final data to a CSV file
# Write the final merged data frame to a CSV ##############################################################################
##Concatenate NEW boxMAP_ID into the concatenated column separated by "|"
Meta_Wally_LN_p5ml_non_matching$Box_map_ID_All_EDIT <- paste(Meta_Wally_LN_p5ml_non_matching$Box_map_ID_All, 
                                            Meta_Wally_LN_p5ml_non_matching$Boxmap_ID_WALLY,
                                            Meta_Wally_LN_p5ml_non_matching$Box_map_ID_LN,
                                            Meta_Wally_LN_p5ml_non_matching$Box_map_ID_p5ml, sep = ",")
#write.csv(Meta_Wally_LN_p5ml_non_matching, "/users/bm0211/RegEx/Water_Energy/Meta_Wally_LN_p5ml_EDIT_Apr25.csv", row.names = FALSE)
###Delete columns that are not needed
##i did it manually so that I can also look at the data
##Further process after manual curation
colnames(Meta_Wally_LN_p5ml_non_matching)
dim(Meta_Wally_LN_p5ml_non_matching)
# Load the curated data ##############################################################################
curated_data <- select(Meta_Wally_LN_p5ml_non_matching, c(1,2,5,7:13,18)) #read.csv("/users/bm0211/RegEx/Water_Energy/Samples_Combined_All.csv")
colnames(curated_data)
##
##remove NA in the BoxMAP column concatenated
curated_data$Box_map_ID_All_EDIT <- gsub("NA\\|", "", curated_data$Box_map_ID_All_EDIT)
curated_data$Box_map_ID_All_EDIT <- gsub("\\|NA", "", curated_data$Box_map_ID_All_EDIT)
##rEMOVE ANY "|" AT THE END OR BEGINNING
curated_data$Box_map_ID_All_EDIT <- gsub("^\\|", "", curated_data$Box_map_ID_All_EDIT)
curated_data$Box_map_ID_All_EDIT <- gsub("\\|$", "", curated_data$Box_map_ID_All_EDIT)

head(curated_data)
length(table(curated_data$Box_map_ID_All_EDIT))
dim(curated_data)
##Attach those sent to Vanderbilt
##Load the excel file Clean_samples_concatenateedited, sheet Vanderbilt_samples
Vanderbilt_data <- import("/Users/Bm0211/RegEx/Water_Energy/Clean_samples_concatenateedited.xlsx", which = "Vanderbilt_samples")
Vanderbilt_data <- select(Vanderbilt_data, c(1,3))
names(Vanderbilt_data)[2] <- "Vander_S"
##Merge curated_data with Vanderbilt_data
curated_data <- merge(curated_data, Vanderbilt_data, by = "Unique.ID", all = TRUE)
###Clean the Box_map_ID_All_concatenated column to remove duplicate box maps
##add metabolomics data
Metabolomics_STUFF <- import("/Users/Bm0211/RegEx/Water_Energy/batchMap_with_Meta.csv")
##Pivot Metabolomics_STUFF wider to get the metabolites as columns
colnames(Metabolomics_STUFF)
Metabolomics_STUFF <- select(Metabolomics_STUFF, c(1,2))
head(Metabolomics_STUFF)
##Merge curated_data with Metabolomics_STUFF by Unique.ID
curated_data_METAB <- merge(curated_data, Metabolomics_STUFF, by = "Unique.ID", all = TRUE)
dim(curated_data)
# Split the data in the 'all_combined' column by "|"
#curated_data_METAB$Box_map_ID_All_EDIT <- sapply(strsplit(curated_data_METAB$Box_map_ID_All_EDIT, "\\,"), function(x) paste(unique(x), collapse = ","))
##Count the tubes in each box 
# Replace NA with empty string, then count the number of elements in each cell
curated_data_METAB$Box_map_ID_All_EDIT[curated_data_METAB$Box_map_ID_All_EDIT == "<NA>"] <- ""
curated_data_METAB$Box_map_ID_All_EDIT[curated_data_METAB$Box_map_ID_All_EDIT == "NA"] <- ""
# Count the number of elements in each cell
curated_data_METAB$Number_Available <- sapply(strsplit(curated_data_METAB$Box_map_ID_All_EDIT, "\\,"), function(x) length(x))
head(curated_data_METAB)

write.csv(curated_data_METAB, "/users/bm0211/RegEx/Water_Energy/Serum_Samples_April_25.csv", row.names = FALSE)
######
################
##########################
###############
######
Sample_manifest <- import("/Users/Bm0211/RegEx/Water_Energy/PBMC_X_SERUM_MAY_1.csv")
sample_map_meta <- select(Sample_manifest, c(1,6,7,8,9,12))
colnames(sample_map_meta)
# Extract box number and position from Box_map_ID
data <- sample_map_meta %>%
  mutate(
    Box_Number = as.numeric(gsub("Box_([0-9]+)_.*", "\\1", LN_2ML)),
    Position = as.numeric(gsub("Box_[0-9]+_.*_([0-9]+)", "\\1", LN_2ML))
  )
# Create an empty list to store the data frames (9x9 grids)
grid_list <- list()
# Loop through each unique box number
for (box in unique(data$Box_Number)) {
  # Filter data for the current box
  box_data <- filter(data, Box_Number == box)
  # Always create a 9x9 matrix filled with NA
  grid <- matrix(NA, nrow = 9, ncol = 9)
  # Fill the grid with Position values based on their Position
  for (i in 1:nrow(box_data)) {
    position <- box_data$Position[i]
    if (!is.na(position)) {
      # Calculate row and column index in the grid
      row_index <- ceiling(position / 9)
      col_index <- position %% 9
      col_index <- ifelse(col_index == 0, 9, col_index)
      # Assign the Position to the correct position in the grid
      grid[row_index, col_index] <- box_data$Unique.ID[i]
    }
  }
  # Convert the matrix to a data frame for easier handling/display
  grid_df <- as.data.frame(grid)
  # Add the grid to the list, named by the box number
  grid_list[[paste0("Box_", box)]] <- grid_df
}

# View the grids in the list
print(grid_list[["Box_1"]])
# If you want to write the grids to files, you can loop through the list
for (box_name in names(grid_list)) {
  write.csv(grid_list[[box_name]], paste0("/users/bm0211/Downloads/data", box_name, "_grid.csv"), row.names = FALSE)
  # Note: Adjust file paths as needed for your environment.
}


#Freezer_wally dataframe and Metabo_samples ##############################################################################
result <- custom_merge(Meta_ONLY, Freezer_wally, by1 = "Unique.ID", by2 = "Unique.ID")
head(result$merged_df)
# The merged dataframe ##############################################################################
Meta_Wally <- result$merged_df
# Data frame of non-matching IDs ##############################################################################
non_matching_IDs <- result$non_matching_ID
# Continue merging with other dataframes and appending non-matching IDs
##Merge 1 with LN boxes ##############################################################################
result_LN <- custom_merge(Meta_Wally, two_ml_tubes, by1 = "Unique.ID", by2 = "LN_boxes_UniqueID")
Metab_Wally_LNtubes <- result_LN$merged_df
head(Metab_Wally_LNtubes)
table(is.na(Metab_Wally_LNtubes$Git_Meta_BC_ID))
non_matching_IDs <- rbind(non_matching_IDs, result_LN$non_matching_ID)
##Merge 2 with 0.5ml tubes ##############################################################################
result_p5ml <- custom_merge(Metab_Wally_LNtubes, p5ml_tubes, by1 = "Git_Meta_BC_ID", by2 = "Barcode_ID_p5ml")
Metab_Wally_LNtubes_p5ml <- result_p5ml$merged_df
non_matching_IDs <- rbind(non_matching_IDs, result_p5ml$non_matching_ID)
# Clean the BoxMap column ##############################################################################
Metab_Wally_LNtubes_p5ml$Box_map_ID <- gsub("NA\\|?", "", Metab_Wally_LNtubes_p5ml$Box_map_ID)
colnames(Metab_Wally_LNtubes_p5ml)
# Write the final merged data frame to a CSV ##############################################################################
write.csv(Metab_Wally_LNtubes_p5ml, "/users/bm0211/RegEx/Water_Energy/Metab_Wally_LNtubes_p5ml.csv", row.names = FALSE)

# Write the non-matching IDs to a CSV file ##############################################################################
write.csv(non_matching_IDs, "/users/bm0211/RegEx/Water_Energy/non_matching_IDs.csv", row.names = FALSE)

##Further process after manual curation
# Load the curated data ##############################################################################
curated_data <- read.csv("/users/bm0211/RegEx/Water_Energy/Clean_samples.csv")
head(curated_data)
dim(curated_data)
colnames(curated_data)
## Check the colum Git_Meta_BC_ID for duplicates and merge their data separated by "|"

# Check for duplicate IDs and concatenate data, separate with "|"
curated_data_concatenated <- curated_data %>%
  group_by(Git_Meta_BC_ID) %>%
  summarise(across(everything(), ~paste(unique(.), collapse = "|"), .names = "{.col}_concatenated"),
            .groups = "drop") 
head(curated_data_concatenated)
dim(curated_data_concatenated)
# Write the concatenated data to a CSV file
write.csv(curated_data_concatenated, "/users/bm0211/RegEx/Water_Energy/Clean_samples_concatenated.csv", row.names = FALSE)
##########################################
###Adding the processed samples
##Those that went through metabolomics
colnames(Metabolomics_Pivoted)
dim(Metabolomics_Pivoted)
Metabo_samples <- dplyr::select(Metabolomics_Pivoted, c(1, 2, 3, 4, 6))
head(Metabo_samples)
dim(Metabo_samples)

##Merge metabo with entire data to figure out samples that went through metabolomics
Metabo_samples <- merge(Metabo_samples, Meta_ONLY, by = "Unique.ID", all = TRUE)
colnames(Metabo_samples)
# Create a new column 'Metabo_barcode' by extracting the barcode from 'Unique.ID'
##Use the barcode to merge Freezer_wally and Metabo_samples
Metabo_samples <- Metabo_samples %>%
  mutate(Metabo_barcode = str_extract(Unique.ID, "(?<=_)(\\d+)(?=_)"))







####Select columns and rename them "Total.cholesterol.mg.dL", "HDL.cholesterol.mg.dL", "Triglycerides.mg.dL","LDL.cholesterol.mg.dL"   
Some_meta <- All_data %>% dplyr::select(c("Unique.ID", "Total.cholesterol.mg.dL.", "HDL.cholesterol.mg.dL", "Triglycerides.mg.dL", "LDL.cholesterol.mg.dL"))
dim(Some_meta)
##Extract the date, barcode and ID from the Unique.ID
Some_meta <- Some_meta %>%
  mutate(
    Date = str_extract(Unique.ID, "^[0-9]+-[0-9]+-[0-9]+"),
    Barcode = str_extract(Unique.ID, "(?<=_)[0-9]+(?=_)"),
    ID = str_extract(Unique.ID, "(?<=_)[0-9]+$")
  )
head(Some_meta)
##########################################
##########merge with merge_data

#####
## Sample map Maneno data <- read.csv("/users/bm0211/Downloads/2ml-Tube-Serum_Box_Map.csv")



#####
##### correlation
for_corr <- Meta_ONLY_Water_corrected %>%
  dplyr::select(Unique.ID, Gender_META, Fetch_Water)
head(for_corr)
dim(for_corr)
###Import the Accelerometer Data
Accelerometer_data <- import("/Users/bm0211/Downloads/turkana_mvpa_8Mar2024.txt")
# Process the data
Accelerometer_data <- Accelerometer_data %>%
  separate(unique_id, into = c("Barcode", "ID", "Date"), sep = "_") %>%
  mutate(ID = sprintf("%02d", as.numeric(ID)),
         Unique.ID = paste(Date, Barcode, ID, sep = "_"))
# View the data
Accelerometer_data <- dplyr::select(Accelerometer_data, c(Unique.ID, AD_MVPA_E5S_B1M80._T100_ENMO_0.24hr, h_sol))
head(Accelerometer_data)
# View the dimensions of the data
dim(Accelerometer_data)
colnames(Cleaned_metadata_file)
Accelerometer_fetch <- merge(Accelerometer_data, for_corr, by = "Unique.ID", all.x = TRUE)
colnames(Accelerometer_fetch)
head(Accelerometer_fetch, 100)
dim(Accelerometer_fetch)
names(Accelerometer_fetch)[2] <- "MVPA"

# Assuming 'Accelerometer_fetch' is already created and column names are set
# Transform 'Fetch_Water' into numeric levels directly
Accelerometer_fetch$Fetch_Water <- case_when(
  Accelerometer_fetch$Fetch_Water %in% c("Most_of_the_day") ~ 3,
  Accelerometer_fetch$Fetch_Water %in% c("Less_than_1_hour") ~ 1,
  Accelerometer_fetch$Fetch_Water %in% c("A_few_hours") ~ 2,
  TRUE ~ NA_real_  # Assign NA to unmatched cases or consider another appropriate action
)
table(Accelerometer_fetch$Fetch_Water)
table(Accelerometer_fetch$MVPA)
# Convert 'MVPA' to numeric if it's not already
Accelerometer_fetch$MVPA <- as.numeric(Accelerometer_fetch$MVPA)

# Handle NA values before scaling and normalization
# Assuming you want to exclude rows with NA in either 'Fetch_Water' or 'MVPA'
Accelerometer_fetch <- na.omit(Accelerometer_fetch)

# Scale and normalize 'Fetch_Water' and 'MVPA'
# Note: This is basic scaling to 0-1 range. Adjust according to your specific needs.
Accelerometer_fetch_transformed <- Accelerometer_fetch %>%
  mutate(Fetch_Water_Scaled = scale(Fetch_Water, center = TRUE, scale = TRUE),
         MVPA_Scaled = scale(MVPA, center = TRUE, scale = TRUE))

# Calculate Pearson's correlation coefficient between scaled and normalized 'Fetch_Water' and 'MVPA'
correlation_result <- cor(Accelerometer_fetch_transformed$Fetch_Water_Scaled, Accelerometer_fetch_transformed$MVPA_Scaled, use = "complete.obs", method = "pearson")

# Print the correlation result
print(correlation_result)
library(dplyr)
library(ggplot2)

Accelerometer_fetch$Fetch_Water <- factor(Accelerometer_fetch$Fetch_Water, 
                                          levels = c("Less_than_1_hour", "A_few_hours", "Most_of_the_day"))

# Drop NA in the 'Fetch_Water' column
Accelerometer_fetch <- Accelerometer_fetch %>%
  dplyr::filter(!is.na(Fetch_Water))

dim(Accelerometer_fetch)
table(Accelerometer_fetch$Fetch_Water)
str(Accelerometer_fetch)
# Calculate Spearman's correlation coefficient
correlation_result <- cor(Accelerometer_fetch$Fetch_Water, Accelerometer_fetch$MVPA, 
                          use = "complete.obs", method = "spearman")
# Plot
ggplot(Accelerometer_fetch, aes(x = Fetch_Water, y = MVPA)) +
  geom_point(aes(color = Fetch_Water), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 2) +
  labs(title = paste("Fetch Water vs. MVPA (Spearman's rho:", round(correlation_result, 3), ")"),
       x = "Fetch Water Category", y = "MVPA") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since color coding by Fetch Water is redundant

# Note: geom_smooth() here uses a linear model for visual purposes only. The actual correlation coefficient is Spearman's.
# Assuming Accelerometer_fetch is your data frame after filtering NA's
ggplot(Accelerometer_fetch, aes(x = Fetch_Water, y = MVPA)) +
  geom_jitter(aes(color = Fetch_Water), width = 0.2, alpha = 0.5) +
  stat_summary(fun.y = mean, geom = "point", size = 4, color = "red") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), color = "red", size = 1.5) +
  labs(title = paste("Fetch Water vs. MVPA (Spearman's rho:", round(correlation_result, 3), ")"),
       x = "Fetch Water Category", y = "MVPA") +
  theme_minimal() +
  theme(legend.position = "none")

####Load the HDL and METABOLOMICS DATA
HDL_stuff <- import("/Users/Bm0211/RegEx/Water_Energy/cvd_risk_results_male_female.csv")
colnames(HDL_stuff)
HDL_stuff <- HDL_stuff %>% select(-c(7:20, 22:29, 33:91, 94, 95))
colnames(HDL_stuff)
table(HDL_stuff$Unique.ID)
####################
#############
######
##
# METABOLOMICS
Metabolomics_STUFF <- import("/Users/Bm0211/RegEx/Water_Energy/Metabolomics_WITH_Meta.csv")
Metabolomics_STUFF <- Metabolomics_STUFF %>%
  select(Sex, Age, `Run.ID`, batch, `indiv.ID`, Unique.ID, normLogIC, Metabolites) 
Metabolomics_Pivoted <- Metabolomics_STUFF %>%
  pivot_wider(names_from = Metabolites, values_from = normLogIC) %>%
  select(indiv.ID, everything())
colnames(Metabolomics_Pivoted)
Metabolomics_Pivoted <- Metabolomics_Pivoted %>% select(c(1:6, 48, 198, 628, 617, 625, 220:223, 256, 287, 588, 490:495))
##Pivot the table or extract the needed information then merge
HDL_with_Metab_number <- merge(HDL_stuff, Metabolomics_Pivoted, by = "Unique.ID", all.x = TRUE)
dim(HDL_with_Metab_number)
colnames(HDL_with_Metab_number)
tail(HDL_with_Metab_number)
table(HDL_with_Metab_number$h_sol)
write.csv(HDL_with_Metab_number, "/Users/Bm0211/RegEx/Water_Energy/HDL_with_Metab_number.csv", row.names = FALSE)
####################
#############
######
##
##Load Marina Data


####################
#############
######
##
#





## Load SAMPLE MAP
##Freezer wally QR Codes have THGP in them that needs to be removed
Freezer_wally <- read.csv("/users/bm0211/RegEx/Water_Energy/Freezer_Wally.csv")
head(Freezer_wally)
dim(Freezer_wally)
# Clean the Barcode column to only have digits
Freezer_wally <- Freezer_wally %>%
  mutate(Sample_Barcode = str_remove_all(Sample_Barcode, "\\D")) %>%
  mutate(Sample_ID = sprintf("%02d", as.numeric(Sample_ID)))
head(Freezer_wally)
##Join the Date Barcode ID columns into one column called Unique.ID seperated by "_"
Freezer_wally <- Freezer_wally %>%
  mutate(Unique.ID = paste(Sample_Date, Sample_Barcode, Sample_ID, sep = "_"))

##Clean Dataframe

## Household style of life index from Gildner 2020: Market integration and soil-transmitted helminth infection among the Shuar of Amazonian Ecuador
All_data$h_sol <- NA
All_data$h_sol <- ifelse(All_data$Number.of.rooms.in.the.household > 1, 1, 0) +
  ifelse(All_data$Presence.of.a.finished.floor. == "Yes", 1, 0) +
  ifelse(All_data$Presence.of.an.iron.concrete.or.slate.roof. == "Yes", 1, 0) +
  ifelse(All_data$Presence.of.electricity. == "Yes", 1, 0) +
  ifelse(All_data$Presence.of.flush.toilet. == "Yes", 1, 0) +
  ifelse(All_data$Does.the.household.have.indoor.tap.water. == "Yes", 1, 0)
table(All_data$h_sol)
Meta_ONLY <- All_data %>% dplyr::select(c("Age", "Sex", "Sampling.location", "Unique.ID", "Tribe", "h_sol"))
Meta_ONLY <- Meta_ONLY %>%
  mutate(
    Git_Meta_BC_ID = paste(
      str_extract(Unique.ID, "(?<=_)[0-9]+(?=_)"),  # Extract the barcode
      str_extract(Unique.ID, "(?<=_)[0-9]+$"),      # Extract the ID
      sep = "_"
    )
  )
dim(Meta_ONLY)
colnames(Meta_ONLY)
##0.5ml tubes
p5ml_tubes <- import("/Users/Bm0211/RegEx/Water_Energy/p5ml.csv")
# Use the separate() function to split the 'Barcode_ID_p5ml' column into 'Barcode_p5ml' and 'ID_p5ml'
# Set 'remove = FALSE' to keep the original column
p5ml_tubes <- p5ml_tubes %>%
  separate(Barcode_ID_p5ml, into = c("Barcode_p5ml", "ID_p5ml"), sep = "_", remove = FALSE)
# View the updated data frame
head(p5ml_tubes)
##2ml tubes in LN ...Here the ID has two digits eg, 09 instead of 9
two_ml_tubes <- import("/Users/Bm0211/RegEx/Water_Energy/Liq_Nit_Boxes_Combined_sheet.csv")
two_ml_tubes <- dplyr::select(two_ml_tubes, c(1, 2, 3, 4, 5))
head(two_ml_tubes)
##Fix the Barcode_ID_LN column where if 5206_01 is the barcode, it should be 5206_1
two_ml_tubes <- two_ml_tubes %>%
  mutate(Barcode_ID_LN = str_replace(Barcode_ID_LN, "_0", "_"))
##Those that went through metabolomics
colnames(Metabolomics_Pivoted)
dim(Metabolomics_Pivoted)
Metabo_samples <- dplyr::select(Metabolomics_Pivoted, c(1, 2, 3, 4, 6))
head(Metabo_samples)
dim(Metabo_samples)

##Merge metabo with entire data to figure out samples that went through metabolomics
Metabo_samples <- merge(Metabo_samples, Meta_ONLY, by = "Unique.ID", all = TRUE)
colnames(Metabo_samples)
# Create a new column 'Metabo_barcode' by extracting the barcode from 'Unique.ID'
##Use the barcode to merge Freezer_wally and Metabo_samples
Metabo_samples <- Metabo_samples %>%
  mutate(Metabo_barcode = str_extract(Unique.ID, "(?<=_)(\\d+)(?=_)"))


# Define a custom merge function to handle duplicates and capture non-matching IDs
custom_merge <- function(df1, df2, by1, by2 = by1, all.x = TRUE) {
  # Merge with suffixes to identify duplicate columns
  merged_df <- merge(df1, df2, by.x = by1, by.y = by2, all.x = all.x, suffixes = c("", ".dup"))
  # Create an empty data frame to store non-matching IDs
  non_matching_ID <- data.frame(non_matching_IDs = character())
  # Identify columns that have a ".dup" version
  dup_cols <- grep("\\.dup$", names(merged_df), value = TRUE)
  # For each duplicate column, combine the original and duplicate if not NA
  for (col in dup_cols) {
    original_col <- sub("\\.dup$", "", col)
    # Combine values, separated by "|", only if the duplicate column is not NA
    merged_df <- merged_df %>%
      mutate(!!sym(original_col) := ifelse(!is.na(.[[col]]), 
                                           paste(.[[original_col]], .[[col]], sep = "|"), 
                                           .[[original_col]])) %>%
      dplyr::select(-all_of(col)) # Remove duplicate column after merging values
  }
  # Capture non-matching unique IDs from df2
  missing_ids <- setdiff(df2[[by2]], df1[[by1]])
  # If there are non-matching IDs, add them to the non_matching_ID data frame
  if (length(missing_ids) > 0) {
    non_matching_ID <- rbind(non_matching_ID, data.frame(non_matching_IDs = missing_ids))
  }
  # Return a list containing the merged data frame and the non_matching_ID data frame
  return(list(merged_df = merged_df, non_matching_ID = non_matching_ID))
}

#Freezer_wally dataframe and Metabo_samples ##############################################################################
result <- custom_merge(Metabo_samples, Freezer_wally, by1 = "Metabo_barcode", by2 = "Sample_Barcode")
head(result$merged_df)
# The merged dataframe ##############################################################################
Metab_Wally <- result$merged_df
# Data frame of non-matching IDs ##############################################################################
non_matching_IDs <- result$non_matching_ID
# Continue merging with other dataframes and appending non-matching IDs
##Merge 1 with LN boxes ##############################################################################
result_LN <- custom_merge(Metab_Wally, two_ml_tubes, by1 = "Git_Meta_BC_ID", by2 = "Barcode_ID_LN")
Metab_Wally_LNtubes <- result_LN$merged_df
non_matching_IDs <- rbind(non_matching_IDs, result_LN$non_matching_ID)
##Merge 2 with 0.5ml tubes ##############################################################################
result_p5ml <- custom_merge(Metab_Wally_LNtubes, p5ml_tubes, by1 = "Metabo_barcode", by2 = "Barcode_p5ml")
Metab_Wally_LNtubes_p5ml <- result_p5ml$merged_df
non_matching_IDs <- rbind(non_matching_IDs, result_p5ml$non_matching_ID)
# Clean the BoxMap column ##############################################################################
Metab_Wally_LNtubes_p5ml$Box_map_ID <- gsub("NA\\|?", "", Metab_Wally_LNtubes_p5ml$Box_map_ID)
colnames(Metab_Wally_LNtubes_p5ml)
# Write the final merged data frame to a CSV ##############################################################################
write.csv(Metab_Wally_LNtubes_p5ml, "/users/bm0211/RegEx/Water_Energy/Metab_Wally_LNtubes_p5ml.csv", row.names = FALSE)

# Write the non-matching IDs to a CSV file ##############################################################################
write.csv(non_matching_IDs, "/users/bm0211/RegEx/Water_Energy/non_matching_IDs.csv", row.names = FALSE)





##Further process after manual curation
# Load the curated data ##############################################################################
curated_data <- read.csv("/users/bm0211/RegEx/Water_Energy/Clean_samples.csv")
head(curated_data)
dim(curated_data)
colnames(curated_data)
## Check the colum Git_Meta_BC_ID for duplicates and merge their data separated by "|"

# Check for duplicate IDs and concatenate data, separate with "|"
curated_data_concatenated <- curated_data %>%
  group_by(Git_Meta_BC_ID) %>%
  summarise(across(everything(), ~paste(unique(.), collapse = "|"), .names = "{.col}_concatenated"),
            .groups = "drop") 
head(curated_data_concatenated)
dim(curated_data_concatenated)
# Write the concatenated data to a CSV file
write.csv(curated_data_concatenated, "/users/bm0211/RegEx/Water_Energy/Clean_samples_concatenated.csv", row.names = FALSE)

#### Sample selection
Sample_selection_file <- import("/Users/Bm0211/RegEx/Water_Energy/Sampleselectionfile.csv")
colnames(Sample_selection_file)

table(Sample_selection_file$Sex)
# Define allowed locations
allowed_locations <- c("Baragoi-Lomerok", "Chumvi-Isiolo", "Dakaye", 
                       "Kambi Robert-Rumuruti", "Lodwar", "Loiyangalani-Kula Mawe", 
                       "Maralal-Allamano", "Moite")

# Filter data for allowed locations and h_sol values of interest
filtered_df <- Sample_selection_file %>%
  filter(Sampling.location %in% allowed_locations, h_sol %in% c(0, 1, 5, 6))

# Separate the dataset by h_sol value
h_sol_01 <- filtered_df %>% filter(h_sol %in% c(0, 1))
h_sol_56 <- filtered_df %>% filter(h_sol %in% c(5, 6))

# Function to sample equal numbers of men and women
sample_equal_gender <- function(data, n_per_gender) {
  # Sample n_per_gender from each gender
  sample_men <- data %>% filter(Sex == "Male") %>% sample_n(n_per_gender)
  sample_women <- data %>% filter(Sex == "Female") %>% sample_n(n_per_gender)
  
  # Combine the samples
  rbind(sample_men, sample_women)
}

# Apply the function to both h_sol_01 and h_sol_56 subsets to get 10 samples each
# Assuming you want 5 men and 5 women for each h_sol value group
samples_h_sol_01 <- sample_equal_gender(h_sol_01, 5)
samples_h_sol_56 <- sample_equal_gender(h_sol_56, 5)

# Combine all samples
final_samples <- rbind(samples_h_sol_01, samples_h_sol_56)
dim(final_samples)
head(final_samples, 20)

write.csv(final_samples, "/users/bm0211/RegEx/Water_Energy/Vanderbilt_samples.csv", row.names = FALSE)


# Assuming you've already loaded the dplyr package
library(dplyr)

# Sample selection
Sample_selection_file <- import("/Users/Bm0211/RegEx/Water_Energy/Sampleselectionfile.csv")

# View column names to verify the structure
colnames(Sample_selection_file)
table(Sample_selection_file$Sex)

# Define allowed locations
allowed_locations <- c("Baragoi-Lomerok", "Chumvi-Isiolo", "Dakaye", 
                       "Kambi Robert-Rumuruti", "Lodwar", "Loiyangalani-Kula Mawe", 
                       "Maralal-Allamano", "Moite")

# Filter data for allowed locations, h_sol values of interest, and Map_ID criteria
filtered_df <- Sample_selection_file %>%
  filter(Sampling.location %in% allowed_locations, h_sol %in% c(0, 1, 4, 5, 6)) %>%
  filter(grepl("\\|", Box_map_ID) & grepl("p5ml_Box", Box_map_ID))

# Separate the dataset by h_sol value
h_sol_01 <- filtered_df %>% filter(h_sol %in% c(0, 1))
h_sol_56 <- filtered_df %>% filter(h_sol %in% c(4, 5, 6))

# Adjust the function to sample equal numbers of men and women
sample_equal_gender <- function(data, n_per_gender) {
  # Adjusted to sample n_per_gender from each gender, ensuring the total is 15 for each h_sol value group
  sample_men <- data %>% filter(Sex == "Male") %>% sample_n(min(n_per_gender, nrow(data %>% filter(Sex == "Male"))))
  sample_women <- data %>% filter(Sex == "Female") %>% sample_n(min(n_per_gender, nrow(data %>% filter(Sex == "Female"))))
  
  # Combine the samples
  rbind(sample_men, sample_women)
}

# Apply the function to both h_sol_01 and h_sol_56 subsets to get 15 samples each
samples_h_sol_01 <- sample_equal_gender(h_sol_01, 20) # Adjusted for 15 total samples, so 7.5 of each gender, rounded as needed
samples_h_sol_56 <- sample_equal_gender(h_sol_56, 20)

# Combine all samples
final_samples <- rbind(samples_h_sol_01, samples_h_sol_56)

# Check the dimensions and view the head of the final samples
dim(final_samples)
head(final_samples, 20)

# Write the final samples to a CSV file
write.csv(final_samples, "/users/bm0211/RegEx/Water_Energy/Vanderbilt_samples.csv", row.names = FALSE)

##Sample results
#The sheet is named Lipo_Report_20240402
Lipo_NMR <- import("/Users/Bm0211/RegEx/Water_Energy/nmr_lipo_vander.xlsx", which = "Lipo_Report_20240402")
colnames(Lipo_NMR)
names(Lipo_NMR)[1] <- "Unique.ID"

Lipo_NMR <- Lipo_NMR %>%
  mutate(barcode = sub(".*_([0-9]+)$", "\\1", Unique.ID))
table(Lipo_NMR$barcode)
table(Sample_selection_file$Metabo_barcode)
# Merge the Lipo_NMR data with the Sample_selection_file data
merged_data <- merge(Sample_selection_file, Lipo_NMR, by.x = "Metabo_barcode", by.y = "barcode", all.y = TRUE)
dim(merged_data)
head(merged_data)
colnames(merged_data)
head(Some_meta)
##########################################
#Merge Some_meta with merged_data
merged_data <- merge(merged_data, Some_meta, by.x = "Metabo_barcode", by.y = "Barcode", all.x = TRUE)
dim(merged_data)
colnames(merged_data)
##Select the needed columns
pilot_pca <- merged_data %>%
  dplyr::select(c(1,2,3,5,6,9,22:ncol(merged_data)))
dim(pilot_pca)
head(pilot_pca)
colnames(pilot_pca)

library(dplyr)
library(factoextra)
library(ggplot2)

# Adjusted to select metabolites from columns 7 to 118
metabolites <- pilot_pca[, 7:118]  
metadata <- pilot_pca[, 1:6]

pca_result <- prcomp(metabolites, scale. = TRUE, center = TRUE)

scores <- as.data.frame(pca_result$x)
visualization_data <- cbind(metadata, scores)

# Example PCA plot colored by Sex
fviz_pca_ind(pca_result,
             geom.ind = "point",
             col.ind = visualization_data$Sex, # Color by Sex
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Add confidence ellipses
             legend.title = "Sex")


# Load the data from the .fam file
##I have three files, GEMMA_files_Chol_matched.fam, GEMMA_files_HDL_matched.fam and GEMMA_files_LDL_matched.fam
##Loop through this and write three well formated files\
# Define the base directory where the .fam files are located
base_dir <- "/Users/bm0211/RegEx/Water_Energy"

# List of phenotype names
phenotypes <- c("Chol", "HDL", "LDL")

# Loop through each phenotype
for (pheno in phenotypes) {
  # Construct the path to the original .fam file
  fam_file_path <- file.path(base_dir, paste0("GEMMA_files_", pheno, "_matched.fam"))
  
  # Read the .fam file
  fam_data <- read.table(fam_file_path, header = FALSE, col.names = c("FID", "IID", "PID", "MID", "Sex", "Pheno"))
  
  # Format the .fam file to keep IID and set other columns to default values
  fam_data$FID <- fam_data$IID  # Set FID to IID for uniqueness
  fam_data$PID <- 0  # Set paternal ID to 0
  fam_data$MID <- 0  # Set maternal ID to 0
  fam_data$Sex <- 0  # Set sex to 0 (unknown)
  fam_data$Pheno <- 0  # Set phenotype to 0 (GEMMA uses -9 for missing, but 0 can be used for unaffected/unknown)
  
  # Define the path for the formatted .fam file
  formatted_fam_path <- file.path(base_dir, paste0(pheno, "_formatted.fam"))
  
  # Write the formatted data back to a new .fam file
  write.table(fam_data, formatted_fam_path, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  
  # Optionally, print the head of the formatted data frame to check
  print(head(fam_data))
}




GWAS_Catalog <- read.delim("/Users/Bm0211/RegEx/Water_Energy/lipid_traits_GWAS_Catalog.txt", stringsAsFactors = FALSE)
# Rename the TRAIT column
colnames(GWAS_Catalog)
names(GWAS_Catalog)[4] <- "TRAIT"
# List of traits of interest
traits_of_interest <- c(
  "LDL cholesterol levels", 
  "LDL cholesterol", 
  "High density lipoprotein cholesterol levels", 
  "High-density lipoprotein 3 cholesterol levels", 
  "High-density lipoprotein 2 cholesterol levels", 
  "HDL3 cholesterol levels", 
  "HDL2 cholesterol levels", 
  "HDL cholesterol levels", 
  "HDL cholesterol", 
  "Cholesterol, total", 
  "Clinical LDL cholesterol levels", 
  "Cholesterol levels", 
  "Cholesterol", 
  "LDL cholesterol to HDL cholesterol ratio", 
  "Serum total cholesterol levels", 
  "Total cholesterol levels"
)

# Subset the dataframe to include only rows with traits of interest
GWAS_Catalog_subset <- GWAS_Catalog[GWAS_Catalog$TRAIT %in% traits_of_interest, ]

# Drop unnecessary columns
GWAS_Catalog_subset <- GWAS_Catalog_subset[, c("TRAIT", "CHR_ID", "CHR_POS", "SNP_GENE_IDS", "SNPS", "SNP_ID_CURRENT", "CONTEXT", "P-VALUE (TEXT)", "OR or BETA", "INITIAL SAMPLE SIZE", "PLATFORM [SNPS PASSING QC]", "PUBMEDID", "DATE", "LINK")]

# Print the first few rows of the cleaned dataframe
head(GWAS_Catalog_subset)
dim(GWAS_Catalog_subset)
colnames(GWAS_Catalog_subset)[10] <- "INITIAL_SAMPLE_SIZE"
table(GWAS_Catalog_subset$INITIAL_SAMPLE_SIZE)

# Read the GWAS results file
gwas_results <- read.table("/Users/bm0211/RegEx/Water_Energy/Based_on_Video_HDL.lmm.assoc.txt", header = TRUE, sep = "")

# Correct column names
names(gwas_results)[names(gwas_results) == "ps"] <- "pos"
names(gwas_results)[names(gwas_results) == "p_lrt"] <- "p"
gwas_results$p <- as.numeric(as.character(gwas_results$p))

# Get SNP with the lowest p-value
lowest_p_value_snps <- gwas_results[order(gwas_results$p), ][1:1000, ]

# Get SNP with the highest effect size (absolute value of beta)
highest_effect_size_snps <- gwas_results[order(abs(gwas_results$l_mle), decreasing = TRUE), ][1:1000, ]

# Extract SNP rs numbers
lowest_p_value_snps$rs <- gsub("^.*:", "", lowest_p_value_snps$rs)
highest_effect_size_snps$rs <- gsub("^.*:", "", highest_effect_size_snps$rs)

# Find overlapping SNPs between the two lists
overlapping_snps <- intersect(lowest_p_value_snps$rs, highest_effect_size_snps$rs)

# Print the results
print("SNPs with the lowest p-values:")
print(lowest_p_value_snps)

print("SNPs with the highest effect sizes:")
print(highest_effect_size_snps)

print("Overlapping SNPs:")
print(overlapping_snps)

# Write the overlapping SNPs to a text file
write.table(overlapping_snps, file = "/Users/bm0211/RegEx/Water_Energy/overlapping_snps.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)



###Check if our top selection SNPs have been associated with traits in other GWAS catalog
# Read the GWAS Catalog data
GWAS_Catalog <- import("/Users/bm0211/RegEx/Water_Energy/full.txt", header = TRUE, sep = "\t", qu)
colnames(GWAS_Catalog)
iHS_results <- read.table("/Users/bm0211/RegEx/Water_Energy/All_SNPs_IHS.csv", header = TRUE, sep = "") 

##Make dataframe with rsid column of the iHS_results and SNPS column of the GWAS_Catalog that match
# Extract information about matching SNPs
matching_snps <- inner_join(iHS_results, GWAS_Catalog, by = c("rsid" = "SNPS"))

head(matching_snps)

matching_snps <- iHS_results[iHS_results$rsid %in% GWAS_Catalog$SNPS, c("rsid")]
head(matching_snps)

# Extract information about matching SNPs
matching_snps <- inner_join(iHS_results, GWAS_Catalog, by = c("rsid" = "SNPS")) %>%
  select(rsid)  # Select only the rsid column from the matching SNPs dataframe

head(matching_snps)

#####

##read in the PBMC Sep 
PBMC_Sep <- import("/Users/bm0211/RegEx/Water_Energy/FRPsamples_Sep_TurkanaOnly_forManifest.txt")
head(PBMC_Sep)
names(PBMC_Sep)[2] <- "Sep_Processed"
names(PBMC_Sep)[3] <- "Sep_QC"
dim(PBMC_Sep)
PBMC_Cryo <- import("/Users/bm0211/RegEx/Water_Energy/FRPsamples_CPT_TurkanaOnly_forManifest.txt")
head(PBMC_Cryo)
names(PBMC_Cryo)[2] <- "Cryo_Processed"
names(PBMC_Cryo)[3] <- "Cryo_QC"
dim(PBMC_Cryo)
##Merge the two dataframes by Unique.ID
PBMC_all <- merge(PBMC_Sep, PBMC_Cryo, by = "Unique.ID", all = TRUE)
dim(PBMC_all)


PBMC_Sep <- import("/Users/bm0211/RegEx/Water_Energy/FRPsamples_Sep_TurkanaOnly_forManifest.txt")
head(PBMC_Sep)
names(PBMC_Sep)[2] <- "Sep_Processed"
names(PBMC_Sep)[3] <- "Sep_QC"
dim(PBMC_Sep)
PBMC_Cryo <- import("/Users/bm0211/RegEx/Water_Energy/FRPsamples_CPT_TurkanaOnly_forManifest.txt")
head(PBMC_Cryo)
names(PBMC_Cryo)[2] <- "Cryo_Processed"
names(PBMC_Cryo)[3] <- "Cryo_QC"
dim(PBMC_Cryo)
##Merge the two dataframes by Unique.ID
PBMC_all <- merge(PBMC_Sep, PBMC_Cryo, by = "Unique.ID", all = TRUE)
dim(PBMC_all)
head(PBMC_all)
##Merge to Larger Data
PBMC_X_SERUM <- merge(PBMC_all, all_non_matching_MANUALadded, by = "Unique.ID", all = TRUE)
dim(PBMC_X_SERUM)
##Drop NA in the Unique.ID column
PBMC_X_SERUM <- PBMC_X_SERUM %>% drop_na(Unique.ID)


# Create the plot
ggplot(filtered_data, aes(x = label, y = outlier_SNPs, fill = chromosome)) +
  geom_bar(stat = "identity") +
  labs(title = "Outlier SNPs by Genomic Region",
       x = "Region",
       y = "Number of Outlier SNPs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the data to check
print(filtered_data)

##write the data to a csv file

##
##Import and process FECAL Manifest
FECAL_CM_May13 <- import("FECAL_CM_May13.csv")
head(FECAL_CM_May13)
##DATE column changed from "14th February 2023" to Year_Month_Day
FECAL_CM_May13 <- FECAL_CM_May13 %>%
  mutate(DATE = as.Date(str_replace(DATE, "th", ""), format = "%d %B %Y"))


