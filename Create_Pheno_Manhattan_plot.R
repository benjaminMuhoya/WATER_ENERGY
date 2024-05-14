library(tidyverse)
library(reshape2)
library(qqman)
library(ggplot2)
library(rio)
library(dplyr)
## Import the needed files
Clean_meta = import("/Users/Bm0211/RegEx/Water_Energy/THGP_database_full_corrected_merged_2024-04-02.txt")
colnames(Clean_meta)
HDL_data = dplyr::select(Clean_meta, c("Unique.ID","HDL.cholesterol.mg.dL",
                                       "LDL.cholesterol.mg.dL","Total.cholesterol.mg.dL.",
                                       "Body.fat.percentage","BMI","Standing.height.cm.", "Age", "Sex", "LDL.HDL.ratio"))
colnames(HDL_data)
colnames(HDL_data) <- c("Unique.ID","HDL","LDL","Chol","Body_fat","BMI","Height", "Age", "Sex","Ratio_LDHDL")
HDL_data[,2:7,10] <- lapply(HDL_data[,2:7,10], as.numeric)

##Create Height Subset Matching HDL data
For_subset <- HDL_data
dim(For_subset)
colnames(For_subset)
For_subset <- select(For_subset, c(1, 2, 7))
head(For_subset, 50)
For_subset[,2:3] <- lapply(For_subset[, 2:3], as.numeric)
For_subset <- For_subset[!is.na(For_subset$HDL), ]
##
calculate_LDL_HDL_Ratio <- function(ldl, hdl) {
  ratio <- rep(NA, length(ldl))
  # Loop through each element
  for(i in 1:length(ldl)) {
    # Check if both LDL and HDL are numeric and not NA
    if(is.numeric(ldl[i]) && !is.na(ldl[i]) && is.numeric(hdl[i]) && !is.na(hdl[i])) {
      # Perform the calculation
      ratio[i] <- ldl[i] / hdl[i]
    } 
    # If either LDL or HDL is not numeric or is NA, ratio[i] will remain NA
  }
  return(ratio)
}
# Apply the function to the LDL and HDL columns of your data frame
HDL_data$LDL_HDL_Ratio <- calculate_LDL_HDL_Ratio(HDL_data$LDL, HDL_data$HDL)

##IMport Wholesome .fam file
April_GENO <- read.table("/Users/bm0211/RegEx/Water_Energy/wholegenome_AllSamples_updatednames.fam", header = FALSE)
head(April_GENO)

##Arrange the columns to have Sex as the fifth column
combined_data <- HDL_data[, c(1,8,9,2:7,10,11)]
colnames(combined_data)
table(combined_data$Sex) ##36 people(Female\Male ambigous)
combined_data$Sex <- ifelse(combined_data$Sex == "Male", 1, ifelse(combined_data$Sex == "Female", 2, -9))
colnames(combined_data)
combined_data[,4:11] <- lapply(combined_data[,4:11], as.numeric)
##Scale the using scale default R then Identify outliers in each 6-12 column using IQR then filter them out
# Scale each column from 6 to 12
combined_data[, 4:11] <- lapply(combined_data[, 4:11], scale)
# Function to remove outliers based on the IQR method
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x[x >= (Q1 - 1.5 * IQR) & x <= (Q3 + 1.5 * IQR)]
}
# Apply the remove_outliers function to each column from 6 to 12
# Note: This will result in columns of different lengths due to removed outliers
# So, create a list where each element is a column after outlier removal
cleaned_columns <- lapply(combined_data[, 4:11], remove_outliers)
# replace outliers with NA (keeping the dataframe structure consistent):
replace_outliers_with_NA <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)] <- NA
  x
}
combined_data[, 4:11] <- lapply(combined_data[, 4:11], replace_outliers_with_NA)
##Sanity Check
dim(combined_data)
table(is.na(combined_data$HDL))
colnames(combined_data)
##Read in the whole_PCA.eigenvec file and merge with the combined_data
PCA_data <- read.table("/Users/bm0211/RegEx/Water_Energy/whole_PCA.eigenvec", header = TRUE)
PCA_data <- dplyr::select(PCA_data, c(2,3)) ##Pick IID and PC1
##merge with combined_data
combined_data <- merge(combined_data, PCA_data, by.y = "IID", by.x = "Unique.ID", all.y = TRUE)
##Create Covariate File with first column as Sex, second as Age and third as PC1
Covariate_file <- dplyr::select(combined_data, c(1, 12, 2:11))
##Ensure, covariate file is in the same order as the combined_data based on IID
Covariate_file <- Covariate_file[order(match(Covariate_file$Unique.ID, April_GENO$V1)),]
##Write Covariate file into a .txt file without the first column and no headers
Covariates <- dplyr::select(Covariate_file, c(1:4))
write.table(Covariates[, -1], file = "/Users/bm0211/RegEx/Water_Energy/Covariate_file_Wholesome.txt", col.names = FALSE, row.names = FALSE, quote = FALSE, sep = "\t")

##Write the combineddata into .phen file that matches .fam file order for matching on Plink
select_data <- dplyr::select(Covariate_file, c(1,5:12))
select_data$FID <- select_data$Unique.ID
names(select_data)[1] <- "IID"
select_data <- dplyr::select(select_data, c(10,1,2:9))
select_data <- select_data[order(match(select_data$IID, April_GENO$V1)),]
write.table(select_data, file = "/Users/bm0211/RegEx/Water_Energy/wholegenome_AllSamples_updatednames.phen", col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")

####################################
######Manhattan Plot after GWAS
# Ensure p-values are numeric
# Load the GWAS results
gwas_results <- read.table("/Users/bm0211/RegEx/Water_Energy/Height.assoc.txt", header = TRUE, sep = "")
# Correct renaming was done previously
head(gwas_results)
names(gwas_results)[names(gwas_results) == "ps"] <- "pos"
names(gwas_results)[names(gwas_results) == "p_score"] <- "p"
gwas_results$p <- as.numeric(as.character(gwas_results$p))
# Check again for non-numeric p-values just in case
sum(is.na(gwas_results$p))
# Choose a y-axis upper limit AND BONFERRONI THRESHOLD
upper_ylim <- 10
bonferroni_threshold <- 3.84e-07 ##given The Bonferroni corrected p-value threshold, given 130,315 tests, is approximately 3.84 × 10−7 3.84×10−7
manhattan(gwas_results,
          chr = "chr",
          bp = "pos",
          snp = "rs",
          p = "p",
          main = "Manhattan Plot for HEIGHT GWAS",
          col = c("blue4", "orange3"),
          annotatePval = bonferroni_threshold,
          ylim = c(0, upper_ylim))
# QQPLOT
qq(gwas_results$p, main = "QQ Plot for HEIGHT GWAS P-values")
# Filtering SNPs above the significance threshold
significant_snps <- gwas_results[gwas_results$P < 5e-6, ]
# Print out the significant SNPs with their information
if(nrow(significant_snps) > 0) {
  print(significant_snps[, c("SNP", "CHR", "P")])
} else {
  cat("No SNPs above the significance threshold.\n")
}
dim(significant_snps)
###GENETICS    GENETICS
########################
##Heritability
##Load the numbers
estimate <- c(0.39, 0.00001, 0.00001, 0.00001, 0.07, 0.55, 0.09, 0.95)
se <- c(0.38, 0.31, 0.19, 0.34, 0.24, 0.22, 0.31, 0.15)
N <- c(350, 272, 322, 551, 524, 683, 257, 343)
Trait <- c("HDL", "LDL", "Chol", "Body_fat", "BMI", "Height", "RatioHDLDL", "Height_ADJUSTED")

# Create a data frame
data <- data.frame(Trait, estimate, se, N)
# Create the bar plot
p <- ggplot(data, aes(x = Trait, y = estimate, fill = Trait)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se), width = 0.4, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = N), vjust = -0.5, size = 3) +
  labs(x = "Trait", y = "Heritability Estimate", title = "Heritability of Traits with Error Bars and Participant Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Print the plot
print(p)

##
PBS_ALLchr <- import("/Users/bm0211/RegEx/Water_Energy/21May20_PBS_ALLchr_outlier_count.txt")
XtX_ALL <- import("/Users/bm0211/RegEx/Water_Energy/outliers_by_regions.txt", header = TRUE)
head(PBS_ALLchr)
dim(PBS_ALLchr)
head(XtX_ALL)
PBS_ALLchr <- import("/Users/bm0211/RegEx/Water_Energy/21May20_PBS_ALLchr_outlier_count.txt")
XtX_ALL <- import("/Users/bm0211/RegEx/Water_Energy/outliers_by_regions.txt", header = TRUE)
head(PBS_ALLchr)
dim(PBS_ALLchr)
head(XtX_ALL)
##DROP first column in the PBS_ALLchr
PBS_ALLchr <- PBS_ALLchr %>% dplyr::select(-c(1))
library(ggplot2)
library(dplyr)

# Sample data
data_PBS <- data.frame(
  region = PBS_ALLchr$region,
  tot_SNPs = PBS_ALLchr$tot_SNPs,
  outlier_SNPs = PBS_ALLchr$outlier_SNPs
)

# Outlier SNPs threshold
outlier_threshold <- 10  # Placeholder to change for different cutoffs

# Filter data to show only regions with outlier SNPs greater than the threshold
filtered_data <- data_PBS %>%
  filter(outlier_SNPs > outlier_threshold)

# Extracting chromosome and positions for better labeling
filtered_data <- filtered_data %>%
  mutate(chromosome = sapply(strsplit(as.character(region), "_"), `[`, 1),
         start = sapply(strsplit(as.character(region), "_"), `[`, 2),
         end = sapply(strsplit(as.character(region), "_"), `[`, 3),
         label = paste("Chr", chromosome, ":", start, "-", end, sep = ""))

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
write.csv(filtered_data, "/Users/bm0211/RegEx/Water_Energy/filtered_data_PBS.csv", row.names = FALSE)
##########################
##Metabolomics
data_df <- import("/Users/bm0211/RegEx/Water_Energy/METABOLITE_PC_CORRECTED_merged_data.csv", header = TRUE)
head(data_df)
dim(data_df)
library(ggplot2)
library(dplyr)

# Filter and summarize the data for 'h_sol'
h_sol_data <- data_df %>%
  filter(term == "h_sol") %>%
  group_by(Metabolic_pathway) %>%
  summarise(Count = n(), .groups = 'drop')
head(data_df)
# Create the bar plot for 'h_sol'
h_sol_plot <- ggplot(h_sol_data, aes(x = Metabolic_pathway, y = Count, fill = Metabolic_pathway)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Metabolic Pathway", y = "Count", fill = "Pathway", title = "Count of Metabolic Pathways for h_sol") +
  scale_fill_viridis(discrete = TRUE)

# Filter and summarize the data for 'Age'
age_data <- data_df %>%
  filter(term == "Age") %>%
  group_by(Metabolic_pathway) %>%
  summarise(Count = n(), .groups = 'drop')

# Create the bar plot for 'Age'
age_plot <- ggplot(age_data, aes(x = Metabolic_pathway, y = Count, fill = Metabolic_pathway)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Metabolic Pathway", y = "Count", fill = "Pathway", title = "Count of Metabolic Pathways for Age") +
  scale_fill_viridis(discrete = TRUE)

# Filter and summarize the data for 'Sex'
sex_data <- data_df %>%
  filter(term == "SexMale") %>%
  group_by(Metabolic_pathway) %>%
  summarise(Count = n(), .groups = 'drop')

# Create the bar plot for 'Sex'
sex_plot <- ggplot(sex_data, aes(x = Metabolic_pathway, y = Count, fill = Metabolic_pathway)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Metabolic Pathway", y = "Count", fill = "Pathway", title = "Count of Metabolic Pathways for Sex") +
  scale_fill_viridis(discrete = TRUE)

# Print the plots
print(h_sol_plot)
print(age_plot)
print(sex_plot)

# Sort data based on the absolute value of the statistic column, then take the top 50
top_metabolites <- data_df %>%
  arrange(desc(abs(statistic))) %>%
  slice(1:50)
# Create a bar plot of the effect sizes with metabolite names on the y-axis
# Define a color palette with 10 distinct colors
color_palette <- brewer.pal(n = 10, name = "Paired")  # 'Paired' is good for distinct colors
effect_plot <- ggplot(top_metabolites, aes(x = estimate, y = reorder(Metabolite, estimate), fill = Metabolic_pathway)) +
  geom_col() +
  scale_fill_manual(values = color_palette) +
  labs(title = "Top 50 Metabolites by Absolute t-value Value",
       x = "Effect Size of h_sol",
       y = "Metabolite",
       fill = "Pathway") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  # Adjust text size for better readability

# Print the plot
effect_plot
head(data_df)
## Box plot PE(18:0/18:0), LysoPE(22:1), PC(15:0/22:5), PC(16:0/19:1)

#1. Metabolomics Excel sheet from Asael
#2. Sample map
#3. Frozen_Data
# read in the HILIC results and remove the columns that are not samples or pooled QC samples
allData <- as.data.frame(import( "~/Documents/AyrolesLab/Turkana 613 samples Dual-neg+pos SUM.xlsx", sheet = "Organized"))
rownames( allData) <- allData$compoundId
dataSub <- allData[ , -c( 1:9)] ##Removing the Blanks
# order the data by injection order (pooled and repeat samples are listed at the end in the original file)
dataSub.ordered <- dataSub[ , c( 618, 1:25, 619, 26:50, 620, 51:75, 621, 76:100, 622, 101:125, 623, 126:150, 624, 151:175, 625, 176:200, 626, 201:239, 629, 240:264, 630, 265:289, 631, 290:314, 632, 315:339, 633, 340:358, 634, 610:612, 359:380, 635, 381:397, 636, 398:422, 637, 423:447, 638, 448:476, 639, 477:501, 640, 502:526, 641, 527:551, 642, 552:560, 613:617, 561:571, 643, 572:596, 644, 597:609)]
## Group the Batches and create an Ordered Batch Map
batchMap <- rbind( data.frame( batch = 1, sample = colnames( dataSub.ordered)[ 1:248]), data.frame( batch = 2, sample = colnames( dataSub.ordered)[ 249:372]), data.frame( batch = 3, sample = colnames( dataSub.ordered)[ 373:416]), data.frame( batch = 4, sample = colnames( dataSub.ordered)[ 417:498]), data.frame( batch = 5.1, sample = colnames( dataSub.ordered)[ 499:586]), data.frame( batch = 5.2, sample = colnames( dataSub.ordered)[ 587:642]))
batchMap$batch <- factor( batchMap$batch, levels = c( 1, 2, 3, 4, 5.1, 5.2))
## Create an additional row with the Injection order into the Machine
order <- batchMap$sample
batchMap = batchMap %>% 
  mutate(Run.ID=row_number())
head(batchMap)
table(batchMap$Run.ID)
##ADD METADATA YOU WANT TO USE
All_data <- import("/Users/Bm0211/RegEx/Water_Energy/THGP_database_full_corrected_merged_2024-04-02.txt")
colnames(All_data)
dim(All_data)
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
Meta_ONLY <- All_data %>% dplyr::select(c("Age", "Sex", "Sampling.location", "Unique.ID", "Tribe", "h_sol"))
sampleDat <- select(Meta_ONLY, c(4,1,2,3,6)) #import("Lifestyle_CAT_Diet_Upd.csv")
head(sampleDat)

##sample Map
sampleMap <- as.data.frame(import("/Users/bm0211/Documents/AyrolesLab/metabolomics_sampleMap_final (1).csv"))
head(sampleMap)
##Merge Map and Unique.ID data 
batchMap_with_Meta <- merge(sampleMap, batchMap, by=2, all = T)
dim(batchMap_with_Meta)
batchMap_with_Meta <- unique(merge(batchMap_with_Meta, sampleDat, by="Unique.ID", all.x = T))
head(batchMap_with_Meta)
dim(batchMap_with_Meta)
table(batchMap_with_Meta$h_sol)

##Writing the ordered metabolites file
dataSub.ordered = rownames_to_column(dataSub.ordered, var = "Metabolites")
##Pivot and merge the data
NARROW_dataSub.ordered <- dataSub.ordered %>% 
  pivot_longer(!Metabolites,
               names_to = "indiv.ID",
               values_to = "Concentration")

NARROW_dataSub.ordered <- left_join(NARROW_dataSub.ordered, batchMap_with_Meta, by=c('indiv.ID'='metabRunNum'))

##Median Center and Log Transform
NARROW_dataSub.ordered_Norm <- NARROW_dataSub.ordered %>%
  group_by("Metabolites") %>%
  mutate(med = median(Concentration)) %>%
  mutate(normIC = Concentration/med) %>%
  ungroup() %>%
  mutate(min.val = min(abs(normIC[normIC != 0]))/10) %>%
  group_by("indiv.ID") %>%
  mutate(normLogIC = log10((normIC + sqrt(normIC^2 + min.val^2))/2))


##Remove "Female|Female|Male", "Female|Male", "Female|Male|Male" from NARROW_dataSub.ordered_NOrm
LongTable_Metabs_Metadata <- NARROW_dataSub.ordered_Norm[!grepl("Female\\|Female\\|Male|Female\\|Male|Female\\|Male\\|Male", NARROW_dataSub.ordered_Norm$Sex), ]
##Regression with metabolites concentration in the normLogIC column ~ h_sol + Age + Sex
table(LongTable_Metabs_Metadata$Metabolites)
# Filter the data for the specified metabolites
filtered_data <- LongTable_Metabs_Metadata %>%
  filter(Metabolites %in% c("PE(18:0/18:0)", "LysoPE(22:1)", "PC(15:0/22:5)", "PC(16:0/19:1)", "Trihydroxycoprostanoic acid", "Ginsenoside Rh3 isomer", "Creatinine"))

# Plot each metabolite
plots_list <- lapply(unique(filtered_data$Metabolites), function(metabolite) {
  data_subset <- filtered_data %>%
    filter(Metabolites == metabolite)
  
  p <- ggplot(data_subset, aes(x = h_sol, y = normLogIC)) +
    geom_point(alpha = 0.6) +  # Use points to visualize data
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line without confidence interval
    labs(title = paste("Concentration of", metabolite, "vs. h_sol"),
         x = "h_sol",
         y = "Normalized Log Concentration") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve label readability
  
  return(p)
})

# Print each plot
for (plot in plots_list) {
  print(plot)
}

##########################################################################################################
##NMR Metabolomics
# Load the column mapping from "HDL_Columns" sheet for renaming
column_mapping <- import("/Users/Bm0211/RegEx/Water_Energy/nmr_lipo_vander.xlsx", sheet = "HDL_Columns")
# Load Sample Selection File, ensuring no duplicate 'Metabo_barcode' entries
Sample_selection_file <- import("/Users/Bm0211/RegEx/Water_Energy/Sampleselectionfile.csv") %>%
  distinct(Metabo_barcode, .keep_all = TRUE)
# Load Lipo NMR Data and rename columns based on mapping
Lipo_NMR <- import("/Users/Bm0211/RegEx/Water_Energy/nmr_lipo_vander.xlsx", sheet = "Lipo_Report_20240402")
for(i in 1:nrow(column_mapping)) {
  old_name <- column_mapping$Old_name[i]
  new_name <- column_mapping$New_name[i]
  if(old_name %in% names(Lipo_NMR)) {
    names(Lipo_NMR)[names(Lipo_NMR) == old_name] <- new_name
  }
}
colnames(Lipo_NMR)
names(Lipo_NMR)[1] <- "Unique.ID_NMR_RUN"
Lipo_NMR <- Lipo_NMR %>%
  mutate(barcode = sub(".*_([0-9]+)$", "\\1", Unique.ID_NMR_RUN))
table(Lipo_NMR$barcode)
# Load additional data and select necessary columns
head(Lipo_NMR)
head(Meta_ONLY)
# Extract additional metadata from 'Unique.ID'
Some_meta <- Meta_ONLY %>%
  mutate(
    Date = str_extract(Unique.ID, "^[0-9]+-[0-9]+-[0-9]+"),
    barcode = str_extract(Unique.ID, "(?<=_)[0-9]+(?=_)"),
    ID = str_extract(Unique.ID, "(?<=_)[0-9]+$")
  )
colnames(Some_meta)
colnames(Lipo_NMR)
##Merge some_meta with Lipo_NMR using barcode
Lipo_NMR <- merge(Lipo_NMR, Some_meta, by = "barcode", all.x = TRUE)
head(Lipo_NMR, 22)
colnames(Lipo_NMR)

##Transform hsol
Lipo_NMR <- Lipo_NMR %>%
  mutate(h_sol = case_when(
    h_sol %in% c(0) ~ "Remote",
    TRUE ~ "Urban"
  ))

# Convert h_sol to a factor for plotting
Lipo_NMR$h_sol <- as.factor(Lipo_NMR$h_sol)

# Create the plot
ggplot(Lipo_NMR, aes(x = h_sol, y = Total_APOA1, fill = h_sol)) +
  geom_boxplot() +
  geom_violin(alpha = 0.3) +
  labs(title = "Boxplot and Violin Plot of Total APOA1 by Lifestyle",
       x = "Lifestyle Category",
       y = "Total APOA1 (mg/dL)") +
  theme_minimal()

# Define the metabolites to plot
metabolites <- c(
  "TG_content_HDL_1", "TG_content_HDL_2", "TG_content_HDL_3", "TG_content_HDL_4",
  "Chol_content_HDL_1", "Chol_content_HDL_2", "Chol_content_HDL_3", "Chol_content_HDL_4",
  "Free_Chol_content_HDL_1", "Free_Chol_content_HDL_2", "Free_Chol_content_HDL_3", "Free_Chol_content_HDL_4",
  "Phospolipid_Chol_content_HDL_1", "Phospolipid_Chol_content_HDL_2", "Phospolipid_Chol_content_HDL_3", "Phospolipid_Chol_content_HDL_4",
  "APOA1_Chol_content_HDL_1", "APOA1_Chol_content_HDL_2", "APOA1_Chol_content_HDL_3", "APOA1_Chol_content_HDL_4",
  "APOA2_Chol_content_HDL_1", "APOA2_Chol_content_HDL_2", "APOA2_Chol_content_HDL_3", "APOA2_Chol_content_HDL_4"
)

# Initialize an empty list to store plots
plots <- list()

# Loop through each metabolite to create plots
for (metabolite in metabolites) {
  p <- ggplot(Lipo_NMR, aes(x = h_sol, y = !!sym(metabolite), fill = h_sol)) +
    geom_boxplot() +
    geom_violin(alpha = 0.3) +
    guides(fill = F) +
    labs(title = paste(metabolite),
         x = "Life_Cat",
         y = "(mg/dL)") +
    theme_minimal()
  plots[[metabolite]] <- p
}

# Arrange the plots in a 4x4 grid
# Split the plots into groups for two 4x4 grids
grid1 <- plots[1:16]  # First 16 plots
grid2 <- plots[17:24]  # Remaining 8 plots
# Arrange the first grid
print(grid.arrange(grobs = grid1, nrow = 4, ncol = 4))
# Arrange the second grid
print(grid.arrange(grobs = grid2, nrow = 4, ncol = 4))

library(biomaRt)
# Load necessary library
library(biomaRt)
library(dplyr)

# Import the CSV file
snp.id <- import("/Users/Bm0211/RegEx/Water_Energy/SNPS_matching_UCSC.txt")
head(snp.id)
# Parse 
tail(snp.id, 30)
colnames(snp.id)
snp.id <- dplyr::select(snp.id, c(2,3,4,5,14,15,16))
##Column Names ((Chrom chromStart  ChromEnd snpID average_heterozygosity Ave_Het_SD FUNCTION))


All_data <- import("/Users/Bm0211/RegEx/Water_Energy/THGP_database_full_corrected_merged_2024-04-02.txt")
dim(All_data)
table(All_data$Presence.of.an.iron.concrete.or.slate.roof.)
colnames(All_data)
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
                                          "Number.of.children", "Length.of.time.residing.in.current.location", 
                                          "Age.of.first.menstrual.period.in.years.", "How.old.were.you.in.years.when.your.first.child.was.born."))
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
table(Meta_ONLY$Length.of.time.residing.in.current.location)
table(Meta_ONLY$How.old.were.you.in.years.when.your.first.child.was.born.)
table(is.na(Meta_ONLY$How.old.were.you.in.years.when.your.first.child.was.born.))
##########################################
Meta_ONLY <- Meta_ONLY %>%
  mutate(`Length.of.time.residing.in.current.location` = as.character(`Length.of.time.residing.in.current.location`)) %>%
  mutate(`Length.of.time.residing.in.current.location` = case_when(
    str_detect(`Length.of.time.residing.in.current.location`, regex("month|week", ignore_case = TRUE)) ~ "1",
    str_detect(`Length.of.time.residing.in.current.location`, regex("<\\s*1\\s*Year", ignore_case = TRUE)) ~ "1",
    str_detect(`Length.of.time.residing.in.current.location`, regex(">\\s*10\\s*Years?", ignore_case = TRUE)) ~ "10",
    str_detect(`Length.of.time.residing.in.current.location`, regex("\\b[0-9]+\\s*-\\s*[0-9]+\\s*Years", ignore_case = TRUE)) ~ as.character(as.numeric(str_extract(`Length.of.time.residing.in.current.location`, "\\b\\d+"))),
    TRUE ~ `Length.of.time.residing.in.current.location`
  ))

# Further clean the 'Length.of.time.residing.in.current.location' column
Meta_ONLY <- Meta_ONLY %>%
  mutate(`Length.of.time.residing.in.current.location` = as.character(`Length.of.time.residing.in.current.location`)) %>%
  mutate(`Length.of.time.residing.in.current.location` = str_extract(`Length.of.time.residing.in.current.location`, "\\d+"))

# Convert the cleaned-up strings to numeric
Meta_ONLY <- Meta_ONLY %>%
  mutate(`Length.of.time.residing.in.current.location` = as.numeric(`Length.of.time.residing.in.current.location`))

# Check the results
table(Meta_ONLY$`Length.of.time.residing.in.current.location`)

##These will be recoded as Reliable water (Tap water, Tank, Springs|Tap water, Tank, Springs, Outside Tap, Piped Water)
##Unreliable water (Piped Water ,Rain Water, River, Lugger, Stream, Pond,
## Valley|Well, Well, Borehole|Well, Borehole|Valley|Well, Borehole|River/Stream, Borehole|Outside Tap
## Borehole|Lugger, Borehole|Lake, )
##Recoding the water source
Meta_ONLY$What.is.your.drinking.water.source. <- recode(Meta_ONLY$What.is.your.drinking.water.source.,
                                                        "Tap water" = "Tap",
                                                        "Tank" = "Tank",
                                                        "Springs" = "Reliable_water",
                                                        "Outside Tap" = "Tap",
                                                        "Piped Water" = "Tap",
                                                        "Springs|Tap water" = "Tap",
                                                        "Borehole|Outside Tap" = "Tap",
                                                        "Rain Water" = "Rain",
                                                        "Lake|Outside Tap" = "Tap",
                                                        "River" = "Unreliable_water",
                                                        "County Govt Water" = "Reliable_water",
                                                        "Buy Water" = "Reliable_water",
                                                        "Lugger" = "River/Stream",
                                                        "Stream" = "River/Stream",
                                                        "Pond" = "Well",
                                                        "Valley" = "River/Stream",
                                                        "Well" = "Well",
                                                        "Valley|Well" = "River/Stream",
                                                        "Borehole|Well" = "Well",
                                                        "Borehole|Valley|Well" = "River/Stream",
                                                        "Borehole|River/Stream" = "River/Stream",
                                                        "Borehole|Lugger" = "River/Stream",
                                                        "Borehole|Lake" = "Lake",
                                                        "River/Stream|Well" = "River/Stream",
                                                        "Oasis" = "Oasis",
                                                        " " = "NA",
                                                        "None" = "NA",
                                                        "Not Answered|Other" = "NA"
                                                       )
## Load SAMPLE MAP
table(Meta_ONLY$Number.of.children)
dim(Meta_ONLY)
cleaned_meta <- Meta_ONLY %>%
  mutate(
    `What.is.your.drinking.water.source.` = ifelse(
      Sampling.location %in% c("Rumuruti", "Kitale", "Segera", "Endana", "Juakali", "Kariunga"),
      "Reliable_water",
      ifelse(
        Sampling.location %in% c("Ikoree-Katilu", "Kambi Robert-Rumuruti"),
        "Borehole",
        `What.is.your.drinking.water.source.`
      )
    )
  )
# Filter out entries in 'number of children' that contain non-digit characters
cleaned_meta <- cleaned_meta %>%
  # Convert the 'Number.of.children' to character, remove non-digit characters, and convert to numeric
  mutate(`Number.of.children` = as.numeric(str_remove_all(`Number.of.children`, "\\D"))) %>%
  # Filter out entries where 'Number.of.children' is NA or greater than 30
  filter(!is.na(`Number.of.children`), `Number.of.children` <= 30)

# Filter out entries in 'number of children' that contain non-digit characters
cleaned_meta <- cleaned_meta %>%
  #remove non-digit characters, and convert to numeric
  mutate(`How.old.were.you.in.years.when.your.first.child.was.born.` = as.numeric(str_remove_all(`How.old.were.you.in.years.when.your.first.child.was.born.`, "\\D")))
#####
dim(cleaned_meta)
table(cleaned_meta$What.is.your.drinking.water.source.)

# Filtering out NA values in the water source
cleaned_meta <- cleaned_meta %>%
  filter(!is.na(`What.is.your.drinking.water.source.`))
cleaned_meta <- cleaned_meta %>%
  mutate(`Number.of.children` = as.numeric(`Number.of.children`))

##
##
##########
###
#####

library(dplyr)
library(ggplot2)
library(emmeans)

# Filter for females and specified water sources
filtered_meta <- cleaned_meta %>%
  filter(Sex == "Female",
         `What.is.your.drinking.water.source.` %in% c("Tap", "River/Stream", "Lake", "Borehole", "Reliable_water", "Well")) %>%
  mutate(`What.is.your.drinking.water.source.` = factor(
    `What.is.your.drinking.water.source.`,
    levels = c("River/Stream", "Lake", "Well", "Borehole", "Reliable_water", "Tap")
  ))

# Calculate summary statistics for mean and median
summary_stats <- filtered_meta %>%
  group_by(`What.is.your.drinking.water.source.`) %>%
  summarise(
    Mean = mean(`Number.of.children`, na.rm = TRUE),
    Median = median(`Number.of.children`, na.rm = TRUE),
    .groups = 'drop'
  )

# Fit an ANOVA model
anova_model <- aov(`Number.of.children` ~ `What.is.your.drinking.water.source.`, data = filtered_meta)
summary(anova_model)

# Conduct pairwise comparisons with "River/Stream" as a reference
pairwise_results <- emmeans(anova_model, pairwise ~ `What.is.your.drinking.water.source.`, adjust = "tukey")
contrast_results <- contrast(emm_results, "trt.vs.ctrl1", ref = 1) 
summary(contrast_results)

# Plot
ggplot(filtered_meta, aes(x = `What.is.your.drinking.water.source.`, y = `Number.of.children`)) +
  geom_boxplot(aes(fill = `What.is.your.drinking.water.source.`), outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "gray") +
  geom_point(data = summary_stats, aes(x = `What.is.your.drinking.water.source.`, y = Mean), color = "black", size = 3, position = position_dodge(width = 0.75)) +
  geom_point(data = summary_stats, aes(x = `What.is.your.drinking.water.source.`, y = Median), color = "red", size = 3, position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Water Source", y = "Number of Children", title = "Distribution of Number of Children among ALL Females by Water Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
table(filtered_meta$What.is.your.drinking.water.source.)

####################
###########
####AGE SPLIT

library(dplyr)
library(ggplot2)
library(emmeans)

# Data preparation: filter for females and specified water sources, categorize by age, drop NAs in Age
filtered_meta <- cleaned_meta %>%
  filter(Sex == "Female",
         `What.is.your.drinking.water.source.` %in% c("Tap", "River/Stream", "Lake", "Borehole", "Reliable_water", "Well"),
         !is.na(Age)) %>%  # Ensure Age is not NA before categorization
  mutate(`What.is.your.drinking.water.source.` = factor(
    `What.is.your.drinking.water.source.`,
    levels = c("River/Stream", "Lake", "Well", "Borehole", "Reliable_water", "Tap")),
    Age_Group = ifelse(Age > 50, "Above 50", "Below 50"))

# Optionally, you might want to check or remove NAs in Age_Group if any unforeseen issue occurs
filtered_meta <- filtered_meta %>%
  filter(!is.na(Age_Group))


# Calculate summary statistics for mean and median
summary_stats <- filtered_meta %>%
  group_by(`What.is.your.drinking.water.source.`, Age_Group) %>%
  summarise(
    Mean = mean(`Number.of.children`, na.rm = TRUE),
    Median = median(`Number.of.children`, na.rm = TRUE),
    .groups = 'drop'
  )

# Fit an ANOVA model for both age groups
anova_model <- aov(`Number.of.children` ~ `What.is.your.drinking.water.source.` * Age_Group, data = filtered_meta)
summary(anova_model)

# Pairwise comparisons with "River/Stream" as a reference
pairwise_results <- emmeans(anova_model, pairwise ~ `What.is.your.drinking.water.source.` | Age_Group, adjust = "tukey")

# Plot with side-by-side facets for age groups
ggplot(filtered_meta, aes(x = `What.is.your.drinking.water.source.`, y = `Number.of.children`)) +
  geom_boxplot(aes(fill = `What.is.your.drinking.water.source.`), outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "gray") +
  geom_point(data = summary_stats, aes(x = `What.is.your.drinking.water.source.`, y = Median), color = "red", size = 3, position = position_dodge(width = 0.75)) +
  facet_wrap(~ Age_Group) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Water Source", y = "Number of Children", title = "Distribution of Number of Children among Females by Water Source and Age MEDIAN in Red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

# Check distribution of water sources by age group
table(filtered_meta$`What.is.your.drinking.water.source.`, filtered_meta$Age_Group)




