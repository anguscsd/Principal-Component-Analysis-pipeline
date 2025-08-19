# set local working directory for easy access to bash pipeline eigen files 
setwd("/Users/angusdixon/MET583_workbook")


# Load eigen files and reference populatoion data from HAWK, filezilla was used 
scree_raw_data <- read.table("gbr_combined.eigenval", header = FALSE)
eigenvec_raw_data <- read.table("gbr_combined.eigenvec")
super_pop_raw <- read.table("all-1000g-phase3-chrall-mac5-v2.super-population")
population_raw <- read.table("all-1000g-phase3-chrall-mac5-v2.population")

# labelling the principal component values with a new column 
scree_raw_data$principal_component <- 1:nrow(scree_raw_data)

# Scree Plot
# Load ggplot2 library
library(ggplot2)

ggplot(scree_raw_data, aes(x = scree_raw_data[, 2], y = scree_raw_data[, 1])) +
  geom_line(color = "black", size = 1) +  # Black lines between the points
  geom_point(color = "red", shape = 19, size = 3) +  # Grey points
  labs(x = "Principal Component", y = "Eigenvalue", title = "Scree Plot - Eigenvalues") +
  scale_x_continuous(breaks = seq(1, max(scree_raw_data[, 2]), by = 1), limits = c(1, 20)) +  # Set x-axis range and labels
  scale_y_continuous(breaks = seq(0, max(scree_raw_data[, 1]), by = 100)) +  # Y-axis labels in increments of 100
  theme_minimal() +  # Minimal theme for better visual appearance
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.line = element_line(colour = "black")  # Retain axis lines
  )

# Creating easier dataframes

# Eigenvec data 
eigenvec_data <- eigenvec_raw_data[, c(1, 2, 3, 4, 5, 6, 7)]  # Extract the 3rd and 4th columns
# Give meaningful column names for clarity
colnames(eigenvec_data) <- c("C1", "C2", "PC1", "PC2", "PC3", "PC4", "PC5")

# populaton data
population <- population_raw[, c(1, 2, 3)]
# Give meaningful column names for clarity
colnames(population) <- c("C1", "C2", "POP")

# populaton data
super_pop <- super_pop_raw[, c(1, 2, 3)]
# Give meaningful column names for clarity
colnames(super_pop) <- c("C1", "C2", "SUPER")

# Merging datasets
library(tidyverse)
library(dplyr)

pca_population <- eigenvec_data %>%
  full_join(population, by = "C1")
head(pca_population)

library(dplyr)

# Calculate the mean and SD for "GBR"
gbr_stats <- pca_population %>%
  filter(POP == "GBR") %>%
  summarise(
    mean_PC1 = mean(PC1, na.rm = TRUE),
    sd_PC1 = sd(PC1, na.rm = TRUE),
    mean_PC2 = mean(PC2, na.rm = TRUE),
    sd_PC2 = sd(PC2, na.rm = TRUE),
    mean_PC3 = mean(PC3, na.rm = TRUE),
    sd_PC3 = sd(PC3, na.rm = TRUE)
  )

# Identify NA rows and modify their population value based on SD range of "GBR"
# +/- 2 SD was chosen 
pca_eGBR <- pca_population %>%
  mutate(
    POP = if_else(
      is.na(POP) &  # Only change where population isnt defined
        (PC1 >= gbr_stats$mean_PC1 - 1 * gbr_stats$sd_PC1 & PC1 <= gbr_stats$mean_PC1 + 1 * gbr_stats$sd_PC1) &
        (PC2 >= gbr_stats$mean_PC2 - 1 * gbr_stats$sd_PC2 & PC2 <= gbr_stats$mean_PC2 + 1 * gbr_stats$sd_PC2) &
        (PC3 >= gbr_stats$mean_PC3 - 1 * gbr_stats$sd_PC3 & PC3 <= gbr_stats$mean_PC3 + 1 * gbr_stats$sd_PC3),
      "eGBR",  # Mark those within the range as "eGBR"
      as.character(POP)  # Keep the original value of POP if not NA or not within the range
    )
  )

# View the result
head(pca_eGBR)

# Perform the left join by "C1" column
super_pca_eGBR <- pca_eGBR %>%
  left_join(super_pop %>% select(C1, SUPER), by = "C1")

# Update SUPER column to "eGBR" where POP is "eGBR"
super_pca_eGBR <- super_pca_eGBR %>%
  mutate(SUPER = ifelse(POP == "eGBR", "eGBR", SUPER))

# Replace NA values in the SUPER column with "UNKNOWN"
super_pca_eGBR <- super_pca_eGBR %>%
  mutate(SUPER = ifelse(is.na(SUPER), "UNKNOWN", SUPER))

# Chcek the number of eGBR predictions 
table(super_pca_eGBR$POP == "eGBR")

# Create a new table with only rows where POP == "eGBR"
eGBR_only <- super_pca_eGBR %>% 
  filter(POP == "eGBR")

eGBR_only <- eGBR_only %>% 
  select(-C2.y, -SUPER)

# View the new table
head(eGBR_only)

# Count number of estimated GBR
nrow(eGBR_only)

# Save eGBR_only as a tab-separated text file to save as .keep in hawk
write.table(eGBR_only, "/Users/angusdixon/MET583_workbook/eGBR_only.txt", 
            sep = "\t", row.names = FALSE, quote = FALSE)

library(ggplot2)

ggplot(super_pca_eGBR, aes(x = PC1, y = PC2)) +
  geom_point(data = super_pca_eGBR %>% filter(SUPER != "eGBR"), aes(color = SUPER)) +
  geom_point(data = super_pca_eGBR %>% filter(SUPER == "eGBR"), aes(color = SUPER)) +
  scale_color_manual(
    values = c(
      "AMR" = "#34495e", 
      "AFR" = "#BB8FCE", 
      "eGBR" = "#FF5733", 
      "EAS" = "#2980b9", 
      "EUR" = "#f1c40f", 
      "SAS" = "#48C9B0"),
    na.value = "grey") +
  ggtitle("Principal Component Analysis: PC1 and PC2") +
  theme_classic()

ggplot(super_pca_eGBR, aes(x = PC1, y = PC3)) +
  geom_point(data = super_pca_eGBR %>% filter(SUPER != "eGBR"), aes(color = SUPER)) +
  geom_point(data = super_pca_eGBR %>% filter(SUPER == "eGBR"), aes(color = SUPER)) +
  scale_color_manual(
    values = c(
      "AMR" = "#34495e", 
      "AFR" = "#BB8FCE", 
      "eGBR" = "#FF5733", 
      "EAS" = "#2980b9", 
      "EUR" = "#f1c40f", 
      "SAS" = "#48C9B0"),
    na.value = "grey") +
  ggtitle("Principal Component Analysis: PC1 and PC3") +
  theme_classic()

ggplot(super_pca_eGBR, aes(x = PC2, y = PC3)) +
  geom_point(data = super_pca_eGBR %>% filter(SUPER != "eGBR"), aes(color = SUPER)) +
  geom_point(data = super_pca_eGBR %>% filter(SUPER == "eGBR"), aes(color = SUPER)) +
  scale_color_manual(
    values = c(
      "AMR" = "#34495e", 
      "AFR" = "#BB8FCE", 
      "eGBR" = "#FF5733", 
      "EAS" = "#2980b9", 
      "EUR" = "#f1c40f", 
      "SAS" = "#48C9B0"),
    na.value = "grey") +
  ggtitle("Principal Component Analysis: PC2 and PC3") +
  theme_classic()

#RANDOM FOREST
library(randomForest)
library(dplyr)
library(pROC)

set.seed(123)

# Remove rows where POP is NA for training
train_data <- pca_population %>% filter(!is.na(POP))

# Create test dataset (individuals with unknown population)
test_data <- pca_population %>% filter(is.na(POP))

# Ensure POP is a factor for classification
train_data$POP <- as.factor(train_data$POP)

# Exclude non-predictive columns like sample IDs
predictors <- colnames(pca_population)[!colnames(pca_population) %in% c("POP", "C1", "C2.x", "C2.y")]

# Define predictor variables (exclude POP and any non-genetic columns)
predictors <- setdiff(names(pca_population), c("POP", "C1", "C2.x", "C2.y"))  # Exclude non-predictive columns

# Train the Random Forest model
rf_model <- randomForest(as.formula(paste("POP ~", paste(predictors, collapse = " + "))),
                         data = train_data, 
                         ntree = 1000, 
                         importance = TRUE)

# Predict population for unknown individuals
test_data$POP_Predicted <- predict(rf_model, newdata = test_data[, predictors])

# Extract individuals predicted to be GBR
gbr_predictions <- test_data %>% filter(POP_Predicted == "GBR")

# View results
print(gbr_predictions)

# Load pROC package if not already loaded
library(pROC)

# Get predicted probabilities for the positive class
predictions <- predict(rf_model, type = "prob")[, 2]

# Create the ROC curve
roc_curve <- roc(train_data$POP, predictions)

# Calculate the AUC
roc_auc <- auc(roc_curve)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve with AUC", col = "blue", lwd = 2)

# Add the AUC value to the plot
text(0.6, 0.4, paste("AUC =", round(roc_auc, 2)), col = "red", cex = 1.5)

#Pruning Visualisation
# Create a dataframe with chromosome number, pruned SNPs, and remaining SNPs
# Pruned SNPs from PLINK
point2_pruned_data <- data.frame(
  Chromosome = 1:22,
  Pruned_SNPs = c(18420, 20336, 16517, 14913, 14129, 14599, 12774, 13283, 11670, 12724, 
                  11325, 11184, 9149, 8071, 7612, 7145, 6004, 6944, 3652, 5703, 3264, 3685),
  Remaining_SNPs = c(7332, 6808, 5959, 5468, 5346, 5235, 4915, 4475, 4263, 4725, 
                     4316, 4435, 3468, 3163, 3000, 3235, 2901, 2978, 2140, 2585, 1466, 1609)
)

# Print the table
print(point2_pruned_data)

library(reshape2)
point2_pruned_data_long <- melt(point2_pruned_data, id.vars = "Chromosome", 
                                variable.name = "Variant_Type", value.name = "Count")

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Create a dataframe
point1_pruned_data <- data.frame(
  Chromosome = as.factor(1:22),
  Pruned_SNPs = c(21990, 23838, 19477, 17570, 16760, 17205, 15185, 15507, 13698, 15031, 
                  13420, 13289, 10841, 9596, 9000, 8702, 7308, 8343, 4545, 6909, 3959, 4428),
  Remaining_SNPs = c(3762, 3306, 2999, 2811, 2715, 2629, 2504, 2251, 2235, 2418, 
                     2221, 2330, 1776, 1638, 1612, 1678, 1597, 1579, 1247, 1379, 771, 866)
)

# Reshape data for ggplot
point1_pruned_data_long <- melt(point1_pruned_data, id.vars = "Chromosome", 
                                variable.name = "Variant_Type", value.name = "Count")
# Create the comparison dataframe
comparison_remaining <- data.frame(
  Chromosome = as.factor(1:22),
  R2_0.1 = c(3762, 3306, 2999, 2811, 2715, 2629, 2504, 2251, 2235, 2418, 
             2221, 2330, 1776, 1638, 1612, 1678, 1597, 1579, 1247, 1379, 771, 866),
  R2_0.2 = c(7332, 6808, 5959, 5468, 5346, 5235, 4915, 4475, 4263, 4725, 
             4316, 4435, 3468, 3163, 3000, 3235, 2901, 2978, 2140, 2585, 1466, 1609)
)

# Reshape data for ggplot
comparison_remaining_long <- melt(comparison_remaining, id.vars = "Chromosome", 
                                  variable.name = "R2_Threshold", value.name = "Remaining_SNPs")

# Plot
ggplot(comparison_remaining_long, aes(x = Chromosome, y = Remaining_SNPs, fill = R2_Threshold)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("R2_0.2" = "#CB4335", "R2_0.1" = "#34495E"),
                    labels = c(expression(R^2 ~ "= 0.1"), expression(R^2 ~ "= 0.2"))) +
  labs(title = "Comparison of Remaining SNPs per Chromosome",
       subtitle = "Showing the SNPs remaining after pruning with the LD Rsquared values 0.1 and 0.2",
       x = "Chromosome",
       y = "Number of Remaining SNPs",
       fill = "LD Threshold (R²)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.subtitle = element_text(size = 9),
        plot.title = element_text(size = 15)) +
  scale_y_continuous(expand = c(0, 0))  # Removes space between x-axis and bars

