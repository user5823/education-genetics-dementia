##Setting working directory and loading dataset
library(data.table)
library(dplyr)
getwd()
setwd("/Users/thea/Desktop/Teams Folder")
df <- fread("5) Data_for_analysis.csv")
colnames(df)

#Create new dataset with WMH only 
sum(!is.na(df$White_matter_hyperintensity))
data <- df[!is.na(df$White_matter_hyperintensity)]

##Stratify education PRS (bottom 50% vs top 50%)
data$EA_PRS_half <- ntile(data$EA_PRS_standarized, 2)
data_bottom_EA_half <- subset(data, EA_PRS_half == 1) #Bottom half
data_top_EA_half <- subset(data, EA_PRS_half == 2) #Top half 

##Binarize years of schooling (using GCSEs as cut-off) 
data$edu_binary <- ifelse(data$Years_of_schooling > 12, 1, 0)
table(data$edu_binary)

##Linear regression in full sample
model_all <- lm(White_matter_hyperintensity ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                  PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_all)

##Linear regression in high EA PRS sample (top 50%)
model_top50_EA_PRS <- lm(White_matter_hyperintensity ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                           PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_top_EA_half)
summary(model_top50_EA_PRS)

##Linear regression in low EA PRS sample (bottom 50%)
model_bottom50_EA_PRS <- lm(White_matter_hyperintensity ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                              PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_bottom_EA_half)
summary(model_bottom50_EA_PRS)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE)
OR_top50_EA <- tidy(model_top50_EA_PRS, conf.int = TRUE)
OR_bottom50_EA <- tidy(model_bottom50_EA_PRS, conf.int = TRUE)

##Extract VAD_PRS rows only
get_VADPRS <- function(df, label) {
  df[df$term == "VAD_PRS_standarized", ] |>
    transform(group = label)
}

PRS_all    <- get_VADPRS(OR_all, "All")
PRS_top50   <- get_VADPRS(OR_top50_EA, "Top 50 EA PRS")
PRS_bottom50    <- get_VADPRS(OR_bottom50_EA, "Bottom 50 EA PRS")

##Comparison 
PRS_results <- rbind(PRS_all, PRS_top50, PRS_bottom50)
print(PRS_results[, c("group", "estimate", "conf.low", "conf.high")])

##Interaction test
model_interaction <- lm(White_matter_hyperintensity ~ VAD_PRS_standarized * EA_PRS_half + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                            PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction)

###Now repeat the same analyses but using binary education groups based on years of schooling

##High education
model_high_edu <- lm(White_matter_hyperintensity ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                       PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = subset(data, edu_binary == 1))
summary(model_high_edu)

##Low education
model_low_edu <- lm(White_matter_hyperintensity ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                      PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = subset(data, edu_binary == 0))
summary(model_low_edu)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE)
OR_high_edu <- tidy(model_high_edu, conf.int = TRUE)
OR_low_edu <- tidy(model_low_edu, conf.int = TRUE)

##Extract VAD_PRS rows only
get_VADPRS <- function(df, label) {
  df[df$term == "VAD_PRS_standarized", ] |>
    transform(group = label)
}

PRS_all    <- get_VADPRS(OR_all, "All")
PRS_high_edu   <- get_VADPRS(OR_high_edu, "High edu")
PRS_low_edu    <- get_VADPRS(OR_low_edu, "Low edu")

##Comparison 
PRS_results_2 <- rbind(PRS_all, PRS_high_edu, PRS_low_edu)
print(PRS_results_2[, c("group", "estimate", "conf.low", "conf.high")])

##Interaction test
model_interaction_2 <- lm(White_matter_hyperintensity ~ VAD_PRS_standarized * edu_binary + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                            PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction_2)
