##Setting working directory and loading dataset
rm(list = ls())
library(data.table)
library(dplyr)
library(broom)
getwd()
setwd("/Users/thea/Desktop/Teams Folder")
df <- fread("5) Data_for_analysis.csv")
colnames(data)

#Create new dataset with WMH only 
sum(!is.na(df$White_matter_hyperintensity))
data <- df[!is.na(df$White_matter_hyperintensity)]

##Stratify VAD PRS (bottom 50% vs top 50%)
data$VAD_PRS_half <- ntile(data$VAD_PRS_standarized, 2)
data_bottom_VAD_half <- subset(data, VAD_PRS_half == 1) #Bottom half
data_top_VAD_half <- subset(data, VAD_PRS_half == 2) #Top half 

##Linear regression in whole sample
model_all <- lm(White_matter_hyperintensity ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                            PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_all)

##Linear regression in high VAD PRS sample (top 50%)
model_top50_VAD_PRS <- lm(White_matter_hyperintensity ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                           PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_top_VAD_half)
summary(model_top50_VAD_PRS)

##Linear regression in low VAD PRS sample (bottom 50%)
model_bottom50_VAD_PRS <- lm(White_matter_hyperintensity ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                              PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_bottom_VAD_half)
summary(model_bottom50_VAD_PRS)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE)
OR_top50_VAD <- tidy(model_top50_VAD_PRS, conf.int = TRUE)
OR_bottom50_VAD <- tidy(model_bottom50_VAD_PRS, conf.int = TRUE)

##Extract EA_PRS rows only
get_EAPRS <- function(df, label) {
  df[df$term == "EA_PRS_standarized", ] |>
    transform(group = label)
}

PRS_all    <- get_EAPRS(OR_all, "All")
PRS_top50   <- get_EAPRS(OR_top50_VAD, "Top 50 VAD PRS")
PRS_bottom50    <- get_EAPRS(OR_bottom50_VAD, "Bottom 50 VAD PRS")

##Comparison 
PRS_results <- rbind(PRS_all, PRS_top50, PRS_bottom50)
print(PRS_results[, c("group", "estimate", "conf.low", "conf.high")])

##Interaction test
model_interaction <- lm(White_matter_hyperintensity ~ VAD_PRS_standarized * EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                          PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction)

###Now using years of schooling

##Linear regression in whole sample
model_all <- lm(White_matter_hyperintensity ~ Years_of_schooling + Sex + Age + Assessment_centre, data = data)
summary(model_all)

##Linear regression in high VAD PRS sample (top 50%)
model_top50_VAD_PRS <- lm(White_matter_hyperintensity ~ Years_of_schooling + Sex + Age + Assessment_centre, data = data_top_VAD_half)
summary(model_top50_VAD_PRS)

##Linear regression in low VAD PRS sample (bottom 50%)
model_bottom50_VAD_PRS <- lm(White_matter_hyperintensity ~ Years_of_schooling + Sex + Age + Assessment_centre, data = data_bottom_VAD_half)
summary(model_bottom50_VAD_PRS)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE)
OR_top50_VAD <- tidy(model_top50_VAD_PRS, conf.int = TRUE)
OR_bottom50_VAD <- tidy(model_bottom50_VAD_PRS, conf.int = TRUE)

##Extract YS_PRS rows only
get_YSPRS <- function(df, label) {
  df[df$term == "Years_of_schooling", ] |>
    transform(group = label)
}

PRS_all    <- get_YSPRS(OR_all, "All")
PRS_top50   <- get_YSPRS(OR_top50_VAD, "Top 50 VAD PRS")
PRS_bottom50    <- get_YSPRS(OR_bottom50_VAD, "Bottom 50 VAD PRS")

##Comparison 
PRS_results <- rbind(PRS_all, PRS_top50, PRS_bottom50)
print(PRS_results[, c("group", "estimate", "conf.low", "conf.high")])


##Interaction test
model_interaction_2 <- lm(White_matter_hyperintensity ~ VAD_PRS_standarized * Years_of_schooling + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                            PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction_2)




