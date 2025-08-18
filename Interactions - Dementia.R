##Setting working directory and loading dataset
library(data.table)
library(dplyr)
library(broom)
getwd()
setwd("/Users/thea/Desktop/Teams Folder")
data <- fread("5) Data_for_analysis.csv")
colnames(data)

##Stratify education PRS (bottom 50% vs top 50%)
data$EA_PRS_half <- ntile(data$EA_PRS_standarized, 2)
data_bottom_EA_half <- subset(data, EA_PRS_half == 1) #Bottom half
data_top_EA_half <- subset(data, EA_PRS_half == 2) #Top half 

##Binarize years of schooling (using GCSEs as cut-off) 
data$edu_binary <- ifelse(data$Years_of_schooling > 12, 1, 0)
table(data$edu_binary)

##Logistic regression of AD PRS on AD in full sample
model_all <- glm(AD ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                   PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                 family = "binomial", data = data)
summary(model_all)

##Logistic regression in high EA PRS sample (top 50%)
model_top50_EA_PRS <- glm(AD ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                            PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                          family = "binomial", data = data_top_EA_half)
summary(model_top50_EA_PRS)

##Logistic regression in low EA PRS sample (bottom 50%)
model_bottom50_EA_PRS <- glm(AD ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                               PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                             family = "binomial", data = data_bottom_EA_half)
summary(model_bottom50_EA_PRS)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE, exponentiate = TRUE)
OR_top50_EA <- tidy(model_top50_EA_PRS, conf.int = TRUE, exponentiate = TRUE)
OR_bottom50_EA <- tidy(model_bottom50_EA_PRS, conf.int = TRUE, exponentiate = TRUE)

##Extract AD_PRS rows only
get_ADPRS <- function(df, label) {
  df[df$term == "AD_PRS_standarized", ] |>
    transform(group = label)
}

PRS_all    <- get_ADPRS(OR_all, "All")
PRS_top50   <- get_ADPRS(OR_top50_EA, "Top 50 EA PRS")
PRS_bottom50    <- get_ADPRS(OR_bottom50_EA, "Bottom 50 EA PRS")

##Comparison 
PRS_results <- rbind(PRS_all, PRS_top50, PRS_bottom50)
print(PRS_results[, c("group", "estimate", "conf.low", "conf.high")])

##Interaction test
model_interaction <- glm(AD ~ AD_PRS_standarized * EA_PRS_half + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                           family = "binomial", data = data)
summary(model_interaction)


##Now stratification by high vs low years of schooling

##High education
model_high_edu <- glm(AD ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                        PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                      family = "binomial", data = subset(data, edu_binary == 1))
summary(model_high_edu)

##Low education

model_low_edu <- glm(AD ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                       PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                     family = "binomial", data = subset(data, edu_binary == 0))
summary(model_low_edu)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE, exponentiate = TRUE)
OR_high_edu <- tidy(model_high_edu, conf.int = TRUE, exponentiate = TRUE)
OR_low_edu <- tidy(model_low_edu, conf.int = TRUE, exponentiate = TRUE)

##Extract AD_PRS rows only
get_ADPRS_2 <- function(df, label) {
  df[df$term == "AD_PRS_standarized", ] |>
    transform(group = label)
}

PRS_all    <- get_ADPRS_2(OR_all, "All")
PRS_high_edu   <- get_ADPRS_2(OR_high_edu, "High edu")
PRS_low_edu    <- get_ADPRS_2(OR_low_edu, "Low edu")

##Comparison 
PRS_results_2 <- rbind(PRS_all, PRS_high_edu, PRS_low_edu)
print(PRS_results_2[, c("group", "estimate", "conf.low", "conf.high")])

##Interaction test
model_interaction_2 <- glm(AD ~ AD_PRS_standarized * edu_binary + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                           family = "binomial", data = data)
summary(model_interaction_2)



##Logistic regression of VAD PRS on VAD in full sample
model_all <- glm(VAD ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                   PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                 family = "binomial", data = data)
summary(model_all)

##Logistic regression in high EA PRS sample (top 50%)
model_top50_EA_PRS <- glm(VAD ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                            PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                          family = "binomial", data = data_top_EA_half)
summary(model_top50_EA_PRS)

##Logistic regression in low EA PRS sample (bottom 50%)
model_bottom50_EA_PRS <- glm(VAD ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                               PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                             family = "binomial", data = data_bottom_EA_half)
summary(model_bottom50_EA_PRS)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE, exponentiate = TRUE)
OR_top50_EA <- tidy(model_top50_EA_PRS, conf.int = TRUE, exponentiate = TRUE)
OR_bottom50_EA <- tidy(model_bottom50_EA_PRS, conf.int = TRUE, exponentiate = TRUE)

##Extract VAD_PRS rows only
get_VADPRS <- function(df, label) {
  df[df$term == "VAD_PRS_standarized", ] |>
    transform(group = label)
}

PRS_all    <- get_VADPRS(OR_all, "All")
PRS_top50   <- get_VADPRS(OR_top50_EA, "Top 50 EA PRS")
PRS_bottom50    <- get_VADPRS(OR_bottom50_EA, "Bottom 50 EA PRS")

##Comparison 
PRS_results_3 <- rbind(PRS_all, PRS_top50, PRS_bottom50)
print(PRS_results_3[, c("group", "estimate", "conf.low", "conf.high")])

##Interaction test
model_interaction_3 <- glm(VAD ~ VAD_PRS_standarized * EA_PRS_half + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                           family = "binomial", data = data)
summary(model_interaction_3)

###Repeat but stratifying by years of schooling.

##High education
model_high_edu <- glm(VAD ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                        PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                      family = "binomial", data = subset(data, edu_binary == 1))
summary(model_high_edu)

##Low education
model_low_edu <- glm(VAD ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                       PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                     family = "binomial", data = subset(data, edu_binary == 0))
summary(model_low_edu)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE, exponentiate = TRUE)
OR_high_edu <- tidy(model_high_edu, conf.int = TRUE, exponentiate = TRUE)
OR_low_edu <- tidy(model_low_edu, conf.int = TRUE, exponentiate = TRUE)

##Extract VAD_PRS rows only
get_VADPRS <- function(df, label) {
  df[df$term == "VAD_PRS_standarized", ] |>
    transform(group = label)
}

PRS_all    <- get_VADPRS(OR_all, "All")
PRS_high_edu   <- get_VADPRS(OR_high_edu, "High edu")
PRS_low_edu    <- get_VADPRS(OR_low_edu, "Low edu")

##Comparison 
PRS_results_4 <- rbind(PRS_all, PRS_high_edu, PRS_low_edu)
print(PRS_results_4[, c("group", "estimate", "conf.low", "conf.high")])

##Interaction test
model_interaction_4 <- glm(VAD ~ VAD_PRS_standarized * edu_binary + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                           family = "binomial", data = data)
summary(model_interaction_4)

