##Setting working directory and loading dataset
rm(list = ls())
library(data.table)
library(dplyr)
library(broom)
getwd()
setwd("/Users/thea/Desktop/Teams Folder")
data <- fread("5) Data_for_analysis.csv")
colnames(data)

##Stratify AD PRS (bottom 50% vs top 50%)
data$AD_PRS_half <- ntile(data$AD_PRS_standarized, 2)
data_bottom_AD_half <- subset(data, AD_PRS_half == 1) #Bottom half
data_top_AD_half <- subset(data, AD_PRS_half == 2) #Top half 

##Logistic regression of EA PRS on AD in full sample
model_all <- glm(AD ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                   PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                 family = "binomial", data = data)
summary(model_all)

##Logistic regression in high AD PRS sample (top 50%)
model_top50_AD_PRS <- glm(AD ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                            PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                          family = "binomial", data = data_top_AD_half)
summary(model_top50_AD_PRS)

##Logistic regression in low AD PRS sample (bottom 50%)
model_bottom50_AD_PRS <- glm(AD ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                               PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                             family = "binomial", data = data_bottom_AD_half)
summary(model_bottom50_AD_PRS)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE, exponentiate = TRUE)
OR_top50_AD <- tidy(model_top50_AD_PRS, conf.int = TRUE, exponentiate = TRUE)
OR_bottom50_AD <- tidy(model_bottom50_AD_PRS, conf.int = TRUE, exponentiate = TRUE)

##Extract EA_PRS rows only
get_EAPRS <- function(df, label) {
  df[df$term == "EA_PRS_standarized", ] |>
    transform(group = label)
}

PRS_all    <- get_EAPRS(OR_all, "All")
PRS_top50   <- get_EAPRS(OR_top50_AD, "Top 50 AD PRS")
PRS_bottom50    <- get_EAPRS(OR_bottom50_AD, "Bottom 50 AD PRS")

##Comparison 
PRS_results <- rbind(PRS_all, PRS_top50, PRS_bottom50)
print(PRS_results[, c("group", "estimate", "conf.low", "conf.high")])

##Interaction test
model_interaction <- glm(AD ~ AD_PRS_standarized * EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                           PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                         family = "binomial", data = data)
summary(model_interaction)


##Now effects of years of schooling 

##Logistic regression of YoS on AD in full sample
model_all <- glm(AD ~ Years_of_schooling + Sex + Age + Assessment_centre, 
                 family = "binomial", data = data)
summary(model_all)

##Logistic regression in high AD PRS sample (top 50%)
model_top50_AD_PRS <- glm(AD ~ Years_of_schooling + Sex + Age + Assessment_centre, 
                          family = "binomial", data = data_top_AD_half)
summary(model_top50_AD_PRS)

##Logistic regression in low AD PRS sample (bottom 50%)
model_bottom50_AD_PRS <- glm(AD ~ Years_of_schooling + Sex + Age + Assessment_centre, 
                             family = "binomial", data = data_bottom_AD_half)
summary(model_bottom50_AD_PRS)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE, exponentiate = TRUE)
OR_top50_AD <- tidy(model_top50_AD_PRS, conf.int = TRUE, exponentiate = TRUE)
OR_bottom50_AD <- tidy(model_bottom50_AD_PRS, conf.int = TRUE, exponentiate = TRUE)

##Extract YS rows only
get_YSPRS <- function(df, label) {
  df[df$term == "Years_of_schooling", ] |>
    transform(group = label)
}

PRS_all    <- get_YSPRS(OR_all, "All")
PRS_top50   <- get_YSPRS(OR_top50_AD, "Top 50 AD PRS")
PRS_bottom50    <- get_YSPRS(OR_bottom50_AD, "Bottom 50 AD PRS")

##Comparison 
PRS_results <- rbind(PRS_all, PRS_top50, PRS_bottom50)
print(PRS_results[, c("group", "estimate", "conf.low", "conf.high")])


##Interaction test
model_interaction_2 <- glm(AD ~ AD_PRS_standarized * Years_of_schooling + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                           family = "binomial", data = data)
summary(model_interaction_2)



##Stratify VAD PRS (bottom 50% vs top 50%)
data$VAD_PRS_half <- ntile(data$VAD_PRS_standarized, 2)
data_bottom_VAD_half <- subset(data, VAD_PRS_half == 1) #Bottom half
data_top_VAD_half <- subset(data, VAD_PRS_half == 2) #Top half 

##Logistic regression of EA PRS on VAD in full sample
model_all <- glm(VAD ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                   PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                 family = "binomial", data = data)
summary(model_all)

##Logistic regression in high VAD PRS sample (top 50%)
model_top50_VAD_PRS <- glm(VAD ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                            PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                          family = "binomial", data = data_top_VAD_half)
summary(model_top50_VAD_PRS)

##Logistic regression in low VAD PRS sample (bottom 50%)
model_bottom50_VAD_PRS <- glm(VAD ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                               PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                             family = "binomial", data = data_bottom_VAD_half)
summary(model_bottom50_VAD_PRS)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE, exponentiate = TRUE)
OR_top50_VAD <- tidy(model_top50_VAD_PRS, conf.int = TRUE, exponentiate = TRUE)
OR_bottom50_VAD <- tidy(model_bottom50_VAD_PRS, conf.int = TRUE, exponentiate = TRUE)

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
model_interaction_3 <- glm(VAD ~ VAD_PRS_standarized * EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                           family = "binomial", data = data)
summary(model_interaction_3)





##Now effects of years of schooling 

##Logistic regression of YoS on VAD in full sample
model_all <- glm(VAD ~ Years_of_schooling + Sex + Age + Assessment_centre, 
                 family = "binomial", data = data)
summary(model_all)

##Logistic regression in high VAD PRS sample (top 50%)
model_top50_VAD_PRS <- glm(VAD ~ Years_of_schooling + Sex + Age + Assessment_centre, 
                          family = "binomial", data = data_top_VAD_half)
summary(model_top50_VAD_PRS)

##Logistic regression in low VAD PRS sample (bottom 50%)
model_bottom50_VAD_PRS <- glm(VAD ~ Years_of_schooling + Sex + Age + Assessment_centre, 
                             family = "binomial", data = data_bottom_VAD_half)
summary(model_bottom50_VAD_PRS)

## Create clean OR tables
OR_all <- tidy(model_all, conf.int = TRUE, exponentiate = TRUE)
OR_top50_VAD <- tidy(model_top50_VAD_PRS, conf.int = TRUE, exponentiate = TRUE)
OR_bottom50_VAD <- tidy(model_bottom50_VAD_PRS, conf.int = TRUE, exponentiate = TRUE)

##Extract YS rows only
get_YSPRS <- function(df, label) {
  df[df$term == "Years_of_schooling", ] |>
    transform(group = label)
}

PRS_all    <- get_YSPRS(OR_all, "All")
PRS_top50   <- get_YSPRS(OR_top50_VAD, "Top 50 AD PRS")
PRS_bottom50    <- get_YSPRS(OR_bottom50_VAD, "Bottom 50 AD PRS")

##Comparison 
PRS_results <- rbind(PRS_all, PRS_top50, PRS_bottom50)
print(PRS_results[, c("group", "estimate", "conf.low", "conf.high")])

##Interaction test
model_interaction_4 <- glm(VAD ~ VAD_PRS_standarized * Years_of_schooling + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                           family = "binomial", data = data)
summary(model_interaction_4)

