##Setting working directory and loading dataset
library(data.table)
library(dplyr)
getwd()
setwd("/Users/thea/Desktop/Teams Folder")
data <- fread("5) Data_for_analysis.csv")
colnames(data)

##Logistic regression of AD PRS on AD diagnosis
model_AD <- glm(AD ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                   PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                 family = "binomial", data = data)
summary(model_AD)

## Create clean OR table
library(broom)
OR_AD <- tidy(model_AD, conf.int = TRUE, exponentiate = TRUE)

##Extract AD_PRS rows only
get_ADPRS <- function(df, label) {
  df[df$term == "AD_PRS_standarized", ] |>
    transform(group = label)
}

PRS_AD    <- get_ADPRS(OR_AD, "All")
print(PRS_AD[, c("group", "estimate", "conf.low", "conf.high", "p.value")])

##Logistic regression of EA PRS on AD outcome
model_EA_AD <- glm(AD ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                      PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                    family = "binomial", data = data)
summary(model_EA_AD)

## Create clean OR table
OR_EA_AD <- tidy(model_EA_AD, conf.int = TRUE, exponentiate = TRUE)

##Extract EA_PRS rows only
get_EAPRS <- function(df, label) {
  df[df$term == "EA_PRS_standarized", ] |>
    transform(group = label)
}

PRS_EA_AD    <- get_EAPRS(OR_EA_AD, "All")
print(PRS_EA_AD[, c("group", "estimate", "conf.low", "conf.high", "p.value")])


##Logistic regression of years of schooling on AD outcome
model_YS_AD <- glm(AD ~ Years_of_schooling + Sex + Age + Assessment_centre, 
                    family = "binomial", data = data)
summary(model_YS_AD)

## Create clean OR table
library(broom)
OR_YS_AD <- tidy(model_YS_AD, conf.int = TRUE, exponentiate = TRUE)

##Extract YoS rows only
get_YSPRS <- function(df, label) {
  df[df$term == "Years_of_schooling", ] |>
    transform(group = label)
}

PRS_YS_AD    <- get_YSPRS(OR_YS_AD, "All")
print(PRS_YS_AD[, c("group", "estimate", "conf.low", "conf.high", "p.value")])





