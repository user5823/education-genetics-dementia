##Setting working directory and loading dataset
library(data.table)
library(dplyr)
getwd()
setwd("/Users/thea/Desktop/Teams Folder")
data <- fread("5) Data_for_analysis.csv")
colnames(data)

##Logistic regression of VaD PRS on VaD diagnosis
model_VAD <- glm(VAD ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                       PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                     family = "binomial", data = data)
summary(model_VAD)

## Create clean OR table
library(broom)
OR_VAD <- tidy(model_VAD, conf.int = TRUE, exponentiate = TRUE)

##Extract VAD_PRS rows only
get_VADPRS <- function(df, label) {
  df[df$term == "VAD_PRS_standarized", ] |>
    transform(group = label)
}

PRS_VAD    <- get_VADPRS(OR_VAD, "All")
print(PRS_VAD[, c("group", "estimate", "conf.low", "conf.high", "p.value")])

##Logistic regression of EA PRS on VAD outcome
model_VAD_EA <- glm(VAD ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                      PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, 
                    family = "binomial", data = data)
summary(model_VAD_EA)

## Create clean OR table
OR_VAD_EA <- tidy(model_VAD_EA, conf.int = TRUE, exponentiate = TRUE)

##Extract EA_PRS rows only
get_EAPRS <- function(df, label) {
  df[df$term == "EA_PRS_standarized", ] |>
    transform(group = label)
}

PRS_VAD_EA    <- get_EAPRS(OR_VAD_EA, "All")
print(PRS_VAD_EA[, c("group", "estimate", "conf.low", "conf.high", "p.value")])



##Logistic regression of years of schooling on VAD outcome
model_VAD_YS <- glm(VAD ~ Years_of_schooling + Sex + Age + Assessment_centre, 
                    family = "binomial", data = data)
summary(model_VAD_YS)

## Create clean OR table
library(broom)
OR_VAD_YS <- tidy(model_VAD_YS, conf.int = TRUE, exponentiate = TRUE)

##Extract YoS rows only
get_YSPRS <- function(df, label) {
  df[df$term == "Years_of_schooling", ] |>
    transform(group = label)
}

PRS_VAD_YS    <- get_YSPRS(OR_VAD_YS, "All")
print(PRS_VAD_YS[, c("group", "estimate", "conf.low", "conf.high", "p.value")])

