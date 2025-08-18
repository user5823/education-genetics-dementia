##Setting working directory and loading dataset
library(data.table)
library(dplyr)
getwd()
setwd("/Users/thea/Desktop/Teams Folder")
df <- fread("5) Data_for_analysis.csv")
colnames(df)

#Create new dataset with NM only 
sum(!is.na(df$Numeric_memory))
data <- df[!is.na(df$Numeric_memory)]

#Linear regression of years of schooling on NM
model_YS <- lm(Numeric_memory ~ Years_of_schooling + Age + Sex + Assessment_centre, data = data)
summary(model_YS)

tidy_model_YS <- tidy(model_YS, conf.int = TRUE)
result_YS <- tidy_model_YS[tidy_model_YS$term == "Years_of_schooling", c("term", "estimate", "conf.low", "conf.high", "p.value")]
print(result_YS)

#Linear regression of EA PRS on NM
model_EA <- lm(Numeric_memory ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 + PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_EA)

tidy_model_EA <- tidy(model_EA, conf.int = TRUE)
result_EA <- tidy_model_EA[tidy_model_EA$term == "EA_PRS_standarized", c("term", "estimate", "conf.low", "conf.high", "p.value")]
print(result_EA)

#Linear regression of AD PRS on NM
model_AD <- lm(Numeric_memory ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 + PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_AD)

tidy_model_AD <- tidy(model_AD, conf.int = TRUE)
result_AD <- tidy_model_AD[tidy_model_AD$term == "AD_PRS_standarized", c("term", "estimate", "conf.low", "conf.high", "p.value")]
print(result_AD)

#Linear regression of VAD PRS on NM
model_VAD <- lm(Numeric_memory ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 + PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_VAD)

tidy_model_VAD <- tidy(model_VAD, conf.int = TRUE)
result_VAD <- tidy_model_VAD[tidy_model_VAD$term == "VAD_PRS_standarized", c("term", "estimate", "conf.low", "conf.high", "p.value")]
print(result_VAD)


