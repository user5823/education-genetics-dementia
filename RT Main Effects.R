##Setting working directory and loading dataset
library(data.table)
library(dplyr)
getwd()
setwd("/Users/thea/Desktop/Teams Folder")
df <- fread("5) Data_for_analysis.csv")
colnames(df)

#Create new dataset with RT only 
sum(!is.na(df$Reaction_time))
data <- df[!is.na(df$Reaction_time)]

#Linear regression of years of schooling on RT
model_YS <- lm(Reaction_time ~ Years_of_schooling + Age + Sex + Assessment_centre, data = data)
summary(model_YS)

tidy_model_YS <- tidy(model_YS, conf.int = TRUE)
result_YS <- tidy_model_YS[tidy_model_YS$term == "Years_of_schooling", c("term", "estimate", "conf.low", "conf.high", "p.value")]
print(result_YS)

#Linear regression of EA PRS on RT
model_EA <- lm(Reaction_time ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 + PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_EA)

tidy_model_EA <- tidy(model_EA, conf.int = TRUE)
result_EA <- tidy_model_EA[tidy_model_EA$term == "EA_PRS_standarized", c("term", "estimate", "conf.low", "conf.high", "p.value")]
print(result_EA)

#Linear regression of AD PRS on RT
model_AD <- lm(Reaction_time ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 + PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_AD)

tidy_model_AD <- tidy(model_AD, conf.int = TRUE)
result_AD <- tidy_model_AD[tidy_model_AD$term == "AD_PRS_standarized", c("term", "estimate", "conf.low", "conf.high", "p.value")]
print(result_AD)

#Linear regression of VAD PRS on RT
model_VAD <- lm(Reaction_time ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 + PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_VAD)

tidy_model_VAD <- tidy(model_VAD, conf.int = TRUE)
result_VAD <- tidy_model_VAD[tidy_model_VAD$term == "VAD_PRS_standarized", c("term", "estimate", "conf.low", "conf.high", "p.value")]
print(result_VAD)












