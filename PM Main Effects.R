##Setting working directory and loading dataset
library(data.table)
library(dplyr)
library(ordinal)
library(MASS)
library(broom)

getwd()
setwd("/Users/thea/Desktop/Teams Folder")
df <- fread("5) Data_for_analysis.csv")
colnames(df)

#Create new dataset with PM only 
sum(!is.na(df$Prospective_memory))
data <- df[!is.na(df$Prospective_memory)]

data$Prospective_memory <- recode(
  data$Prospective_memory,
  `2` = 1,
  `1` = 2,
  `0` = 0
)
table(data$Prospective_memory)

data$Prospective_memory <- factor(data$Prospective_memory, 
                                  levels = c(0, 1, 2), 
                                  ordered = TRUE)

#Prep for ordinal logistic regression
str(data$Prospective_memory)
str(data$Sex)
str(data$Assessment_centre)

data$Prospective_memory <- ordered(data$Prospective_memory)
data$Sex <- factor(data$Sex)
data$Assessment_centre <- factor(data$Assessment_centre)

#Ordinal logistic regression of Years of schooling on PM
model <- clm(Prospective_memory ~ Years_of_schooling + Age + Sex + Assessment_centre, data = data)
summary(model)
sum_model <- summary(model)
coefs <- coef(sum_model)

log_odds <- coefs["Years_of_schooling", "Estimate"]
se <- coefs["Years_of_schooling", "Std. Error"]
zval <- coefs["Years_of_schooling", "z value"]
pval <- coefs["Years_of_schooling", "Pr(>|z|)"]

odds_ratio <- exp(log_odds)
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))


#Ordinal logistic regression of EA PRS on PM
model_EA <- clm(Prospective_memory ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 + PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_EA)

sum_model_EA <- summary(model_EA)
coefs <- coef(sum_model_EA)
log_odds <- coefs["EA_PRS_standarized", "Estimate"]
se <- coefs["EA_PRS_standarized", "Std. Error"]
zval <- coefs["EA_PRS_standarized", "z value"]
pval <- coefs["EA_PRS_standarized", "Pr(>|z|)"]

odds_ratio <- exp(log_odds)

ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))


#Ordinal logistic regression of AD PRS on PM
model_AD <- clm(Prospective_memory ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 + PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_AD)

sum_model_AD <- summary(model_AD)

coefs <- coef(sum_model_AD)

log_odds <- coefs["AD_PRS_standarized", "Estimate"]
se <- coefs["AD_PRS_standarized", "Std. Error"]
zval <- coefs["AD_PRS_standarized", "z value"]
pval <- coefs["AD_PRS_standarized", "Pr(>|z|)"]

odds_ratio <- exp(log_odds)

ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))


#Ordinal Logistic regression of VAD PRS on PM
model_VAD <- clm(Prospective_memory ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 + PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_VAD)

sum_model_VAD <- summary(model_VAD)

coefs <- coef(sum_model_VAD)

log_odds <- coefs["VAD_PRS_standarized", "Estimate"]
se <- coefs["VAD_PRS_standarized", "Std. Error"]
zval <- coefs["VAD_PRS_standarized", "z value"]
pval <- coefs["VAD_PRS_standarized", "Pr(>|z|)"]

odds_ratio <- exp(log_odds)

ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))

