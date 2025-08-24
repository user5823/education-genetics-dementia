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


##Stratify AD PRS (bottom 50% vs top 50%)
data$AD_PRS_half <- ntile(data$AD_PRS_standarized, 2)
data_bottom_AD_half <- subset(data, AD_PRS_half == 1) #Bottom half
data_top_AD_half <- subset(data, AD_PRS_half == 2) #Top half 


#Prep for ordinal logistic regression
data$Prospective_memory <- ordered(data$Prospective_memory)
data$Sex <- factor(data$Sex)
data$Assessment_centre <- factor(data$Assessment_centre)
data$Genotype_batch <- factor(data$Genotype_batch)

data_bottom_AD_half$Prospective_memory <- factor(data_bottom_AD_half$Prospective_memory, 
                                                 levels = c(0, 1, 2), 
                                                 ordered = TRUE)

data_bottom_AD_half$Prospective_memory <- ordered(data_bottom_AD_half$Prospective_memory)
data_bottom_AD_half$Sex <- factor(data_bottom_AD_half$Sex)
data_bottom_AD_half$Assessment_centre <- factor(data_bottom_AD_half$Assessment_centre)
data_bottom_AD_half$Genotype_batch <- factor(data_bottom_AD_half$Genotype_batch)


#Ordinal logistic regression of EA PRS on PM in bottom half AD PRS 
model_bottom50_AD_PRS <- clm(Prospective_memory ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                               PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_bottom_AD_half)
summary(model_bottom50_AD_PRS)

# Get the summary
sum_model_bottom <- summary(model_bottom50_AD_PRS)

# Coefficient table
coefs <- coef(sum_model_bottom)

# Extract values 
log_odds <- coefs["EA_PRS_standarized", "Estimate"]
se <- coefs["EA_PRS_standarized", "Std. Error"]
zval <- coefs["EA_PRS_standarized", "z value"]
pval <- coefs["EA_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))


#Ordinal logistic regression of EA PRS on PM in top half AD PRS 
data_top_AD_half$Prospective_memory <- factor(data_top_AD_half$Prospective_memory, 
                                              levels = c(0, 1, 2), 
                                              ordered = TRUE)
data_top_AD_half$Prospective_memory <- ordered(data_top_AD_half$Prospective_memory)
data_top_AD_half$Sex <- factor(data_top_AD_half$Sex)
data_top_AD_half$Assessment_centre <- factor(data_top_AD_half$Assessment_centre)
data_top_AD_half$Genotype_batch <- factor(data_top_AD_half$Genotype_batch)


model_top50_AD_PRS <- clm(Prospective_memory ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                            PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_top_AD_half)
summary(model_top50_AD_PRS)

# Get the summary
sum_model_top <- summary(model_top50_AD_PRS)

# Coefficient table
coefs <- coef(sum_model_top)

# Extract values 
log_odds <- coefs["EA_PRS_standarized", "Estimate"]
se <- coefs["EA_PRS_standarized", "Std. Error"]
zval <- coefs["EA_PRS_standarized", "z value"]
pval <- coefs["EA_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))

model_interaction <- clm(Prospective_memory ~ AD_PRS_standarized*EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                           PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction)





#Ordinal logistic regression of YoS on PM in bottom half AD PRS 
model_bottom50_AD_PRS <- clm(Prospective_memory ~ Years_of_schooling + Sex + Age + Assessment_centre, data = data_bottom_AD_half)
summary(model_bottom50_AD_PRS)

# Get the summary
sum_model_bottom <- summary(model_bottom50_AD_PRS)

# Coefficient table
coefs <- coef(sum_model_bottom)

# Extract values 
log_odds <- coefs["Years_of_schooling", "Estimate"]
se <- coefs["Years_of_schooling", "Std. Error"]
zval <- coefs["Years_of_schooling", "z value"]
pval <- coefs["Years_of_schooling", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))


#Ordinal logistic regression of YoS on PM in top half AD PRS 
model_top50_AD_PRS <- clm(Prospective_memory ~ Years_of_schooling + Sex + Age + Assessment_centre, data = data_top_AD_half)
summary(model_top50_AD_PRS)

# Get the summary
sum_model_top <- summary(model_top50_AD_PRS)

# Coefficient table
coefs <- coef(sum_model_top)

# Extract values 
log_odds <- coefs["Years_of_schooling", "Estimate"]
se <- coefs["Years_of_schooling", "Std. Error"]
zval <- coefs["Years_of_schooling", "z value"]
pval <- coefs["Years_of_schooling", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))

model_interaction_2 <- clm(Prospective_memory ~ AD_PRS_standarized*Years_of_schooling + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction_2)












##Stratify VAD PRS (bottom 50% vs top 50%)
data$VAD_PRS_half <- ntile(data$VAD_PRS_standarized, 2)
data_bottom_VAD_half <- subset(data, VAD_PRS_half == 1) #Bottom half
data_top_VAD_half <- subset(data, VAD_PRS_half == 2) #Top half 


#Prep for ordinal logistic regression
data_bottom_VAD_half$Prospective_memory <- factor(data_bottom_VAD_half$Prospective_memory, 
                                                 levels = c(0, 1, 2), 
                                                 ordered = TRUE)

data_bottom_VAD_half$Prospective_memory <- ordered(data_bottom_VAD_half$Prospective_memory)
data_bottom_VAD_half$Sex <- factor(data_bottom_VAD_half$Sex)
data_bottom_VAD_half$Assessment_centre <- factor(data_bottom_VAD_half$Assessment_centre)
data_bottom_VAD_half$Genotype_batch <- factor(data_bottom_VAD_half$Genotype_batch)


#Ordinal logistic regression of EA PRS on PM in bottom half VAD PRS 
model_bottom50_VAD_PRS <- clm(Prospective_memory ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                                PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_bottom_VAD_half)
summary(model_bottom50_VAD_PRS)

# Get the summary
sum_model_bottom <- summary(model_bottom50_VAD_PRS)

# Coefficient table
coefs <- coef(sum_model_bottom)

# Extract values 
log_odds <- coefs["EA_PRS_standarized", "Estimate"]
se <- coefs["EA_PRS_standarized", "Std. Error"]
zval <- coefs["EA_PRS_standarized", "z value"]
pval <- coefs["EA_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))


#Ordinal logistic regression of EA PRS on PM in top half VAD PRS 
data_top_VAD_half$Prospective_memory <- factor(data_top_VAD_half$Prospective_memory, 
                                              levels = c(0, 1, 2), 
                                              ordered = TRUE)
data_top_VAD_half$Prospective_memory <- ordered(data_top_VAD_half$Prospective_memory)
data_top_VAD_half$Sex <- factor(data_top_VAD_half$Sex)
data_top_VAD_half$Assessment_centre <- factor(data_top_VAD_half$Assessment_centre)
data_top_VAD_half$Genotype_batch <- factor(data_top_VAD_half$Genotype_batch)


model_top50_VAD_PRS <- clm(Prospective_memory ~ EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_top_VAD_half)
summary(model_top50_VAD_PRS)

# Get the summary
sum_model_top <- summary(model_top50_VAD_PRS)

# Coefficient table
coefs <- coef(sum_model_top)

# Extract values 
log_odds <- coefs["EA_PRS_standarized", "Estimate"]
se <- coefs["EA_PRS_standarized", "Std. Error"]
zval <- coefs["EA_PRS_standarized", "z value"]
pval <- coefs["EA_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))

model_interaction <- clm(Prospective_memory ~ VAD_PRS_standarized*EA_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                           PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction)





#Ordinal logistic regression of YoS on PM in bottom half VAD PRS 
model_bottom50_VAD_PRS <- clm(Prospective_memory ~ Years_of_schooling + Sex + Age + Assessment_centre, data = data_bottom_VAD_half)
summary(model_bottom50_VAD_PRS)

# Get the summary
sum_model_bottom <- summary(model_bottom50_VAD_PRS)

# Coefficient table
coefs <- coef(sum_model_bottom)

# Extract values 
log_odds <- coefs["Years_of_schooling", "Estimate"]
se <- coefs["Years_of_schooling", "Std. Error"]
zval <- coefs["Years_of_schooling", "z value"]
pval <- coefs["Years_of_schooling", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))


#Ordinal logistic regression of YoS on PM in top half VAD PRS 
model_top50_VAD_PRS <- clm(Prospective_memory ~ Years_of_schooling + Sex + Age + Assessment_centre, data = data_top_VAD_half)
summary(model_top50_VAD_PRS)

# Get the summary
sum_model_top <- summary(model_top50_VAD_PRS)

# Coefficient table
coefs <- coef(sum_model_top)

# Extract values 
log_odds <- coefs["Years_of_schooling", "Estimate"]
se <- coefs["Years_of_schooling", "Std. Error"]
zval <- coefs["Years_of_schooling", "z value"]
pval <- coefs["Years_of_schooling", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))

model_interaction_2 <- clm(Prospective_memory ~ VAD_PRS_standarized*Years_of_schooling + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction_2)
