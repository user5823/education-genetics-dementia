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


##Stratify education PRS (bottom 50% vs top 50%)
data$EA_PRS_half <- ntile(data$EA_PRS_standarized, 2)
data_bottom_EA_half <- subset(data, EA_PRS_half == 1) #Bottom half
data_top_EA_half <- subset(data, EA_PRS_half == 2) #Top half 

##Binarize years of schooling (using GCSEs as cut-off) 
data$edu_binary <- ifelse(data$Years_of_schooling > 12, 1, 0)
table(data$edu_binary)

#Prep for ordinal logistic regression
data$Prospective_memory <- ordered(data$Prospective_memory)
data$Sex <- factor(data$Sex)
data$Assessment_centre <- factor(data$Assessment_centre)
data$Genotype_batch <- factor(data$Genotype_batch)

data_bottom_EA_half$Prospective_memory <- factor(data_bottom_EA_half$Prospective_memory, 
                                                 levels = c(0, 1, 2), 
                                                 ordered = TRUE)

data_bottom_EA_half$Prospective_memory <- ordered(data_bottom_EA_half$Prospective_memory)
data_bottom_EA_half$Sex <- factor(data_bottom_EA_half$Sex)
data_bottom_EA_half$Assessment_centre <- factor(data_bottom_EA_half$Assessment_centre)
data_bottom_EA_half$Genotype_batch <- factor(data_bottom_EA_half$Genotype_batch)


#Ordinal logistic regression of AD PRS on PM in bottom half EA PRS 
model_bottom50_EA_PRS <- clm(Prospective_memory ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                               PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_bottom_EA_half)
summary(model_bottom50_EA_PRS)

# Get the summary
sum_model_bottom <- summary(model_bottom50_EA_PRS)

# Coefficient table
coefs <- coef(sum_model_bottom)

# Extract values 
log_odds <- coefs["AD_PRS_standarized", "Estimate"]
se <- coefs["AD_PRS_standarized", "Std. Error"]
zval <- coefs["AD_PRS_standarized", "z value"]
pval <- coefs["AD_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))


#Ordinal logistic regression of AD PRS on PM in top half EA PRS 
data_top_EA_half$Prospective_memory <- factor(data_top_EA_half$Prospective_memory, 
                                              levels = c(0, 1, 2), 
                                              ordered = TRUE)
data_top_EA_half$Prospective_memory <- ordered(data_top_EA_half$Prospective_memory)
data_top_EA_half$Sex <- factor(data_top_EA_half$Sex)
data_top_EA_half$Assessment_centre <- factor(data_top_EA_half$Assessment_centre)
data_top_EA_half$Genotype_batch <- factor(data_top_EA_half$Genotype_batch)


model_top50_EA_PRS <- clm(Prospective_memory ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                            PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_top_EA_half)
summary(model_top50_EA_PRS)

# Get the summary
sum_model_top <- summary(model_top50_EA_PRS)

# Coefficient table
coefs <- coef(sum_model_top)

# Extract values 
log_odds <- coefs["AD_PRS_standarized", "Estimate"]
se <- coefs["AD_PRS_standarized", "Std. Error"]
zval <- coefs["AD_PRS_standarized", "z value"]
pval <- coefs["AD_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))

model_interaction <- clm(Prospective_memory ~ AD_PRS_standarized*EA_PRS_half + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                           PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction)




#Repeating analyses for stratification by binary years of schooling 

##High education
model_high_edu <- clm(Prospective_memory ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                        PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = subset(data, edu_binary == 1))
summary(model_high_edu)

# Get the summary
sum_model_high <- summary(model_high_edu)

# Coefficient table
coefs <- coef(sum_model_high)

# Extract values
log_odds <- coefs["AD_PRS_standarized", "Estimate"]
se <- coefs["AD_PRS_standarized", "Std. Error"]
zval <- coefs["AD_PRS_standarized", "z value"]
pval <- coefs["AD_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))


##Low education
model_low_edu <- clm(Prospective_memory ~ AD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                       PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = subset(data, edu_binary == 0))
summary(model_low_edu)

# Get the summary
sum_model_low <- summary(model_low_edu)

# Coefficient table
coefs <- coef(sum_model_low)

# Extract values 
log_odds <- coefs["AD_PRS_standarized", "Estimate"]
se <- coefs["AD_PRS_standarized", "Std. Error"]
zval <- coefs["AD_PRS_standarized", "z value"]
pval <- coefs["AD_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))

model_interaction_2 <- clm(Prospective_memory ~ AD_PRS_standarized*edu_binary + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction_2)






#Ordinal logistic regressions for VaD PRS on PM

#Bottom EA PRS 
model_bottom50_EA_PRS <- clm(Prospective_memory ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                               PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_bottom_EA_half)
summary(model_bottom50_EA_PRS)

# Get the summary
sum_model_bottom <- summary(model_bottom50_EA_PRS)

# Coefficient table
coefs <- coef(sum_model_bottom)

# Extract values for Years_of_schooling
log_odds <- coefs["VAD_PRS_standarized", "Estimate"]
se <- coefs["VAD_PRS_standarized", "Std. Error"]
zval <- coefs["VAD_PRS_standarized", "z value"]
pval <- coefs["VAD_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))

#Top EA PRS
model_top50_EA_PRS <- clm(Prospective_memory ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                            PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data_top_EA_half)
summary(model_top50_EA_PRS)

# Get the summary
sum_model_top <- summary(model_top50_EA_PRS)

# Coefficient table
coefs <- coef(sum_model_top)

# Extract values for Years_of_schooling
log_odds <- coefs["VAD_PRS_standarized", "Estimate"]
se <- coefs["VAD_PRS_standarized", "Std. Error"]
zval <- coefs["VAD_PRS_standarized", "z value"]
pval <- coefs["VAD_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))

model_interaction <- clm(Prospective_memory ~ VAD_PRS_standarized*EA_PRS_half + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                           PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction)


#Repeat but stratification by observed years of schooling 


##High education
model_high_edu <- clm(Prospective_memory ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                        PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = subset(data, edu_binary == 1))
summary(model_high_edu)

# Get the summary
sum_model_high <- summary(model_high_edu)

# Coefficient table
coefs <- coef(sum_model_high)

# Extract values 
log_odds <- coefs["VAD_PRS_standarized", "Estimate"]
se <- coefs["VAD_PRS_standarized", "Std. Error"]
zval <- coefs["VAD_PRS_standarized", "z value"]
pval <- coefs["VAD_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))


##Low education
model_low_edu <- clm(Prospective_memory ~ VAD_PRS_standarized + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                       PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = subset(data, edu_binary == 0))
summary(model_low_edu)

# Get the summary
sum_model_low <- summary(model_low_edu)

# Coefficient table
coefs <- coef(sum_model_low)

# Extract values 
log_odds <- coefs["VAD_PRS_standarized", "Estimate"]
se <- coefs["VAD_PRS_standarized", "Std. Error"]
zval <- coefs["VAD_PRS_standarized", "z value"]
pval <- coefs["VAD_PRS_standarized", "Pr(>|z|)"]

# Odds ratio
odds_ratio <- exp(log_odds)

# 95% CI for odds ratio
ci_lower <- exp(log_odds - 1.96 * se)
ci_upper <- exp(log_odds + 1.96 * se)

# Output
cat(sprintf("  Odds Ratio       = %.4f\n", odds_ratio))
cat(sprintf("  95%% CI           = [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  p-value          = %.4g\n", pval))

model_interaction_2 <- clm(Prospective_memory ~ VAD_PRS_standarized*edu_binary + Sex + Age + Assessment_centre + Genotype_batch + PCA_1 + PCA_2 + PCA_3 + PCA_4 + PCA_5 +
                             PCA_6 + PCA_7 + PCA_8 + PCA_9 + PCA_10, data = data)
summary(model_interaction_2)






