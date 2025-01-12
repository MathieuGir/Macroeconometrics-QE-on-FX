##### LAST TRY 2SLS #####

# packages
library(dplyr)
library(lmtest)
library(sandwich)

# data
df <- read.csv('~/work/monthly_data_without_exp.csv')

# time
df$date <- as.Date(df$date)

# ordering the data by date
df <- df %>% arrange(date)

# Future values of balance sheet over M next periods
M <- 9
df$y <- (lead(df$BS, M - 1) - df$BS) / df$BS

# cleaning
df <- na.omit(df)

# FIRST STAGE: Regressing Meeting_ECB and Meeting_FED over omega 
model_first_stage_ECB <- lm(Meeting_ECB ~ omega, data = df)
model_first_stage_FED <- lm(Meeting_FED ~ omega, data = df)

# validity of instruments
summary(model_first_stage_ECB)
summary(model_first_stage_FED)

# predicting first stage 
df$pred_Meeting_ECB <- predict(model_first_stage_ECB, newdata = df)
df$pred_Meeting_FED <- predict(model_first_stage_FED, newdata = df)

# correlation with the instrument ?
cor(df$pred_Meeting_ECB, df$Meeting_ECB)  # very weak
cor(df$pred_Meeting_FED, df$Meeting_FED)  # very weak

# SECOND STAGE: regressing y over the instrumented variables
model_second_stage <- lm(y ~ pred_Meeting_ECB + pred_Meeting_FED, data = df)
summary(model_second_stage)

# robust standard errors?
robust_model_second_stage <- coeftest(model_second_stage, vcov = vcovHC(model_second_stage, type = "HC1"))
print(robust_model_second_stage)


 ### CONCLUSIONS
# very weak instrument
# weak correlation with variables
# issues of statistical significance
# for some reason FED disappears. either singularity or perfect colinearity.
