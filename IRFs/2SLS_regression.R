##################### TWO STEP LEAST SQUARE REGRESSION ###################

# PHILOSOPHY OF THE REGRESSION
# We use a two-step least squares regression, to isolate the effect of QE shocks from other effects on exchange rates.
# The first step removes endogeneity (non-QE shocks)
# The second step estimates the pure effect of QE shocks on our interest variable.

# DATA WE USE
# Exchange rate series (st)
# Central bank balance sheet data (BSt for both ECB and Fed).
# QE announcement indicators (aECB and aFED)
# Control variables


# libraries
library(dplyr)
library(lmtest)
library(sandwich)

# loading the data
df <- read.csv('~/work/monthly_data_without_exp.csv')

# time fomat
df$date <- as.Date(df$date)
# ordering with respect to the date
df <- df %>% arrange(date)

# future values of balance sheet over M next periods
M <- 9
df$y <- (lead(df$BS, M-1) - df$BS) / df$BS  # Calcul de la variation relative de BS

# deleting NAs
df <- na.omit(df)


# FIRSTS STAGE : regressing meeting ecb and meeting fed over omega
model_first_stage_ECB <- lm(Meeting_ECB ~ omega, data = df)
model_first_stage_FED <- lm(Meeting_FED ~ omega, data = df)

# checking the validity of instruments
summary(model_first_stage_ECB)
summary(model_first_stage_FED)

# predicting first stage - instrumented variables
df$pred_Meeting_ECB <- predict(model_first_stage_ECB, newdata = df)
df$pred_Meeting_FED <- predict(model_first_stage_FED, newdata = df)

# checking colinearity between instruments and endogenous variables
cor(df$pred_Meeting_ECB, df$Meeting_ECB)
cor(df$pred_Meeting_FED, df$Meeting_FED)

# too weak correlations -> instrument could be invalid

# SECOND STAGE : regressing y over the instrumented variables
model_second_stage <- lm(y ~ pred_Meeting_ECB + pred_Meeting_FED, data = df)
summary(model_second_stage)

# checking robust errors
robust_model_second_stage <- coeftest(model_second_stage, vcov = vcovHC(model_second_stage, type = "HC1"))
print(robust_model_second_stage)

# problème sur le omega...
# omega seems to be too weakly correlated to the variables.
#despite some statistical significance, it does not isolate enough information about the QE shockcs.
# leads to confusing issues concerning colinearity with the FED.
# i then tried to do an IRF despite this unsatisfying result : 


########### ESSAYONS D'OBTENIR UNE IRF MALGRE DES RESULTATS INCOMPLETS #######

# gathering the coefficients of our 2SLS regression
coef(model_second_stage)

# ... but i didn't feel it
# if the instrument omega was the issue, let's walk another path.


# it will necessarily be less satisfying than a rich instrument but hey


######### something else ############################

# trying to build an IRF 

# gathering the DATA
head(df$date)  # checking dates
head(merged_data_clean$week_date)  # checking dates
# filter dates to merge through publication dates
df_filtered <- df[df$date %in% merged_data_clean$week_date, ]
# merge
merged_data_2 <- merge(df_filtered, merged_data_clean, by.x = "date", by.y = "week_date", all.x = TRUE)


# creating the instrument variable to rpz the QE shock
merged_data_2$QE_shock <- merged_data_2$ECBASSETSW - merged_data_2$WALCL

# VAR
install.packages("vars")
library(vars)
var_select <- VARselect(merged_data_2[, c("EUR.USD", "QE_shock")], lag.max = 10, type = "both")
var_select$selection  # optimal order of the var

# optimal order seems to be 3 (considering AIC, HQ, SC)
# estimating the VAR model using this optimal lag
var_model <- VAR(merged_data_2[, c("EUR.USD", "QE_shock")], p = 3, type = "both")

# i can now estimate IRF ??
# for the 10 next periods
irf_model <- irf(var_model, impulse = "QE_shock", response = "EUR.USD", n.ahead = 18, boot = TRUE)

# plot
plot(irf_model)




###### another IRF : d_rate

# we use the same 'instrument' and the same VAR
var_model_d_rate <- VAR(merged_data_2[, c("d_rate", "QE_shock")], p = 3, type = "both")

# computing the IRF for 10 periods
irf_model_d_rate <- irf(var_model_d_rate, impulse = "QE_shock", response = "d_rate", n.ahead = 10, boot = TRUE)
# plotting
plot(irf_model_d_rate)

# looking at the coefficients
irf_model_d_rate$irf
# looks like a durable loss
# their explanation is on page 501 : impact of QE on frictions in money markets
# increased liquidity (esp when demand for central bank reserves is not fully satiated) => can compress liquidity premia of money market rates


########## cip deviation

# estimating var
var_model_cip <- VAR(merged_data_2[, c("cip", "QE_shock")], p = 3, type = "both")

# computing 10 periods
irf_model_cip <- irf(var_model_cip, impulse = "QE_shock", response = "cip", n.ahead = 10, boot = TRUE)
#plotting
plot(irf_model_cip)



##### surprise_index

# var estimation
var_model_surprise <- VAR(merged_data_2[, c("surprise_index", "QE_shock")], p = 3, type = "both")

# computation
irf_model_surprise <- irf(var_model_surprise, impulse = "QE_shock", response = "surprise_index", n.ahead = 10, boot = TRUE)
#plot
plot(irf_model_surprise)

















