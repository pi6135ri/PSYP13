#################
# LOAD PACKAGES #
#################

library(psych)
library(lm.beta)
library(tidyverse)
library(gridExtra)
library(car)
library(lmtest)


###########################
# CREATE CUSTOM FUNCTIONS #
###########################

# error_plotter: helps visualize the error of the predictions of the regressions line.
error_plotter <- function(mod, col = "black", x_var = NULL) {
  mod_vars = as.character(mod$call[2])
  data = as.data.frame(eval(parse(text = as.character(mod$call[3]))))
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern = "~",
      mod_vars)) - 2)
  x = substr(mod_vars, as.numeric(gregexpr(pattern = "~", mod_vars)) +
      2, nchar(mod_vars))
  data$pred = predict(mod)
  if (x == "1" & is.null(x_var)) {
    x = "response_ID"
    data$response_ID = 1:nrow(data)
  } else if (x == "1") {
    x = x_var
  }
  plot(data[, y] ~ data[, x], ylab = y, xlab = x)
  abline(mod)
  for (i in 1:nrow(data)) {
    clip(min(data[, x]), max(data[, x]), min(data[i, c(y,
      "pred")]), max(data[i, c(y, "pred")]))
    abline(v = data[i, x], lty = 2, col = col)
  }
}


####################################################################
# DATA: GET, CHECK FOR IRREGULARITIES, CLEAN, CHECK THAT IT WORKED #
####################################################################

# acquire data from GitHub repository
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")

# check data for irregularities
view(data_sample_1) # view data in the data viewer tool
str(data_sample_1) # investigate the structure of the dataset
data_sample_1 %>% summary() # get a basic overview
data_sample_1 %>% describe() # get another basic overview
data_sample_1 %>% ggplot() + aes(x = pain) +
  geom_histogram(bins = 50) # histogram for variable 'pain'
data_sample_1 %>% ggplot() + aes(x = age) +
  geom_histogram(bins = 50) # histogram for variable 'age'
data_sample_1 %>% ggplot() + aes(x = STAI_trait) +
  geom_histogram(bins = 50) # histogram for variable 'STAI_trait'
data_sample_1 %>% ggplot() + aes(x = pain_cat) +
  geom_histogram(bins = 50) # histogram for variable 'pain_cat'
data_sample_1 %>% ggplot() + aes(x = cortisol_serum) +
  geom_histogram(bins = 50) # histogram for variable 'cortisol_serum'
data_sample_1 %>% ggplot() + aes(x = cortisol_saliva) +
  geom_histogram(bins = 50) # histogram for variable 'cortisol_saliva'
data_sample_1 %>% ggplot() + aes(x = mindfulness) +
  geom_histogram(bins = 50) # histogram for variable 'mindfulness'
data_sample_1 %>% ggplot() + aes(x = weight) +
  geom_histogram(bins = 50) # histogram for variable 'weight'
data_sample_1 %>% ggplot() + aes(x = IQ) +
  geom_histogram(bins = 50) # histogram for variable 'IQ'
data_sample_1 %>% ggplot() + aes(x = household_income) +
  geom_histogram(bins = 50) # histogram for variable 'household_income'
  
### CONCLUSIONS. No NAs. Clean columns: ID, pain, sex, age, pain_cat, cortisol_serum, cortisol_saliva, 
###     mindfullness, weight, IQ. STAI_trait presents one impossibly low outlier, 3.50, which should 
###     be corrected to 35.00. household_income also also presents an impossibly low outlier: -4562. 
###     I am not sure what to do with that. I could remove the minus, but even the corrected value, 
###     4562, would be the lowest of all. The second lowest would be 10242, as seen by running: 
###     sort(data_sample_1$household_income, FALSE)[2]
###     I will only clean what columns I need for my first analysis, for the moment, i.e. STAI_trait.

# clean data
data_sample_1_cleaned_1 = data_sample_1 %>% mutate(STAI_trait = replace(
  STAI_trait, STAI_trait == 3.50, 35.00), )

# check that cleaning worked and that everything is in order
view(data_sample_1_cleaned_1) # view data in the data viewer tool
str(data_sample_1_cleaned_1) # investigate the structure of the dataset
data_sample_1_cleaned_1 %>% summary() # get a basic overview
data_sample_1_cleaned_1 %>% describe() # get another basic overview
data_sample_1_cleaned_1 %>% ggplot() + aes(x = pain) +
  geom_histogram(bins = 50) # histogram for variable 'pain'
data_sample_1_cleaned_1 %>% ggplot() + aes(x = age) +
  geom_histogram(bins = 50) # histogram for variable 'age'
data_sample_1_cleaned_1 %>% ggplot() + aes(x = STAI_trait) +
  geom_histogram(bins = 50) # histogram for variable 'STAI_trait'
data_sample_1_cleaned_1 %>% ggplot() + aes(x = pain_cat) +
  geom_histogram(bins = 50) # histogram for variable 'pain_cat'
data_sample_1_cleaned_1 %>% ggplot() + aes(x = cortisol_serum) +
  geom_histogram(bins = 50) # histogram for variable 'cortisol_serum'
data_sample_1_cleaned_1 %>% ggplot() + aes(x = cortisol_saliva) +
  geom_histogram(bins = 50) # histogram for variable 'cortisol_saliva'
data_sample_1_cleaned_1 %>% ggplot() + aes(x = mindfulness) +
  geom_histogram(bins = 50) # histogram for variable 'mindfulness'
data_sample_1_cleaned_1 %>% ggplot() + aes(x = weight) +
  geom_histogram(bins = 50) # histogram for variable 'weight'
data_sample_1_cleaned_1 %>% ggplot() + aes(x = IQ) +
  geom_histogram(bins = 50) # histogram for variable 'IQ'
data_sample_1_cleaned_1 %>% ggplot() + aes(x = household_income) +
  geom_histogram(bins = 50) # histogram for variable 'household_income'

### CONCLUSIONS. Cleaning worked as intended. household_income, still problematic, left untouched.


#############################################
# DATA: CHECK FOR DEVIATIONS FROM NORMALITY #
#############################################

# Shapiro-Wilks test for every variable
#   On the basis of the test statistic W, the p-value tests the Null hypothesis (H0) 
#   that the sample have been drawn from a normal distribution. The lower this value, 
#   the smaller the chance - when the p-value is lower than 0.05, you can conclude that 
#   the sample deviates from normality.
shapiro.test(data_sample_1_cleaned_1$ID)
shapiro.test(data_sample_1_cleaned_1$pain)
shapiro.test(data_sample_1_cleaned_1$sex)
shapiro.test(data_sample_1_cleaned_1$age)
shapiro.test(data_sample_1_cleaned_1$STAI_trait)
shapiro.test(data_sample_1_cleaned_1$pain_cat)
shapiro.test(data_sample_1_cleaned_1$cortisol_serum)
shapiro.test(data_sample_1_cleaned_1$cortisol_saliva)
shapiro.test(data_sample_1_cleaned_1$mindfulness)
shapiro.test(data_sample_1_cleaned_1$weight)
shapiro.test(data_sample_1_cleaned_1$IQ)
shapiro.test(data_sample_1_cleaned_1$household_income)

### CONCLUSIONS. No variable deviates from normality.


#######################
# SAMPLE DEMOGRAPHICS #
#######################

data_sample_1_cleaned_1%>%summary() # get a basic overview, focus on variables 'sex' and 'age'
data_sample_1_cleaned_1%>%describe() # get a basic overview, focus on variables 'sex' and 'age'

### CONCLUSIONS. N=160; 80 males, 80 females; age range 27-54, mean 40.17, s.d. 5.04.


#############################
# SET UP REGRESSION MODELS  #
#############################

model_1 = lm(pain ~ age + sex, data = data_sample_1_cleaned_1) # set up regression model 1
summary(model_1) # view model 1

### CONCLUSIONS. Model 1 is: 
### pain = 7.98795 - 0.08924*age + 0.64818*sex
###   with female = 0 and male = 1.
### "In a first regression model we predicted postoperative pain after wisdom tooth surgery (as expressed on 
###  a numerical rating scale of 0 to 10, where 0 means "no pain" and 10 means "worst pain I can imagine) 
###  with age (in years) and sex as predictors."

model_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, 
             data = data_sample_1_cleaned_1) # set up regression model 2
summary(model_2) # view model 2

### CONCLUSIONS. Model 2 is: 
### pain = 1.76592 - 0.02165*age + 0.46249*sex - 0.04170*STAI_trait + 0.10619*pain_cat - 
###   0.28865*mindfulness - 0.07007*cortisol_serum + 0.68733*cortisol_saliva
###   with female = 0 and male = 1.
### "In a second regression model we predicted postoperative pain after wisdom tooth surgery (as expressed 
###  on a numerical rating scale of 0 to 10, where 0 means "no pain" and 10 means "worst pain I can imagine) 
###  with age (in years) sex, trait anxiety (as measured through STAI-T), pain catastrophizing (as measured 
###  through the Pain Catastrophizig Scale), mindfulness (as measured through MAAS), serum cortisol levels 
###  and saliva cortisol levels as predictors."


#################################################################
# IDENTIFY DATA POINTS WITH HIGH LEVERAGE IN THE SELECTED MODEL #
#################################################################

model_2 %>% plot(which = 5)
# Cases that are close to the middle of the regression line on the scatterplot have less leverage while 
# cases at the ends have more. Cases that have high residual error and high leverage are influential cases. 
# Cook's distance is a number that quantifies this, taking into account the combination of "extremeness" 
# and leverage. 

model_2 %>% plot(which = 4)
# It is not well established what should be considered a problematic case, but there are 
# some rules of thumb. Some say that cases with Cook's distance > 1 are influential, while some use a 
# threshold of Cook's distance > 4/N. (In our case this is 4/160 = 0.025). We have no cases that would have 
# a Cook's distance higher than 1, but we have several where it is higher than 0.025. So we might have a 
# problem with outliers according to one of the criteria.
# Lets test whether the assumptions of multiple regression hold true, and determine if we need to do 
# anything with these cases.


######################################################
# CHECK ASSUMPTIONS OF LINEAR REGRESSION FOR MODEL 2 #
######################################################

# Check that the residuals of the model are normally distributed
model_2 %>% plot(which = 2) # QQ plot. Cases should be alligned with the theoretical diagonal.
enframe(residuals(model_2)) %>% ggplot() + 
  aes(x = value) + geom_histogram() # Histogram of the residuals. Should look roughly like a bell.
describe(residuals(model_2)) # Skewness and kurtosis > 1 can indicate the violation of the 
                             # assumption of normality.

### CONCLUSIONS. The plots and the statistics indicate that the residuals of model 2 are normally 
### distributed.

# Check that the relationship between the outcome variable and the predictors is linear
model_2 %>% residualPlots() # Lines should be roughly flat. If Pr(>|Test stat|) < 0.05, then 
                            # linearity might be violated.

### CONCLUSIONS. Even though there is minor curvature visible on the plots, the tests are 
### all non significant, so the linearity assumption seems to hold true for model 2.

# Check the assumption of homoscedasticity
model_2 %>% bptest() # Breush-Pagan test. Homoscedasticity might be violated if p < 0.05.
model_2 %>% ncvTest() # Non-Constant Variance Score (NCV) test. Homoscedasticity might be 
                      # violated if p < 0.05.

### CONCLUSIONS. The tests are both non significant, so the homoscedasticity assumption seems to hold 
### true for model 2.

# Check that there is no multicollinearity
model_2 %>% vif() # VIF above 3 indicates problematic multicollinearity

### CONCLUSIONS. Not surprisingly, both VIF values for cortisol_serum and cortisol_saliva are above 3, 
### indicating problematic multicollinearity. Chances are the two predictors are positively correlated 
### with each other. 

# Enters the mythical pairs.panels()
data_sample_1_cleaned_1 %>% select(age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, 
                                   cortisol_saliva) %>% pairs.panels(col = "red", lm = T)

### CONCLUSIONS. Suspiscion confirmed, they are correlated with each other (.87). It is a case of 
### data collinearity. The two variables are pretty much interchangeable for the purpose of our 
### research question (they are both proxy measures of cortisol levels), so picking one and dropping 
### the other seems to be the best course of action. I decide to retain serum cortisol levels, as they 
### are often regarded in medical research as more reliably related to stress.
### Restart from setting up the two regression models.


################################
# SET UP REGRESSION MODELS (2) #
################################

model_1 = lm(pain ~ age + sex, data = data_sample_1_cleaned_1) # set up regression model 1
summary(model_1) # view model 1

### CONCLUSIONS. Model 1 is: 
### pain = 7.98795 - 0.08924*age + 0.64818*sex
###   with female = 0 and male = 1.
### "In a first regression model we predicted postoperative pain after wisdom tooth surgery (as expressed on 
###  a numerical rating scale of 0 to 10, where 0 means "no pain" and 10 means "worst pain I can imagine) 
###  with age (in years) and sex as predictors."

model_2_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, 
             data = data_sample_1_cleaned_1) # set up regression model 2
summary(model_2_2) # view model 2

### CONCLUSIONS. Model 2 is: 
### pain = 3.02731 - 0.04575*age + 0.48971*sex - 0.00693*STAI_trait + 0.07990*pain_cat - 
###   0.29917*mindfulness - 0.43565*cortisol_serum
###   with female = 0 and male = 1.
### "In a second regression model we predicted postoperative pain after wisdom tooth surgery (as expressed 
###  on a numerical rating scale of 0 to 10, where 0 means "no pain" and 10 means "worst pain I can imagine) 
###  with age (in years) sex, trait anxiety (as measured through STAI-T), pain catastrophizing (as measured 
###  through the Pain Catastrophizig Scale), mindfulness (as measured through MAAS), and serum cortisol levels 
###   as predictors."


#####################################################################
# IDENTIFY DATA POINTS WITH HIGH LEVERAGE IN THE SELECTED MODEL (2) #
#####################################################################

model_2_2 %>% plot(which = 5)
# Cases that are close to the middle of the regression line on the scatterplot have less leverage while 
# cases at the ends have more. Cases that have high residual error and high leverage are influential cases. 
# Cook's distance is a number that quantifies this, taking into account the combination of "extremeness" 
# and leverage. 

model_2_2 %>% plot(which = 4)
# It is not well established what should be considered a problematic case, but there are 
# some rules of thumb. Some say that cases with Cook's distance > 1 are influential, while some use a 
# threshold of Cook's distance > 4/N. (In our case this is 4/160 = 0.025). We have no cases that would have 
# a Cook's distance higher than 1, but we have several where it is higher than 0.025. So we might have a 
# problem with outliers according to one of the criteria.
# Lets test whether the assumptions of multiple regression hold true, and determine if we need to do 
# anything with these cases.


##########################################################
# CHECK ASSUMPTIONS OF LINEAR REGRESSION FOR MODEL 2 (2) #
##########################################################

# Check that the residuals of the model are normally distributed
model_2_2 %>% plot(which = 2) # QQ plot. Cases should be alligned with the theoretical diagonal.
enframe(residuals(model_2_2)) %>% ggplot() + 
  aes(x = value) + geom_histogram() # Histogram of the residuals. Should look roughly like a bell.
describe(residuals(model_2_2)) # Skewness and kurtosis > 1 can indicate the violation of the 
# assumption of normality.

### CONCLUSIONS. The plots and the statistics indicate that the residuals of model 2 are normally 
### distributed.

# Check that the relationship between the outcome variable and the predictors is linear
model_2_2 %>% residualPlots() # Lines should be roughly flat. If Pr(>|Test stat|) < 0.05, then 
# linearity might be violated.

### CONCLUSIONS. There is some curvature visible on the plots, and the tests for STAI_trait and 
### pain_cat are significant, so the linearity assumption seems to be violated.
### The relationship between these two predictors and the outcome variable might better be 
### captured by including higher order terms in the model.

# Set up new regression model
model_2_3 = lm(pain ~ age + sex + STAI_trait + I(STAI_trait^2) + pain_cat + I(pain_cat^2) +
                 mindfulness + cortisol_serum, data = data_sample_1_cleaned_1)

# Compare it with the previous one
AIC(model_2_2)
AIC(model_2_3)

### CONCLUSIONS. Model 2.3 actually provides a better fit than 2.2.
### Restart from setting up two regression models.


#################################
# SET UP REGRESSION MODELS  (3) #
#################################

model_1 = lm(pain ~ age + sex, data = data_sample_1_cleaned_1) # set up regression model 1
summary(model_1) # view model 1

### CONCLUSIONS. Model 1 is: 
### pain = 7.98795 - 0.08924*age + 0.64818*sex
###   with female = 0 and male = 1.
### "In a first regression model we predicted postoperative pain after wisdom tooth surgery (as expressed on 
###  a numerical rating scale of 0 to 10, where 0 means "no pain" and 10 means "worst pain I can imagine) 
###  with age (in years) and sex as predictors."

model_2_3 = lm(pain ~ age + sex + STAI_trait + I(STAI_trait^2) + pain_cat + I(pain_cat^2) +
                 mindfulness + cortisol_serum, data = data_sample_1_cleaned_1) # set up regression model 2
summary(model_2_3) # view model 2

### CONCLUSIONS. Model 2 is: 
### pain = 15.436097 - 0.042581*age + 0.522833*sex - 0.450279*STAI_trait + 0.005579*STAI_trait^2 - 
### 0.195601*pain_cat + 0.004571*pain_cat^2 - 0.282111*mindfulness + 0.452977*cortisol_serum
###   with female = 0 and male = 1.
### "In a second regression model we predicted postoperative pain after wisdom tooth surgery (as expressed 
###  on a numerical rating scale of 0 to 10, where 0 means "no pain" and 10 means "worst pain I can imagine) 
###  with age (in years) sex, trait anxiety (as measured through STAI-T), pain catastrophizing (as measured 
###  through the Pain Catastrophizig Scale), mindfulness (as measured through MAAS), and serum cortisol levels 
###  as predictors."


#####################################################################
# IDENTIFY DATA POINTS WITH HIGH LEVERAGE IN THE SELECTED MODEL (3) #
#####################################################################

model_2_3 %>% plot(which = 5)
# Cases that are close to the middle of the regression line on the scatterplot have less leverage while 
# cases at the ends have more. Cases that have high residual error and high leverage are influential cases. 
# Cook's distance is a number that quantifies this, taking into account the combination of "extremeness" 
# and leverage. 

model_2_3 %>% plot(which = 4)
# It is not well established what should be considered a problematic case, but there are 
# some rules of thumb. Some say that cases with Cook's distance > 1 are influential, while some use a 
# threshold of Cook's distance > 4/N. (In our case this is 4/160 = 0.025). We have no cases that would have 
# a Cook's distance higher than 1, but we have several where it is higher than 0.025. So we might have a 
# problem with outliers according to one of the criteria.
# Lets test whether the assumptions of multiple regression hold true, and determine if we need to do 
# anything with these cases.


##########################################################
# CHECK ASSUMPTIONS OF LINEAR REGRESSION FOR MODEL 2 (3) #
##########################################################

# Check that the residuals of the model are normally distributed
model_2_3 %>% plot(which = 2) # QQ plot. Cases should be alligned with the theoretical diagonal.
enframe(residuals(model_2_3)) %>% ggplot() + 
  aes(x = value) + geom_histogram() # Histogram of the residuals. Should look roughly like a bell.
describe(residuals(model_2_3)) # Skewness and kurtosis > 1 can indicate the violation of the 
# assumption of normality.

### CONCLUSIONS. The plots and the statistics indicate that the residuals of model 2 are normally 
### distributed.

# Check that the relationship between the outcome variable and the predictors is linear
model_2_3 %>% residualPlots() # Lines should be roughly flat. If Pr(>|Test stat|) < 0.05, then 
# linearity might be violated.

### CONCLUSIONS. Even though there is minor curvature visible on the plots, the tests are 
### all non significant, so the linearity assumption seems to hold true for model 2.

# Check the assumption of homoscedasticity
model_2_3 %>% bptest() # Breush-Pagan test. Homoscedasticity might be violated if p < 0.05.
model_2_3 %>% ncvTest() # Non-Constant Variance Score (NCV) test. Homoscedasticity might be 
# violated if p < 0.05.

### CONCLUSIONS. The tests are both non significant, so the homoscedasticity assumption seems to hold 
### true for model 2.

# Check that there is no multicollinearity
model_2_3 %>% vif() # VIF above 3 indicates problematic multicollinearity

### CONCLUSIONS. Of course, structural multicollinearity emerges, as higher terms for both 
### STAI_trait and pain_cat are derived from the respective lower ones. I need to center the 
### two variables.

# Add the two centered variables to the dataset
data_sample_1_cleaned_1_centered = data_sample_1_cleaned_1 %>% 
  mutate(STAI_trait_centered = STAI_trait - mean(STAI_trait), 
         pain_cat_centered = pain_cat - mean(pain_cat))
# View it
view(data_sample_1_cleaned_1_centered)


### Back to setting up two regression models.


#################################
# SET UP REGRESSION MODELS  (4) #
#################################

model_1 = lm(pain ~ age + sex, data = data_sample_1_cleaned_1) # set up regression model 1
summary(model_1) # view model 1

### CONCLUSIONS. Model 1 is: 
### pain = 7.98795 - 0.08924*age + 0.64818*sex
###   with female = 0 and male = 1.
### "In a first regression model we predicted postoperative pain after wisdom tooth surgery (as expressed on 
###  a numerical rating scale of 0 to 10, where 0 means "no pain" and 10 means "worst pain I can imagine) 
###  with age (in years) and sex as predictors."

model_2_4 = lm(pain ~ age + sex + STAI_trait_centered + I(STAI_trait_centered^2) + pain_cat_centered + I(pain_cat_centered^2) +
                 mindfulness + cortisol_serum, data = data_sample_1_cleaned_1_centered) # set up regression model 2
summary(model_2_4) # view model 2

### CONCLUSIONS. Model 2 is: 
### pain = 4.585468 - 0.042581*age + 0.522833*sex - 0.010167*STAI_trait_centered + 0.005579*STAI_trait_centered^2 - 
### 0.076800*pain_cat_centered + 0.004571*pain_cat_centered^2 - 0.282111*mindfulness + 0.452977*cortisol_serum
###   with female = 0 and male = 1.
### "In a second regression model we predicted postoperative pain after wisdom tooth surgery (as expressed 
###  on a numerical rating scale of 0 to 10, where 0 means "no pain" and 10 means "worst pain I can imagine) 
###  with age (in years) sex, trait anxiety (as measured through STAI-T), pain catastrophizing (as measured 
###  through the Pain Catastrophizig Scale), mindfulness (as measured through MAAS), and serum cortisol levels 
###  as predictors."


#####################################################################
# IDENTIFY DATA POINTS WITH HIGH LEVERAGE IN THE SELECTED MODEL (4) #
#####################################################################

model_2_4 %>% plot(which = 5)
# Cases that are close to the middle of the regression line on the scatterplot have less leverage while 
# cases at the ends have more. Cases that have high residual error and high leverage are influential cases. 
# Cook's distance is a number that quantifies this, taking into account the combination of "extremeness" 
# and leverage. 

model_2_4 %>% plot(which = 4)
# It is not well established what should be considered a problematic case, but there are 
# some rules of thumb. Some say that cases with Cook's distance > 1 are influential, while some use a 
# threshold of Cook's distance > 4/N. (In our case this is 4/160 = 0.025). We have no cases that would have 
# a Cook's distance higher than 1, but we have several where it is higher than 0.025. So we might have a 
# problem with outliers according to one of the criteria.
# Lets test whether the assumptions of multiple regression hold true, and determine if we need to do 
# anything with these cases.


##########################################################
# CHECK ASSUMPTIONS OF LINEAR REGRESSION FOR MODEL 2 (3) #
##########################################################

# Check that the residuals of the model are normally distributed
model_2_4 %>% plot(which = 2) # QQ plot. Cases should be alligned with the theoretical diagonal.
enframe(residuals(model_2_4)) %>% ggplot() + 
  aes(x = value) + geom_histogram() # Histogram of the residuals. Should look roughly like a bell.
describe(residuals(model_2_4)) # Skewness and kurtosis > 1 can indicate the violation of the 
# assumption of normality.

### CONCLUSIONS. The plots and the statistics indicate that the residuals of model 2 are normally 
### distributed.

# Check that the relationship between the outcome variable and the predictors is linear
model_2_4 %>% residualPlots() # Lines should be roughly flat. If Pr(>|Test stat|) < 0.05, then 
# linearity might be violated.

### CONCLUSIONS. Even though there is minor curvature visible on the plots, the tests are 
### all non significant, so the linearity assumption seems to hold true for model 2.

# Check the assumption of homoscedasticity
model_2_4 %>% bptest() # Breush-Pagan test. Homoscedasticity might be violated if p < 0.05.
model_2_4 %>% ncvTest() # Non-Constant Variance Score (NCV) test. Homoscedasticity might be 
# violated if p < 0.05.

### CONCLUSIONS. The tests are both non significant, so the homoscedasticity assumption seems to hold 
### true for model 2.

# Check that there is no multicollinearity
model_2_4 %>% vif() # VIF above 3 indicates problematic multicollinearity

### CONCLUSIONS. The test does not indicate problematic multicollinearity.


###################################
# CONDUCT HIERARCHICAL REGRESSION #
###################################

summary(model_1)$adj.r.squared # adjusted R squared for model 1
summary(model_2_4)$adj.r.squared # adjusted R squared for model 2
summary(model_2_4)$adj.r.squared - summary(model_1)$adj.r.squared # difference in R squared (model 2 - model 1)

### CONCLUSIONS. Model 1 explains 12.88854% of the variance; model 2 explains 43.32843% of the variance, 
### i.e., 30.4399% more.

AIC(model_1) # measure of model fit for model 1, = 576.3424
AIC(model_2_4) # measure of model fit for model 2, = 513.3213

### CONCLUSIONS. The difference in AIC of the two models is larger than 2, so the two model are significantly 
### different in their model fit. Smaller AIC means less error and better model fit, so model 2 appears to be 
### the one to be retained.

anova(model_1, model_2_4) # as one model is a subset of the other, the anova can be used to compare the two 
                        # based on their residual error and degrees of freedom.

### CONCLUSIONS. The anova F test was significant, indicating that the models are significantly different in 
### terms of their residual errors. 

summary(model_1) 
summary(model_2_4)

### CONCLUSIONS. "Model 2 was significantly better than model 1, explaining 43.33% of the variance in 
### postoperative pain (F (8, 151) = 16.20, p < .001, Adj. R^2 = 0.43, AIC = 513.32), i.e., 30.44% more 
### than model 1, which only explained 12.89% of it (F (2, 157) = 12.76, p < .001, Adj. R^2 = 0.13, 
### AIC = 576.34). An anova F test also indicated that the two models' residual errors were significantly 
### different (F (6, 151) = 15.06, p < .001)."


######################
# COEFFICIENT TABLES #
######################

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

coef_table(model = model_1)

coef_table(model = model_2_4)
