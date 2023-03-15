# BASIC INFORMATION -------------------------------------------------------
# Author: Quang Phong
# Student ID: 6286943
# R Code for Final Exam - Descriptive and predictive data

# This file only includes codes necessary for quick re-running.
# To see all reasoning and outputs, please look at the Report attached. 


# DATA LOADING AND UNDERSTANDING  ----------------------------------------

# Load data
load("final/gamedata.Rdata")

# Rename the data frame as "df_Game" for name convention consistency
df_Game <- gamedata
rm(gamedata)

# Have a quick look at the data
head(df_Game)

# QUESTION 1  ------------------------------------------------------------

# Print summary statistics of all variables in the dataset
summary(df_Game)
str(df_Game)
## Comment: 
## There are 10 variables in this dataset of 60000 observations.
## Categorical variables include: monthindex, playerindex, genre, motivation, type, gender, dummydecember.
## Continuous variables include: spending, income.
## Discrete variables are: age. Technically, if age takes any value (e.g., 15.78 years old), it is considered continuous. However, here age only takes integer values, so it is considered discrete variable.

# Convert dummydecember to factors
df_Game$dummydecember <- as.factor(df_Game$dummydecember)
df_Game$genre <- as.factor(df_Game$genre)

# Check the types of variables again
str(df_Game)
## Comment: Now each variable has been assigned to its correct data type

# Check the number of gamers
length(unique(df_Game$playerindex))
## Comment: There are 500 games in the dataset.

# Check the number of months
length(unique(df_Game$monthindex))
## Comment: There are 120 months in the dataset.

# Age: Why?


# QUESTION 2  ------------------------------------------------------------

## 2A --------------------------------------------------------------------

# Choose all time series data for gamer ID 1 [INCOME, SPENDING]
df_Gamer1 <- df_Game[which(df_Game$playerindex == 1), ]

# Print summary of the selected dataset
summary(df_Gamer1)

# Plot the spending of this gamer over 10 years
plot(df_Gamer1$spending, type = "l")

## Comment on the characteristics of this gamer:
## This player is a female, belonging to 40% female players in 500 players here.
## She was 19 years old at the start of the research period and became 29 at the end. This means she was younger than an average player in this data set.
## Motivation to play of this player was always for destruction, which is different from the majority of observations in the dataset who has competition motivation.
## She never played any game of social type, which is unlike the majority of observations in the dataset who likes to play social games.
## In terms of mean income, she earned 1963, 24.6% less than the average of all players (2602).
## Regarding mean spending, she spent 132.5, 64.3% less than the average of all players (371.3).
## Looking at the plot of spending, we can see the amount of money she spent on gaming fluctuated and did not stay the same for every month.


## 2B -------------------------------------------------------------------------

# Step 1: Check the stationarity of the spending data for gamer 1
library(tseries)
adf.test(df_Gamer1$spending)
kpss.test(df_Gamer1$spending)

# We reject Null Hypothesis in Unit Root Test (ADF) as the p-value = 0.01 < 0.05(H0: the time series is non-stationary).
# Also, we cannot reject Null Hypothesis in KPSS Test as the p-value = 0.1 > 0.05 (H0: the time series is stationary).
# Therefore, both of the tests support that the spending over time for gamer 1 is stationary.

# Step 2: Check the seasonality
# Regarding time series properties of gamer 1's spending, I will first look at the plot of spending for that player
plot(df_Gamer1$spending, type = "l")

## By simply looking at this plot, I estimate that there can be seasonality of 12 months, BUT NO TREND. 

# To corroborate this I will plot the monthly means of spending of this player over 10 years

## Let's create month of year variable so that we can group by month of year later
library("dplyr")
df_Gamer1$monrhOfYear <- ifelse(as.numeric(df_Gamer1$monthindex) %% 12 != 0,
                                as.numeric(df_Gamer1$monthindex) %% 12,
                                12)
plot(df_Gamer1 %>% 
       group_by(monrhOfYear) %>%
       summarise(sum = sum(spending)),
     type = "l")
## Comment: As seen from the graph, the average spending in month 12 of every year is higher.


## However, additional statistical evidence must be exhibited.

# Then I will look at ACF and PACF.
# While ACF reveals ..., PACF is able to tell me ...
acf(df_Gamer1$spending, lag.max = 120)
pacf(df_Gamer1$spending, lag.max = 120)

## Comment: 
## Có autocorrelation vì t???i threshold
## From the ACF and PACF plots, there seems to have seasonality of 12 months in the spending of this gamer.
## In details, the autocorrelation at lag 12 is significantly high and gradually geometrically decays at lag 24, 36, 48, 60, ..., 
## and we also have high partial autocorrelation at lag 12.
## Therefore, it is likely that we have seasonal AR (ARIMA (0,0,0),(1,0,0)12).



# Step 3: Check the trend

## First, I will plot the annual spending of gamer 1
## Let's create year variable so that we can group by year later
df_Gamer1$year <- (as.numeric(df_Gamer1$monthindex)-1) %/% 12 + 1

plot(df_Gamer1 %>% 
       group_by(year) %>%
       summarise(sum = sum(spending)),
     type = "l")
## The spending decreased after 10 years, but not always decreased over time
## There seems to be no trend.

## Second, check by regression
model1 <- lm(df_Gamer1$spending ~ df_Gamer1$monthindex)
summary(model1)
## As the impact of monthindex on spending is not statistically significant, there is no trend in the data for gamer 1.
## V??? THÊM PLOT VS ABLINE

## 2C --------------------------------------------------------------------

# Print summary of the selected dataset
summary(df_Gamer1)

## comment t??? câu a

## nhìn summary th???y motivation, type gi??? nguyên.

# Plot each variable over time for gamer 1 (only changing variables)
plot(df_Gamer1$income, type = "l")
plot(df_Gamer1$genre, type = "l")
plot(df_Gamer1$age, type = "l")

## 2D --------------------------------------------------------------------
## should i include lags here? MAYBE I SHOULD?
model2 <- lm(spending ~ income + genre +  age + dummydecember - 1, data = df_Gamer1)
summary(model2)

## Based on the results, there are 4 variables that statistically significantly explain the spending of gamer 1.
## dummydecember has a positive effect on the spending with p-value < 0.05. In December, the player invested on average 51.64 higher than each of the other months.
## genre also has significant effects on the spending with p-value < 0.05. The player spends most when playing misc genre, followed by simulation, puzzle, strategy, platform, and finally shooter.
## NH???N XÉT C??? TH??? 
## Income also has a positive effect on the spending although the magnitude is small (0.01)
## Lastly, age has a negative effect at 0.05 significant level. 
## Adjusted R-squared:  0.8546 --> high --> model explains 85.5%

## 2E --------------------------------------------------------------------

## There are several disadvantages of this "average regression" instead of multi-level regression analysis


## L???Y VÍ D??? C??? TH???: INCOME TH???NG 1 G???N NHUNG SPENDING CÁCH R???T NHI???U

## First, because we take average of every explanatory variables and also the explained variable, there will be information loss.
## For example, the increases and decreases in one variable after a period of all players can balance each others, resulting in an average variable that did not change much over time. 
## Also here, we are interested in the effects of lagged spending variables, and we also see the seasonality in spendng of gamer 1.
## But if we do use average spending of all players, it is harder to detect them as there will be more noises.
## Except for the case where all players share the same seasonality patterns.

## Gamer 1 spent 63.4% less than the average. 
## Compared std in spending of gamer 1 with average gamer. If it is smaller, the average regression will be leaned to the bigger changes of other players.


## Second, a disadvantage of this "average regression" is that you cannot use time-invariant variables to explain the spending.
## For example, the genders of all players will be fixed over time, so it will not be included in this "average model".
## However, if we makes several typical profiles and run regression for each, we can compare and see the effects of these unique characteristics of a group on spending.
## MOTIVATION, ETC


## The gamer1, for example, did not change her motivation to play and genre of game over 120 months, so we know which variable to be excluded from the regresion.
## But with the "average regression", all variables have to be considered.



## The first disadvantage of this "average regression" is that not all variables can be taken average without losing information.
## For example, we have categorical variables like type of game, genres, etc. 

## Instead, it is better to divide our players to several groups (typical profiles) then run different regression models


# QUESTION 3 ------------------------------------------------------------------

## 3A -------------------------------------------------------------------------

# Choose time series data for gamer ID 500
df_Gamer500 <- df_Game[which(df_Game$playerindex == 500), ]

# Print summary of the selected dataset
summary(df_Gamer500)

# Plot the spending of this gamer over 10 years
plot(df_Gamer500$spending, type = "l")

## Comment on the characteristics of this gamer:
## This player is a male, belonging to 60% male players in 500 players here.
## He was 42 years old at the start of the research period and became 52 at the end. This means he was older than an average player in this data set.
## Motivation to play of this player was always for competition, which is similar to the majority of observations in the dataset.
## He always played games of social type, which is like the majority of observations in the dataset who likes to play social games.
## In terms of mean income, she earned 2985, 14.7% higher than the average of all players (2602).
## Regarding mean spending, she spent 603.8, 62.6% higher than the average of all players (371.3).
## Looking at the plot of spending, we can see the amount of money he spent on gaming fluctuated and did not stay the same for every month.

## Compared to player 1, player 500 had a completely opposite profile. He differs in gender, age cohort, income, spending on game, game type, and motivation to play. 
## This is why basing the analysis on these two gamers is more beneficial.
## We can understand and determine which factors lead to changes in monthly spending on games for two different customer profiles instead of one, which gives us a bigger picture, better forecasting and more insights into what to do for each customer segment to boost spending.
## More importantly, by analyzing the 2 games, we can examine inter-dependency of their spendings and mutual effects of explantory variables on the spendings at a given time. For example, there can be negative correlation between monthly spending of player 1 and player 500, which cannot be explored without analyzing the time series at the same time.

## 3B -------------------------------------------------------------------------
# First, we need to check stationarity conditions for both 2 time series.
# Because VAR is an extension of AR model which also requires stationarity conditions.

# As we already check it for gamer 1, now we will check the stationarity of the spending data for gamer 500
adf.test(df_Gamer500$spending)
kpss.test(df_Gamer500$spending)

# We reject Null Hypothesis in Unit Root Test (ADF) as the p-value = 0.01 < 0.05(H0: the time series is non-stationary).
# Also, we cannot reject Null Hypothesis in KPSS Test as the p-value = 0.1 > 0.05 (H0: the time series is stationary).
# Therefore, both of the tests support that the spending over time for gamer 500 is stationary.
# So we do not need differencing calculation to apply VAR for these time series

# Second, estimate VAR(1) for the spendings of 2 gamers without explanatory variables
# Create a data frame that has variables of 2 gamers over time

df_2gamers <- df_Gamer1 %>%
  inner_join(df_Gamer500, by = "monthindex", suffix = c("1", "500"))
ts_2gamers <- ts(df_2gamers)

library(vars)

# VAR(1) model without any explanatory variables
# "const" --> why not choose --> r-square negative
model3 <- VAR(ts_2gamers[, c("spending1", "spending500")], p=1, type="none")
summary(model3)

## Comment on the estimation results.

## Lagged (1) spending of gamer 500 plays an important role in predicting both players' spending at time t.

## Lagged (1) spending of gamer 500 has a statistically significant (p-value ~ 0) positive correlation with spending of gamer 1 at time t, with a fair estimate (0.24).
## However, lagged (1) spending of gamer 1 cannot help predict her spending at time t.
## Adjusted R-Square of the model is 0.9622, which is high.

## Lagged (1) spending of gamer 500 has a statistically significant (p-value ~ 0) positive correlation with his spending at time t, with a very big estimate (0.997).
## Lagged (1) spending of gamer 1 cannot help predict the spending of gamer 500 at time t.
## Adjusted R-Square of the model is 0.9954, which is high.


model4 <- VAR(ts_2gamers[, c("spending1", "spending500")], p=1, type="const")
summary(model4) # ko chon thang nay
# In this model, only const explains the 

## 3C -------------------------------------------------------------------------
# VAR(1) with explanatory variables
# We will include genre1, genre500, income1, income500 which are variables that change over time and are likely to explain the changes of spending1 and spending500.
model5 <- VAR(ts_2gamers[, c("spending1", "spending500", "genre1", "genre500", "income1", "income500")], p=1, type="none")
summary(model5)

# Comment on estimation results???
## age effects ???
# maybe I should not include const at all because of low R-squared
model6 <- VAR(ts_2gamers[, c("spending1", "spending500", "genre1", "genre500", "income1", "income500")], p=1, type="const")
summary(model6)

# To compare this model to the one in question 3B, we will use BIC.
BIC(model3)
BIC(model5)
## VAR(1) model in question 3B uses fewer parameters to explain the changes in spending of 2 players, thus it has lower BIC
## Therefore, it fits the data better.


## 3D -------------------------------------------------------------------------

# NH??? NÓI T???I SAO CH???N SPENDING500 D??? XÉT (VÌ NÓ SIGNIFICANT)
causality(model3, cause = "spending500")$Granger
## p-value ~ 0 < 0.05 --> We reject H0. We conclude that spending500 Granger-causes spending1.
causality(model3, cause = "spending1")$Granger
## p-value ~ 0.9704 > 0.05 --> We cannot reject H0. We cannot conclude that spending1 Granger-causes spending500.

## LÀM TUONG T??? CHO 3C
causality(model5, cause = "spending500")$Granger
## p-value ~ 0 < 0.05 --> We reject H0. We conclude that spending500 Granger-causes spending1.
causality(model5, cause = "spending1")$Granger
## p-value ~ 0.9704 > 0.05 --> We cannot reject H0. We cannot conclude that spending1 Granger-causes spending500.


chisq.test(df_2gamers$genre1, df_2gamers$genre500, correct = FALSE)

## Because p-value is 0.04 < 0.05, so we reject null hypothesis (The two variables are independent).
## We conclude that the 2 genres of games that player1 and player500 play are related to each other.

## V??? 2 PLOTS C???A GENRE C???A 2 TH???NG OVER TIME


## 3E -------------------------------------------------------------------------

## The number of parameters to be estimated in model3 (lag 1, without intercept) is:
## 2*2 = 4 (parameters)
## WHY NO ERROR TERM --> ?

## The number of parameters to be estimated in model5 (lag 1, without intercept) is:
## 6*6 = 36 (parameters)


## 3F -------------------------------------------------------------------------
## Pro: 2 players belong to the same group where they play the same game patterns over time. 
## Therefore, the spending of player 1 and spending of player 500 can be dependent on each other.
## This is not the case for all 500 players. So if including all of them, we will have more noises.

## PRO: run time nhanh hon


## Pro: less likely to be overfitting and less likely to suffer from the curse of dimensionality. 
## When we include 500 players, we will have (3*500)*(3*500) = 2250000 parameters (lag 1 for spending, income, genre, without intercept)
## According to ..., require minimum 10 observations per predictor, which is minimum number of 2250000 * 10 = 22500000 observations.
## Meanwhile, we only have 60000 observations. 

## Easier and more intuitive for interpretation than for 500 variables.
## Simpler for estimation.


## Cons: Does not take into account the effects of possible other players' spendings and explantory variables. High chance that the spending of player 1 does not just depend on player 500, also on some of the others of the same group.


# QUESTION 4 ------------------------------------------------------------------

## 4A -------------------------------------------------------------------------
library(plm)
library(graphics)
library(gplots)
# create panel data index, make it a panel data frame
pn_Game <- pdata.frame(df_Game, index = c("playerindex", "monthindex"))

# Coplot
coplot

# Plot the spending data with respect to genres
plotmeans(spending ~ genre, data=pn_Game, main = "Heterogeineity across genres")
## Comment: spending differs according to the genre of games that is played

## 4B -------------------------------------------------------------------------

# Individual fixed effects model
modelFixed1 <- plm(spending ~ income + genre +  age + dummydecember + motivation + type + gender
                   , data = pn_Game, 
                     model = "within", effect = "individual")
print(summary(fixef(modelFixed1)))
summary(modelFixed1)

## Comment: Fixed effects of income, genre, age, dummydecember are all significant (p-value < 0.05) and different from zero (except for income with estimate of 0.0097)
## 

# ow about genre fixed effects model???
#modelFixed2 <- plm(spending ~ income + factor(genre)  +  age 
      #             + dummydecember + motivation + type + gender
     #              , data = pn_Game, 
         #          model = "within", effect = "individual")
#modelFixed2Dummy <- plm(spending ~ income + factor(genre)  +  age 
 #                  + dummydecember + motivation + type + gender
 ##                  , data = pn_Game, 
 #                  model = "within", effect = "individual")
#print(summary(fixef(modelFixed2Dummy)))
#summary(modelFixed2Dummy)

modelFixed2 <- plm(spending ~ income + genre +  age + dummydecember + motivation + type + gender
                   , data = pn_Game,  index = ("genre"),
                   model = "within", effect = "individual")
print(summary(fixef(modelFixed2)))
summary(modelFixed2)


## 4C ---------------------------------------------------------------------

## Compared to the model in question 2, the estimates here show some differences.
## Age variable is also significant here, which makes sense because in the first model we do not have diversity in age as we just focus on one player.
## The magnitude of the parameters are smaller in this model because it is more generalized, and are the remaining after taking out the dummy variable for each individual.

## Advantage of this panel data model to the representative individual model is that 
## we can use this to predict spending for every single player rather than just the first player.
## Also, we can see 2 different types of effects: common effects shared between them, and individual effects.

## HERE ALSO INCLUDE MOTIVATION, TYPE, GENDER vì cái trên kia constant.


## 4D ----------------------------------------------------------------------
# Estimate a random effects model 
modelRandom1 <- plm(spending ~ income + genre +  age + dummydecember + motivation + type + gender, data = pn_Game, 
                   model = "random", effect = "individual", random.method = "amemiya")
print(summary(modelRandom1))
ranef(modelRandom1)

# Comment??????????


## 4E  -----------------------------------------------------------------------
# Use a Hausman test to test whether the additional exogeneity assumption is violated.
# H0 : Both fixed and random effects models can be used (the exogeneity assumption in the random effects model is not violated).
# HA : One of the models is inconsistent (the random effects model is inconsistent, since the fixed effects model does not have the exogeneity assumption).
phtest(modelFixed1, modelRandom1)

# What is your conclusion about the use of the random effects model for these data?
## As the p-value is 1, lower than 0.05, we accept the H0. 
## The exogeneity assumption in the random effects model is not violated. The random effects model is consistent and can be used.


## 4F ------------------------------------------------------------------------
## Pro: more accurate,
## Less bias


## Indeed, VAR in question 3 is also considered panal data model.


## Only lack lagged dependent and independent variables???  

## Don't think that the absence of them biases the panel data estimation results.
## Because we already set monthindex as index.


## Yes it can cause bias. Because it do not really understand which lagged values. 
## It will just treat monthindex as a category like sector, rather than increasing value?
## Evidence: lower R-squar, lower BIC


# QUESTION 5 ----------------------------------------------------------------

## 5A  ----------------------------------------------------------------

# Create binary series
df_Game$largeSpending <- ifelse(df_Game$spending > 450, 1, 0)

# Select the player with the equal number of high and low monthly spendings
# So that we have balanced category for dependent variable
library(dplyr)


df_Count <- df_Game %>% 
  group_by(playerindex) %>%
  summarize(sum = sum(largeSpending))
df_Count %>% filter(sum == 60)

# We can choose player 121 as a representative player because that player has 60 months with high spendings and 60 months with low spendings

## 5B  ----------------------------------------------------------------
df_Gamer121 <-  df_Game[which(df_Game$playerindex == 121), ]
df_Gamer121$largeSpending <- as.factor(df_Gamer121$largeSpending)

# Should I split data to train vs test?

# Find starting points
beta0 <- lm(df_Gamer121$largeSpending ~ df_Gamer121$income + df_Gamer121$genre + df_Gamer121$dummydecember + df_Gamer121$age -1)$coef
beta0
# Use the glm function to estimate the logit model
modelLogit <- glm(largeSpending ~ income + genre + dummydecember + age - 1, 
                  family=binomial(link='logit'), start = beta0, data = df_Gamer121)

logLik(modelLogit)
modelLogit$coefficients 
summary(modelLogit)

## Comment on log lik (high?)
## The variables in the model have significant impacts on predicting the binary spending are income, age, and levels misc, puzzle, simulation of genre variable.

## Income is associated with higher log odds of large spending (gamer's success). This is in line with question 2, where the higher the income is, the higher the monthly spending on game of player1 is, keeping others unchanged..
## However, the effect is quite small, at 0.0036.

## Among the genres of games played, misc leads to higher log odds of large spending than simulation, followed by puzzle.  
## Compared to player1 in question2, the impacts of these 3 genres are similar, playing misc is associated with higher spending than simulation, then puzzle.

## Finally, the effects of age here is positive.
## The older the player is, the higher the log odds he spends more than 450 per month.
## This is only in line with player500, but different from the player1 in question 3.

## Interestingly, the monthly spending in December does not lead to higher log odds of large spending compared to other months, keeping others constant.
## This is not in line with the results in question 2, where December is positively associated with spending.
## This may result from the choice of threshold at 450, which is not yet a good cut off for player121 to differentiate between high spending in December versus other months.


## 5C  ----------------------------------------------------------------

predDataLogit <- predict(modelLogit, df_Gamer121, type = "response", se.fit = TRUE)
upr <- predDataLogit$fit + predDataLogit$se.fit*1.96 ##upper bounds
lwr <- predDataLogit$fit - predDataLogit$se.fit*1.96 ##lower bounds
fit <- predDataLogit$fit

plotCI(predDataLogit$fit[order(predDataLogit$fit)], ui = upr[order(predDataLogit$fit)], li = lwr[order(predDataLogit$fit)])

## To estimate the accuracy of predicted probabilities, we should see 2 plots side by side: predictabilities and real spending of gamer 121

par(mfrow=c(1,2))

plotCI(predDataLogit$fit[order(predDataLogit$fit)], ui = upr[order(predDataLogit$fit)], li = lwr[order(predDataLogit$fit)], col = "brown")
abline(h=0.5)

plot(df_Gamer121$spending[order(predDataLogit$fit)], col = "blue")
abline(h = 450)

## As can be seen in the graphs, the probabilities estimated are mostly accurate. The higher the probabilities are corresponding to the higher spendings.


## 5D  ----------------------------------------------------------------
## Although the logit model gives us the log odds which are nice to obtain, the choice of threshold can make the model a little biased.
# Compare to the results in question 4, here we also obtain the positive effects of income, positive effects of age on spending.
# However, we do not see the impacts of genre and dummydecember. This can result from the fact that the dependent variable is in binary form with a threshold of spending at 450.
# For example, dummydecember increases spending, but it does not necessarily increase the spending from below threshold to above threshold compared to other months.
# The same logic can be applied to genre. Compared to question 2, where we also apply modelling for one player, but in that case, the dependent variable is continuous, so the effect can be more easily detected than this binary outcome.

# Also, the lagged variables are not included in this model. Including lagged spending can increase the accuracy and fitness of the model.









