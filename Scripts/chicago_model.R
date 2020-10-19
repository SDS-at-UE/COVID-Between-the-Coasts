## Chicago Linear Model
## Maya Frederick

library(tidyverse)
library(glmnet) # lasso
library(MASS) # stepAIC
library(leaps) # regsubsets

# read in the master chicago service data set
chicago_master <- read_csv("Data/chicago_master_for_service_model.csv")

# read in COVID data
chicago_covid <- read_csv("Data/chicago_updated_cases.csv")
chicago_covid <- na.omit(chicago_covid)
chicago_covid <- chicago_covid %>% 
  mutate(case_rate = Cases/Population*100000) %>% 
  mutate(pos_rate = Cases/Tested)

# join the two data sets 
chicago_master2 <- full_join(chicago_master, chicago_covid, by = c("GEOID" = "Zip"))

# just wanted the case rate, so i get rid of the other variables from chicago_covid
chicago_master2 <- chicago_master2[,-c(36:38,40)]

# first model: simple linear model with all the variables
model_1 <- lm(case_rate ~ . -GEOID, data = chicago_master2)
summary(model_1)
# 6 significant variables: prop_female_19_to_34, prop_female_with_HI, prop_19_to_34_with_HI
##                         prop_female_19_to_34_with_HI, prop_white, prop_female
# additionally, the adjusted r-squared is 0.8286. p-value is significant.

# model 2: simple linear model with only the significant variables from above
model_2 <- lm(case_rate ~ prop_female_19_to_34 + prop_female_with_HI + prop_19_to_34_with_HI +
                prop_female_19_to_34_with_HI + prop_White + prop_female, data = chicago_master2)
summary(model_2)
# adjusted r-squared goes way down to 0.5241. p-value is still significant. 

### Lasso ###

# setting up the data 
x <- model.matrix(case_rate ~ ., chicago_master2)[,-1] # trim off the first column
x=x[,-1]

# lasso model
lasso <-glmnet(x = x,y = chicago_master2$case_rate, alpha=1)

c <- coef.glmnet(lasso, s = 'lambda.min', exact = TRUE)
inds <- which(c != 0)
variables <- row.names(c)[inds]
variables <- variables[!variables %in% '(Intercept)']
view(variables)
# gives 7 variables 

### foward/backward/stepwise AIC ###
full <- lm(case_rate ~ . -GEOID, chicago_master2)
base <- lm(case_rate ~ 1, chicago_master2)

# foward 
stepAIC(base, direction = "forward", scope = list(upper = full,lower = base), trace = FALSE)$anova

# backward
stepAIC(full, direction = "backward", trace = FALSE)$anova

# stepwise
stepAIC(base, direction = "both", scope = list(upper = full,lower = base), trace = FALSE)$anova
stepAIC(full, direction = "both", scope = list(upper = full,lower = base), trace = FALSE)$anova

# some variables are the same as the lasso, backwards and stepwise backward eliminated few variables


####################################################
# I couldn't get the lasso to run on my computer. So I recreated the analysis below:

grid <- 10^seq(10, -2, length = 100)
lasso_start <- glmnet(x, chicago_master2$case_rate, alpha = 1, lambda = grid)

set.seed(1)
cv_out <- cv.glmnet(x, chicago_master2$case_rate, alpha = 1)
best_lam <- cv_out$lambda.min
lasso_final <- glmnet(x, chicago_master2$case_rate, alpha = 1, lambda = grid)
lasso_coef <- predict(lasso_final, type = "coefficients", s = best_lam)
lasso_coef
