######## Data loading and cleaning

# the pacman package will allow us to install and load multiple packages at once
# it will only install packages that you do not currently have, but it will load all 
# packages mentioned in the vector

# This line cleans out R's memory to ensure you are starting with a 
# fresh session
rm(list = ls())

# this option prevents R from automatically coercing character strings
# into ANOVA style factors
options(stringsAsFactors = FALSE)

# install.packages("pacman")
pacman::p_load(MASS, glmnet, relaimpo, yhat, Hmisc, tigerstats, xlsx, devtools) 


setwd('~/Desktop/Projects/Active/SIOP 2017/Master Tutorial - Modern Methods')

# Federal Human Capital Survey 2008 Data set
#  "Survey data documenting each government employee's 
#   personal and employment background, current position, 
#   job satisfaction, work relationships with other employees and supervisors, 
#   experiences within their agencies, and perceived promotional opportunities." 
#   There are 74 survey questions and several demographic type questions
#
# Data obtained from https://catalog.archives.gov/id/4529783

#   let's see what happens when try to read in the data 
d  <- read.csv('Data/FHCS2008_public_release.csv')
summary(d)

# it's weird that we are getting so many character vectors- what's going on?

xtabs(~d$Q11)

# From the technical specifications document, we know that 'X' is 
# treated as NA; we can use this information when reading in data to 
# automatically coerce that string to NA
# let's try reading in the data again

d  <- read.csv('Data/FHCS2008_public_release.csv', na.strings = c('X'))
summary(d)

# check dimensionality
dim(d)
# [1] 212223     88

# the mcol function will produce a matrix of column names for a given object x
mcol <- function(x) matrix(colnames(x))
# shows us that the 74 quesitons are written as 'Q1', 'Q2', etc.
# also that they occupy columns 3:76

str(d[, 3:76]) # check that the variables are numbers

# what if we wanted a data set with only numeric variables?
# we can use sapply to determine with variables are numeric
NumericDataBinary <- sapply(d, is.numeric)

# this will pull in only the numeric variables identified in the line above
NumericData <- d[!! NumericDataBinary]


############################################
# variable selection algorithms


# reduce the data set to only the variables we will be using for the variable
# selection algorithms

# If you we inspect the FHCS Codebook, we find that question Q62 
# could act as a criterion: Considering everything, how satisfied 
# are you with your pay?
#
# For now we will keep all of the survey items and include the following
# demographic variables:
#
#   DLOC    = Where do you work? 
#   DSUPER  = What is your supervisory status
#   DAGYTEN = How long have you been with your current agency 
#
# summary information about these variables

table(d[, 'DLOC'])
table(d[, 'DSUPER'])
table(d[, 'DAGYTEN'])

# lapply(d[, c('DLOC', 'DSUPER', 'DAGYTEN')], table)

# notice that these 3 variables has a case that is blank.. we should 
# convert this to NA; first we will see what the blank is

unique(d[, 'DLOC'])
unique(d[, 'DSUPER'])
unique(d[, 'DAGYTEN'])

# lapply(d[, c('DLOC', 'DSUPER', 'DAGYTEN')], unique)
d[, c('DLOC', 'DSUPER', 'DAGYTEN')][d[, c('DLOC', 'DSUPER', 'DAGYTEN')] == ' '] <- NA

# we could recode these variables to use their item options
# there are a number of ways to do this... we will the recoding explicitly

# recode DLOC
d$dloc_recode <- NA
d$dloc_recode[d$DLOC == 'A'] <- 'Headquarters'
d$dloc_recode[d$DLOC == 'B'] <- 'Field'

# recode DSUPER
d$dsuper_recode <- NA
d$dsuper_recode[d$DSUPER == 'A'] <- 'Non_Supervisor'
d$dsuper_recode[d$DSUPER == 'B'] <- 'Team Leader'
d$dsuper_recode[d$DSUPER == 'C'] <- 'Supervisor'
d$dsuper_recode[d$DSUPER == 'D'] <- 'Manager'
d$dsuper_recode[d$DSUPER == 'E'] <- 'Executive'

# recode DAGYTEN
d$dagyten_recode <- NA
d$dagyten_recode[d$DAGYTEN == 'A'] <- 'Less_than_1_year'
d$dagyten_recode[d$DAGYTEN == 'B'] <- '1_to_3_years'
d$dagyten_recode[d$DAGYTEN == 'C'] <- '4_to_5_years'
d$dagyten_recode[d$DAGYTEN == 'D'] <- '6_to_10_years'
d$dagyten_recode[d$DAGYTEN == 'E'] <- '11_to_20_years'
d$dagyten_recode[d$DAGYTEN == 'F'] <- 'More_than_20_years'

# grab the variables we will be using and put them in a new data.frame

d1 <- d[, c(paste0('Q', 1:74), 'dloc_recode', 'dsuper_recode', 'dagyten_recode')]

# casewise remove NAs - will make things easier...
# could always impute... but that is another tutorial

d2 <- na.omit(d1)
dim(d2)

## we will split our data into predictors and criterion

## create model matrix of predictors - this is a quick way to
## dummy code the categorical variables..
X <- data.matrix(model.matrix(~ ., d2[, colnames(d2) != 'Q62']))
X <- X[, -1]   # get rid of intercept column (just a column of ones)

# prevent strange behavior that results from using model.matrix by
# forcing these attributes to be NULL
attr(X, 'assign')    <- NULL 
attr(X, 'contrasts') <- NULL

y  <- as.numeric(d2[, 'Q62'])


######
###### Alright time for some variable selection

###### Elastic Net
##
## A combination of Lasso and Ridge regression. 
## Good read: Zhou & Hastie (2005)
## Great read: Hastie, Tibshirani, and Friedman (2008)
##             The elements of statistical learning
## Also, check out the glmnet vignette for more information

## We will start by pretending that we have a data set that isn't 
## large enouph to have a holdout sample...

## subset of the data; set.seed for reproducibility
set.seed(123)

n_sample  <- 100
rand_samp <- sample(1:nrow(d2), n_sample, rep = FALSE); rand_samp

X_samp <- X[rand_samp, ]
y_samp <- y[rand_samp]


## 10 fold CV to tune the regularization parameter in Elastic Net
## In 10-fold CV, the sample is randomly plit into 10 (equal) sized 
## subsamples. Of the 10 subsamples, a single is chosen as the validation
## set and the remaining 9 are used to train the model. This procedure
## is repeated for each subsample..
## useful args:
##    alpha - controls mixing between ridge and lasso; ridge = 0 and lasso = 1  
##    nfolds - nuber of folds for CV; defaults to 10
##    keep  - keep the id's corresponding to what data was used in each fold
m1 <- cv.glmnet(x = X_samp, y = y_samp, alpha = 1, keep = TRUE)

## we can plot the results of the cv models
## the plot shows the MSE with associated SE interval
## the first vertical dashed line shows the model with teh lowest MSE
## the second vertical dashed line shows the model with largest lambda
## that is within one standard error of the minimum MSE
plot(m1)

## you can use the predict function for any of the models that were fit
## during the model building. It defaults to the largest lambda within one 
## standard error of the minimum MSE. You don't have to use this value...
## Some people favor it because the model associated with this value is more 
## parsimonious than the minimum MSE lambda model, and tends to due better
## in smaller sample sizes 

## args:
##  object - cv.glmnet object
##  newx   - matrix of new values for x 
##  s      - which lambda (model) to use? can supply 'lambda.min' which 
##           corresponds to the lambda with the smallest MSE, 'lambda.1se'
##           which corresponds to the largest lambda within one 
##           standard error of the minimum MSE lambda, or a numeric value..
predict(object = m1, newx = X[1:10, ], s = 'lambda.1se')


#####
##### Now we are going to use  CV for the alpha parameter as well

# percentage of mixing between lasso and ridge with 1 = lasso
alpha <- seq(0, 0.9, length = 10) 

# container for the results of the cv models
m2 <- vector('list', length(alpha) + 1); m2

# set seed for reproducibility 
set.seed(123)

for(i in 1:length(alpha)) {
    m2[[i]] <- cv.glmnet(X_samp, y_samp, foldid = m1$foldid, a = alpha[i])
}

# add m1 to the list of cv models
m2[[11]] <- m1

# create matrix of output to determine the best tuning parameters
tune_mat <- data.frame(CVM        = rep(NA, 11), 
                       min_lambda = rep(NA, 11),
                       alpha      = c(alpha, 1))

# fill out the matrix
for(i in 1:length(m2)) {
    tune_mat$CVM[i] <- min(m2[[i]]$cvm)
    tune_mat$min_lambda[i] <- m2[[i]]$lambda.min
}

tune_mat[order(tune_mat$CVM), ]

# the results suggest that the second cv model pass contains the tuning parameters 
# that yield the lowest MSE
low_mse <- m2[[2]]

# check out modee coefficients
coef(low_mse)

# can plot the results of the 10 fold cross validation
plot(low_mse)

# can plot the coefficient profile plot of the coefficient paths
# useful args:
#   xvar  - what is on the x-axis; we will be using 'lambda' for consistency
#           with earlier plots
#   label - show variable labels?

plot(low_mse$glmnet.fit, xvar = 'lambda', label = TRUE)


###### Now we will pretend we have enough data to do use training and 
###### validation sample to build and test our models
set.seed(191787882)
n_sample1  <- 360
rand_samp1 <- sample(1:nrow(d2), n_sample1, rep = FALSE); rand_samp1

X_samp1 <- X[rand_samp1, ]
y_samp1 <- y[rand_samp1]


# split data into training set and test set
X_train <- X_samp1[1:(n_sample1 * .75), ]
y_train <- y_samp1[1:(n_sample1 * .75)]

X_test  <- X_samp1[(n_sample1 * .75 + 1):n_sample1, ]
y_test  <- y_samp1[(n_sample1 * .75 + 1):n_sample1]

n_test <- length(y_test); n_test




# this time we will only look at alpha = 0, 0.5, and 1 for simpler
# code and demonstration

set.seed(1)
num_folds <- 10
fold_id   <- sample(1:10, length(y_train), replace = TRUE)
alphas    <- c(0, 0.5, 1)

m3 <- vector('list', length(alphas)); m3

set.seed(123)
for(i in 1:length(m3)) {
    m3[[i]] <- cv.glmnet(X_train, y_train, a = alphas[i], foldid = fold_id)
}

### we have 3 models that we can now test (one best model per alpha value)

yhat1  <- predict(m3[[1]], s = m3[[1]]$lambda.1se, newx = X_test)
yhat2  <- predict(m3[[2]], s = m3[[2]]$lambda.1se, newx = X_test)
yhat3  <- predict(m3[[3]], s = m3[[3]]$lambda.1se, newx = X_test)


mse1 <- mean((y_test - yhat1)^2); mse1
mse2 <- mean((y_test - yhat2)^2); mse2
mse3 <- mean((y_test - yhat3)^2); mse3

test_mse <- c(ridge = mse1, enet5 = mse2, lasso = mse3)
sort(test_mse, decreasing = FALSE)

rsq_a1 <- 1 - (mse1/var(y_test)); rsq_a1
rsq_a2 <- 1 - (mse2/var(y_test)); rsq_a2
rsq_a3 <- 1 - (mse3/var(y_test)); rsq_a3

test_rsq_a <- c(ridge = rsq_a1, enet5 = rsq_a2, lasso = rsq_a3)
sort(test_rsq_a, decreasing = TRUE)

## so the enet .50 wins 


### for funsies we can look at what the training pass suggested 
### as the best model

train_only <- c(ridge = min(m3[[1]]$cvm), 
                enet5 = min(m3[[2]]$cvm),
                lasso = min(m3[[3]]$cvm))

sort(train_only, decreasing = FALSE)

# suggested enet .50 as the best as well...



## for another fun comparison - we can use multiple regression
X_train_lm <- as.data.frame(X_train) # coerce design matrix to data.frame
X_test_lm  <- as.data.frame(X_test)

lm_m     <- lm(y_train ~ ., X_train_lm)
summary(lm_m)
yhat_reg <- predict(lm_m, newdata = X_test_lm)

num_coef <- length(lm_m$coefficients) - 1

lm_mse   <- mean((y_test - yhat_reg)^2); lm_mse
lm_rsq_a <- 1 - lm_mse/var(y_test); lm_rsq_a

# worse than all three methods from before


# step-wise regression

lm_train_data <- data.frame(y_train, X_train_lm)
lm_lower <- lm(y_train ~ 1, data = lm_train_data)
lm_upper <- lm(y_train ~ ., data = lm_train_data)

step    <- stepAIC(lm_lower, direction = "forward", 
                   scope = list(lower = lm_lower, upper = lm_upper))
step$anova 

step_num_coef <- length(step$coefficients); step_num_coef
yhat_step     <- predict(step, newdata = X_test_lm)
lm_step_mse1  <- mean((y_test - yhat_step)^2)
lm_step_rsq1  <- 1 - lm_step_mse1/var(y_test)

c(lm_step_mse1, lm_step_rsq1) 

sort(test_mse)
sort(test_rsq_a, dec = T)

# step-wise not doin so well either... 


# what if we used the variables suggested by the enet 0.50?

m3_2_coef <- coef(m3[[2]]); m3_2_coef # gives us a sparse matrix of coefficients
m3_2_coef <- matrix(m3_2_coef); m3_2_coef
rownames(m3_2_coef) <- rownames(coef(m3[[2]])); m3_2_coef
var_names <- rownames(m3_2_coef[m3_2_coef != 0, , drop = FALSE]); var_names

X_train_reg <- X_train_lm[, var_names[-1]]
lm_m1       <- lm(y_train ~ ., X_train_reg)
yhat_reg1   <- predict(lm_m1, newdata = X_test_lm[, var_names[-1]])

lm_mse1 <- mean((y_test - yhat_reg1)^2); lm_mse1
lm_rsq1 <- 1 - lm_mse1/var(y_test); lm_rsq1

sort(test_mse)
sort(test_rsq_a, dec = T)

## the best...


#####################################
#variable importance

# after deciding what variables to use in your model- someone might ask you: 
#what is the most important driver of employee satisfaction with their pay?
# next we are going to look at how we might answer that question

# first let's get a dataset with our criterion- Q62, and the predictor 
# variables we found above from the enet 0.50?

importance_variables <- c( "Q62", var_names)

# let's remove the "(Intercept)" from this list by removing the first item
importance_variables <- importance_variables[-2]

# let's pull these items from the full dataset

importance_data <- subset(d, select = c(importance_variables))


# get a full correlation matrix
importance_correlations <- rcorr(as.matrix(importance_data))$r

# let's run a standard regression with our variables on the full dataset
importance_regression <- lm(Q62 ~ ., data = importance_data)
summary(importance_regression)

# note the number of observations used in the regression
length(importance_regression$residuals)


# the regression output is useful if you want to examine the statistical significance of your predictors
# this output provides unstandardized regression weights
# you might want to see the standardized regression weights
# we can get the standardized weights using matrix algebra and the correlation matrix

# first get the correlation matrix of just the predictors
# remember that the criterion is in the first row and column of the correlation matrix
R.X <- importance_correlations[2:(nrow(importance_correlations)), 2:(ncol(importance_correlations))]

# we will also need the vector of just the predictor-criterion correlations
rxy <- importance_correlations[2:(nrow(importance_correlations)), 1]

# beta will be our vector of standardized regression coefficients
beta <- solve(R.X) %*% rxy

colnames(beta) <- "beta"

# we can also use matrix algebra to get the model r squared
# note that these results will differ slightly from those 
# produced by lm since the correlation matrix was produced with 
# pair-wise rather than list-wise deletion
OLSrsquared <- t(beta) %*% R.X %*% beta

# one popular approach to relative importance analysis is dominance analysis
# dominance analysis examines all possible subsets of regression models
# frequently people use general dominance weights
# general dominance weights are mathematically equivalent to the lmg
# approach used in the relaimpo package the relaimpo package is
# computationally more efficient for calculating general dominance weights 
# than other cran packages

# there are multiple input objects you can use in calc.relimp
# you can use the lm object:
lm_dominance <- calc.relimp(importance_regression, type = "lmg")

# a correlation or covariance matrix:
corr_dominance <- calc.relimp(importance_correlations, type = "lmg")

# the full dataset
fulldata_dominance <- calc.relimp(importance_data, type = "lmg")

# note that these results can differ due to missing data handling and rounding error

lmg_outputs <- cbind(lm_dominance$lmg, fulldata_dominance$lmg, corr_dominance$lmg)

colnames(lmg_outputs)<- c("Dominance: lm input", "Dominance: full data input","Dominance: correlation matrix input")

lmg_outputs

# notice that the lm input and the full data input are the same
# the correlation matrix input is slightly different

# dominance analysis is used to partition the explained variance
# the general dominance weights sum to r squared
# one way to use general dominance weights is to say "variable i accounts for x% of the variance explained by this model"
# let's convert our results to %variance accounted for
# remember that r squared is slightly different depending on how we calculated it
# an easy way to do this is using colPerc from the tigerstats package

colPerc(lmg_outputs)

# relative weights has been proposed as another method for paritioning the variance accounted for across all variables in a model
# it approximates general dominance weights
# it has recently been criticized due to the mathematical derivation of the formula 

rlw_output <- rlw(importance_data, colnames(importance_data)[1], colnames(importance_data)[2:ncol(importance_data)])
colnames(rlw_output) <- "Relative Weights Output"

#in order to compare- let's combine our outputs so far

Rawrelative_importance_output <- cbind(lmg_outputs, rlw_output)

# now let's add in the percent variance accounted for
Percentrelative_importance_output <- colPerc(Rawrelative_importance_output)[1:nrow(Rawrelative_importance_output),]

# let's make our column name more specific

colnames(Percentrelative_importance_output)<- paste("Percent Variance",colnames(Percentrelative_importance_output))

# now let's combine all our output, betas, dominance analysis, relative weights, and bivariate correlations
full_importance_output<- cbind(Rawrelative_importance_output, Percentrelative_importance_output, beta, rxy)

#maybe we want to break down our analyses by group 
# DPAYCAT indicates the pay category/grade of the survey participant

xtabs(~d$DPAYCAT)

# there are 7 possible responses as well as what looks like a set of non-responders
# if we want the same results we had above, we do not want to have to do that everytime
# we may also want to save our data to an excel sheet for easy sharing/referencing


# let's first write a function that would produce the matrix of relative importance output we have above
# our inputs will be a dataset, a vector of predictor variable names, and a dependent variable
# we are going to take the code we used above and edit it slightly for this function to work

RelativeImportanceOutput <- function(dataset, IV, DV){
    # first we get a dataset with only our IVs and DVs, the DV will be the last column
    importance_data <- subset(dataset, select = c(DV,IV))
    # let's grab our lm output, remember we have formatted our data so 
    # our DV will be the first column
    Y <- importance_data[, 1]
    X <- importance_data[, 2:ncol(importance_data)]
    importance_regression <- lm(Y ~ ., data = X)
    # next we are grabbing our full correlation matrix of predictors and the criterion
    importance_correlations <- rcorr(as.matrix(importance_data))$r
    # get just the criterion correlations
    R.X <- importance_correlations[2:(nrow(importance_correlations)), 2:(ncol(importance_correlations))]
    # get just the zero-order correlation coefficients
    rxy <- importance_correlations[2:(nrow(importance_correlations)), 1]
    #beta using matrix algebra
    beta <- solve(R.X) %*% rxy
    colnames(beta) <- "beta"
    # now get the various outputs from dominance analysis
    lm_dominance <- calc.relimp(importance_regression, type = "lmg")
    corr_dominance <- calc.relimp(importance_correlations, type = "lmg")
    fulldata_dominance <- calc.relimp(importance_data, type = "lmg")
    # relative weights
    rlw_output <- rlw(importance_data, colnames(importance_data)[1], colnames(importance_data)[2:ncol(importance_data)])
    #put together raw results
    rawoutputs <- cbind(lm_dominance$lmg, fulldata_dominance$lmg, corr_dominance$lmg, rlw_output)
    colnames(rawoutputs) <- c("Dominance: lm input", "Dominance: full data input","Dominance: correlation matrix input","Relative Weights Output")
    # create percent variance accounted for outputs
    Percent_Variance <- colPerc(rawoutputs)[1:nrow(rawoutputs),]
    colnames(Percent_Variance) <- paste("Percent Variance",colnames(Percent_Variance))
    # make sure the full output can be accessed outside of the function by using <<
    full_importance_output <<- cbind(rawoutputs, Percent_Variance, beta, rxy)
}

#now let's use this function to produce results for each of our compensation groups

# first, let's create a vector with all unique values for DPAYCAT

UniquePayCategories <- unique(d$DPAYCAT)

UniquePayCategories

# great, this also means if the values here ever change, our code will still work

# let's use this vector to create the output we want for all groups

for(i in 1:length(UniquePayCategories)){
    groupdata <- subset(d, DPAYCAT == UniquePayCategories[i])
    categoryname <- UniquePayCategories[i]
    RelativeImportanceOutput(groupdata, var_names[-1], "Q62")
    filename <- paste("Group", categoryname)
    write.xlsx(full_importance_output, "Relative Importance Output by Pay Category.xlsx", sheetName= filename, append=TRUE)
    print(categoryname)
}
    
# as mentioned above relative weights has been recently criticized
# due to this criticism we would recommend that you use dominance analysis instead of rlw when you are choosing between the two
# many say that dominance analysis takes too long
# however, when using correlation matrices and the calc.relimp function it can actually be quite fast
# let's take a look at the different execution times

system.time(lm_dominance <- calc.relimp(importance_regression, type = "lmg"))
system.time(fulldata_dominance <- calc.relimp(importance_data, type = "lmg"))
system.time(corr_dominance <- calc.relimp(importance_correlations, type = "lmg"))
system.time(rlw_output <- rlw(importance_data, colnames(importance_data)[1], colnames(importance_data)[2:ncol(importance_data)]))


###################################################################################
###################################################################################
###################################################################################
###################################################################################

# devtools::install_github("allengoebl/iopsych")

library(iopsych)

# Simulate Correlation Matrix ---------------------------------------------------------

# Load Data
data(dls2007)
d_vec <- dls2007[ , 1]; d_vec
cor_mat <- dls2007[, 2:7]; cor_mat


# Simulate range restricted data
dat <- mvrnorm(n=500, mu=c(0, 0, 0, 0, 0, 0), Sigma=cor_mat)
selected <- (rowMeans(dat[ , 1:4]) > 0)
hired_applicants <- dat[selected, ]

incumbent_mat <- round(cor(hired_applicants), digits=2)
applicant_mat <- cor_mat[1:4, 1:4]


# Correct Correlation matrix -----------------------------------------------------------


# Estimate unrestricted correlation matrix.
unrestrict_mat <- lMvrrc(incumbent_mat, applicant_mat)

# Tidy up matrix -- don't round in a real analysis
unrestrict_mat <- round(unrestrict_mat, digits=2) 
colnames(unrestrict_mat) <- colnames(cor_mat)
unrestrict_mat

# Correct Criterion Unreliability
rel_vec <- c(1, 1, 1, 1, .51, .51)
corrected_mat <- reliabate(unrestrict_mat, rel_vec)
diag(corrected_mat) <- rep(1,1,1,1,1,1)


# Combine Criterion --------------------------------------------------------------------


# Create Composite Criterion
wt <- matrix(c(0, 0, 0, 0, 1, 1), 1, 6); wt
full_mat <- fuseMat(corrected_mat, wt=wt)

#Drop old Criterion
new_mat <- full_mat[c(1:4, 7), c(1:4, 7)]
colnames(new_mat) <- c("P1", "P2", "P3", "P4", "C1")
new_mat


# Regression ----------------------------------------------------------------------------


m1 <- rmatReg(new_mat, y_col = 5, x_col = 1:4, N = 100); m1
  

# Subgroup Differences -----------------------------------------------------------------


comp_d <- dComposite(rxx=applicant_mat, d_vec=-d_vec[1:4], wt_vec=t(m1$beta))
comp_d

comp_ai <- aiEst(d=comp_d, sr=.5, pct_minority=.3)
comp_ai$ai


# Pareto Optimality ---------------------------------------------------------------------


pareto <- paretoXY(r_mat=new_mat, x_col=1:4, y_col=5, d_vec=-d_vec[1:4], pred_lower=c(0,0,0,0))
names(pareto)


plot(pareto$mr_d, xlab="multiple correlation", ylab="subgroup difference")

plot(pareto$mr_d[order(pareto$mr_d[, 1]), ], xlab="multiple correlation", 
     ylab="subgroup difference", type = 'l', lwd = 4, col = 'blue')

# Transform d to adverse impact ---------------------------------------------------------


multiple_r <- pareto$mr_d[ , 1]
d_score <- pareto$mr_d[ , 2]

#Compute adverse impact
ai_scores <- rep(NA, length(d_score))
for (i in 1:length(d_score)) {
  ai_scores[i] <- aiEst(d_score[i], sr=.5, pct_minority=.3)$ai
}

plot(multiple_r, ai_scores, xlab = "multiple correlation", ylab="adverse impact")


# Transform multiple r to utility ----------------------------------------------------------


#Compute utility
dollars <- rep(NA, length(multiple_r))
for (i in 1:length(dollars)) {
  dollars[i] <- utilityB(n=50, sdy=40, rxy=multiple_r[i], sr=.5, pux=NULL, cost=10, period=5,
                         v=.5, tax=.05, i=.05)
}

plot(dollars, ai_scores, xlab="dollars (thousands)", ylab="adverse impact")

# line!
plot(sort(dollars), ai_scores[order(dollars)], xlab="dollars (thousands)", 
     ylab="adverse impact", lwd = 4, col = 'green', type = 'l')




