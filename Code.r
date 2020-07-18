library(tidyverse)
library(tidyr)
library(naniar)
library(mice)
library(data.table)
library(MASS)
library(dplyr) 
library(mice)
library(plyr)
library(data.table)
library(mitools)
library(MLmetrics)
library(mice)
library(miceadds)
library(mitml)
library(robustbase)
library(MKmisc)
source("studentFunctions.R")
source("miPredictionRoutines.R")

# Set random number seeds
set.seed(173649)
plotDir <- "../plot/"
dataDir  <- "../data/"
fileName <- "wvs_data.rds"

df <- readRDS(paste0(dataDir, fileName))



variables <- c("V2","V7","V45","V47","V48","V51","V57","V59",
               "V84","V87","V96","V98", "V117","V130","V137","V139","V140","V203",
               "V204","V208","V227","V229","V235","V240","V242")


new_variables <- c("V2","V7","V47","V48","V50","V51","V84","V87","V96","V117",
                   "V130","V133","V137","V140","V213","V227")
  

  
database <- df[ ,new_variables]

#Get the unique values for each country
country_code = unique(database$V2)

#New column with countries as categorical values
country_name = c("China","Germany","Russia","United States","India")
data1 = cbind(country_code,country_name)
countries = data.frame(unlist(data1))
countries = factor(database$V2,labels = country_name)




      # Replacing missing values with NA
database_without = database[,c(2:16)]
database_without[database_without <= 0] <- NA
#final_database <- cbind(database_without,countries)

### Univariate analysis 


#Find the outliers in the columns
madOut <- lapply(database_without[ , ], cut = 3, madOutliers)
madOut

#The outliers will be turned into NAs and there will be treated the same. 





V96 <-madOutliers(database_without$V96)
database_without$V96[V96] <- NA
V133 <-madOutliers(database_without$V133)
database_without$V133[V133] <- NA
V137 <-madOutliers(database_without$V137)
database_without$V137[V137] <- NA
V140 <-madOutliers((database_without$v140))
database_without$V140[V140] <- NA



#Cheking fo the total proportion of NA's after the conversion 
nas <- colMeans(is.na(database_without))
nas
#Categorical data to factor
#cat_data <- c("V57", "V80", "V229","V240")
#database[,cat_data] <- lapply(database[cat_data], factor)
final_database <- cbind(database_without,countries)

#Covariance coverage rate for the dataset
covariance_matrix <- md.pairs(final_database)$rr / nrow(final_database)

#Compute the missing data and their visual pattern 
pattern <- md.pattern(final_database,plot = FALSE)

#Inspect if the covariance coverage is less that 0.75
sum(covariance_matrix[lower.tri(covariance_matrix)] < 0.75)


#How many distinct missing data patterns exist in these data?

nrow(pattern) - 1

# How many missing data patterns have only one missing value?
one_missing <- pattern[ , ncol(pattern)] == 1
sum(one_missing)

#How many observations are affected by patterns that involve only one
#missing value?

sum(as.numeric(rownames(pattern))[one_missing])




      ##Multiple imputation for the treatement of missing data 
head(database)

meth <- rep("pmm", ncol(final_database))
names(meth) <- colnames(final_database)
meth["countries"] <- "polr"
?mice
#Use mice::quickpred to generate a predictor matrix
predMat <- quickpred(final_database, mincor = 0.2,include = "countries")
predMat

# Impute missing values using the method vector 
database3 <- mice(final_database, m = 20, maxit = 10, predictorMatrix = predMat,method = meth, seed = 173649)


# Create traceplots of imputed variables' means and SDs
plot(database3)

# Sanity check the imputations by plotting observed vs. imputed densities
densityplot(database3)


## Remove the factor variables from each imputed dataset (because we don't want
## categorical variables in our data when checking for multivariate outliers):
miceOut2 <-
  subset_datlist(datlist = database3,
                 select  = setdiff(colnames(final_database), c("countries")),
                 toclass = "mids")

## Create list of multiple imputed datasets:
imputed_list <- complete(miceOut2, "all")

## Check for multivariate outliers in each imputed datset:
olList <- lapply(imputed_list, mdOutliers, critProb = 0.99)

## Count the number of times each observation is flagged as an outlier:
olCounts <- table(unlist(olList))

## Define the threshold for voting (will be 10 in this case):
thresh <- ceiling(database3$m / 10)

## Define a vector of row indices for outliers:
outs <- as.numeric(names(olCounts[olCounts >= thresh]))

## Exclude outlying observations from mids object:
miceOut3 <- subset_datlist(datlist = database3, # We're using the original imputations
                           subset  = setdiff(1 : nrow(final_database), outs),
                           toclass = "mids")


 ##Inferencial analysis



#------------------Continuous variable moderation---------------------


## Focal effect 
#After controlling for country, does the people perception that  a woman
# earning more money than her husband being a bad thing significantly predict 
# their belief about income equality?

women1 <- with(miceOut3, lm(V96 ~ V47))
pooled1 <- pool(women1)
summary(pooled1)
pool.r.squared(women1)
## Addiditive model

women2 <- with(miceOut3,  lm(V96 ~ V47 + countries))
pooled2 <- pool(women2)
pool.r.squared(women2)
summary(pooled2)
# After controlling for country, V47 seems to be a good predictor 
#term  estimate  std.error statistic        df     p.value
# V47  -0.01638503 0.03046239  -0.5378774 1018.2833 5.907792e-01
# countriesGermany -0.66624588 0.08033552  -8.2932915 1665.0020 2.220446e-16

## Moderation

#After controlling for country, does people's perception about women earning money 
# and degrading their husbands' morale and income equality vary as a function of people's perception 
#that men make better political leaders than women?
women3 <- with(miceOut3, lm(V96 ~ V47 * V51 + countries))
pooled3 <- pool(women3)
summary(pooled3)
pool.r.squared(women3)
#After controlling for country, the moderation V47:V51 is good
#term     estimate  std.error    statistic        df      p.value
#V47:V51 -0.002281079 0.03108991  -0.07337041 2870.9784 9.415165e-01

#The model shows a significant result at a = 0.05
anova1 <-anova(women2,women3)
anova1
# test statistic df1      df2 df.com     p.value        riv
#2 ~~ 1  6.380024   2 4574.383  12222 0.001710205 0.06937021


#------------------Categorical variable moderation---------------------#


#After controlling for the opinion that men make better political leaders than women,
# does the people perception that  " a woman
# earning more money than her husband being a bad thing" and  
# their belief about income equality vary per country ?


cat_women1 <- with(miceOut3,lm(V96 ~ V47 + countries + V51))
cat_pool1 <- pool(cat_women1)
summary(cat_pool1)
pool.r.squared(cat_women1)
# term     estimate  std.error   statistic        df      p.value
#V47  0.008216374 0.03118000   0.2635142 1097.2973 7.922037e-01
#countriesGermany -0.594267196 0.08246137  -7.2066132 1963.4967 8.144596e-13




cat_women2 <-  with(miceOut3,lm(V96 ~ V47 * countries + V51))
cat_pool2 <- pool(cat_women2)
summary(cat_pool2)
pool.r.squared(cat_women2)
#term    estimate  std.error  statistic        df      p.value
#V47:countriesGermany -0.01374589 0.10214725 -0.1345694 2021.9797 8.929657e-01
# V47:countriesRussia -0.02638696 0.08669664 -0.3043597  801.4139 7.609329e-01
# V47:countriesUnited States  0.08319955 0.09978181  0.8338148 1616.0561 4.045086e-01



#The ANOVA model shows a non significant result with p-value over 0.5
anova_cat <- anova(cat_women2,cat_women1)
anova_cat
#test statistic df1      df2 df.com   p.value        riv
#1 ~~ 2 0.7252286   4 5152.193  12219 0.5745848 0.09622321

## Predictive modeling 
 
#We split the multiple impted data sets into training(70%), validation(15%) and testing set(15%)

n <- nrow(imputed_list[[1]])


index1<- sample(c(rep("train", 9209), rep("valid", 1973), rep("test", n-11182)))


imputed_list2 <- splitImps(imps = imputed_list, index = index1)
imputed_list2


#We create our models
models <- c("V140~ V117+ V84 + V227", 
            "V140~ V87 + V130+ V133",
            "V140~ V137 + V213 + V7")

# train model and compute validation-set's MSE
mse <- c()
for(m in models) {
  fits     <- lapply(X   = imputed_list2$train,
                     FUN = function(x, mod) lm(mod, data = x),
                     mod = m)
  mse[m] <- mseMi(fits = fits, newData = imputed_list2$valid)
}

mse[1]
mse[2]
mse[3]

#So our second model seems to have the lower relative prediction error based on validation set

#We merge the Multiple Impouted training and validation sets
index2   <- gsub(pattern = "valid", replacement = "train", x = index1)
imputed_list3 <- splitImps(imputed_list, index2)
imputed_list3

#We implement a 10 fold Validation
tmp <- sapply(imputed_list3$train, cv.lm, K = 10, models = models, seed = 235776)

#We Aggregate the MI-based CVEs:
cve <- rowMeans(tmp)

names(which.min(cve))

#So the second model should be prefered based on its relative low cross validation error

#We Refit the winning model and compute test-set MSEs:
fits <- lapply(X   = imputed_list3$train,
               FUN = function(x, mod) lm(mod, data = x),
               mod = models[which.min(cve)])

mse3 <- mseMi(fits = fits, newData = imputed_list3$test)

#So the estimated error of the best model is:
mse3

#In order to make inference about our best model we pool all the regressions into one 
#and we ceck for its coefficients

pool1 <- pool(fits)
summary(pool1)
mira_class <- as.mira(fits)
pool.r.squared(mira_class)


##Checking the summary of pool1 we see that all the variables are stattistically
#important for both a = 0.05 and a = 0.01.
#More Specifically, for each additional point of V87, our dependet variable V140
#is expected to reduced by 0.14. Similarly for each additional point of V130, 
# our dependent variable is going to decreased by 0.72. On the other hand, 
# for each additional poiont of V133 our depndent variable is expected to 
#increase by 0.43.
