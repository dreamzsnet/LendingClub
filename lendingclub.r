## The project is based on the Lending Club's Loan Data for the Year of 2015. 
## The project intends to provide an insight on Loan's Performance based on the 
## various variables that were captured by the Lending Club like "Loan Amount","Borrower's Grade", "Employment Grade" etc. 

# library packages
library(aod) # Analysis
library(dplyr) # data manipulation 
library(ggplot2) # visualization

# Clean the Environment
rm(list = ls())

# Check whether there exist the file already; if not download the data from Lending Club's Website
if(!file.exists("LoanStats3d.csv.")) {
  download.file("https://resources.lendingclub.com/LoanStats3d.csv.zip", destfile = "LoanStats3d.csv.zip", mode = "wb")
  unzip("LoanStats3d.csv.zip")
}

# Read the downloanded data
if(!exists("def")) {
  def <- read.csv(file = "LoanStats3d.csv", header = TRUE, skip = 1, stringsAsFactors = FALSE)
}


## Slicing and Dicing data
# Select only relevant variables
var <- c("id", "loan_amnt", "int_rate", "grade", "emp_length", "home_ownership", 
         "annual_inc", "loan_status", "purpose", "addr_state", "dti", "delinq_2yrs", 
         "inq_last_6mths", "pub_rec", "revol_util", "total_acc" 
)


# Create a new data frame
loan <- def[var]

# Convert interest rate numbers to numeric (strip percent signs)
loan$int_rate <- as.numeric(sub("%", "", loan$int_rate))
loan$revol_util <- as.numeric(sub("%", "", loan$revol_util))

# Since Current status doesn't reflect whether the loan gets defaulted or not;
# Drop data relating to Current in Loan Status 
loan <- loan %>% filter(loan$loan_status != "Current") %>% droplevels()

# Drop NONE & OTHER variables from Home Ownership as they have very litte data
loan <- loan %>% filter(home_ownership!= "ANY" & home_ownership!= "") %>% droplevels()

# Drop Wedding & Renewal Energy from Loan Purpose as there are very few observations
loan <- loan %>% filter(purpose!= "wedding" & purpose!= "renewable_energy") %>% droplevels()

# Drop outlier Debt to Income Ratios
loan <- loan %>% filter(dti <= 50) %>% droplevels()

# Drop insignificant Revolving Utilization ratios
loan <- loan %>% filter(revol_util <= 125)

# Drop insignificant Total Accounts variables
loan <- loan %>% filter(total_acc <= 75) %>% droplevels()

# Drop outlier Annual Income
loan <- loan %>% filter(annual_inc <= 450000) %>% droplevels()


# Convert Last six month inquiry variable into factors
loan$inq_last_6mths <- factor(loan$inq_last_6mths)
levels(loan$inq_last_6mths) <- c("0", "1", "2", "3", rep("4", 5)) # reduce the variable observations

# Convert employment length to numeric
# First remove year/s from the charecters
loan$emp_length <- as.character(gsub("year|s$", "", loan$emp_length))

# Change employment length to 0 for < 1 & n/a
loan$emp_length <- as.character(gsub("< 1", "0", loan$emp_length))
loan$emp_length <- as.character(gsub("n/a", "0", loan$emp_length))

# Remove white space
loan$emp_length <- as.character(gsub(" ", "", loan$emp_length))

# Remove + and change 10+ to 10
loan$emp_length <- as.character(gsub("\\+$", "", loan$emp_length))

# Now change the format into numerci
loan$emp_length <- as.numeric(loan$emp_length)

# Change delinquencies data into factor
loan$delinq_2yrs <- factor(loan$delinq_2yrs)

# Delinquencies in the past 2 Years (combine factors levels for any > 3)
levels(loan$delinq_2yrs) <- c("0", "1", "2", "3", rep("4+", 20))
loan$delinq_2yrs <- as.factor(gsub("\\+$", "", loan$delinq_2yrs)) # drop + sign and while space

# Change Public Records into factor
loan$pub_rec <- as.factor(loan$pub_rec)

# Number of Public Records (break factor levels into 0, 1, 2+)
levels(loan$pub_rec) <- c("0", "1", "2", "3", rep("4+", 25))

# Drop + sign and white space from the variables
loan$pub_rec <- as.factor(gsub("\\+$", "", loan$pub_rec))


# Check for missing values
sum(is.na(loan))

# Cleaning the missing data
loan <- loan[complete.cases(loan),]


# Reduce loan status to binary "Performing" and "NonPerforming" Measures:
loan$new_status <- factor(ifelse(loan$loan_status %in% c("Current", "Fully Paid"), 
                                 "Performing", "NonPerforming"))


## Exploratory data analysis
# Performance of Loans by Grade
ggplot(loan, aes(x = grade, fill = new_status)) + geom_bar(position = "fill")

## Loan's Grade is positively correlated with Loan's Performance. 
## Higher the Loan Grade higher the Default rates


# Performance of Loans by Home Ownership Type
ggplot(loan, aes(x = home_ownership, fill = new_status)) + geom_bar(position = "fill")

## Borrower with Mortgage or Owns their own home tends to pay back their loan than 
## the Borrower Renting home

# Performance of Loans by Debt to Income Ratio
ggplot(loan, aes(x = dti, fill = new_status)) + geom_histogram(position = "fill", binwidth = 5)

## Debt to Income Ratio positively correlates with Loan Default Rates. 
## Higher the ratio higher chances of defaulting the Loan.

# Performance of Loans by Revolving Utilization
ggplot(loan, aes(x = revol_util, fill = new_status)) + geom_histogram(position = "fill", binwidth = 15)

## As in the case of DTI, higher the Revolving Utilization rate higher the loan getting defaulted.

# Performance of Loans by Loan Purpose
ggplot(loan, aes(x = purpose, fill = new_status)) + geom_bar(position = "fill")

## Ratio of default seems minimal for the Borrower financing their cars. 
## Whereas default rate is maximum for the Borrower financing their small business.
## This may be the case due to poor business models that could generate enough Cash Flows.

# Performance of Loans by Inquiries in the Last Six Months
ggplot(loan, aes(x = inq_last_6mths, fill = new_status)) + geom_bar(position = "fill")

## The graph shows that higher the rate of contacts by the Borrower seeking loans, higher the default ratio.
## This may be due to the desparation of getting loan at anyway and not paying back.

# Performance of Loans by Number of Total Accounts
ggplot(loan, aes(x = total_acc, fill = new_status)) + geom_histogram(position = "fill", binwidth = 10)

## Total number of accounts is negatively correlated with the Loan's Performance. 
## Higher the number of total accounts, lower the default rates. 

# Performance of Loans by Annual Income
ggplot(loan, aes(x = annual_inc, fill = new_status)) + geom_histogram(position = "fill", binwidth = 20000)

## As usal, borrower with higher Annual Income tends to payback their loan as 
## opposite to the borrow with lower Annual Income.

# Performance of Loans by Loan Amount (break into < 15k, 15k - 30k, 30k - 35k)
ggplot(loan, aes(x = loan_amnt, fill = new_status)) + geom_histogram(position = "fill", binwidth = 7000)

## Higher the Loan Amount, higher the default rates.

# Performance of Loans by Employment Length
ggplot(loan, aes(x = emp_length, fill = new_status)) + geom_bar(position = "fill")

## Borrow with stable job or are employed longer tends to perform well in terms of Loan Repayment.

# Performance of Loans by Delinquencies in the past 2 Years
ggplot(loan, aes(x = delinq_2yrs, fill = new_status)) + geom_bar(position = "fill")

## Borrow who have no record of delinquencies over past two years, tend to pay back their loan.
## As such, delinquencies rate is positively correlated with the default rate as higher the delinquncy,
## higher the defaults.

# Performance of Loans by number of Public Bankruptcies Records
ggplot(loan, aes(x = pub_rec, fill = new_status)) + geom_bar(position = "fill")

## Higher the Public Bankruptcies rate of the borrower, higher the defaults


## Predicting the loan performance
# Recoding the new_status variable into binary
loan$perform[loan$new_status == "Performing"] <- 1
loan$perform[loan$new_status == "NonPerforming"] <- 0

# Changig the sub-grade into factor
loan$new_grade <- as.factor(loan$grade)
levels(loan$new_grade) <- c(1:7) # Assigning the numeric value to each Grade


# fitting the logistic regression 
model <- glm(perform ~ loan_amnt + new_grade + emp_length +
             annual_inc + dti + revol_util + total_acc, data = loan, 
             family = "binomial")

# model output
summary(model)

## Confidence Intervals using profiled log-likelihood
confint(model)

# Wald Test Analysis 
wald.test(b = coef(model), Sigma = vcov(model), Terms = 3:8)

## Chi-squared test statistic of 1813, with 6 degrees of freedom is associated with p-value
## 0.0 indicating that overall effect of Grade is statistically significant

l <- cbind(0,0,1,-1,0,0,0,0,0,0,0,0,0)
wald.test(b = coef(model), Sigma = vcov(model), L = l)

## Chi-squared test statistic of 216.3, with 1 defree of freedom is associated with p-value 
## 0.0 indicating that the difference between the coefficient for Grade=2 and the coefficient
## for Grade=3 is statistically significant

## odds ratios and 95% CI
exp(cbind(OR = coef(model), confint(model)))

## For one year increase in Employment Length, the odds of being pay off the loan increases by a 
## factor of 1.04. Whereas, for One Thousand increase in Annual Income, the odds of being pay off 
## the loan increases by a factor of 1. Similarly, for each account increase, the odds of loan pay
## off increases by a factor of 1.1.

# Predicted Probability of Loan Payment by each value of Grade holding all other variables at 
# their means
new_loan <- with(loan,
                 data.frame(loan_amnt = mean(loan_amnt), new_grade = factor(1:7),
                            emp_length = mean(emp_length), annual_inc = mean(annual_inc), 
                            dti = mean(dti), revol_util = mean(revol_util), total_acc = mean(total_acc)))


new_loan$prob <- predict(model, newdata = new_loan, type = "response")
new_loan

# Simulating test data 100 times
new_loan1 <- with(loan,
                  data.frame(loan_amnt = rep(seq(from = 1000, to = 35000, length.out = 100), 7), 
                             new_grade = factor(rep(1:7, each = 100)), emp_length = rep(seq(from = 0, to = 10, length.out = 100), 7), 
                             annual_inc = rep(seq(from = 4500, to = 500000, length.out = 100), 7), 
                             dti = rep(seq(from = 0, to = 70, length.out = 100), 7), revol_util = rep(seq(from = 0, to = 140, length.out = 100), 7), 
                             total_acc = rep(seq(from = 0, to = 150, length.out = 100), 7)))

new_loan2 <- cbind(new_loan1, predict(model, newdata = new_loan1, type="link", se=TRUE))

new_loan2 <- within(new_loan2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first & last few rows of final dataset
head(new_loan2)
tail(new_loan2)

# Visualization of Predicted Probabilities against Grade and the Loan Amount
ggplot(new_loan2, aes(x = loan_amnt, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = new_grade), alpha = .2) +
  geom_line(aes(colour = new_grade), size=1)

## As shown by the exploratory data analysis, the Borrower's Grade and Loan Amount has positive 
## correlation with the performance of the Loan, the probability of Loan Payoff decreases substantially
## as the Grade and the Loan Amount increases.

# Finding the difference in deviances between the residual deviance for the model with predictors 
# and the null model
with(model, null.deviance - deviance)

# Degrees of freedom for the difference between the two models
with(model, df.null - df.residual)

# Findng the p-value
with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

## Chi-square of 4974.14 with 12 degrees of freedom and an associated p-value of less than 0.001 tells us 
## that our model as a whole fits significantly better than an empty model

# Model's log liklihood
logLik(model)

## Conclusion: As shown by the exploratory and the predictive analysis, borrow with longer
## history of employment, higher annual income, lower debt to income ration and has their own
## home or mortgage tend to have good credit grade/ratings and tend to payoff their loans
## within the loan tenure. As opposed, borrow with records of delinquencies over past two years
## have inquired frequently to get loan (desparation), have public records of bankruptcies 
## and exhausted their revolving credits tend to get lower credit rating/grade and tends to 
## default. Hence, the lenders should careful enough to assess these factors before lending 
## their nest eggs to the prospect borrowers.

 $ heroku buildpacks:set heroku/php
