#These lines access needed software packages in R
library('languageR')
library('Hmisc')
library('Design')

#This line tells R to fetch the dataset
loaddata = read.csv('Ch4data.csv')

#This code prints a summary of the load data that is in the .csv file
summary.load.data=summary(loaddata)
print(summary.load.data)

#This code performs the linear regression analysis and prints out the results
load.lrm=lrm(CONSTRUCTION ~ VERB + REDUCED + PARTICIPLE + VERB:PARTICIPLE, data=loaddata, x=T, y=T, linear.predictors=T)
print(load.lrm)

#This code performs an anova analysis of the linear regression results so that the variables can be compared
anova.table=anova(load.lrm, test="Chisq")
print(anova.table)

#This is an alternative way to perform a logistic regression analysis
load.glm=glm(CONSTRUCTION ~ VERB + REDUCED + PARTICIPLE + VERB:PARTICIPLE, family=binomial, data=loaddata)
print(summary(load.glm))

#This gives the confidence intervals for the glm model
print("These are the confidence interval values:")
print(exp(confint(load.glm)))

#This gives the odds of success for each predictor variable
print("These are the odds of success for each predictor variable:")
print(exp(load.glm$coefficients))