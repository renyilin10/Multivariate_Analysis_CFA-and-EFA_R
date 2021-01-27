#### Sample Confirmatory Factor Analysis and Exploratory Factor Analysis code to evaluate the validity of survey measurements ####

###By Yilin Ren


############################################################
#### Sample code for running CFA on the THI scale #####
############################################################
library("lavaan")

THI.data <- read.csv("fulldata.csv")  
#### note: "fulldata.csv" is the complete dataset that have scores for each individual item for all your measurements
#### in this example, fulldata contains 2 scales, one is THI (with 3 subscales containing 25 items), the other is TFI (with 8 subscales containing 25 items).

THI.cfa <- 'Functional =~ F1 + F2 + F4 + F7 + F9 + F12 + F13 + F14 + F15 + F18 + F20 + F24
            Emotional  =~ E3 + E6 + E10 + E16 + E17 + E21 + E22 + E25
            Catastrophic  =~ C5 + C8 + C11 + C19 + C23'

#### note: the sign =~ means the former is measured by the latter ####
#### F1, F2, F4,.....F24 are the questions measures functional influence in THI scale,
#### and we need to use them as the exact column names in the "fulldata.csv" file.


#run the cfa model using cfa()
THI.result1 <- cfa(THI.cfa, data=THI.data)

#summarize the results
summary(THI.result1, standardized=TRUE, fit.measures=TRUE)

#### note: from the summary report, we can check the fit indices score such as RMSEA, CFI, TLI, SRMR. ####
#### RMSEA < .06, CFI > .95, or TLI > .95 suggests acceptable fit (applies to ML with continuous data) 
#### Also, we can use the standard SRMR < .08, as indicator for acceptable fit (Hu & Bentler, 1999). 
#### If RMSEA, CFI ,TLI tell the same story, we don’t have to look at SRMR (we don’t trust SRMR if three others tell the same story). 
#### But if three tell a different story, we can look at SRMR.

#### Look at the P-value(Chi-square) of the results, if The chi-square test has a p value less than 0.05, 
#### which rejects the null hypothesis that this is a good model, indicating that we should modify the model. 
#### We want a p-value (chi-square) above 0.05, which suggests good model fit. 
#### Note that chi-square test will be very sensitive when there is a large sample size (>1000), 
#### and it will basically reject the null even when we have a good but not perfect model. 



## If the fit indices doesn't suggest a good model fit, we then request modification indices
modindices(THI.result1)

###from the modification indice output, the first three columns tells what parameters can be freely estimated
### the fourth column "mi" tells the value of modification indices - This column contains the most impotant information.
### THe last three columns tell the change of the parameter value if you freely estimate it
### sepc.lv is the change of parameter if we standardize the latent variable
### sepc.all is the change of standardized parameter we should look at!
### spec.nox is the change of parameter if we standardized all exogenous observed variables.
### exogenous means the variables are predictors, and are not predicted by anything else.

###We start from the largest MI values, and add the relation it suggest to the model.

#sometimes, modification indices will suggest cross-loading
#Let's add a cross-loading as an example
#suppose we see "Emotional =~ 5C" in the modindices(THI.result1) report has hight MI values, which indicates Emotional should also be measured by C5
#we then build a new model reflecting that:

THI.cfa2 <- 'Functional =~ F1 + F2 + F4 + F7 + F9 + F12 + F13 + F14 + F15 + F18 + F20 + F24
            Emotional  =~ E3 + E6 + E10 + E16 + E17 + E21 + E22 + E25
            Catastrophic  =~ C5 + C8 + C11 + C19 + C23
            Emotional  =~ C5'

THI.result2 <- cfa(THI.cfa2, data=THI.data)
summary(THI.result2, standardized=TRUE, fit.measures=TRUE)
modindices(THI.result2)
#### Same as above, need to check the fit indices first to see whether it's a good fit, 
#### if not, look at the new modification indices to see if there are any relation has high MI value that can be added to the model

anova(THI.result1, THI.result2)
####the chi-square difference test between the first model "THI.cfa" and the second modified model "THI.cfa2"


############################################################
#### Sample code for running EFA on the TFI scale #####
############################################################
library("psych")
library("GPArotation") #this package enables rotation methods
library("lavaan")
library("paran")

TFI_data <- read.csv("fulldata.csv")

# First check the model indices if we set the factors as 8 (since the TFI has 8 subscales)
efa_result <- fa(TFI_data, nfactors = 8, fm="ml", rotate = "oblimin")
efa_result
### use fa() function to run efa
# nfactors is the number of factors in the model
# fm is the estimation method. "ml" means maximum likelihood
# rotate is the rotation method. oblimin is the default rotation method because usually the subscales (latent variables) are correlated with each other

efa_result$loadings #loadings, Ford et al. (1986) suggests loadings > .40 should be considered “significant”. 
efa_result$Phi #factor correlation
efa_result$STATISTIC #chi-square
efa_result$dof #degrees of freedom
efa_result$PVAL #chi-square p value

efa_result$RMSEA
efa_result$TLI
###the following line of code calculates CFI 
1 - (1 - efa_result$TLI)* efa_result$dof / efa_result$null.dof  #CFI


## If the model indices doesn't look good, we can use parallel analysis to decide the optimal number of factors and re-run cfa
paran(TFI_data, cfa=TRUE, graph=TRUE, color=TRUE, col=c("black", "red", "blue")) ## run parallel analysis to see how many factors we need
## From the parallel analysis figure, any dot from the red line (unadjusted eigenvalue) that above the blue line (random eigenvalue) indicates that we should keep it as a factor. 
## If a dot is a little above or below the random Ev, that give us some uncertainty for that factor.


## If the parallel analysis suggest there are actually 4 factors, then we use the code:
efa_result2 <- fa(TFI_data, nfactors = 4, fm="ml", rotate = "oblimin") ## Just change the nfactors=4
efa_result2

##Then we need to check whether the model indices for efa_result2 is better than the original model that has 8 factors.





