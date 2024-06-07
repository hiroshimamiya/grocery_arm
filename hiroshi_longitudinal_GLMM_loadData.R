### script to load data for longitudinal regression model 

# Also test fitt to coutn data instead of binary 
#https://drizopoulos.github.io/GLMMadaptive/articles/ZeroInflated_and_TwoPart_Models.html
#https://cran.r-project.org/web/packages/GLMMadaptive/readme/README.html

### AIC
#https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#can-i-use-aic-for-mixed-models-how-do-i-count-the-number-of-degrees-of-freedom-for-a-random-effect



rm(list=ls())

library(lme4)
library(tidyverse)

mDataSample <- readRDS("data_hiroshi/mDataSample.rds")
mDataSmallSample <- readRDS("data_hiroshi/mDataSmallSample.rds")

# getting CI 
#https://stackoverflow.com/questions/26417005/odds-ratio-and-confidence-intervals-from-glmer-output

funcCoef <- function(a) tidy(a,conf.int=TRUE,exponentiate=TRUE,effects="fixed") %>% select(term, estimate)



### Lmerfurmula ----------
formulaBase     = 
  as.formula(cat_chips ~ cat_soda + educ + income + pfam + popn + pimm + cat_other + holidayInd + lag.cat_other  + 
               (1 | customer))


formulaBaseIntX = 
  as.formula(cat_chips ~ income*cat_soda + educ*cat_soda + 
               cat_soda + educ + income + pfam + popn + pimm + cat_other + holidayInd + lag.cat_other +  
               (1 | customer))


customerID_GLMM_smallSample <- unique(mDataSmallSample$customer)
saveRDS(customerID_GLMM_smallSample, "data_hiroshi/customerID_GLMM_smallSample.rds")

length(customerID_GLMM_smallSample)
nrow(mDataSmallSample)

