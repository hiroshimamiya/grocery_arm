
################################################################################
### load libraries -----------------------------------------

gc()
rm(list=ls())
source("loadFunc.R")

# No need to run lengthy data prep codes 
#source("ARM_nonMember_dataPrep.R")

### Load data 
transactions_nc <- readRDS("data_hiroshi/transaction_nonMember.rds")
transactions_c <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/transactions_card.rds")


numItems = 10



#*************************************************************************
#  ARM --------------------
# #
#*************************************************************************
#nameVar <- read.csv("data_hiroshi/ProductCategoryReportList_Manuscript.csv")

apriori.ctl <- list(sort=-1, verbose = FALSE)
apriori.prm <- list(supp=0.005, conf=0.01, maxlen = 2, ext=T) # picked 3 because of baseline transaction 

rules <- apriori (transactions_nc, parameter = apriori.prm) # Min Support as 0.001, confidence as 0.8.

### Soda ------------------------------------------------------
targetCat= "Soda"
targetRule<-head(sort(subset(rules, subset = lhs %in% targetCat), by="lift"), numItems)

ruleCI_formatted <- funcCIFormat(targetRule)
T <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat)

ft <- funcTableWord(T)
save_as_docx(ft, path = paste("manuscript_hiroshi/ARM_nonmember_", targetCat, ".docx", sep = ""), pr_section = sect_properties)
tSoda <- T
tSoda$Cat <- "Soda"

library(magrittr)
# Plot 
dfnoM <- funcGetTableARM(ruleCI_formatted, R = targetRule, target = targetCat) %>% 
  funcReshapeARMtable(num=numItems)
# member DF 
dfM <- readRDS(paste("data_hiroshi/df_ARM_member_colorPLot_", targetCat, ".rds", sep = ""))
df <- funcCombineCohortRR(dfnoM, dfM)  
funcPlotForestARM_RRonly(df, maxX = 3.1) + 
  theme(legend.position = c(0.7, 0.2)) 
ggsave(paste("manuscript_hiroshi/ARM_NONmember_colorPLot_", targetCat, ".pdf", sep = "")
       , width = 8, height = 3, units = "in")


### Veggies ------------------------------------------------------
#nameVar <- read.csv("data_hiroshi/ProductCategoryReportList_Manuscript.csv")
apriori.ctl <- list(sort=-1, verbose = FALSE)
apriori.prm <- list(supp=0.01, conf=0.01, maxlen = 2, ext=T) # picked 3 because of baseline transaction 

rules <- apriori (transactions_nc, parameter = apriori.prm) # Min Support as 0.001, confidence as 0.8.



targetCat= "Fresh Vegetables"
targetRule<-head(sort(subset(rules, subset = lhs %in% targetCat ), by="lift"), numItems)

ruleCI_formatted <- funcCIFormat(targetRule)
T <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat)

ft <- funcTableWord(T)
save_as_docx(ft, path = paste("manuscript_hiroshi/ARM_nonmember_", targetCat, ".docx", sep = ""), pr_section = sect_properties)
tVeg <- T
tVeg$Cat <- "Vegetables"

# Plot 
# Plot 
dfnoM <- funcGetTableARM(ruleCI_formatted, R = targetRule, target = targetCat) %>% 
  funcReshapeARMtable(num=numItems)
# member DF 
dfM <- readRDS(paste("data_hiroshi/df_ARM_member_colorPLot_", targetCat, ".rds", sep = ""))
df <- funcCombineCohortRR(dfnoM, dfM) 

funcPlotForestARM_RRonly(df, minX = 2,maxX = 7) + 
  theme(legend.position = c(0.7, 0.2)) 
ggsave(paste("manuscript_hiroshi/ARM_NONmember_colorPLot_", targetCat, ".pdf", sep = "")
       , width = 8, height = 3, units = "in")




### Fruits ------------------------------------------------------
targetCat= "Fresh Fruits"
targetRule<-head(sort(subset(rules, subset = lhs %in% targetCat & size(rhs) == 1), by="lift"), 10)

ruleCI_formatted <- funcCIFormat(targetRule)
T <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat)

ft <- funcTableWord(T)
save_as_docx(ft, path = paste("manuscript_hiroshi/ARM_nonmember_", targetCat, ".docx", sep = ""), pr_section = sect_properties)
tFru <- T
tFru$Cat <- "Fruits"


# Plot 
dfnoM <- funcGetTableARM(ruleCI_formatted, R = targetRule, target = targetCat) %>% 
  funcReshapeARMtable(num=numItems)
# member DF 
dfM <- readRDS(paste("data_hiroshi/df_ARM_member_colorPLot_", targetCat, ".rds", sep = ""))
df <- funcCombineCohortRR(dfnoM, dfM) 

funcPlotForestARM_RRonly(df, maxX = 5) + 
  theme(legend.position = c(0.7, 0.2)) 
ggsave(paste("manuscript_hiroshi/ARM_NONmember_colorPLot_", targetCat, ".pdf", sep = "")
       , width = 8, height = 3, units = "in")






# Save all tables 
ruleFormat<- rbind(tSoda, tVeg, tFru) %>% relocate(Cat) %>% dplyr::rename(TargetCategory = Cat)
ruleFormat <- funcTableWord(ruleFormat)
save_as_docx( ruleFormat, path = "manuscript_hiroshi/ARM_nonmember_allCategories.docx", pr_section = sect_properties)

