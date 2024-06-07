
#*****************************************************************************
#
### ARM Substitutions----------------------------------------------------
#
#*****************************************************************************
source("loadFunc.R")

transactions  <- readRDS("data_hiroshi/transactions_card_flowchart.rds")
#nameVar <- read.csv("data_hiroshi/ProductCategoryReportList_Manuscript.csv")

apriori.ctl <- list(sort=-1, verbose = FALSE)
rules <- apriori(transactions, 
                 parameter = list(supp=0.00000000001, conf=0.0000001, minlen=2, maxlen=2),
                 control = apriori.ctl)


### Soda ------------------------------------------------------
targetCat= "Soda"
targetRule <- sort(subset(rules, subset = lhs %in% targetCat & lift < 1), by="lift", decreasing = FALSE)
ruleCI_formatted <- funcCIFormat(targetRule)
TSoda <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat)
TSoda$Cat <- "Soda"

#ft <- funcTableWord(T)
#save_as_docx(path = paste("manuscript_hiroshi/ARM_substitue_", targetCat, ".docx", sep = ""), pr_section = sect_properties)

targetCat= "Fresh Vegetables"
targetRule <- sort(subset(rules, subset = lhs %in% targetCat & lift < 1), by="lift", decreasing = FALSE)
ruleCI_formatted <- funcCIFormat(targetRule)
TVeg <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat)
TVeg$Cat <- "Fresh Vegetables"

targetCat= "Fresh Fruits"
targetRule <- sort(subset(rules, subset = lhs %in% targetCat & lift < 1), by="lift", decreasing = FALSE)
ruleCI_formatted <- funcCIFormat(targetRule)
TFru <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat)
TFru$Cat <- "Fruits"

ruleFormat<- rbind(TSoda, TVeg, TFru) %>% relocate(Cat) %>% dplyr::rename(TargetCategory = Cat)
ruleFormat <- funcTableWord(ruleFormat)
save_as_docx("ARM_SubstituionRUle" = ruleFormat, path = "manuscript_hiroshi/ARM_substitutionRule.docx")


### non-members ----------------------------------------------------------------------
transactions_nc <- readRDS("data_hiroshi/transaction_nonMember.rds")

rules <- apriori(transactions_nc, 
                 parameter = list(supp=0.001, conf=0.00001, minlen=2, maxlen=2),
                 control = apriori.ctl)

targetCat= "Soda"
targetRule <- sort(subset(rules, subset = lhs %in% targetCat & lift < 1), by="lift", decreasing = FALSE)
ruleCI_formatted <- funcCIFormat(targetRule)
TSoda <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat)
TSoda$Cat <- "Soda"

targetCat= "Fresh Vegetables"
targetRule <- sort(subset(rules, subset = lhs %in% targetCat & lift < 1), by="lift", decreasing = FALSE)
ruleCI_formatted <- funcCIFormat(targetRule)
TVeg <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat)
TVeg$Cat <- "Fresh Vegetables"

targetCat= "Fresh Fruits"
targetRule <- sort(subset(rules, subset = lhs %in% targetCat & lift < 1), by="lift", decreasing = FALSE)
ruleCI_formatted <- funcCIFormat(targetRule)
TFru <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat)
TFru$Cat <- "Fruits"

ruleFormat<- rbind(TSoda, TVeg, TFru) %>% relocate(Cat) %>% dplyr::rename(TargetCategory = Cat)
ruleFormat <- funcTableWord(ruleFormat)

save_as_docx("ARM_SubstituionRUle_NC" = ruleFormat, path = "manuscript_hiroshi/ARM_substitutionRule_NC.docx")



###_Codes to chceck frequency of substition to check pair with lift<1 hae very low freq-----------------------------------------
apriori.ctl <- list(sort=-1, verbose = FALSE)
rules <- apriori(transactions, 
                 parameter = list(supp=0.000000001, conf=0.00001, minlen=2, maxlen=2),
                 control = apriori.ctl)

# manually confirm frequency of substiutions 
inspect(subset(rules, subset = rhs %in% "Soda" & size(rhs) == 1 & lift < 1))
inspect(subset(rules, subset = lhs %in% "Soda" & size(lhs) == 1 & lift < 1))

freq.items <- eclat(transactions, parameter = list(supp = 0.000001, maxlen = 2))
freqItem <- inspect(head(sort (freq.items, by="support", decreasing=TRUE), 102))


freqPair <- eclat(transactions, parameter = list(supp = 0.000001, maxlen = 2, minlen = 2))
inspect(head(sort (freqPair, by="support", decreasing=TRUE), 50))

#inspect(subset(freqPair, subset = items %in% "Soda" & items %in% "Vegan/Vegetarian Food"))
#inspect(subset(freqPair, subset = items %in% "Fresh Fruits" & items %in% "Sports Energy Drinks"))

inspect(subset(rules, subset = lhs %in% "Soda" & rhs %in% "Soy/Rice/Nut Beverages" ))
inspect(subset(rules, subset = rhs %in% "Soda" & lhs %in% "Soy/Rice/Nut Beverages" ))
inspect(subset(freqPair, subset = items %in% "Soda" & items %in% "Soy/Rice/Nut Beverages"))



