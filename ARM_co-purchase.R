
#*****************************************************************************
#
#
### ARM ----------------------------------------------------
## co-purchasing pattern analysis using market basket data 
#
#*****************************************************************************
rm(list=ls())
source("loadFunc.R")



savePlots <- FALSE

## type of cardholders, frequent or default analysis 
loadCohort <- ""
if(loadCohort == "490"){# those who spent 490 $ per month
  transactions <- readRDS("data_hiroshi/transactions_card_dollar490Moi.rds")
  saveFileName <- "__frequent490__"
}else if(loadCohort == "4moi"){ # whose who visited 4 times a month 
  transactions <- readRDS("data_hiroshi/transactions_card_fre4moi.rds")
  saveFileName <- "__frequent4times__"
}else{
  transactions  <- readRDS("data_hiroshi/transactions_card_flowchart.rds")
  saveFileName = ""
}


## List offood categories 
nameVar <- read.csv("data_hiroshi/ProductCategoryReportList_Manuscript.csv")

# number of items to display 
numItem = 25

# control parameters for ARM apriori algorithm 
apriori.ctl <- list(sort=-1, verbose = FALSE)
rules <- apriori(transactions, 
                 parameter = list(supp=0.01, conf=0.05, minlen=2, maxlen=2, ext=T),
                 control = apriori.ctl)

# Below is more non-descriminatory search, not the JMIR manuscript but for presentation of armVIZ
#rules <- apriori(transactions, 
#                 parameter = list(supp=0.0001, conf=0.0005, minlen=2, maxlen=2, ext=T),
#                 control = apriori.ctl)






### Soda - old codes, do not run  
inspect(sort(subset(rules, subset = rhs %in% "Soda" & size(rhs) == 1), by = "lift"))
inspect(sort(subset(rules, subset = lhs %in% "Soda" & size(lhs) == 1), by = "lift"))
inspect(sort(subset(rules, subset = rhs %in% "Soda" ), by = "lift", decreasing = TRUE))
#
#sodaRule<-sort(subset(rules, subset = rhs %in% "Soda" & size(rhs) == 1), by="lift")
#
#sodaRuleCI_formatted <- sodaRule %>% 
#  funcRuleCI() %>% 
#  funcFormatRule() %>% 
#  mutate(OR = paste(OR, "(", L, ", ", U, ")", sep = " ")) 

#library(flextable)
#ft <- flextable(data = sodaRuleCI_formatted) %>% 
#  theme_booktabs() %>% 
#  autofit
#save_as_docx("Soda Rule" = ft, path = "manuscript_hiroshi/sodaRule.docx")



###--------------------------------------------------------------

targetCat= "Soda"
targetRule <- sort(subset(rules, subset = lhs %in% targetCat), by="lift", decreasing = TRUE)
ruleCI_formatted <- funcCIFormat(targetRule)

#Table 
ft <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat) %>% 
  funcTableWord()

# Plot 
df <- funcGetTableARM(ruleCI_formatted, R = targetRule, target = targetCat) %>% 
  funcReshapeARMtable(num=numItem)

funcPlotForestARM2(df, 
  maxX = 3.0, 
  colorArray = rep("black",3), 
  yName = "Consequent category and its pairwise frequency with soda in percent") + 
  theme(legend.position = c(0.7, 0.2)) 
if(savePlots){
  save_as_docx("soda"=ft, path = paste("manuscript_hiroshi/ARM_member_", saveFileName, targetCat, ".docx", sep = ""), 
               pr_section = sect_properties)
  saveRDS(df , paste("data_hiroshi/df_ARM_member_colorPLot_", saveFileName, targetCat, ".rds", sep = ""))
  ggsave(paste("manuscript_hiroshi/ARM_member_colorPLot_", saveFileName, targetCat, ".pdf", sep = "")
  , width = 8, height = 7, units = "in")
}





#targetRuleGraph <- head(sort(subset(rules, subset = lhs %in% targetCat), by="lift", decreasing = TRUE), 10)
#plot(targetRuleGraph, method = "graph")



# Plot for network - not going to the manuscript 
plot(rules, method = "graph", engine = "htmlwidget", max = 300)
plot(targetRule, method = "graph", engine = "htmlwidget", max = 20)




### VEG
targetCat= "Vegetables"
targetRule <- sort(subset(rules, subset = lhs %in% targetCat), by="lift", decreasing = TRUE)

ruleCI_formatted <- funcCIFormat(targetRule)
ft <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat) %>% 
  funcTableWord()
# Plot 
df <- funcGetTableARM(ruleCI_formatted, R = targetRule, target = targetCat) %>% 
  funcReshapeARMtable(num=numItem)

funcPlotForestARM2(df, maxX = 5.5, 
                   colorArray = rep("black",3), 
  yName = "Consequent category and its pairwise frequency with vegetables in percent") + 
  theme(legend.position = c(0.9, 0.2))

if(savePlots){
  save_as_docx("Veg" = ft, path = paste("manuscript_hiroshi/ARM_member_", saveFileName, targetCat, ".docx", sep = ""), pr_section = sect_properties)
  saveRDS(df , paste("data_hiroshi/df_ARM_member_colorPLot_", saveFileName, targetCat, ".rds", sep = ""))
  ggsave(paste("manuscript_hiroshi/ARM_member_colorPLot_", saveFileName, targetCat, ".pdf", sep = "")
       , width = 8, height = 7, units = "in")
}




### FRU 
targetCat= "Fruits"
targetRule <- sort(subset(rules, subset = lhs %in% targetCat), by="lift", decreasing = TRUE)
ruleCI_formatted <- funcCIFormat(targetRule)
ft <- funcGetTableARM(ruleCI_formatted, R = targetRule,target = targetCat) %>% 
  funcTableWord()

# Plot 
df <- funcGetTableARM(ruleCI_formatted, R = targetRule, target = targetCat) %>% 
  funcReshapeARMtable(num=numItem)

funcPlotForestARM2(df, maxX = 5.0, 
                   colorArray = rep("black",3), 
  yName = "Consequent category and its pairwise frequency with fruits in percent") + 
  theme(legend.position = c(0.9, 0.2)) 

if(savePlots){
  save_as_docx("Fru" = ft, path = paste("manuscript_hiroshi/ARM_member_", saveFileName, targetCat, ".docx", sep = ""), pr_section = sect_properties)
  saveRDS(df , paste("data_hiroshi/df_ARM_member_colorPLot_", saveFileName, targetCat, ".rds", sep = ""))
  ggsave(paste("manuscript_hiroshi/ARM_member_colorPLot_", saveFileName, targetCat, ".pdf", sep = "")
       , width = 8, height = 7, units = "in")
}






### Optional anlaysis - Multi-category rules (supplementary figure) --------------------------------------------------------------
apriori.ctl <- list(sort=-1, verbose = FALSE)
rules <- apriori(transactions, 
                 parameter = list(supp=0.01, conf=0.01, minlen=2, maxlen=4, ext=T),
                 control = apriori.ctl)
summary(rules)

#### old code to gt table ---------
#sodaRule<-sort(subset(rules, subset = lhs %in% "Soda" ), by="lift")
#inspect(sodaRule)
##{salty-snacks, sweet-snacks-candy, vegetables}   => {soft-drinks} if specified as RHS
#sodaRuleOR_formatted <- sodaRule %>% 
#  funcRuleCI() %>% 
#  funcFormatRule(sides = "lhs") %>% 
#  mutate(OR = paste(OR, "(", L, ", ", U, ")", sep = " ")) %>% 
#  data.frame(rhs = inspect(sodaRule)$rhs) %>% 
#  select(-L, -U, -count) %>% 
#  relocate(Category, rhs) %>% 
#  mutate(CategoryR = gsub("[{}]","", rhs))
#
#sodaRuleRR_formatted <- sort(subset(rules, subset = lhs %in% "Soda"), by="lift") %>% 
#  funcRuleCIRR() %>% 
#  funcFormatRuleRR()
#
#T <- sodaRuleOR_formatted  %>% 
#  left_join(
#    sodaRuleRR_formatted, 
#    by=c("Category" = "Category", "CategoryR" = "CategoryR")
#  ) %>%   
#  left_join(nameVar %>%  mutate(Category = tolower(Category))) %>% 
#  select(Category, CategoryR, support, lift, RR, OR) %>% 
#  dplyr::rename(`Frequency(%)` = support, Lift = lift,  `RR (95%CI)`= RR, `OR (95%CI)`=OR)  
#
#ft <- flextable(data = T[1:35, ]) %>% 
#  theme_booktabs() %>% 
#  autofit %>% 
#  line_spacing(space = 0.7, part = "all") %>% 
#  hline_top(part = "all", border = fp_border(color="black", width = 1)) %>% 
#  hline_bottom(border =  fp_border(color = "black", width = 1))



#### Code to generate plots ----------------------
targetCat= "Soda"
numItem = 15
targetRule <- sort(subset(rules, subset = lhs %in% targetCat), by="lift", decreasing = TRUE)
targetRule <- head(sort(subset(rules, subset = lhs %in% targetCat), by="lift", decreasing = TRUE), numItem)
inspect(targetRule)

#                category  est     lwr    upr index   effectEst
#1  Salty Snacks  (1.1 %) 2.88 -100.00 100.00     1        Lift
#2  Salty Snacks  (1.1 %) 2.88 -100.00 100.00     2        Lift
#


ruleOR <- 
targetRule %>% 
  funcRuleCI() %>% 
  mutate(Antecedant = gsub("[{}]","", lhs), Consequent = gsub("[{}]","", rhs) )  %>%  
  select(Antecedant, Consequent,  support, lift, contains("oddsRatio"),count) %>%  
  dplyr::rename(OR = oddsRatio, L = oddsRatio.LL, U = oddsRatio.UL) %>% 
  mutate(across(support:U, .fns = function(x) round(x,2))) %>% 
  dplyr::mutate(freq = round(support*100, 2), 
         lift = round(lift, 2), 
         est = round(OR,2),
         lwr = round(L, 2), 
         upr = round(U, 2), 
         effectEst = "OR (95% CI)", 
         index = 1:n()) %>% 
  select(Antecedant, Consequent, freq, lift, est, lwr, upr, effectEst, index)
  

ruleRR <-  
targetRule %>% 
  funcRuleCIRR() %>% 
  mutate(Antecedant = gsub("[{}]","", lhs), Consequent = gsub("[{}]","", rhs) )  %>%  
  select(Antecedant, Consequent,  support, lift, contains("relativeRisk"),count) %>% 
  dplyr::rename(RR = relativeRisk, L = relativeRisk.LL, U = relativeRisk.UL) %>% 
  mutate(across(support:U, .fns = function(x) round(x,2))) %>% 
  dplyr::mutate(freq = round(support*100, 2), 
         lift = round(lift, 2), 
         est = round(RR,2),
         lwr = round(L, 2), 
         upr = round(U, 2), 
         effectEst = "RR (95% CI)", 
         index = 1:n()) %>% 
  select(Antecedant, Consequent, freq, lift, est, lwr, upr, effectEst, index)

ruleLift <- ruleRR %>%  
  mutate(est = lift, lwr = -100, upr = 100, effectEst = "Lift") %>%  
  select(Antecedant, Consequent, freq, est, lwr, upr, effectEst, index)
ruleRR <- ruleRR %>% select(-lift)
ruleOR <- ruleOR %>% select(-lift)

df <- rbind(
  head(ruleRR, numItem), 
  head(ruleOR, numItem), 
  head(ruleLift, numItem)
  ) %>% 
  mutate(category = paste(Antecedant, "   ----   ", Consequent, "   ", "(", freq, "%)",  sep = "")) %>% 
  mutate(category = str_replace_all(category, ",", " , ")) %>% 
  select(-Consequent, -Antecedant) %>% 
  relocate(category)   
  


#df <- funcGetTableARM(ruleCI_formatted, R = targetRule, target = targetCat) %>% 
#  funcReshapeARMtable(num=numItem)

funcPlotForestARM2(df, maxX = 6., minX = 2.0, 
                   colorArray = rep("black",3), 
                   yName = "Antecedant combination and consequent \n with thier frequency in percent") + 
  theme(legend.position = c(0.9, 0.2))



