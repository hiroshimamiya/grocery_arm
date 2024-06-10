
### Summarize the results of Longitudinal Regression models  


rm(list=ls())
source("loadFunc.R")

##########################################################################
library(broom.mixed)
confint.glmer <- function(object, params, paramNames, foodName = "food",...) {
  cc <- tidy(object,conf.int=TRUE,exponentiate=TRUE,effects="fixed") %>% 
    dplyr::select(term, estimate, conf.low, conf.high) %>% 
    dplyr::rename(est = estimate, lwr = conf.low, upr = conf.high) 
  cc<- cc[match(params, cc$term), ] %>% 
    filter(term %in% params) %>% 
    data.frame(foodName = foodName, paramsNames = paramNames) %>% 
    relocate(foodName, paramsNames) 
  rownames(cc) <- cc$term
  cc$term <- NULL
  return(cc)
}

bool_saveForest = FALSE

datPath <- ifelse(as.character(Sys.info()["nodename"]) == "DB-COMP",  "/home/hiroshi/projects/kodyloCardData/", "~/R/nielsenAnalysis/kodyloCardData/")

##########################################################################
paramName = c("Soft drinks", "Income", "Education", "Soda X Income", "Soda X Education")
params <- c("cat_soda", "income", "educ", "income:cat_soda", "cat_soda:educ")

fit_chips <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_soda_chips_withInterX.rds")
fit_candy <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_soda_candy_withInterX.rds")
fit_juice <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_soda_juice_withInterX.rds")

tidy(fit_chips,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tidy(fit_candy,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tidy(fit_juice,conf.int=TRUE,exponentiate=TRUE,effects="fixed")

a <- "Salty snacks"; b <- "Sweet snacks and candies"; c<-"Juices and drinks"

r <- rbind(
  confint.glmer(fit_chips, params= params, paramNames = paramName, foodName = a),
  confint.glmer(fit_candy, params= params, paramNames = paramName, foodName = b),
  confint.glmer(fit_juice, params= params, paramNames = paramName, foodName = c)
)

r$foodName <- factor(r$foodName, levels = c(a, b, c))
r$index <- 1:nrow(r)

funcPlotForest(r, maxX = 2.5) + ggtitle("Association of soda with salty snacks, candies, and juice")

if(bool_saveForest){
  ggsave("manuscript_hiroshi/glmerSoda.png", width = 11, height = 3)
  ggsave("manuscript_hiroshi/glmerSoda.pdf", width = 11, height = 3)
}
rSoda <- r[r$paramsNames == "Soft drinks", ]


### Veggie #############
fitGlmer_veg_canVeg <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_veg_canVeg_withInterX.rds")
fitGlmer_veg_salad <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_veg_salad_withInterX.rds")
fitGlmer_veg_cheese <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_veg_cheese_withInterX.rds")

paramNames = c("Vegetable", "Income", "Education", "Veg X Income", "Veg X Education")
params <- c("cat_veggie", "income", "educ", "income:cat_veggie", "educ:cat_veggie")

a <- "Packaged salads / stir fries"; b <- "Canned vegetables"; c<-"Deli cheese"

r <- rbind(
  confint.glmer(fitGlmer_veg_salad, params= params, paramNames = paramNames, foodName = a),
  confint.glmer(fitGlmer_veg_canVeg, params= params, paramNames = paramNames, foodName = b),
  confint.glmer(fitGlmer_veg_cheese, params= params, paramNames = paramNames, foodName = c)
)

r$foodName <- factor(r$foodName, levels = c(a, b, c)) 
r$index <- 1:nrow(r)

funcPlotForest(r, maxX = 3.5) + ggtitle("Association of vegetables with salad, canned vegetables, and cheese")
if(bool_saveForest){
  ggsave("manuscript_hiroshi/glmerVeg.png", width = 11, height = 3)
  ggsave("manuscript_hiroshi/glmerVeg.pdf", width = 11, height = 3)
}
rVeg <- r[r$paramsNames == "Vegetable", ]


### Fruits ###########
fitGlmer_fru_cer <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_fru_cer_withInterX.rds")
fitGlmer_fru_yog <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_fru_yog_withInterX.rds")
fitGlmer_fru_salad <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_fru_salad_withInterX.rds")

paramNames = c("Fruits", "Income", "Education", "Fruits X Income", "Fruits X Education")
params <- c("cat_fruits", "income", "educ", "income:cat_fruits", "educ:cat_fruits")

a <- "Packaged salads / stir fries"; b <- "Yogurt"; c<-"Cereals"

r <- rbind(
  confint.glmer(fitGlmer_fru_salad, params= params, paramNames = paramNames, foodName = a),
  confint.glmer(fitGlmer_fru_yog, params= params, paramNames = paramNames, foodName = b),
  confint.glmer(fitGlmer_fru_cer, params= params, paramNames = paramNames, foodName = c)
)

r$foodName <- factor(r$foodName, levels = c(a, b, c)) 
r$index <- 1:nrow(r)

p<-funcPlotForest(r, maxX = 2.5)
p + ggtitle("Assoication of fruits with salad, yogurt, and cereal ")
if(bool_saveForest){
  ggsave("manuscript_hiroshi/glmerFruit.png", width = 11, height = 3)
  ggsave("manuscript_hiroshi/glmerFruit.pdf", width = 11, height = 3)
}
rFru <- r[r$paramsNames == "Fruits", ]


library(tidyr)
rAll <- rbind(rSoda, rVeg, rFru) %>% 
  as.data.frame() %>% 
  dplyr::mutate(categoryOrder = ifelse(paramsNames == "Soft drinks", "a", ""), 
                categoryOrder = ifelse(paramsNames == "Vegetable", "b", categoryOrder), 
                categoryOrder = ifelse(paramsNames == "Fruits", "c", categoryOrder) 
  ) %>% 
  arrange(categoryOrder, desc(est)) %>% 
  dplyr::rename(Exposure = paramsNames) %>%
  group_by(Exposure) %>% 
  arrange(categoryOrder, index) %>% 
  dplyr::mutate(IndexExposure = row_number()) %>%
  #mutate(Exposure = ifelse(IndexExposure == 1, Exposure, "")) %>% 
  ungroup() %>% 
  dplyr::mutate(index = row_number()) %>% 
  select(Exposure, foodName, est, lwr, upr, IndexExposure, categoryOrder)




xMax = 3
p1 <- funcPlotBar(rAll[rAll$Exposure == "Soft drinks", ], xMax = xMax, FALSE, title = "A")
p2 <- funcPlotBar(rAll[rAll$Exposure == "Vegetable", ], xMax = xMax, FALSE, title = "B")
p3 <- funcPlotBar(rAll[rAll$Exposure == "Fruits", ], xMax = xMax, TRUE, title = "C")
p1 <- ggplotGrob(p1)
p2 <- ggplotGrob(p2)
p3 <- ggplotGrob(p3)

pc <- gridExtra::gtable_rbind(p1, p2, p3, size = "max")
grid.newpage()

# this oen does not save 
if(bool_saveForest){
  png("manuscript_hiroshi/glmerAllMain.png", height = 7, width = 10)
  grid.draw(pc)
  dev.off()
}




### IntX comparison --  Comparison of model fit for AIC between with and without interx  
# models with out intx were run in separate code 

#https://stackoverflow.com/questions/55693401/in-r-read-files-from-folder-in-a-list-and-assign-list-element-names-by-the-file

li <- list.files("/home/hiroshi/projects/kodyloCardData/data_hiroshi", pattern="noIntX.rds", full.names = TRUE)
liX <- lapply(li, function(x) str_replace(x, "_noIntX", "_withInterX"))
  
listFile <- lapply(li, readRDS)
listFileX <- lapply(liX, readRDS)

aicModels <- data.frame(
  Modelnames = unlist(lapply(li, str_remove, "/home/hiroshi/R/nielsenAnalysis/kodyloCardData/data_hiroshi//fitGlmer_")), 
  `AIC(without interactions)` = unlist(lapply(listFile, AIC)), 
  `AIC(with interactions)` = unlist(lapply(listFileX, AIC)), 
  diff = unlist(lapply(listFile, AIC)) - unlist(lapply(listFileX, AIC))
)

flextable(data = aicModels) %>% 
  theme_booktabs() %>% 
  autofit() %>% 
save_as_docx(path = "manuscript_hiroshi/AICTable.docx")






### RR delta method-------------------------------------
#https://migariane.github.io/DeltaMethodEpiTutorial.nb.html
#https://cran.r-project.org/web/packages/logisticRR/vignettes/logisticRR.html
#https://stats.oarc.ucla.edu/r/faq/how-can-i-estimate-the-standard-error-of-transformed-regression-parameters-in-r-using-the-delta-method/
#https://migariane.github.io/DeltaMethodEpiTutorial.nb.html  
#https://ete-online.biomedcentral.com/articles/10.1186/s12982-021-00107-2
#https://search.r-project.org/CRAN/refmans/car/html/deltaMethod.html
# I used this
#https://stats.oarc.ucla.edu/r/faq/how-can-i-estimate-the-standard-error-of-transformed-regression-parameters-in-r-using-the-delta-method/
#



# load fit objects from models
li <- list.files("data_hiroshi/", pattern="noIntX.rds", full.names = TRUE)
li <- list.files("data_hiroshi/", pattern="withInterX.rds", full.names = TRUE)
fitList <- lapply(li, readRDS)
fitNames <- lapply(li, stringr::str_remove, "data_hiroshi//fitGlmer_")

# Calculate mean 
predList <- lapply(fitList, funcFitNewData)
predList <- setNames(predList, fitNames)
meanList <- lapply(predList, function(x) mean(x$pred1)/mean(x$pred0))

# Calculate SE 
SEList <- lapply(fitList, funcGetDeltaSE)
SEList <- setNames(SEList, fitNames)
CIList <- mapply(function(mean, SE){
  data.frame(
    est=mean, 
    lwr = mean - qnorm(0.975)*SE, 
    upr = mean + qnorm(0.975)*SE)}, 
  mean = meanList, SE = SEList, SIMPLIFY = F)
names(CIList) <- fitNames

estRR <- bind_rows(CIList, .id = "catName")

estRR$paramsNames <- NA
estRR[grep("soda_", estRR$catName), "paramsNames"] <- "Soft drinks"
estRR[grep("veg_", estRR$catName), "paramsNames"] <- "Vegetable"
estRR[grep("fru_", estRR$catName), "paramsNames"] <- "Fruits"

estRR$foodName <- NA
estRR[grep("_cer_", estRR$catName), "foodName"] <- "Cereals"
estRR[grep("_salad_", estRR$catName), "foodName"] <- "Packaged salads / stir fries"
estRR[grep("_candy_", estRR$catName), "foodName"] <- "Sweet snacks and candies"
estRR[grep("_yog_", estRR$catName), "foodName"] <- "Yogurt"
estRR[grep("_canVeg_", estRR$catName), "foodName"] <- "Canned vegetables"
estRR[grep("_juice_", estRR$catName), "foodName"] <- "Juices and drinks"
estRR[grep("_cheese_", estRR$catName), "foodName"] <- "Deli cheese"
estRR[grep("_chips_", estRR$catName), "foodName"] <- "Salty snacks"


estRR <- estRR %>% 
  dplyr::rename(Exposure = paramsNames) %>% 
  left_join(rAll %>%  dplyr::rename(est_OR=est, lwr_OR=lwr, upr_OR= upr), 
            by = c("Exposure" = "Exposure", "foodName" = "foodName")) %>% 
  arrange(categoryOrder, IndexExposure)

xMax = 3
p1 <- funcPlotBar(estRR[estRR$Exposure == "Soft drinks", ], xMax = xMax, FALSE, title = "A", xname = "Relative risk and 95% CI" )
p2 <- funcPlotBar(estRR[estRR$Exposure == "Vegetable", ], xMax = xMax, FALSE, title = "B", xname = "Relative risk and 95% CI")
p3 <- funcPlotBar(estRR[estRR$Exposure == "Fruits", ], xMax = xMax, TRUE, title = "C", xname = "Relative risk and 95% CI")
p1 <- ggplotGrob(p1)
p2 <- ggplotGrob(p2)
p3 <- ggplotGrob(p3)

pc <- gridExtra::gtable_rbind(p1, p2, p3, size = "max")
grid.newpage()

# this oen does not save 
png("manuscript_hiroshi/glmerAllMain.png", height = 7, width = 10)
grid.draw(pc)
dev.off()





### Confounders and rand effects  ---------------------------------------
noB <- readRDS("data_hiroshi/fitGlmer_veg_salad_withInterX_noBasketSize.rds")
B <-   readRDS("data_hiroshi/fitGlmer_veg_salad_withInterX.rds")
noC <- readRDS("data_hiroshi/fitGlm_veg_salad_withInterX_noConfounder.rds")
noR <- readRDS("data_hiroshi/fitGlm_veg_salad_withInterX_noRand.rds")

tidy(noB,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tidy(noR, exponentiate = TRUE)
tidy(noC,exponentiate=TRUE)
tidy(B,conf.int=TRUE,exponentiate=TRUE,effects="fixed")

AIC(noB); AIC(B)







### Regression on weakly associated items -------------------------------------
### Veggie #############
fitGlmer_veg_yog <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_yog_veg_withInterX.rds")
fitGlmer_veg_sau <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_sau_veg_withInterX.rds")
fitGlmer_veg_cer <- readRDS("/home/hiroshi/projects/kodyloCardData/data_hiroshi/fitGlmer_cer_veg_withInterX.rds")

paramNames = c("Vegetable", "Income", "Education")
params <- c("cat_veggie", "income", "educ")

a <- "Yogurt"; b <- "Sausages/Bacons"; c<-"Cereals"

# load fit objects from models
fitList <- list(fitGlmer_veg_sau,fitGlmer_veg_yog,  fitGlmer_veg_cer)
fitNames <- list("fitGlmer_veg_sau", "fitGlmer_veg_yog", "fitGlmer_veg_cer")

# Calculate mean 
predList <- lapply(fitList, funcFitNewData)
predList <- setNames(predList, fitNames)
meanList <- lapply(predList, function(x) mean(x$pred1)/mean(x$pred0))

# Calculate SE 
SEList <- lapply(fitList, funcGetDeltaSE)
SEList <- setNames(SEList, fitNames)
CIList <- mapply(function(mean, SE){
  data.frame(
    est=mean, 
    lwr = mean - qnorm(0.975)*SE, 
    upr = mean + qnorm(0.975)*SE)}, 
  mean = meanList, SE = SEList, SIMPLIFY = F)
names(CIList) <- fitNames

estRR <- bind_rows(CIList, .id = "catName")

estRR$paramsNames <- NA
estRR[grep("soda_", estRR$catName), "paramsNames"] <- "Soft drinks"
estRR[grep("veg_", estRR$catName), "paramsNames"] <- "Vegetable"
estRR[grep("fru_", estRR$catName), "paramsNames"] <- "Fruits"

estRR$foodName <- NA
estRR[grep("_cer", estRR$catName), "foodName"] <- "Cereals"
estRR[grep("_salad_", estRR$catName), "foodName"] <- "Packaged salads / stir fries"
estRR[grep("_candy_", estRR$catName), "foodName"] <- "Sweet snacks and candies"
estRR[grep("_yog", estRR$catName), "foodName"] <- "Yogurt"
estRR[grep("_sau", estRR$catName), "foodName"] <- "Sausages/Bacons"
estRR[grep("_canVeg_", estRR$catName), "foodName"] <- "Canned vegetables"
estRR[grep("_juice_", estRR$catName), "foodName"] <- "Juices and drinks"
estRR[grep("_cheese_", estRR$catName), "foodName"] <- "Deli cheese"
estRR[grep("_chips_", estRR$catName), "foodName"] <- "Salty snacks"
estRR[grep("_chips_", estRR$catName), "foodName"] <- "Salty snacks"


#estRR <- estRR %>% 
#  dplyr::rename(Exposure = paramsNames) %>% 
#  left_join(rAll %>%  dplyr::rename(est_OR=est, lwr_OR=lwr, upr_OR= upr), 
#            by = c("Exposure" = "Exposure", "foodName" = "foodName")) %>% 
#  arrange(categoryOrder, IndexExposure)

estRR$IndexExposure <- 1:3
funcPlotBar(estRR, xMax = xMax, TRUE, title = "B", xname = "Relative risk and 95% CI")

xMax = 3
p1 <- funcPlotBar(estRR[estRR$Exposure == "Soft drinks", ], xMax = xMax, FALSE, title = "A", xname = "Relative risk and 95% CI" )
p2 <- funcPlotBar(estRR[estRR$Exposure == "Vegetable", ], xMax = xMax, FALSE, title = "B", xname = "Relative risk and 95% CI")
p3 <- funcPlotBar(estRR[estRR$Exposure == "Fruits", ], xMax = xMax, TRUE, title = "C", xname = "Relative risk and 95% CI")
p1 <- ggplotGrob(p1)
p2 <- ggplotGrob(p2)
p3 <- ggplotGrob(p3)

pc <- gridExtra::gtable_rbind(p1, p2, p3, size = "max")
grid.newpage()

# this oen does not save 
png("manuscript_hiroshi/glmerAllMain.png", height = 7, width = 10)
grid.draw(pc)
dev.off()


