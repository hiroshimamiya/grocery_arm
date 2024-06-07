

# Including month 
start_time <- Sys.time()
lmerFitSmallMonth<- glmer(cat_chips ~ income*cat_soda + educ*cat_soda + cat_soda + educ + income + 
pfam + popn + pimm + cat_other + lag.cat_other + lag.cat_chips + factor(month) +(1 | customer), 
data = mDataSmallSample,  family = binomial(link = "logit"))
end_time <- Sys.time(); start_time - end_time 
exp(lmerFit@beta)

# Not including month 
start_time <- Sys.time()
lmerFitSmall<- glmer(cat_chips ~ income*cat_soda + educ*cat_soda + cat_soda + educ + income + 
                       pfam + popn + pimm + cat_other + lag.cat_other + lag.cat_chips +  (1 | customer), 
                     data = mDataSmallSample, family = binomial(link = "logit"))




anova(lmerFitSmallMonth,lmerFitSmall)

cc <- confint(lmerFitSmall,parm="beta_")  ## slow (~ 11 seconds)
ctab <- cbind(est=fixef(lmerFitSmall),cc)


# getting CI 
#https://stackoverflow.com/questions/26417005/odds-ratio-and-confidence-intervals-from-glmer-output
  


### Lmerfurmula ----------
formulaBase     = as.formula(cat_chips ~ cat_soda +  educ + income + pfam + popn + pimm + total + holidayInd + lag.total  + (1 | customer))
formulaBaseIntX = as.formula(cat_chips ~income*cat_soda + educ*cat_soda + cat_soda + educ + income + pfam + popn + pimm + total + holidayInd + lag.total +  (1 | customer))

### Test with very small data 
# Area effects test 
sampleCustomer <- sample(unique(mData$customer), 300)
mDataTest <- mData[mData$customer %in% sampleCustomer, ]
mDataTest <- mDataTest %>%  arrange(customer, timestamp)
dim(mDataTest)

lmerFitSmall<- glmer(update(formulaBaseIntX, ~.  -(1 | customer) +(1 | neigh_imputed/customer) ), 
  glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
  data = mDataTest, 
  family = binomial(link = "logit"))

fitGlmer_test <- glmer(
  formula = formulaBaseIntX,
  data = mDataTest,
  glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
  family = binomial(link = "logit"))
anova(lmerFitSmall, fitGlmer_test)

lmerFitCross<- glmer(update(formulaBaseIntX, ~.  -(1 | customer) +(1 | store) + (1 | customer) ), 
  glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
  data = mDataTest, 
  family = binomial(link = "logit"))
anova(lmerFitSmall, lmerFitCross)

#https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified

### Soda and candy, and soda and juice ######################
bool_lmerTest <- TRUE
fmSoda <- as.formula()
if(bool_lmerTest){
  system.time(
    fitGlmer_chips <- glmer(
    formula = formulaBaseIntX,
    data = mDataSmallSample,
    glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
    family = binomial(link = "logit"))
  )
  saveRDS(fitGlmer_chips , "data_hiroshi/fitGlmer_chips.rds")
  rm(fitGlmer_chips)
}

bool_lmerCandy = TRUE
if(bool_lmerCandy){
  system.time(
    fitGlmer_candy <- glmer(
      formula = update(formulaBaseIntX, cat_candy~ .),
      data = mDataSmallSample,
      glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
      family = binomial(link = "logit"))
  )
  summary(fitGlmer_candy)
  saveRDS(fitGlmer_candy , "data_hiroshi/fitGlmer_candy.rds")
  rm(fitGlmer_candy)
}

bool_lmerJuice= TRUE
if(bool_lmerJuice){
  fitGlmer_juice <- glmer(
    formula =  update(formulaBaseIntX, cat_juice~ .),
    data = mDataSmallSample,
    glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
    family = binomial(link = "logit"))
  summary(fitGlmer_juice)
  saveRDS(fitGlmer_juice, "data_hiroshi/fitGlmer_juice.rds")
  rm(fitGlmer_juice)
}



### Veggie Other food categories ##########################
bool_lmer_veg_can = TRUE
if(bool_lmer_veg_can){
  fitGlmer_veg_canVeg <- glmer(
    update(formulaBase, 
           cat_canVeg ~ . - cat_soda - income:cat_soda - cat_soda:educ + cat_veggie*income + cat_veggie*educ), 
    #update(formulaBase, cat_canVeg ~ . - cat_soda + cat_veggie), 
    data = mDataSmallSample,
    glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
    family = binomial(link = "logit"))
  summary(fitGlmer_veg_canVeg)
  saveRDS(fitGlmer_veg_canVeg , "data_hiroshi/fitGlmer_veg_canVeg.rds")
  rm(fitGlmer_veg_canVeg)
}

bool_lmer_veg_salad= TRUE 
if(bool_lmer_veg_salad){
  fitGlmer_veg_salad <- glmer(
    update(formulaBase, 
           cat_salad ~ . - cat_soda - income:cat_soda - cat_soda:educ + 
             cat_veggie*income + cat_veggie*educ), 
    data = mDataSmallSample,
    glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
    family = binomial(link = "logit"))
  summary(fitGlmer_veg_salad)
  saveRDS(fitGlmer_veg_salad , "data_hiroshi/fitGlmer_veg_salad.rds")
  rm(fitGlmer_veg_salad)
}


bool_lmer_veg_cheese= TRUE 
if(bool_lmer_veg_cheese){
  fitGlmer_veg_cheese <- glmer(
    update(formulaBase, 
           cat_cheese ~ . - cat_soda - income:cat_soda - cat_soda:educ + 
             cat_veggie*income + cat_veggie*educ), 
    data = mDataSmallSample,
    glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
    family = binomial(link = "logit"))
  summary(fitGlmer_veg_cheese)
  saveRDS(fitGlmer_veg_cheese , "data_hiroshi/fitGlmer_veg_cheese.rds")
  rm(fitGlmer_veg_cheese)
}



### FRUITS and Other food categories ##########################
bool_lmer_fru_salad = TRUE
if(bool_lmer_fru_salad){
  fitGlmer_fru_salad <- glmer(
    update(formulaBase, 
           cat_salad ~ . - cat_soda - income:cat_soda - cat_soda:educ + 
             cat_fruits*income + cat_fruits*educ), 
    data = mDataSmallSample,
    glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
    family = binomial(link = "logit"))
  summary(fitGlmer_fru_salad)
  saveRDS(fitGlmer_fru_salad , "data_hiroshi/fitGlmer_fru_salad.rds")
  rm(fitGlmer_fru_salad)
}

bool_lmer_fru_yog= TRUE 
if(bool_lmer_fru_yog){
  fitGlmer_fru_yog <- glmer(
    update(formulaBase, 
           cat_yog ~ . - cat_soda - income:cat_soda - cat_soda:educ + 
             cat_fruits*income + cat_fruits*educ), 
    data = mDataSmallSample,
    glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
    family = binomial(link = "logit"))
  summary(fitGlmer_fru_yog)
  saveRDS(fitGlmer_fru_yog , "data_hiroshi/fitGlmer_fru_yog.rds")
  rm(fitGlmer_fru_yog)
}


bool_lmer_fru_cer= TRUE 
if(bool_lmer_fru_cer){
  fitGlmer_fru_cer <- glmer(
    update(formulaBase, 
           cat_cer ~ . - cat_soda - income:cat_soda - cat_soda:educ + 
             cat_fruits*income + cat_fruits*educ), 
    data = mDataSmallSample,
    glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), 
    family = binomial(link = "logit"))
  summary(fitGlmer_fru_cer)
  saveRDS(fitGlmer_fru_cer , "data_hiroshi/fitGlmer_fru_cer.rds")
  rm(fitGlmer_fru_cer)
}



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


##########################################################################
paramName = c("Soft drinks", "Income", "Education", "Soda X Income", "Soda X Education")
params <- c("cat_soda", "income", "educ", "income:cat_soda", "cat_soda:educ")

fit_chips <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/fitGlmer_chips.rds")
fit_candy <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/fitGlmer_candy.rds")
fit_juice <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/fitGlmer_juice.rds")

tidy(fit_chips,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tidy(fit_candy,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tidy(fit_juice,conf.int=TRUE,exponentiate=TRUE,effects="fixed")

a <- "Salty snacks"; b <- "Sweet snacks and candies"; c<-"Juices and drinks "

r <- rbind(
  confint.glmer(fit_chips, params= params, paramNames = paramName, foodName = a),
  confint.glmer(fit_candy, params= params, paramNames = paramName, foodName = b),
  confint.glmer(fit_juice, params= params, paramNames = paramName, foodName = c)
)

r$foodName <- factor(r$foodName, levels = c(a, b, c))
r$index <- 1:nrow(r)

funcPlotForest(r, maxX = 2.5) + ggtitle("Association of soda with salty snacks, candies, and juice")
ggsave("manuscript_hiroshi/glmerSoda.png", width = 9, height = 4)

rSoda <- r[r$paramsNames == "Soft drinks", ]


### Veggie #############
fitGlmer_veg_canVeg <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/fitGlmer_veg_canVeg.rds")
fitGlmer_veg_salad <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/fitGlmer_veg_salad.rds")
fitGlmer_veg_cheese <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/fitGlmer_veg_cheese.rds")

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

funcPlotForest(r, maxX = 2.5) + ggtitle("Association of vegetables with salad, canned vegetables, and cheese")
ggsave("manuscript_hiroshi/glmerVeg.png", width = 9, height = 4)

rVeg <- r[r$paramsNames == "Vegetable", ]


### Fruits ###########
fitGee_fru_cer <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/fitGlmer_fru_cer.rds")
fitGee_fru_yog <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/fitGlmer_fru_yog.rds")
fitGee_fru_salad <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/fitGlmer_fru_salad.rds")

paramNames = c("Fruits", "Income", "Education", "Fruits X Income", "Fruits X Education")
params <- c("cat_fruits", "income", "educ", "income:cat_fruits", "educ:cat_fruits")

a <- "Packaged salads / stir fries"; b <- "Yogurt"; c<-"Cereals"

r <- rbind(
  confint.glmer(fitGee_fru_salad, params= params, paramNames = paramNames, foodName = a),
  confint.glmer(fitGee_fru_yog, params= params, paramNames = paramNames, foodName = b),
  confint.glmer(fitGee_fru_cer, params= params, paramNames = paramNames, foodName = c)
)

r$foodName <- factor(r$foodName, levels = c(a, b, c)) 
r$index <- 1:nrow(r)

p<-funcPlotForest(r, maxX = 2.5)
p + ggtitle("Assoication of fruits with salad, yogurt, and cereal ")
ggsave("manuscript_hiroshi/glmerFruit.png", width = 9, height = 4)

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


funcPlotBar <- function(d, xMax=3.5, axi = FALSE, title=""){
  xname <- "Odds ratio and 95% CI"
  p<-ggplot(data=d, aes(y=IndexExposure, x=est, xmin=lwr, xmax=upr))+ 
    geom_point()+ 
    geom_errorbarh(height=.4)+
    scale_x_continuous(limits=c(0.1,xMax), breaks = c(0.5:5), name=xname)+
    scale_y_continuous(name = "", breaks=1:nrow(d), labels = d$foodName, trans="reverse") +
    geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) +
    theme_classic() + 
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
          axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain")) +
    theme(strip.text.y = element_text(size=14,  vjust = 1),
          strip.background = element_rect(colour="white", fill="white")) + 
    coord_cartesian(xlim = c(0.5, xMax)) + 
    theme(axis.ticks.length=unit(.3, "cm")) +
    ggtitle(title) + 
    theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          plot.caption.position =  "plot")
  if(!axi){
    p <-p + theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank()) 
  }
  return(p)  
}
xMax = 2.5
p1 <- funcPlotBar(rAll[rAll$Exposure == "Soft drinks", ], xMax = xMax, FALSE, title = "A")
p2 <- funcPlotBar(rAll[rAll$Exposure == "Vegetable", ], xMax = xMax, FALSE, title = "B")
p3 <- funcPlotBar(rAll[rAll$Exposure == "Fruits", ], xMax = xMax, TRUE, title = "C")
p1 <- ggplotGrob(p1)
p2 <- ggplotGrob(p2)
p3 <- ggplotGrob(p3)

pc <- gridExtra::gtable_rbind(p1, p2, p3, size = "max")
grid.newpage()

# this oen does not save 
png("manuscript_hiroshi/glmerAllMain.png", height = 7, width = 9)
grid.draw(pc)
dev.off()

