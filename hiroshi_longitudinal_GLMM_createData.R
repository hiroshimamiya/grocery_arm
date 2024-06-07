# Script to create longitudinal data for confirmatory regression  models


#library(Matrix)
library(data.table)
# If need to create a wide and sparse data (colums are foodc ategories), follow this https://www.r-bloggers.com/2016/01/casting-a-wide-and-sparse-matrix-in-r/
# https://stackoverflow.com/questions/61130613/dcast-with-multiple-variables-in-val-var-option
# https://hausetutorials.netlify.app/0004_dataform_join.html
library(DBI)
library(RPostgreSQL)
library(dplyr)
library(plyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(reshape2)
library(Hmisc)
library(forecast)
library(scales)
library(viridis)
library(wesanderson)
library(RColorBrewer)
library(grid)
library(kableExtra)
library(ggmap)
library(rgeos)
library(rgdal)
library(proj4)
library(sp)
library(spdep)
library(arules)
library(arulesSequences)
library(arulesViz)
# if needs to create wide table in kody's way, 
### and see gee section he coded 
#soda.transact.wide <- soda.transact %>%
#  dplyr::select(-c(n, pr, wt, pp)) %>%
#  spread(category, sp) %>%
#  mutate(soda = `soft-drinks`,
#         coffee = `iced-tea-coffee`,
#         juice = `juices-drinks`,
#         salty = `salty-snacks`,
#         sweets = `sweet-snacks-candy`,
#         energy = `sports-energy-drinks`,
#         sweets.0 = factor(ifelse(sweets > 0, 1, 0)),
#         salty.0 = factor(ifelse(salty > 0, 1, 0)),
#         coffee.0 = factor(ifelse(coffee > 0, 1, 0)),
#         juice.0 = factor(ifelse(juice > 0, 1, 0)),
#         energy.0 = factor(ifelse(energy > 0, 1, 0)),
#         soda.0 = factor(ifelse(soda > 0, 1, 0))) %>%
#  dplyr::select(-c(`iced-tea-coffee`, `juices-drinks`, `salty-snacks`,
#                   `soft-drinks`, `sports-energy-drinks`, `sweet-snacks-candy`))


rm(list=ls())
### Load data ----------------------------

source("loadFunc.R")

# mini is being loaded, so basket.sample no longer needed 
#basket.sample <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/basket.sample_clean_flowchart.rds")

upc.class <- dbGetQuery(con, "select * from kodyandrew.metro_upc_class;")
upc.class.full <- dbGetQuery(con, "select * from kodyandrew.metro_upc_class_full;")

cards.mtl　<- dbGetQuery(con, "select * from kodyandrew.cards_mtl_hiroshi_flowchart")

pc2se <- RPostgres::dbGetQuery(geo, "select * from kodyandrew.pc2se2")

da2ct <- RPostgres::dbGetQuery(geo, "select dauid, ctuid from public.da2016 where cmaname = 'Montréal'")

da2neigh <- RPostgres::dbGetQuery(geoH, "
select a.dauid, b.neighbourhood,
    case
        when b.neighbourhood is NULL then 0
        else b.neighbourhood
        end as neigh_imputed
    from public.da2016 a
    left join hiroshi.da2neigh b
    on a.dauid = b.dauid
where cmapuid = '24462';")



da2ct %>% group_by(dauid) %>% dplyr::filter(n() > 1)
pc2da <- dbGetQuery(geo, "select * from kodyandrew.pc2da_hybrid") #_v2")
pc2da2ct <- pc2da %>% 
  dplyr::group_by(pc) %>% 
  dplyr::arrange(desc(popwt), .by_group = TRUE) %>% 
  filter(row_number()==1 ) %>% 
  select(pc, dauid) %>% 
  left_join(da2ct)
pc2se <- pc2se %>%  
  left_join(pc2da2ct)

# This one is already aggregated SES for unique PC across overlaping Da using popwt
# this is same as pc2se, except containign my own SES variables 
pc2da2sesAggMtl <- readRDS("data_hiroshi/pc2da2sesAggMtl.rds")
dim(pc2da2sesAggMtl)
pc2da2sesAggMtl <- pc2da2sesAggMtl %>%  
  left_join(pc2da2ct)
dim(pc2da2sesAggMtl)

store_ses_buffer <- dbGetQuery(geoH, "select * from hiroshi.hiroshi_metrostore_ses_buffer_storeid;")

undup <- readRDS("~/R/nielsenAnalysis/kodyMetro/data_hiroshi/undup_mtl_hiroshi_flowChart.rds")


### SES on card --------------------------------
#need to link with pc3se and pc2da.mtl to get SES 
loc_mtl <- cards.mtl %>% 
  select(customer_id, postal_code, dauid_largestPopWt) %>% 
  distinct(.) %>% 
  left_join(pc2da2sesAggMtl, by = c("postal_code" = "pc")) %>% 
  #left_join(pc2se, by = c("postal_code" = "pc")) %>% 
  mutate(
    incomeTer = cut(income, 
                    breaks=quantile(income, probs=seq(0,1, by=0.333), na.rm=TRUE), 
                    include.lowest=TRUE), 
    educTer = cut(educ, 
                  breaks=quantile(educ, probs=seq(0,1, by=0.333), na.rm=TRUE), 
                  include.lowest=TRUE)) %>% 
  as.data.table()
#loc_mtl 290261     12
dim(loc_mtl)

head(loc_mtl$incomeTer)
loc_mtl$incomeTer <- as.numeric(loc_mtl$incomeTer)
head(loc_mtl$incomeTer) # higher , more income 
head(loc_mtl$educTer)
loc_mtl$educTer <- as.numeric(loc_mtl$educTer)
head(loc_mtl$educTer) # higher, more educated

# attach neigh indicator 
dim(loc_mtl)
loc_mtl <- loc_mtl %>% 
  left_join(da2neigh, by = c("dauid" = "dauid"))



### Prep --------------------------------------------------------------------
### this code is essetnialy same as "mini" data above, except filtered out non-food and null categories revmoved
#m <- basket.sample %>%
#  left_join(upc.class, by="upc") %>%
#  filter(!is.na(category)) %>%
#  filter(food == 1) %>%
#  filter(!is.na(upc)) %>% 
#  select(customer, transact, upc, spending, total, category, store, timestamp, quantity) %>% # dim() 598623 if only soda and chips 
#  distinct(.) # 597868 , 14249813 if all items
#
#m$proportion <- ifelse(m$total == 0, 0, 
#                       ifelse(m$spending < 0, 0,
#                              ifelse(m$total < m$spending, 1,
#                                     abs(m$spending)/abs(m$total))))

### new mini data from my own exclusion criteria in the motnreal_v.0___H_sampleDesc.R
m <- readRDS("data_hiroshi/mini_flowchart.rds")


# make it to data table after removing some duplicates  
`%notin%` <- Negate(`%in%`)
dim(m)
m <- m %>%
  filter(transact %notin% undup$transact) %>% 
  as.data.table() 
dim(m)
m <- m[order(customer, timestamp)]

### CATEGORY -----------------------------------------------------------------------
m <- m %>% mutate(cat_soda = ifelse(category =="soft-drinks", quantity, 0), 
                  cat_chips = ifelse(category =="salty-snacks", quantity, 0), 
                  cat_water = ifelse(category =="water", quantity, 0),
                  cat_frozen = ifelse(category =="frozen-meals-sides", quantity, 0),
                  cat_sausage = ifelse(category =="sausages-bacon", quantity, 0),
                  cat_butter = ifelse(category =="butter-margarine", quantity, 0),
                  cat_veggie = ifelse(category =="vegetables", quantity, 0),
                  cat_fruits = ifelse(category =="fruits", quantity, 0),
                  cat_candy = ifelse(category =="sweet-snacks-candy", quantity, 0), 
                  cat_salad = ifelse(category =="packaged-salads-stir-fries", quantity, 0), 
                  cat_herb = ifelse(category == "fresh-herbs", quantity, 0), 
                  cat_canFruit = ifelse(category =="canned-fruits", quantity, 0), 
                  cat_froFruit = ifelse(category =="frozen-fruits", quantity, 0), 
                  cat_yog = ifelse(category =="yogurt", quantity, 0), 
                  cat_cer = ifelse(category =="cereals", quantity, 0), 
                  cat_canVeg = ifelse(category =="canned-vegetables", quantity, 0), 
                  cat_cheese = ifelse(category =="my-cheese-counter", quantity, 0), 
                  cat_juice = ifelse(category =="juices-drinks", quantity, 0)
                  
                  
                  # package cheese
                  # muffin
                  # chiken
                  #condiments
                  # beef
                  # milk
)
#unique(m$category)[grep("cheese",unique(m$category))]
#> unique(m$category)[grep("cheese",unique(m$category))]
#[1] "packaged-cheese"   "my-cheese-counter"
#> unique(m$category)[grep("muffin",unique(m$category))]
#[1] "muffins-bagels-baked-goods"             "muffins-bagels-baked-goods-gluten-free"
#> unique(m$category)[grep("chi",unique(m$category))]
#[1] "chicken-turkey"             "chilled-desserts-dough"     "chicken-turkey-gluten-free"
#> unique(m$category)[grep("condime",unique(m$category))]
#[1] "condiments-toppings"             "condiments-toppings-gluten-free"
#> unique(m$category)[grep("beef",unique(m$category))]
#[1] "beef-veal"
#> unique(m$category)[grep("milk",unique(m$category))]
#[1] "milk-cream"             "milk-cream-gluten-free"


targetCategories <- data.frame( 
  newName = c("cat_soda",   
              "cat_chips",     
              "cat_water", 
              "cat_frozen",         
              "cat_sausage",    
              "cat_butter",       
              "cat_veggie",       
              "cat_fruits",       
              "cat_candy",         
              "cat_salad",                 
              "cat_herb",     
              "cat_canFruit", 
              "cat_froFruit",  
              "cat_yog", 
              "cat_cer", 
              "cat_canVeg",        
              "cat_cheese",        
              "cat_juice"), 
  category =c("soft-drinks", 
              "salty-snacks", 
              "water",     
              "frozen-meals-sides", 
              "sausages-bacon", 
              "butter-margarine", 
              "vegetables",       
              "fruits",           
              "sweet-snacks-candy", 
              "packaged-salads-stir-fries",
              "fresh-herbs", 
              "canned-fruits",
              "frozen-fruits", 
              "yogurt",  
              "cereals", 
              "canned-vegetables", 
              "my-cheese-counter", 
              "juices-drinks") 
)

### remove 
rm(basket.sample, cards.mtl)
#saveRDS(m, "data_hiroshi/m_longitudinal.rds")
#m<-readRDS("data_hiroshi/m_longitudinal.rds")









###  Prepare wide data --------------------------------------------------------------
# New category for Csd, chip, and everything else to be removed 
#mCategory <- m %>% 
#  mutate(categoryMod = ifelse(category == "salty-snacks", "chips",
#                             ifelse(category == "soft-drinks", "CSD", "other"))) %>% 

# make it to wide (wide spread category, but category list can be in list as in arule data structure, or kept as long)
#mL <- dcast(mCategory, customer + transact + store + total + timestamp ~ categoryMod, value.var = "proportion") %>% 
#  as.data.table()

#targetCategories <- data.frame(
#  newName =      c("cat_soda", "cat_chips", "cat_water", "cat_frozen", "cat_sausage", "cat_butter"),  
#  category = c("soft-drinks", "salty-snacks", "water", "frozen-meals-sides", "sausages-bacon", "butter-margarine")
#)

# To wide table 
mL <- m %>% 
  left_join(targetCategories, by = "category") %>% 
  mutate(newName = ifelse(is.na(newName), "cat_other", as.character(newName))) %>% 
  as.data.table()
mL <- data.table::dcast(mL, customer + transact + store + total + timestamp ~ newName, value.var = "quantity", fun.aggregate = sum) 

# SES
mL <- mL %>% merge(y = loc_mtl, by.x = "customer", by.y = "customer_id",  all.x = TRUE)

# Order by time of transaction 
mL <- mL[order(as.numeric(customer), timestamp)]

# 
#saveRDS(m, "data_hiroshi/m_long_forLongditudinal.rds")


### LAG --------------------------------------------------------------
# Lagged variable 
#nm1 <- c("total", "y", "chips")
#nm2 <- paste("lag", nm1, sep="_")
#mL[, (nm2) := shift(.SD, type='lag'), by = customer, .SDcols=nm1]
#mL[is.na(lag_y), lag_y := 0]

### model data with small # of transactions removed  
# Outcome, chips or not given soda 
#mL[cat_soda > 0, y := ifelse(cat_chips > 0, 1, 0)]
#mL[,y_chips := ifelse(cat_chips > 0, 1, 0)]


# lagged outcome and basket size 
nm1 <- c(grep("cat_.*", colnames(mL), value=TRUE), "total")
nm3 <- paste("lag", nm1, sep=".")
mL[, (nm3) :=  shift(.SD, type = "lag"), by=customer, .SDcols=nm1]

rm(m)




mData <- as.data.frame(mL) 

### data for offset model, no centering of values 
#missingOffset <-  mDataMerged %>% filter(is.na(fittedOffset_GLM))
# unique(missingOffset$customer)


### Store 
mData <- mData %>% 
  #  filter(customer %in% idx) %>% 
  group_by(customer) %>% 
  mutate(store = as.numeric(store)) %>% 
  dplyr::mutate(numTransact = n()) %>% 
  #filter(numTransact > minTransact & numTransact < maxTransact) %>% 
  ungroup()

# link with store SES
# this join will remove 5%of stores outside Mtl 
mData <- mData %>% 
  mutate(store = as.numeric(store)) %>% 
  inner_join(store_ses_buffer, by = "store") %>% 
  select(!(store_uid:geom_store)) %>% 
  select(!(area_3km:open)) 
dim(mData)# 1257873 



# Time variables
holidays <- readRDS("data_hiroshi/holiday.rds")
dim(holidays)
length(complete.cases(holidays))
dim(mData)
mData <- mData %>% 
  mutate(transDate = as.Date(timestamp)) %>% 
  left_join(holidays , by = c("transDate" = "date"))
dim(mData)


# Remove negative count (which might be return of item)
nrow(mData); 
mData %>% filter(cat_other < 0)
mData <-  mData %>% 
  filter_at(vars(starts_with("cat_")), all_vars(.  >= 0))
nrow(mData)




# Make categoreis binary, except "other" that is combined quanities of all other categories , 
catIdx <- grepl("cat_", colnames(mData)) & !grepl("cat_other", colnames(mData))

catName <- colnames(mData)[catIdx]

dim(mData)
 mData %>% mutate(across(all_of(catName), .fns = list(cat = as.numeric))) %>% 
dim()

 # Preseve count variable
mData <- mData %>% 
   mutate_at(vars(contains("cat_")), funs("count" = I(.)))


# Make to binary 
table(mData$cat_other); table(mData$cat_soda)
fc <- function(x) ifelse(x > 0, 1, 0)
mData <- mData %>% 
  dplyr::mutate_at(catName,  function(x) ifelse(x > 0, 1, 0)) 

# check if converted to binary or not 
colMax <- function(data) sapply(data, max, na.rm = TRUE)
mData %>% select(catName) %>% colMax
table(mData$cat_other); table(mData$cat_soda)

###
mData %>%  
  select(contains("cat")) %>% 
  summary(mean = mean)


# Center and scaling varaibles other than binary category of purchasing 
shopperCov <- c("income", "educ", "pfam", "popn", "pimm", "empl",
                "popsecondary", "meanage", "meanfamsize", 
                "total", "lag.total", "cat_other", "lag.cat_other")
storeCov <- c("inch_3km", "educ_3km", "empl_3km", "popn_3km", "immig_3km")




# remove NA ,created due to lag variable or outcome (not ching consequent item )
dim(mData)
mData <- mData[complete.cases(mData), ]
dim(mData)#1254856 






### Subset data  ----------------------------------------------
set.seed(1)
mData$month <- as.factor(months(mData$timestamp))

sampleCustomer <- sample(unique(mData$customer), 10000)
mDataSample <- mData[mData$customer %in% sampleCustomer, ]
mDataSample <- mDataSample %>% arrange(customer, timestamp)

length(unique(mDataSample$customer))
dim(mDataSample)

sampleCustomer <- sample(unique(mData$customer), 3000)
mDataSmallSample <- mData[mData$customer %in% sampleCustomer, ]
mDataSmallSample <- mDataSmallSample %>%  arrange(customer, timestamp)


### Standardize --------------------------

mData<-mData %>% 
  dplyr::mutate_at(shopperCov, ~(scale(., scale=TRUE, center = TRUE) %>% as.vector)) %>% 
  dplyr::mutate_at(storeCov, ~(scale(., scale=TRUE, center = TRUE) %>% as.vector)) 

mDataSample<-mDataSample %>% 
  dplyr::mutate_at(shopperCov, ~(scale(., scale=TRUE, center = TRUE) %>% as.vector)) %>% 
  dplyr::mutate_at(storeCov, ~(scale(., scale=TRUE, center = TRUE) %>% as.vector)) 

mDataSmallSample<-mDataSmallSample %>% 
  dplyr::mutate_at(shopperCov, ~(scale(., scale=TRUE, center = TRUE) %>% as.vector)) %>% 
  dplyr::mutate_at(storeCov, ~(scale(., scale=TRUE, center = TRUE) %>% as.vector)) 

mDataSmallSample %>% 
  dplyr::select(any_of(shopperCov)) %>% 
  sapply(mean)


saveRDS(mData, "data_hiroshi/mData.rds")
saveRDS(mDataSample, "data_hiroshi/mDataSample.rds")
saveRDS(mDataSmallSample, "data_hiroshi/mDataSmallSample.rds")




### GLM ---------------------------------
bool_glm = FALSE
if(bool_glm){
  start_time <- Sys.time()
  fitGlm <- glm(
    formula = cat_chips ~ income*cat_soda + educ*cat_soda + cat_soda + educ + income + 
      pfam + popn + pimm + cat_other + factor(customer) + factor(ctuid),
    data = mDataSample,
    family = binomial(link = "logit"))
  summary(fitGlm)
  end_time <- Sys.time(); start_time - end_time 
}

### GLMM --------------------------------
bool_GLMM = FALSE
if(bool_GLMM){
  start_time <- Sys.time()
  lmerFit<- glmer(cat_chips ~ income*cat_soda + educ*cat_soda + cat_soda + educ + income + 
                    pfam + popn + pimm + other + lag.cat_other + lag.cat_chips + (1 | customer), 
                  data = mDataSample, 
                  family = binomial(link = "logit"))
  end_time <- Sys.time(); start_time - end_time 
  exp(lmerFit@beta)
}


### BRMS and INLA ------------------------------
#https://search.r-project.org/CRAN/refmans/brms/html/car.html
#http://paul-buerkner.github.io/brms/reference/car.html
# Intro:    https://paul-buerkner.github.io/brms/
#https://www.rensvandeschoot.com/tutorials/brms-started/


# INLA - simpson -  https://faculty.washington.edu/jonno/SISMIDmaterial/2-RINLA.pdf
# https://www.flutterbys.com.au/stats/tut/tut12.10.html 
# https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLAfeatures.html
# Longitudinal : http://julianfaraway.github.io/brinla/examples/longitudinal.html
# longitudinal: https://becarioprecario.bitbucket.io/inla-gitbook/ch-multilevel.html#multilevel-models-for-longitudinal-data

# inla longitudinal 
#   https://becarioprecario.bitbucket.io/inla-gitbook/ch-multilevel.html
#inla longitudinal 2 
#   http://julianfaraway.github.io/brinla/examples/longitudinal.html


# Set prior on precision
bool_inla = FALSE
if(bool_inla){
  library(INLA)
  prec.prior <- list(prec = list(param = c(0.001, 0.001)))
  prec.prior <- list(prec = list(param = c(0.01, 0.01)))
  
  # see how to generate matrix for rand efefct 
  Zlt <- as(model.matrix( ~ 0 + ctuid:customer, data = mDataSample), "Matrix")
  system.time(
    inla.fit.sample.ct <- inla(cat_chips ~income*cat_soda + educ*cat_soda + cat_soda + educ + income + other + factor(ctuid) +
                                 f(customer, model = "iid", hyper = prec.prior),
                               data = mDataSample, control.predictor = list(compute = TRUE))
  )
  #num.threads=X
  summary(inla.fit.sample)
  summary(inla.fit)
}

### BRM takes 20 hrs 
bool_brm = FALSE
if(bool_brm){
  iFit <- brm(cat_chips ~ 1 + (1 | customer), 
              data   = mData, 
              warmup = 1000, 
              iter   = 10000, 
              chains = 3, 
              inits  = "random",
              cores  = 3, 
              family = bernoulli(link = "logit")) 
}


