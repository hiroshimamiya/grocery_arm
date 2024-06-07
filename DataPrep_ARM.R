# filename  metro_montreal
# version   5.0
# author    kody crowell
# date      jan 14 2019




rm(list=ls())
# set seed
set.seed(42)

source("loadFunc.R")
######## memory commands
## defrag memory 
#gc()
# save.image(file=".RData")

# load(file=".RData")
# memory.limit()


# save tables/images/data  or not? 
bool_savePlotsFiles <- FALSE
bool_basketDescritive <- FALSE

# descriptive stats? (If bool_savePlotsFiles is TRUE, some of these lines need to be true as well)





# get data
### >>> Basket  -----
cards.in.basket <- dbGetQuery(con, "select customer_id, mem_num::text,
                              member_status, enr_date, postal_code
                              from kodyandrew.cards_in_basket")
###head(cards.in.basket)
###dim(cards.in.basket) - 1462991
### >>> card.move ------ 
### data too large, save locally 
#card.move <- dbGetQuery(con, "select distinct customer_id as customer,
#                        count(distinct postal_code)-1 as changes
#                        from data.cardx
#                        group by customer_id
#                        order by changes;")
if(bool_savePlotsFiles){
#saveRDS(card.move, "data_hiroshi/card.move.rds")
}
card.move <- readRDS("data_hiroshi/card.move.rds")

###table(card.move$changes) ### 19350, 281, 13 for one, two, and three moves
### >>> card.times ----- card month and year 
###card.times <- dbGetQuery(con, "select customer, month, year from kodyandrew.total_card_monthly")
if(bool_savePlotsFiles){
  #saveRDS(card.times, "data_hiroshi/card.times.rds")
}
card.times <- readRDS("data_hiroshi/card.times.rds")

### >>> PCCF ------
pccf <- dbGetQuery(geo, "select distinct pc, dauid from kodyandrew.pccf_clean")
pccf2013 <- dbGetQuery(geo, "select dauid, pc, sli, long, lat from public.pccf2013dec_national")
pccf2010 <- dbGetQuery(geo, "select dauid, pc, sli, long, lat from public.pccf2010dec_national")

### >>> Geographies -----
pc2da <- dbGetQuery(geo, "select * from kodyandrew.pc2da_hybrid") #_v2")

### Hiroshi - find one to one match for pct to da, this is for indetifying area people belong to for rand effect 
pc2da <- pc2da %>% 
  dplyr::group_by(pc) %>% 
  dplyr::arrange(desc(popwt), .by_group = TRUE) %>% 
  filter(row_number()==1 ) %>% 
  select(pc, dauid) %>% dplyr::rename(dauid_largestPopWt = dauid) %>% 
  left_join(pc2da, by = "pc")  %>% 
  ungroup()




mtl2016 <- dbGetQuery(geo, "select dauid, cduid, cdname from public.da2016
                      where cduid = '2466'") # just the island
cma2016 <- dbGetQuery(geo, "select dauid, cduid, cdname from public.da2016
                      where cmauid = '462'") # CMA

cards.missing <- dbGetQuery(con, "select * from kodyandrew.missing_customers;") ###4130

# pc2da mappings specific to montreal / cma
pc2da.mtl <- pc2da[pc2da$dauid %in% mtl2016$dauid,]
pc2da.cma <- pc2da[pc2da$dauid %in% cma2016$dauid,]


### DF flowchar -------------
dfFlow <- data.frame(
  step = c("original"), 
  numID = length(unique(cards.in.basket$customer_id)), 
  nrow=nrow(cards.in.basket)
)


#************************************************************************************
#*
### @Start card in basket Clean   -----------------------------------------------------------------
#************************************************************************************

# edit date information, postal code to caps, create fsa
cards.in.basket$enr_date <- as.Date(cards.in.basket$enr_date, '%Y-%m-%d')
cards.in.basket$postal_code <- toupper(cards.in.basket$postal_code)
cards.in.basket$fsa <- as.character(substr(cards.in.basket$postal_code, 1, 3))

# not in any years of pccf?
idx.pc <- which(cards.in.basket$postal_code %!in% as.character(unlist(pccf$pc))
                & cards.in.basket$postal_code %!in% as.character(unlist(pccf2010$pc))
                & cards.in.basket$postal_code %!in% as.character(unlist(pccf2013$pc)))
n_distinct(cards.in.basket$customer_id[idx.pc]) #check: 2687 cards without pccf matching addr


##### which ones in the list are of invalid form? (444 row / 408 pc / 430 cust)
pcvalid <- grepl("[ABCEGHJKLMNPRSTVXY][0-9]([ABCEGHJKLMNPRSTVWXYZ][0-9]){2}", 
                 cards.in.basket$postal_code[idx.pc])
nrow(cards.in.basket[idx.pc[which(!pcvalid)],]) #check: 444
n_distinct(cards.in.basket$postal_code[idx.pc[which(!pcvalid)]]) # 408 # distinct num pc 
n_distinct(cards.in.basket$customer_id[idx.pc[which(!pcvalid)]]) # 430 #distinct num customer
invalid.pc <- cards.in.basket$postal_code[idx.pc[which(pcvalid)]] ### 2415 


### EXC cards PC  -----
# of those in "valid" form: empirically check this by searching google maps?
# against google's ToS :( 

cards.in.basket <- cards.in.basket[-idx.pc,]  ### 1460132  
dfFlow <- 
rbind(dfFlow, data.frame(
  step = "idx.pc", 
  nrow = nrow(cards.in.basket),  #check: 1460132
  numID = length(unique(cards.in.basket$customer_id))  #check: 1372546
  )
)


### EXC cards time -------------------------------
idx.id <- which(card.times$customer %!in% cards.in.basket$customer_id) #### perchase month outside time period 
card.times <- card.times[-idx.id,] %>%
  mutate(customer = as.character(customer),
         month = as.factor(month),
         year = as.factor(year))

# time index -- find cards only outside of time range
# min date is 2015-01-28 // max date is 2017-10-04
idx.time <- which(card.times$year == 2007 |
                    (card.times$year == 2015 & card.times$month == 1) |
                    (card.times$year == 2017 & card.times$month == 10))

card.in <- unique(card.times$customer[-idx.time]) ### 291179
card.out <- unique(card.times$customer[idx.time]) ### 128640

# omit cards outside of time range
idx.out <- which(cards.in.basket$customer_id %in% card.out[which(card.out %!in% card.in)]) # 318

cards.in.basket <- cards.in.basket[-idx.out,]  ###check: 307897
dfFlow <- 
  rbind(dfFlow, data.frame(
    step = "time - idx.out", 
    nrow = nrow(cards.in.basket),  
    numID = length(unique(cards.in.basket$customer_id)) 
  )
)


### EXC Montreal PC --------------------------------
# limit to montreal
cards.in.basket <- cards.in.basket[cards.in.basket$postal_code %in% pc2da.mtl$pc,]  ### 313429
dfFlow <- 
  rbind(dfFlow, data.frame(
    step = "Mtl: PC = pc2da.mtl$pc", 
    nrow = nrow(cards.in.basket),  
    numID = length(unique(cards.in.basket$customer_id)) 
  )
)


# get rid of cards with postal code changes   ### Hiroshi Change # perhaps keep these people? 
### EXC cards moved ----------------------------
changed <- cards.in.basket %>%
  inner_join(card.move, by=c("customer_id" = "customer")) %>%
  filter(changes > 0) %>%
  dplyr::select(-changes) %>%
  distinct(.)
idx.chg <- unique(changed$customer_id) # 3832 
length(unique(changed$customer_id)) #check 3832

nrow(cards.in.basket)#check: 313429
cards.in.basket <- cards.in.basket %>%   ### Montreal people, not moved 308216
  inner_join(card.move, by=c("customer_id" = "customer")) %>%
  filter(changes == 0) %>%
  dplyr::select(-changes) %>%
  distinct(.)
nrow(cards.in.basket)#check: 308216

dfFlow <- 
  rbind(dfFlow, data.frame(
    step = "moved in city - card.move", 
    nrow = nrow(cards.in.basket),  
    numID = length(unique(cards.in.basket$customer_id)) 
  )
)

### RM
rm(card.move)


# find those not in any pccf year
# not in 2017 -- 840 nrow; (164 pc, 805 customers)
# not in 2013 -- 1 nrow (1 pc, 1 customer)
# not in 2010 -- none!
idx.pc.17 <- which(cards.in.basket$postal_code %!in% pccf$pc) 
idx.pc.13 <- which(cards.in.basket$postal_code %in% pccf2013$pc &
                     cards.in.basket$postal_code %!in% pccf$pc)
idx.pc.10 <- which(cards.in.basket$postal_code %in% pccf2010$pc &
                     cards.in.basket$postal_code %!in% pccf2013$pc &
                     cards.in.basket$postal_code %!in% pccf$pc)

### RM 
rm(pccf); rm(pccf2010); rm(pccf2013)

### EXC cards time2 ------------------------------
# This sectio moved to above


### EXC card zero spend -------------------------------------------
# cards with zero expenditure?
nrow(cards.in.basket)
idx.zero <- which(cards.in.basket$customer_id %in% cards.missing$customer_id)
cards.in.basket <- cards.in.basket[-idx.zero,] #check: 306978 
dfFlow <- 
  rbind(dfFlow, data.frame(
    step = "zero spending - idx.zero from cards.missing", 
    nrow = nrow(cards.in.basket),  
    numID = length(unique(cards.in.basket$customer_id)) 
  )
)



### EXC FREQ outlier transactions -----------------
# basket.sample[basket.sample$spending > 300,]
# basket.sample[basket.sample$total > 1000,]
# basket.sample[basket.sample$total < 0,]
card.monthly <- readRDS("data_hiroshi/card.monthly.rds")
length(unique(card.monthly$customer)) # 290261
length(unique(cards.in.basket$customer_id))




### EXC Infreqnet purchasing

# aggregate visits by month across stores (note that card.monthly data have duplicate month-year if visiting mutiple stores )
freqVisit <- card.monthly %>%
  group_by(customer, month, year) %>%
  dplyr::summarize(visits = sum(visits)) 
#freqVisit %>% arrange(customer, year, month) %>% head()
#card.monthly %>% arrange(customer, year, month) %>% head()

# Person-level totoal # of store visits 
cohortDate <- card.monthly %>%
  group_by(customer) %>%
  mutate(yrmo= zoo::as.yearmon(paste(year, month), "%Y %m")) %>% 
  dplyr::summarize(entry = min(yrmo), 
            exit = max(yrmo), 
            numMonthStoreVisit = n()) #Not used for anything, this is just month count 

freqVisit <- freqVisit %>% 
  left_join(cohortDate) %>% 
  mutate(numCardTime = (as.numeric(exit) - as.numeric(entry))*12+1) %>% # Number of Months: card time 
  group_by(customer) %>% 
  dplyr::mutate(numMonthShp = n()) # number of months store vist was observed 

#freqVisit %>% filter(customer == "1003326") %>% data.frame()

# who shopped at least half of months during at-risk time? 
idx.customer.freq <- freqVisit %>% 
  filter(numMonthShp >  numCardTime*0.5) %>% 
  select(customer) %>% 
  distinct() %>% 
  ungroup()

# total card number in Mtl 
length(unique(cards.in.basket$customer_id))
# card number who whopped at least half months among all months under card time 
length(unique(idx.customer.freq$customer))


cards.in.basket <- cards.in.basket %>% 
  inner_join(idx.customer.freq, by= c("customer_id" = "customer"))

dfFlow <- 
  rbind(dfFlow, data.frame(
    step = "infrequent shopping", 
    nrow = nrow(cards.in.basket),  
    numID = length(unique(cards.in.basket$customer_id)) 
  )
)

### Number of visits each month
#summary(freqVisit$visits)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00    2.00    4.00    5.18    7.00  104.00

### EXC optional, identify freq shopper for sensitivity analysis -----------------------





# by Kody --how many are a frequent shopper?
#freq.50.1 <- card.monthly %>%
#  group_by(customer) %>%
#  filter(min(total) > 50 &
#           min(visits) > 1) %>%
#  dplyr::select(customer) %>%
#  distinct(.) # 2914

### !!!! NOTE Hiroshi ----
# why frequ shopper aggregated above in original analysis are filtered based all months (disaggregated) 
# As well, monthly spening of ottal 50 appears small, and monthly visit of 1 month is rather restrctive
# is also does not allow people who doe not spend 1 month , 
# how many are a frequent shopper based on more loose definition (not aggregated by customer)

#freq.50.1.anyMonth <- card.monthly %>%
#  filter(total > 50 & visits > 1) %>%
#  dplyr::select(customer) %>%
#  distinct(.) # 2914
### dim(freq.50.1.anyMonth) # 248200

### 287 cases, no 151 why? thoug the number i sstill too small 
#length(which(cards.sample %in% freq.50.1$customer)) # 151

### <<< SAVE cards.in.basket  cleaned ---------
if(bool_savePlotsFiles){
  saveRDS(cards.in.basket, "data_hiroshi/cards.in.basket_clean_flowchart.rds")
}
### CARD.Mtl wtih ovelapping DA----------------

# link to cards in basket, drop un-needed info which contributes to count
nrow(cards.in.basket) #Check originally 306978, now  267216
length(unique(cards.in.basket$customer_id)) #251246

cards.in.basket %>% 
  #dplyr::select(-c(mem_num, member_status, enr_date, fsa)) %>% 
  dplyr::select(-c(mem_num, member_status, enr_date, fsa)) %>%  
  distinct() %>% 
  nrow() 
 # note that customer_ID is reqpted in some cases (but mem_num is unique)

# who is duplicated ?? 
length(unique(cards.in.basket$customer_id)) ###check 290261;
length(unique(cards.in.basket$mem_num)) ###check 306978;

# ahoq duplicated customer ID 
dup_id <- cards.in.basket %>% 
  group_by(customer_id) %>% 
  filter(n()>1) %>% 
  arrange(customer_id) 
# duplicated one always have one false and one true member status? 
table(dup_id$member_status) #check 16717 15819 
#how many have different FSA addr? 
dup_id %>% 
  group_by(customer_id, postal_code) %>% 
  dplyr::mutate(count = n()) %>% 
  filter(count != 2) %>% 
  nrow() #check: 2552

### There insnt really a need to make this cards.mtl out of basket.card, but
### it is just distinct form (aggreagted by card id, which were previously duplicated by card states)
length(unique(cards.in.basket$customer_id)) #251246

cards.mtl <- cards.in.basket %>% 
  dplyr::select(-c(mem_num, member_status, enr_date, fsa)) %>%
  left_join(dplyr::select(pc2da.mtl, pc, dauid, weight, popwt,dauid_largestPopWt), 
            by=c('postal_code'='pc')) %>%   ### without distinct, replcited due to PC, 317295
  distinct(.)    #####259586
nrow(cards.mtl) #check: 259586
length(unique(cards.mtl$customer_id)) #check: 251246

# duplicated due to PC multi-overlap with DA
dup_id <- cards.mtl %>% 
  group_by(customer_id) %>% 
  filter(n()>1) %>% 
  arrange(customer_id) 

cards.mtl %>% 
  inner_join(dup_id) %>% 
  head(10)

### @ start card montly -----
## 94.32% true in mtl basket
## 90.46% true outside of basket (customer_id nin cards_in_basket_clean)
length(which(cards.in.basket$member_status == TRUE))/nrow(cards.in.basket)

rm(cards.in.basket)


# note save - Hiroshi #dbWriteTable(con, c("kodyandrew", "cards_mtl"), value=cards.mtl[,1:2], row.names=FALSE)
### <<< WRITE cards.mtl --------------------

if(bool_savePlotsFiles){
  dbWriteTable(con, c("kodyandrew", "cards_mtl_hiroshi_flowchart"), value=cards.mtl, row.names=FALSE, overwrite = TRUE)
}


#************************************************************************************

### @END cards in basket clean -------------

#************************************************************************************
### RM
rm(pc2da.cma)


### SAMPLE --------------------------------------- 
cards.sample <- sample(unique(cards.mtl$customer_id), size=15000)






#the latter one includes matching DA to Pt by largest area overlap 

# for cma need to evaluate da.nin.census, not in unreleased stats can data 
# with unmatched pop wts either reserves or those not in qc

### demographics table and map using card.monthly  --------------------------------------------

#stores <- read.csv("/home/kodyandrew/metro/data_hiroshi/metro_stores.csv", header=T)
### >>>load stores ---------- 
stores <- read.csv("data/metro_stores.csv", header=T)
### No long and lag, this is generated by metro_sotre_clean.R
### >>>load distainces ---------
distances <- dbGetQuery(geo, "select * from kodyandrew.store_da_proximity;")

### >>>> location - census 
pc2se <- RPostgres::dbGetQuery(geo, "select * from kodyandrew.pc2se2")
da2se <- RPostgres::dbGetQuery(geo, "select * from kodyandrew.da2se;")
da2se_hiroshi <- RPostgres::dbGetQuery(geoH, 
  "SELECT geouid, dauid, 
  median_income AS medIncome, 
  prop_post_secondary AS popSecondary, 
  count_pop AS countPop,
  count_dwell AS countDwell, 
  mean_age AS meanAge, 
  mean_famsize AS meanFamsize
  FROM hiroshi.yuma_da_ses_2016 where geouid is not null;
  ;")
census2016 <- RPostgres::dbGetQuery(geo, "select * from kodyandrew.census2016")

# Attach additional ses variables of mine 
da2se <- da2se %>% 
  left_join(da2se_hiroshi)

# Attach da-level SES to pc2da to get pc to ses adding my custom ses for mtl 
# this can be used to attced to any pc, as the df is already aggregaed by pc 
pc2da2sesMtl <-pc2da %>%  
  filter(pc %in% pc2da.mtl$pc) %>% 
  left_join(select(da2se, geouid, popn, income, empl, educ, pfam, inch, pc, pmar, phsd, pimm, 
    popsecondary, countpop, countdwell, meanage, meanfamsize, medincome), 
  by=c('dauid'='geouid', "pc" = "pc"))
dim(pc2da); dim(pc2da2sesMtl)

# aggregated by PC by using weights 
pc2da2sesAggMtl <- 
pc2da2sesMtl %>% 
  group_by(pc) %>% 
  dplyr::summarize_at(vars(popn:medincome), 
                      ~wtd.mean(.x, weights=popwt))
if(bool_savePlotsFiles){
  saveRDS(pc2da2sesAggMtl, "data_hiroshi/pc2da2sesAggMtl.rds")
}

### check with original pc2se 
pc2se %>%  filter(pc %in% c("HA1A0", "G0A1C0"))
pc2da2sesAggMtl %>%  filter(pc %in% c("G0A1A0", "G0A1C0"))

pc2da2sesAggMtl %>% head() 
pcc <- pc2da2sesAggMtl %>% head() %>% select(pc)
pc2se %>% inner_join(pcc)

### Card montly spending selected based on the card_mtl above
### This is different from card.monthly loaded above, as it is attached to total 
### Attach total spending to card.montlhy 

### >>> READ card montly --------------------
card.monthly <- dbGetQuery(con, "select customer, store_id as store, month, year,
                           n_visits as visits, total_spent::numeric as total 
                           from kodyandrew.total_card_monthly 
                           where customer in (select distinct customer_id from 
                           kodyandrew.cards_mtl_hiroshi_flowchart);")
if(bool_savePlotsFiles){
  #saveRDS(card.monthly, "data_hiroshi/card.monthly_flowchart.rds")
}
  #card.monthly <- readRDS("data_hiroshi/card.monthly_flowchart.rds")



# pad store id with zeroes
stores$uid <- as.character(stores$uid)
stores$store <- str_pad(stores$store, 6, pad="0")
card.monthly$store <- str_pad(card.monthly$store, 6, pad = "0")
distances$uid <- as.character(distances$uid)
distances$dauid <- as.character(distances$dauid)

# get indices of cards not in monthly db (all stores accounted for)
all(cards.mtl$customer_id %in% card.monthly$customer) # all in!
idx.nin <- which(card.monthly$customer %!in% cards.mtl$customer_id)

card.monthly <- card.monthly %>%
  mutate(customer = as.character(customer),
         # card_id = as.character(card_id),
         store = as.character(store),
         month = as.factor(month),
         year = as.factor(year))
#???  why use 2007?
# create new time variable, omit observations outside of time range
card.monthly$time <- with(card.monthly, interaction(year, month))
idx.time <- which(card.monthly$year == 2007 |
                    (card.monthly$year == 2015 & card.monthly$month == 1) |
                    (card.monthly$year == 2017 & card.monthly$month == 10))


### EXC card monthly time------
card.monthly <- card.monthly[-idx.time,] %>%
  mutate(year = factor(year),
         time = factor(time))

dim(card.monthly) #7534593  # not it is 7223083
# complete dataset according to time -- each should have 32 observation times
card.monthly <- card.monthly %>% 
  dplyr::select(-c(month, year)) %>%
  complete(nesting(customer, store), time, fill=list(visits=0, total=0)) 
dim(card.monthly) # 31462737         5

### <<< WRITE Card montly ---------------------
### Save card.montly after limiting date and generating explicit missingness 
if(bool_savePlotsFiles){
  dbWriteTable(con, c("kodyandrew", "card_monthly_hiroshi_flowchart"), card.monthly,  row.names=FALSE, overwrite = TRUE  )
}
### remove this very sparse card data (implicit missing tasaction in months )
rm(card.monthly)

### @END card monthly ---- 






#************************************************************************************
### @ Stats DEMOG -------------------------------------------------------------------
#****************************

bool_stats_DEMOG <- FALSE

if(bool_stats_DEMOG){
  # calculate mtl island average for statistics
  mtl.stats <- cards.mtl %>% 
    select(dauid) %>%
    left_join(select(da2se, geouid, 
                     popn, income, empl, educ, pfam, pmar, phsd, pimm, inch, 
                     popsecondary, countpop, countdwell, meanage, meanfamsize, medincome), 
              by=c('dauid'='geouid')) %>%
    mutate(educ = educ*100,
           pfam = pfam*100,
           phsd = phsd*100,
           pimm = pimm*100,
           pmar = pmar*100, 
           popn=popn, 
           popsecondary = popsecondary*100, 
           countpop = countpop, 
           countdwell = countdwell, 
           meanage = meanage, 
           meanfamsize = meanfamsize, 
           medincome = medincome, 
           inch = inch) %>%
    distinct(.)
  
  # of area to be suppressed, usually from income (remove these areas)
  mtl.stats %>% filter(inch == 0 | educ == 0 | pfam ==0 | pimm ==0 | phsd == 0 | income == 0) # 44 areas 
  mtl.stats <- mtl.stats %>%  filter(inch != 0 & educ != 0 & pfam !=0 & pimm != 0 & phsd != 0 & income != 0)
  
  demog.mtl <- mtl.stats %>%
    dplyr::summarise_at(c('popn', 'income', 'empl',
                          'educ', 'pfam', 'pmar', 'phsd', 'pimm', 'inch', 
                          'popsecondary', 'countpop', 'countdwell', 'meanage', 'meanfamsize', 'medincome'
                          ),
                        funs(`AVG`=mean(., na.rm=T),
                             `SDEV`=sd(., na.rm=T),
                             `Q1`=quantile(., probs=0.25, na.rm=T),
                             `Q2`=quantile(., probs=0.50, na.rm=T),
                             `Q3`=quantile(., probs=0.75, na.rm=T),
                             `MIN`=min(.),
                             `MAX`=max(.)))
  
    tableOneSum <- as_tibble(cbind(nms = names(demog.mtl), t(demog.mtl)))
    names(tableOneSum) <- c('VAR', 'VALUE')
    tableOneSum <- makeTbl(tableOneSum)

  

  
  ### cadholders demog calcualtion ****************************************
  # include distance as a variable?
  # using ncard or popwt? ncard leads to duplicates
  #??? there are multiple da in a single postal code? H1V3H4 24662058, 24663328
  cohort <- cards.mtl %>%
    group_by(dauid) %>%
    dplyr::summarise(ncard = n_distinct(customer_id),
                     ncard.wtd = sum(popwt)) %>%
    left_join(select(da2se, geouid, popn, income, empl, educ, pfam, 
                     pmar, phsd, pimm, inch, 
                     popsecondary, countpop, countdwell, meanage, meanfamsize, medincome
                     ), by=c('dauid'='geouid')) %>%
    mutate(educ = educ*100,
           pfam = pfam*100,
           phsd = phsd*100,
           pimm = pimm*100,
           pmar = pmar*100, 
           popsecondary = popsecondary*100, 
           inch = inch, 
           income = income) %>%
    mutate(prop = ifelse(popn != 0, ncard/popn, 0),
           prop.wtd = ifelse(popn != 0, ncard.wtd/popn, 0)) %>%
    distinct(.) %>%
    # left_join(distances, by=c('dauid'="dauid", "uid"="uid")) %>%
    # mutate(dist = distance/1000) %>%
    # select(-distance) %>%
    mutate_all(funs(replace(., is.na(.), 0)))
  
  head(cohort)
  cohort %>% filter(inch ==0)
  cohort <- cohort %>% filter(inch != 0 & educ != 0 & pfam !=0 & pimm != 0)
  
  ### RM
  rm(distances)
  
  # for now, set all props > 1 to 1
  cohort[which(cohort$prop > 1),]
  cohort$prop[which(cohort$prop > 1)] <- 1
  
  cohort[which(cohort$prop.wtd > 1),]
  cohort$prop.wtd[which(cohort$prop.wtd > 1)] <- 1
  
  # mean, sd, cv, skewness, kurtosis, min, max, percentiles (iqr?)
  # cardwt required normwt=T in wtd calculation, default assumes repeats
  demographics <- cohort %>% 
    # distinct(dauid, .keep_all = TRUE) %>%
    mutate(prop = prop*100,
           prop.wtd = prop.wtd*100) %>%
    dplyr::summarise_at(c('popn', 'prop', 'prop.wtd', 'income', 'empl',
                          'educ', 'pfam', 'pmar', 'phsd', 'pimm', 'inch', 
                          'popsecondary', 'countpop', 'countdwell', 'meanage', 'meanfamsize', 'medincome'),
                        funs(`AVG`=wtd.mean(., weights=ncard.wtd),
                             `SDEV`=sqrt(wtd.var(., weights=ncard.wtd)),
                             `Q1`=wtd.quantile(., weights=ncard.wtd, probs=0.25),
                             `Q2`=wtd.quantile(., weights=ncard.wtd, probs=0.50),
                             `Q3`=wtd.quantile(., weights=ncard.wtd, probs=0.75),
                             `MIN`=min(.),
                             `MAX`=max(.)))
  ### Demographic Table ----
  # create tibble tables
  tableOne <- as_tibble(cbind(nms = names(demographics), t(demographics)))
  names(tableOne) <- c('VAR', 'VALUE')
  tableOne <- makeTbl(tableOne)

  # SMPL2, ses directly taken from PC 
  # sample cards and check demographics from sampled cards above
  smpl <- cards.mtl %>%
    filter(customer_id %in% cards.sample) %>% 
    group_by(dauid) %>%
    dplyr::summarise(ncard = n_distinct(customer_id),
                     ncard.wtd = sum(popwt)) %>%
    left_join(select(da2se, geouid, popn, income, empl, educ, pfam, inch,  
                     pmar, phsd, pimm, 
                     popsecondary, countpop, countdwell, meanage, meanfamsize, medincome), 
              by=c('dauid'='geouid')) %>%
    mutate(prop = ifelse(popn != 0, ncard/popn, 0),
           prop.wtd = ifelse(popn != 0, ncard.wtd/popn, 0)) %>%
    mutate(educ = educ*100,
           pfam = pfam*100,
           phsd = phsd*100,
           pimm = pimm*100,
           pmar = pmar*100, 
           income = income, 
           inch = inch) %>%
    distinct(.) %>% 
    mutate_all(funs(replace(., is.na(.), 0)))


  # for now, set all props > 1 to 1
  length(which(smpl$prop > 1))
  smpl$prop[which(smpl$prop > 1)] <- 1
  
  length(which(smpl$prop.wtd > 1))
  smpl$prop.wtd[which(smpl$prop.wtd > 1)] <- 1
  
  ### removed suppressed areas 
  smpl %>% filter(inch == 0)
  nrow(smpl) #2683
  smpl <- smpl %>% filter(inch != 0 & educ != 0 & pfam !=0 & pimm != 0)
  nrow(smpl) # 2648 areas 

  # mean, sd, cv, skewness, kurtosis, min, max, percentiles (iqr?)
  # cardwt required normwt=T in wtd calculation, default assumes repeats
  demog.smpl <- smpl %>% 
    mutate(prop = prop*100,
           prop.wtd = prop.wtd*100) %>%
    dplyr::summarise_at(c('popn', 'prop', 'prop.wtd', 'income', 'empl',
                          'educ', 'pfam', 'pmar', 'phsd', 'pimm', 'inch', 
                          'popsecondary', 'countpop', 'countdwell', 'meanage', 'meanfamsize', 'medincome'),
                        funs(`AVG`=wtd.mean(., weights=ncard),
                             `SDEV`=sqrt(wtd.var(., weights=ncard)),
                             `Q1`=wtd.quantile(., weights=ncard, probs=0.25),
                             `Q2`=wtd.quantile(., weights=ncard, probs=0.50),
                             `Q3`=wtd.quantile(., weights=ncard, probs=0.75),
                             `MIN`=min(.),
                             `MAX`=max(.)))

  ### make sample demongraph table -----
  tableSample <- as_tibble(cbind(nms = names(demog.smpl), t(demog.smpl)))
  names(tableSample) <- c('VAR', 'VALUE')
  tableSample <- makeTbl(tableSample)
  
  
  smpl2 <- cards.mtl %>% 
    filter(customer_id %in% cards.sample) %>%  # 15505
    distinct(customer_id, postal_code) %>% #15000
      left_join(select(pc2da2sesAggMtl, pc, popn, income, empl, educ, pfam, inch,  
                     pmar, phsd, pimm, 
                     popsecondary, countpop, countdwell, meanage, meanfamsize, medincome), 
              by=c('postal_code'='pc')) %>% #15000
    dplyr::summarise_at(c('income', 'empl','educ', 'pimm', 'inch', 
                          'popsecondary', 'countpop', 'countdwell', 'meanage', 'meanfamsize', 'medincome'),
                        funs(`Q1`=quantile(., probs=0.25, na.rm = TRUE),
                             `Median`=quantile(., probs=0.50, na.rm = TRUE),
                             `Q3`=quantile(.,  probs=0.75, na.rm = TRUE),
                             )
                        )

  tableSample <- as_tibble(cbind(nms = names(smpl2), t(smpl2)))
  names(tableSample) <- c('VAR', 'VALUE')
  tableSample <- makeTbl(tableSample)
  as.data.frame(t(tableSample))
    mutate_all(funs(replace(., is.na(.), 0)))


  ### Finally, store buffer area ----------------------------
  drv <- dbDriver("PostgreSQL")
  geoH <- dbConnect(drv, dbname="geo", host="132.216.183.3", 
                   user="hiroshi", pass="*******")
  storeSES <- dbGetQuery(geoH, 
    "select distinct store_postal_codes,
      overlap_3km_cropped,
      overlap_5km_cropped,
      inch_3km, inch_5km, empl_3km, empl_5km,
      sparent_3km, sparent_5km,
      phsd_3km, phsd_5km,
      immig_3km, immig_5km,
      popn_3km, popn_5km,
      educ_3km, educ_5km,
      income_3km, income_5km,
      median_income_3km, median_income_5km,
      prop_post_secondary_3km, prop_post_secondary_5km,
      count_dwell_3km, count_dwell_5km,
      mean_age_3km, mean_age_5km,
      mean_famsize_3km, mean_famsize_5km
      FROM hiroshi.hiroshi_metrostore_ses_buffer_storeid;")
  
  storeSES$educ_3km <- storeSES$educ_3km*100
  storeSES$educ_5km <- storeSES$educ_5km*100
  storeSES$immig_3km <- storeSES$immig_3km*100
  storeSES$immig_5km <- storeSES$immig_5km*100
  
  
  storeBuffer <- dbGetQuery(geoH, 
    "select * from hiroshi_metro_da_store_weights")
  
  
  
  #### sample SES 
  # 3km, weight by popualtion 
  tableStore <- storeSES %>% 
    dplyr::summarise_at(vars(ends_with("3km")),
                        funs(`AVG`=wtd.mean(., weights=popn_3km),
                             `Q1`=wtd.quantile(., weights=popn_3km, probs=0.25),
                             `Q2`=wtd.quantile(., weights=popn_3km, probs=0.50),
                             `Q3`=wtd.quantile(., weights=popn_3km, probs=0.75),
                             IQR =paste(round(wtd.quantile(., weights=popn_3km,probs=0.25),2),
                                        round(wtd.quantile(., weights=popn_3km,probs=0.75),2),
                                        sep = " - "))) %>% 
    rename_with(~ gsub('3km', '', .x))
  
  # 5km, weight by popualtion 
  tableStore <- rbind(tableStore, 
    storeSES %>% 
      dplyr::summarise_at(vars(ends_with("5km")),
                          funs(`AVG`=mean(.),
                               `Q1`=quantile(., probs=0.25),
                               `Q2`=quantile(., probs=0.50),
                               `Q3`=quantile(., probs=0.75),
                               IQR =paste(round(wtd.quantile(., weights=popn_5km,probs=0.25),2),
                                          round(wtd.quantile(., weights=popn_5km,probs=0.75),2),
                                          sep = " - "))) %>% 
      rename_with(~ gsub('5km', '', .x))
    )
  
  # 3km no wt
  tableStore <- rbind(tableStore, 
                      storeSES %>% 
                        dplyr::summarise_at(vars(ends_with("3km")),
                                            funs(`AVG`=mean(.),
                                                 `Q1`=quantile(., probs=0.25),
                                                 `Q2`=quantile(., probs=0.50),
                                                 `Q3`=quantile(., probs=0.75), 
                                                 IQR =paste(round(quantile(., probs=0.25),2),
                                                            round(quantile(., probs=0.75),2),
                                                            sep = " - "
                                                            )) 
                                            ) %>% 
                        rename_with(~ gsub('3km', '', .x))
  )
  # 5km, no wt 
  tableStore <- rbind(tableStore, 
                      storeSES %>% 
                        dplyr::summarise_at(vars(ends_with("5km")),
                                            funs(`AVG`=mean(.),
                                                 `Q1`=quantile(., probs=0.25),
                                                 `Q2`=quantile(., probs=0.50),
                                                 `Q3`=quantile(., probs=0.75), 
                                                 IQR =paste(round(quantile(., probs=0.25),2),
                                                            round(quantile(., probs=0.75),2),
                                                            sep = " - "
                                                 )) 
                                            
                                            ) %>% 
                        rename_with(~ gsub('5km', '', .x))
  )
  
  
  
  ### make sample demongraph table --------------------------------------------
  tableStoreC <- as_tibble(cbind(nms = names(tableStore), t(tableStore)))
  colnames(tableStoreC) <- c('VAR', '3km_wt', '5km_wt', '3km', '5km')
  rownames(tableStoreC) <- tableStoreC$VAR  
  
  tableStoreC <- tableStoreC[c(
      "educ__Q2", 
      "educ__IQR", 
      "inch__Q2", 
      "inch__IQR", 
      "empl__Q2", 
      "empl__IQR", 
      "immig__Q2", 
      "immig__IQR", 
      "popn__Q2", 
      "popn__IQR", 
      "mean_famsize__Q2", 
      "mean_famsize__IQR", 
      "mean_age__Q2", 
      "mean_age__IQR"
      ),]
  
  tableStoreFormatted <- tableStoreC %>% 
    filter(!grepl("IQR",VAR)) %>% 
    dplyr::rename_all(function(x){paste0("Median", x)})%>%
    dplyr::mutate_at(vars(matches("km")), function(x) round(as.numeric(x),2)) %>% 
    cbind(tableStoreC %>% filter(grepl("IQR",VAR)) %>%  rename_all(function(x){paste0("IQR", x)})) 
    
  
  
  ft <- flextable(data = tableStoreFormatted) %>% 
    theme_booktabs() %>% 
    autofit
  
  tmp <- tempfile(fileext = ".docx")
  read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
  browseURL(tmp)
  
  
  
  
  ###      # of areas included and excluded 
  length(unique(storeBuffer$dauid))
  storeBuffer <- storeBuffer %>% 
    filter(inch != 0 & educ != 0 & pfam !=0 & pimm != 0)
  length(unique(storeBuffer$dauid))
  
  storeBuffer %>% 
    filter(overlap_3km != 0) %>% 
    distinct(dauid) %>% 
    nrow() #2798
  
  storeBuffer %>% 
    filter(overlap_5km != 0) %>% 
    distinct(dauid) %>% 
    nrow() #3082
  

  
  
  ### <<<TABLE descritive 
  funcTable <- function(tb){
    tb <- tb %>%
      dplyr::select(STATISTIC, educ, income, empl, inch, pimm, pfam, phsd, pmar, popn, 
                    meanage, meanfamsize
                    ) %>%
      filter(STATISTIC %in% c("Q2", "Q1", "Q3")) %>%  
      t() %>% data.frame() 
    colnames(tb) <- c("Q1", "Q2", "Q3")  
    tb$IQR = paste(tb$Q1, tb$Q3, sep = " - ")
    tb %>% mutate(median = Q2) %>% select(median, IQR)
  }
  
  tableSampleC <- cbind(
    funcTable(tableSample),
    funcTable(tableOne),
    funcTable(tableOneSum)
  )
  
  
  
  colnames(tableSampleC) <- paste(rep(c("sampleCard", "allCard", "allDA"),  each = 2), colnames(tableSampleC), sep = "\n")
  tableSampleC <- tableSampleC %>%  
    mutate(VAR = rownames(tableSampleC)) %>% 
    relocate(VAR)
  
  
  ft <- flextable(data = tableSampleC) %>% 
    theme_zebra %>% 
    autofit
  
  # Create a temp file
  tmp <- tempfile(fileext = ".docx")
  # Create a docx file
  read_docx() %>% 
    body_add_flextable(ft) %>% 
    print(target = tmp)
  
  
  # write to csv
  if(bool_savePlotsFiles){
    write.table (tableStoreC, "data_hiroshi/table_one_store_mtl.csv", sep=",", row.names=F)
    write.table (tableOne,    "data_hiroshi/table_one_mtl.csv", sep=",", row.names=F)
    write.table (tableOneSum, "data_hiroshi/table_one_mtl_sum.csv", sep=",", row.names=F)
    write.table(tableSample,  "data_hiroshi/table_smp_mtl.csv", sep=",", row.names=F)
    write.table(tableSampleC, "data_hiroshi/table_one_combined.csv", sep=",", row.names=F)
  }
} # bool_stats_DEMOG












### @ END STATS DEMOC ------------------


rm(qcda); rm(qc.da); 
rm(map.qcda); rm(map.mtlda); 
rm(map.da.nin.card)



### @ START basket (sample)--------------------------------------------------------------
# upload to database, use postgres to get basket composition
### <<< WRITE card_sample ------------------------
if(bool_savePlotsFiles){
  dbWriteTable(con, "card_sample_flowchart", cards.mtl[cards.mtl$customer_id %in% cards.sample,], overwrite=T, row.names=F) #append=T)
}

# the query above takes like 30 min, so save and
### >>> basket and upc data --------------------
### uncommented as the data is too large 
if(exists("notrun")){
  basket.sample <- dbGetQuery(con, "select b.customer, b.store, b.timestamp, 
    b.register_lane as register, b.transaction, b.upc, 
    b.department, b.quantity::numeric, b.spending::numeric, t.total::numeric, b.promotion
    from customer_baskets_mtl b
    inner join kodyandrew.basket_totals t on
    b.store = t.store and
    b.register_lane = t.register_lane and
    b.transaction = t.transaction and
    b.timestamp = t.timestamp
    where b.customer in (select customer_id from card_sample_flowchart);")
  
  #basket.sample <- dbGetQuery(con, "select * from kodyandrew.basket_sample_flowchart;")
  if(bool_savePlotsFiles){
  #saveRDS(basket.sample, "data_hiroshi/basket.sample_flowchart.rds")
  }
  ################################################## >>>>>>>>>>>----
}


# the query above takes like 30 min, so save and
### >>> basket and upc data --------------------
basket.sample <- readRDS("data_hiroshi/basket.sample_flowchart.rds")

library(data.table)
setDT(basket.sample)


### for some reason, store colum is duplicated...remove 
#basket.sample <- basket.sample[ , !duplicated(colnames(basket.sample))] 


#### statistics (update after dropping time)
# nrow             14663533 (99.2%)
# ntransactions    1508043
# ncustomer        15000

# convert string <NA> to proper null
basket.sample$upc[basket.sample$upc == '<NA>'] <- NA
basket.sample$department[basket.sample$department == '<NA>'] <- NA


### EXC basket sample time ------------
summary(basket.sample$timestamp)

# create date variable
basket.sample$date <- format(as.Date(basket.sample$timestamp), "%Y-%m")

# create transaction variable
basket.sample$transact <-  group_indices(basket.sample, store, transaction, timestamp, register)

dfFlow <- 
  rbind(dfFlow, data.frame(
    step = "DB_basket.sample", 
    nrow = length(unique(basket.sample$transact)),  
    numID = length(unique(basket.sample$customer)) 
  )
  )

### 2FREQ outlier transactions -----------------







### UPC --------------------
### >>> UPC class ------
upc.class.full <- dbGetQuery(con, "select * from kodyandrew.metro_upc_class_full;")
upc.class <- dbGetQuery(con, "select * from kodyandrew.metro_upc_class;")

if(bool_savePlotsFiles){
  ###saveRDS(customer.freq, "data_hiroshi/customer.freq.rds")
}
#customer.freq <- readRDS("data_hiroshi/customer.freq.rds")

basket.sample <-  basket.sample %>%
  left_join(upc.class %>% dplyr::select(upc, category, food), by="upc")


### EXC large transaction 
### extreme number of items 
idx.transact.excessItem <- 
  basket.sample %>% 
  group_by(customer, transact) %>% 
  dplyr::summarise(nitm = sum(quantity)) %>% 
  filter(nitm < 150) %>% 
  select(customer, transact, nitm)

basket.sample <- basket.sample %>% 
  filter(transact %in% idx.transact.excessItem$transact)

dfFlow <- 
  rbind(dfFlow, data.frame(
    step = "basket.sample_tooManyItems", 
    nrow = length(unique(basket.sample$transact)),  
    numID = length(unique(basket.sample$transaction)) 
  )
)

# Table Transaction summary for category, department, upc and total ##################################################
# get transaction details
### Make sure that transaction is actually not duplicted in the same day 

basket.sample.monthly.extreme <- basket.sample[, .(countTransact = length(unique(transact))), by=list(customer, date)] 
nrow(basket.sample.monthly.extreme)
basket.sample.monthly.extreme <- basket.sample.monthly.extreme %>%  
  filter(countTransact < 40) 
nrow(basket.sample.monthly.extreme)
# 220 cardholder-month 
#nrow(basket.sample.monthly.extreme)
#sum(basket.sample.monthly.extreme$countTransact)
# 10508
#length(unique(basket.sample.monthly.extreme$customer))
#43

basket.sample <- basket.sample[basket.sample.monthly.extreme, ,on = c(customer = "customer", date = "date")]

dfFlow <- 
  rbind(dfFlow, data.frame(
    step = "basket.sample_tooManyTrans", 
    nrow = length(unique(basket.sample$transact)),  
    numID = length(unique(basket.sample$transaction)) 
  )
)


### This one has note removed non-food items 
start_time <- Sys.time()
basket.stats <- basket.sample %>%
  group_by(customer, transact) %>%
  dplyr::summarise(n = n(),
                   nitm = sum(quantity),
                   nupc = n_distinct(upc),
                   ndep = n_distinct(department),
                   ncat = n_distinct(category),
                   nna = n_distinct(is.na(category)),
                   total = unique(total))
if(bool_savePlotsFiles){
  saveRDS(basket.stats, "data_hiroshi/basket.stats_flowchart.rds")
}
  basket.stats <- readRDS("data_hiroshi/basket.stats_flowchart.rds")

# create details of shop table, create plot
# link to products table, get prices etc...ssb / junk food
basket.details <- basket.stats %>%
  ungroup %>%
  dplyr::summarise_at(c('n', 'nitm', 'nupc', 'ndep', 'ncat', 'nna', 'total'),
                      funs(`AVG`=mean(.),
                           `SDEV`=sqrt(var(.)),
                           `CIL`=quantile(., probs=0.025),
                           `CIU`=quantile(., probs=0.975),
                           `Q1`=quantile(., probs=0.25),
                           `Q2`=quantile(., probs=0.50),
                           `Q3`=quantile(., probs=0.75),
                           `MIN`=min(.),
                           `MAX`=max(.)))

tableShopDetails <- as_tibble(cbind(nms = names(basket.details), t(basket.details)))
names(tableShopDetails) <- c('VAR', 'VALUE')
tableShopDetails <- makeTbl(tableShopDetails)

### write
if(bool_savePlotsFiles){
  write.table(tableShopDetails, "data_hiroshi/table_shop_mtl_flowchart.csv", sep=",", row.names=F)
}

### remove 
rm(basket.stats, basket.details)











### MINI basket creation ----
### >>> mini load create mini basket to save space -----
# 14699269

mini <- basket.sample %>%
  mutate(category = ifelse(is.na(upc), "dept-specific-bottle", category)) %>%
  mutate(category = ifelse(food == 0, "non-food-item", category)) %>%
  mutate(category = ifelse(is.na(category), "null", category)) %>%
  filter(category %!in% c('dept-specific-bottle', 'null', 'non-food-item')) %>%
  select(customer, transact, upc, spending, total, category, timestamp, quantity, store) %>%
  left_join(upc.class.full %>%  dplyr::select(-category), by="upc") %>%
  distinct(.) # mutate not working??? check and manually recode
nrow(mini)

# calculate proportion
# deal with 0 total, spending < 0 and spending > total
mini$proportion <- ifelse(mini$total == 0, 0, 
                          ifelse(mini$spending < 0, 0,
                                 ifelse(mini$total < mini$spending, 1,
                                        abs(mini$spending)/abs(mini$total))))

### <<< SAVE mini-------------
if(bool_savePlotsFiles){
  saveRDS(mini, "data_hiroshi/mini_flowchart.rds")
}
#mini <- readRDS("data_hiroshi/mini_flowchart.rds")

# create mini baskets based on transacts 
dfFlow <- 
  rbind(dfFlow, data.frame(
    step = "filtered baskets", 
    nrow = length(unique(mini$transact)),  
    numID = length(unique(mini$customer))
  )
  )
### @ END of MINI-------------




### Category and department definitions regrouping ------------------------
unique(mini$category)
# category table 
ct <- mini[order(department, category),  .(trNum = uniqueN(transact), itemNum=uniqueN(upc)), by=list(department, category)] %>% 
  data.frame()

# are all categories unique, if conditioned on department ? 
#no , there are duplicates, like same name in both in fresh and frozen department
ct %>%  
  group_by(department, category) %>% 
  filter(n()>1)
# this complete and original listing is edited into  the file below within local pc 
if(bool_savePlotsFiles){
  write.csv(ct, "data_hiroshi/ProductTable_detailedUnreported_original.csv")
}

#Edited in PC manually, with remove flag and merge flag - loading back 
ct2 <- read_csv("data_hiroshi/ProductTable_detailedUnreported_Manuscript.csv")
ct2 <- ct2 %>% dplyr::rename(upcNum = `UPC Num`, ID = `...1`)
# Deaprtment_no dup is modified 
# Remove var indicates suggested remoal of rare category, due to lack of depat name, or contianing only one UPC
# Merge Var contain grouping variable to compress seleval items into one to preevent replication 

# nameVar 
catCommonNames <- read.csv("data_hiroshi/ProductCategoryReportList_Manuscript.csv")
catCommonNames <- catCommonNames %>% 
  mutate(category = tolower(Category)) 
#name var indicates common category name made by me 
setDT(catCommonNames)

### Merge seafood 
#mini[like(category, "fish"), ] %>%  group_by(category, department) %>%  dplyr::summarise(count = n())
#mini[like(category, "seaf"), ] %>%  group_by(category, department) %>%  dplyr::summarise(count = n())


### Merge pizza
dim(mini)
#mini[like(category, "frozen"), ] %>%  group_by(category, department) %>%  dplyr::summarise(count = n())
#mini[category == "frozen-pizza-pasta-gluten-free" |
#     category == "frozen-pizza-pasta" |
#     (category == "pizza-pasta" & department == "frozen-food")
#   , category := "frozen-pizza-pasta"]
#mini[like(category, "frozen"), ] %>%  group_by(category, department) %>%  dplyr::summarise(count = n())
#mini[category == "frozen-pizza-pasta", category := "frozen-meals-sides"]
#mini[like(category, "frozen"), ] %>%  group_by(category, department) %>%  dplyr::summarise(count = n())


### merge with re-categorization 
setDT(ct2)
dim(mini)
mini_merged <- mini[ct2, on = c(category = "category", department = "department")] %>% 
  dplyr::rename(catID = ID, 
                catMerge = Merge,  
                catRemove = Remove) %>%
  select(-TransactNum, -upcNum) %>% 
  mutate(categoryNew = ifelse(is.na(catMerge), category, catMerge)) %>% 
  dplyr::rename(departmentNew = department_noDup) 
# CategoryNew is newly created variable, whose original cat name is replacted by cat merge varaibles 

#### check grouping 
mini_merged[like(category, "frozen"), ] %>%  group_by(category, department, categoryNew, departmentNew) %>%  dplyr::summarise(count = n())
mini_merged[like(category, "fish"), ] %>%  group_by(category, department, categoryNew, departmentNew) %>%  dplyr::summarise(count = n())
mini_merged[like(category, "seaf"), ] %>% group_by(category, department, categoryNew, departmentNew) %>%  dplyr::summarise(count = n())
mini_merged[like(category, "salty-sna"), ] %>% group_by(category, department, categoryNew, departmentNew) %>%  dplyr::summarise(count = n())
mini_merged[like(category, "deli-me"), ] %>%  group_by(category, department, categoryNew, departmentNew) %>%  dplyr::summarise(count = n())

# Check counts 
#mini_merged[!is.na(catMerge), ] %>%  
#  group_by(category, department, categoryNew, departmentNew) %>%  
#  dplyr::summarise(count = n()) %>% 
#  dplyr::arrange(departmentNew, categoryNew) %>% 
#  data.frame()


# rename category and depat var to modified one, and old one can be kept as "original_"
mini_merged_commonName <- mini_merged %>% 
  left_join(catCommonNames, by = c("categoryNew" = "category")) %>% 
  mutate(categoryCommonName = ifelse(is.na(name), 
                        str_to_title(gsub('-', ' ', categoryNew)), name)) %>% 
  select(-catMerge)
# Category common name is final variable name used for analysis and reporting 

### Inspect 
dim(mini); dim(mini_merged); dim(mini_merged_commonName)

#mini_merged_commonName %>% 
#  group_by(categoryNew, categoryCommonName, departmentNew, catRemove) %>%
#  dplyr::summarize(count = n()) %>% 
#  arrange(departmentNew, categoryNew, categoryCommonName, catRemove) %>% 
#  data.frame()
### remove items without department name and product categowith with only 1 upc 
### as it is likely transaction erro 
mini_merged_commonName <- mini_merged_commonName %>% 
  filter(is.na(catRemove)) %>% 
  select(-catRemove)


dfFlow <- 
  rbind(dfFlow, data.frame(
    step = "Transaction error, no dept name", 
    nrow = length(unique(mini_merged_commonName$transact)),  
    numID = length(unique(mini_merged_commonName$customer))
  )
)

dim(mini_merged); dim(mini_merged_commonName)


mini_merged_commonName <- mini_merged_commonName %>% 
  dplyr::rename(category_original = category, 
         department_original = department) %>% 
  dplyr::rename(category = categoryCommonName, 
                department = departmentNew) %>% 
  select(-name, -categoryNew)
  

### @stats Product List Table --------------------------------------------------

if(bool_productListTable){
  prodList <- mini_merged_commonName %>% 
    group_by(department, category) %>% 
    dplyr::summarise(countUPC = n_distinct(upc)) %>% 
    mutate(Range = cut(countUPC ,c(1,50,100,500, 1000, 2000, 3000, 5000), dig.lab=10)) %>% 
    mutate(department = str_to_title(department)) %>% 
    select(-countUPC)
  
  pT <- flextable(data = prodList) %>% 
    theme_booktabs() %>% 
    autofit %>% 
    hline_top(part = "all", border = fp_border(color="black", width = 1)) %>% 
    hline_bottom(border =  fp_border(color = "black", width = 1)) %>% 
    line_spacing(space = 0.8, part = "body", unit = "cm") %>% 
  merge_v(j = "department",
          target = c("department"))
  if(bool_savePlotsFiles){
    save_as_docx(pT, path = "manuscript_hiroshi/categoryListDescriptive.docx")
  }
}

### MINI replace --------------
mini <- mini_merged_commonName

###@ END MINI category ---------------


### remove transaction errors 
mini[quantity < 1, .N ]
mini[total < 0, .N ]

mini <- mini[quantity > 0 & total > 0 , ]

dfFlow <- 
  rbind(dfFlow, data.frame(
    step = "negative total basket from n=mini", 
    nrow = length(unique(mini$transact)),  
    numID = length(unique(mini$customer))
  )
  )






### <<< Save mini with new category ------------------------
if(bool_savePlotsFiles){
  saveRDS(mini, "data_hiroshi/miniNewCategoryName_flowchart.rds")
  
  customerID_regression <- readRDS("~/projects/kodyMetro/data_hiroshi/customerID_GLMM_smallSample.rds")
  mini_ARM <- mini %>%
    filter(!customer %in% customerID_regression )
    
  saveRDS(mini_ARM, "data_hiroshi/miniNewCategoryName_flowchart_ARM.rds")
  
}
  #mini <- readRDS("data_hiroshi/miniNewCategoryName_flowchart.rds")











### @stats START Basket descriptive -----------------------------------

if(bool_basketDescritive){
  basketSummary <- mini %>%
    dplyr::group_by(transact, timestamp, customer) %>%
    dplyr::summarise(sumQuant = sum(quantity), 
                     totalDollar = unique(total), 
                     sumDistinctCategory = n_distinct(category))
  
  basketSummary <- setDT(basketSummary)
  
  basketDescriptive <- basketSummary  %>%  
    dplyr::summarise_at(c('sumQuant', 'totalDollar', 'sumDistinctCategory'),
    funs( Mean=mean(.),
          Q1=quantile(., probs=0.25),
          Median=quantile(., probs=0.50),
          Q3=quantile(., probs=0.75),
          MIN=min(.),
          MAX=max(.)))
  
  tableShopDetails <- as_tibble(cbind(nms = names(basketDescriptive), t(basketDescriptive)))
  names(tableShopDetails) <- c('VAR', 'VALUE')
  tableShopDetails <- makeTbl(tableShopDetails)
  
  library(flextable)
  ft <- flextable(data = tableShopDetails) %>% 
    theme_booktabs() %>% 
    autofit

  if(bool_savePlotsFiles){
    save_as_docx("basket member" = ft, path = "manuscript_hiroshi/basketSize_member.docx")
    saveRDS(tableShopDetails, "manuscript_hiroshi/tableShopDetails_members.rds")
  }
  
  ### Basket size including speficic category, soda, fru, and veg
  funcCategoryBasketSummary <- function(targetCat){
    catBasketSummary <- mini %>% 
      filter(category_original == targetCat) %>% 
      select(transact) %>% # 17440records of soda 
      left_join(mini) %>% 
      dplyr::group_by(transact, timestamp, customer) %>% # 2680181 records of categories within transaction containing soda 
      dplyr::summarise(sumQuant = sum(quantity), 
                       totalDollar = unique(total), 
                       sumDistinctCategory = n_distinct(category)) %>% 
      setDT()  %>%  
      dplyr::summarise_at(c('sumQuant', 'totalDollar', 'sumDistinctCategory'),
                          funs( Mean=mean(.),
                                Q1=quantile(., probs=0.25),
                                Median=quantile(., probs=0.50),
                                Q3=quantile(., probs=0.75),
                                MIN=min(.),
                                MAX=max(.)))
    tableShopDetails <- as_tibble(cbind(nms = names(catBasketSummary), t(catBasketSummary)))
    names(tableShopDetails) <- c('VAR', 'VALUE')
    makeTbl(tableShopDetails)
  }

  funcBasketTrans <- function(a){
    a %>%
      gather(key = key, value =value, 2:ncol(a)) %>% 
      spread(key= names(a)[1], value = "value") %>% 
      mutate(IQR= paste("(", Q1,",", Q3 , ")", sep = "") ) %>% 
      select(-Q1, -Q3) %>%
      filter(key != "Category") %>% 
      select(key, Median, IQR)
  }
  

  vectorTargetCat <- c("soft-drinks", "fruits", "vegetables", "sports-energy-drinks")
  vectorTargetCatCommonName <- c("soda", "fru", "veg", "ced")
  
  v <- vectorTargetCat %>%  
    purrr::map(funcCategoryBasketSummary) %>% 
    purrr::map(funcBasketTrans) %>% 
    setNames(vectorTargetCat) 

  v2 <- do.call(cbind, v) %>% 
    setNames(
      apply(expand.grid(c("name","Median", "IQR"), vectorTargetCatCommonName), 1, paste, collapse=".")
    ) %>% 
    mutate(Statistics = name.soda) %>% 
    select(-contains("name.")) %>% 
    relocate(Statistics) %>% 

  
 Ft <- flextable(data = v2) %>% 
    theme_booktabs() %>% 
    autofit %>% 
    hline_top(part = "all", border = fp_border(color="black", width = 1)) %>% 
    hline_bottom(border =  fp_border(color = "black", width = 1)) %>% 
    line_spacing(space = 0.8, part = "body", unit = "cm")
 

    if(bool_savePlotsFiles){
    save_as_docx(Ft, path = "manuscript_hiroshi/basketSizeTargetCats.docx")
  }

unique(mini$category)[grep("energy", unique(mini$category))]
  
  #STATISTIC sumDistinctCategory sumQuant totalDollar   Category
  #1    Median                   8       12       43.49       Soda
  #2    Median                   7       16       41.81     Fruits
  #3    Median                   8       19       43.14 Vegetables
}


### @End basket descriptive --------------------








### Transaction data ---------------
mini.baskets <- mini %>%
  dplyr::group_by(transact, timestamp, customer) %>%
  dplyr::summarise(basket = as.vector(list(category))) %>% 
  mutate(date = paste(year(timestamp), month = month(timestamp), sep = "-"))

# Split sample for ARM
mini.baskets_ARM <- mini_ARM %>%
  dplyr::group_by(transact, timestamp, customer) %>%
  dplyr::summarise(basket = as.vector(list(category))) %>% 
  mutate(date = paste(year(timestamp), month = month(timestamp), sep = "-"))

### RM 

#rm(list = ls(pattern='idx*'))

# compute transactions  ### >>> MBA data uses arrules::as  ---- 
transactions <- as(mini.baskets$basket, "transactions")
if(bool_savePlotsFiles){
  saveRDS(transactions, "data_hiroshi/transactions_card_flowchart.rds")
}

transactions_ARM <- as(mini.baskets_ARM$basket, "transactions")
if(bool_savePlotsFiles){
  saveRDS(transactions_ARM, "data_hiroshi/transactions_card_flowchart_ARM.rds")
}


#transactions <- readRDS("data_hiroshi/transactions_card_flowchart.rds")

# rm intermedite records
rm(mini_merged, mini_merged_commonName)



### @ START Sensitivity anlaysis -- Get frequnet list from previously saved files -----------------
idx.month.490 <- readRDS("data_hiroshi/freqMonthDollar_0_490.rds")
idx.month.490_agregate <- idx.month.490 %>%  distinct(customer, month, year)
nrow(idx.month.490_agregate); nrow(idx.month.490)

# 490$ per month 
mini.baskets.freq.490 <- mini.baskets %>%  
  dplyr::mutate(month = month(timestamp), year=year(timestamp)) %>% 
  inner_join(idx.month.490_agregate, by = c("customer"="customer", "month"="month", "year"="year"))
nrow(mini.baskets.freq.490); nrow(mini.baskets)
transactions.freq.490 <- as(mini.baskets.freq.490$basket, "transactions")
if(bool_savePlotsFiles){
  saveRDS(transactions.freq.490, "data_hiroshi/transactions_card_dollar490Moi.rds")
}

# 4 visits per month
idx.month.4moi <- readRDS("data_hiroshi/freqMonthDollar_4_0.rds")
idx.month.4moi_agregate <- idx.month.4moi %>%  distinct(customer, month, year)
nrow(idx.month.4moi_agregate); nrow(idx.month.4moi)
mini.baskets.freq.4moi <- mini.baskets %>%  
  dplyr::mutate(month = month(timestamp), year=year(timestamp)) %>% 
  inner_join(idx.month.4moi_agregate, by = c("customer"="customer", "month"="month", "year"="year"))
nrow(mini.baskets.freq.4moi); nrow(mini.baskets)
transactions.freq.4moi <- as(mini.baskets.freq.4moi$basket, "transactions")

if(bool_savePlotsFiles){
  saveRDS(transactions.freq.4moi, "data_hiroshi/transactions_card_fre4moi.rds")
}

#transactions <- transactions.freq.490
### @ END sensitivity frequency basket


### @ stats START Frequency plots -------------------------------------------

freq.items <- eclat(transactions, parameter = list(supp = 0.005, maxlen = 1))
freqItem <- inspect(head(sort (freq.items, by="support", decreasing=TRUE), 50))
#{vegetables}                    0.43400370 734530 if supp = 0.005
#  {vegetables}                    0.43400370 734530 if supp = 0.01
#freqPair <- eclat(transactions, parameter = list(supp = 0.01, maxlen = 2, minlen = 2))
#freqPair <- inspect(head(sort (freqPair, by="support", decreasing=TRUE), 30))

freqItem <- freqItem %>% 
  mutate(Category = gsub("[{}]","", items)) %>% 
  relocate(Category) %>% 
  mutate(Proportion = round(support*100, 2), Count = count) %>% 
  select(-items, -support, -count) 

if(bool_savePlotsFiles){
  saveRDS(freqItem, "data_hiroshi/freqItem.rds")  
  }

freqItem_nc <- readRDS("data_hiroshi/freqItem_nc.rds")

itemList <- freqItem %>% arrange(desc(Proportion)) %>% head(25) %>% select(Category)

freqItem_nc <- freqItem_nc %>%  
  mutate(dat = "Non Cardholders") %>% 
  inner_join(itemList)

freqItem <- freqItem %>%  
  mutate(dat = "Cardholders") %>% 
  inner_join(itemList)

freqItemCombined <- rbind(freqItem, freqItem_nc)  

# some re-writing 
freqItemCombined$Category <- stringr::str_to_title(freqItemCombined$Category)

#nameVar <- read.csv("data_hiroshi/ProductCategoryReportList_Manuscript.csv")

freqItemCombined <- freqItemCombined %>% 
  mutate(CardStatus = factor(dat)) %>% 
  mutate(Category = factor(Category)) %>% 
  left_join(catCommonNames, by = "Category") %>% 
  mutate(Category = forcats::fct_reorder(Category , Proportion)) 

ggplot(freqItemCombined, aes(x = Category, y = Proportion/100, fill = dat)) +
  geom_bar(stat = "identity",  position = position_dodge(), colour="black")+  
  coord_flip() +
  theme(axis.title=element_text(size=10,face="bold"),
        axis.text = element_text(size = 10),
        title=element_text(size=8,face="bold"))+
  ylab("Frequency") +  xlab("Food category") +
  scale_fill_manual(values = c("black","white")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #theme(text=element_text(size=16,color = "black")) + 
  theme(axis.text = element_text(size = 10, colour = "black"))  + 
  guides(fill=guide_legend(title="Cardholder status"))
#theme(axis.title = element_text(size = 20)) 

if(bool_savePlotsFiles){
  ggsave("manuscript_hiroshi/frequencyPlot.png", height = 8, width = 8)
  ggsave("manuscript_hiroshi/frequencyPlot.pdf", height = 8, width = 8)
}

### @END stats frequency plots 

### multiple item rules 
freq.items <- eclat(transactions, parameter = list(supp = 0.005, maxlen = 5))
freqItem <- inspect(head(sort (freq.items, by="support", decreasing=TRUE), 50))
#{vegetables}                    0.43400370 734530 if supp = 0.005
#  {vegetables}                    0.43400370 734530 if supp = 0.01
#freqPair <- eclat(transactions, parameter = list(supp = 0.01, maxlen = 2, minlen = 2))
#freqPair <- inspect(head(sort (freqPair, by="support", decreasing=TRUE), 30))



### FOr area plot ---------
#freqItemC <- 
#  freqItem %>% 
#  mutate(dat = "Cardholders") %>% 
#  right_join(freqItem_nc %>% mutate(dat = "non-Cardholders"), by = "Category") %>% 
#  mutate(catID = as.numeric(as.factor(Category))) %>% 
#  group_by(catID) %>%  
#  dplyr::mutate(ct = n()) %>% 
#  filter(ct > 1) %>% 
#  ungroup() %>% 
#  mutate(catID = as.numeric(as.factor(Category))) %>% 
#  data.frame()
#p <- ggplot(data.frame(freqItemC), 
#  aes(x= dat, stratum = Category, allvium = catID, 
#      fill = Category, y = Proportion, label = Category)) + 
#  scale_x_discrete(expand = c(.1, .1)) +
#  geom_flow() +
#  geom_stratum(alpha = .5) +
#  geom_text(stat = "stratum", size = 3) +
#  theme(legend.position = "none") +
#  ggtitle("vaccination survey responses at three points in time")
#p <- ggplot(freqItemC, aes(x=dat, y=Proportion, fill=Category, group = Category)) + 
#  geom_area(alpha=0.6 , size=1, colour="black")




