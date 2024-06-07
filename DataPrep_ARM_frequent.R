# filename  metro_montreal
# version   5.0
# author    kody crowell
# date      jan 14 2019





### Data 
# codes to load data are up to line 245 in DataPrep_ARM.R




### 
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

dfFlow_freq <- 
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
# Select customer, not basket
#freq.50.1 <- card.monthly %>%
#  group_by(customer) %>% filter(min(total) > 50 & min(visits) > 1) %>%
#  dplyr::select(customer) %>% distinct(.) # 2914 

# Select customer, not basket
#freq.0.1 <- card.monthly %>%
#  group_by(customer) %>% filter(min(total) > 0 & min(visits) > 1) %>%
#  dplyr::select(customer) %>% distinct(.) # 2914 

# Select customer, not basket
#freq.0.0 <- card.monthly %>%
#  group_by(customer) %>% filter(min(total) == 0 | min(visits) == 0) %>% arrange(customer, month, year)

freq.4.490.transactions <- card.monthly %>%
  filter(total != 0) %>% 
  group_by(customer, year, month) %>%  
  dplyr::mutate(visitsTotal = sum(visits), spendTotal = sum(total)) %>% 
  filter( spendTotal  > 491) %>% 
  arrange(customer, month, year) %>% 
  ungroup()

freq.4.transactions <- card.monthly %>%
  filter(total != 0) %>% 
  group_by(customer, year, month) %>%  
  dplyr::mutate(visitsTotal = sum(visits), spendTotal = sum(total)) %>% 
  filter(visitsTotal > 4 & spendTotal  > 0) %>% 
  arrange(customer, month, year) %>% 
  ungroup()


nrow(freq.4.490.transactions)
nrow(freq.4.transactions)
basket.sample %>%  filter(total != 0) %>%  nrow; 


saveRDS(freq.4.490.transactions, "data_hiroshi/freqMonthDollar_0_490.rds")
saveRDS(freq.4.transactions, "data_hiroshi/freqMonthDollar_4_0.rds")


### what are zero total sollar months??
#idx_emptyTransaction <- card.monthly %>% filter(total == 0) 

## note that bakset.sample is loaded later, 
#basket.sample %>% 
#  left_join(idx_emptyTransaction, by =c("customer"="customer", "month" = "month", "year" ="year"))
### Transactions with zero total sales in the customer.month data have no corresponding transaction in basket.sample table 

### !!!! NOTE Hiroshi ----
# why frequ shopper aggregated above in original analysis are filtered based all months (disaggregated) 
# As well, monthly spening of ottal 50 appears small, and monthly visit of 1 month is rather restrctive
# is also does not allow people who doe not spend 1 month , 
# how many are a frequent shopper based on more loose definition (not aggregated by customer)

#freq.50.1.anyMonth <- card.monthly %>%
#  filter(total > 50 & visits > 1) %>%
#  dplyr::select(customer) %>%
#  distinct(.) # 2914
#nrow(freq.50.1.anyMonth) # 248200

### 287 cases, no 151 why? thoug the number i sstill too small 
#length(which(cards.sample %in% freq.50.1$customer)) # 151

### <<< SAVE cards.in.basket  cleaned ---------
saveRDS(cards.in.basket, "data_hiroshi/cards.in.basket_clean_flowchart_freq.rds")




