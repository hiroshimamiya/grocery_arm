### Kody's old basket data 
### Load data ----------------------------
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="metro", host="132.216.183.3", 
                 user="kodyandrew", pass="********")
# set seed
set.seed(42)

# define macros
'%!in%' <- function(x,y)!('%in%'(x,y))


upc.class <- dbGetQuery(con, "select * from kodyandrew.metro_upc_class;")

basket.sample <- readRDS("data_hiroshi/basket.sample.rds")

### for some reason, store colum is duplicated...remove 
basket.sample <- basket.sample[ , !duplicated(colnames(basket.sample))]

basket.sample$upc[basket.sample$upc == '<NA>'] <- NA
basket.sample$department[basket.sample$department == '<NA>'] <- NA

# omit observations outside of time range / nrow = 120703
idx.t2 <- which(year(basket.sample$timestamp)== 2007 |
                  (year(basket.sample$timestamp) == 2015 & 
                     month(basket.sample$timestamp) == 1) |
                  (year(basket.sample$timestamp) == 2017 & 
                     month(basket.sample$timestamp) == 10))

basket.sample <- basket.sample[-idx.t2,] %>%
  mutate(register = as.character(register)) 

# create date variable
basket.sample$date <- format(as.Date(basket.sample$timestamp), "%Y-%m")

# deal with duplicated stores


# create transaction variable
basket.sample$transact <-  group_indices(basket.sample, store, transaction, timestamp, register)




# create mini basket to save space - 
#mini <- basket.sample %>%
#  left_join(upc.class %>% 
#              dplyr::select(-department), by="upc") %>%
#  mutate(category = ifelse(is.na(upc), "dept-specific-bottle",
#                           ifelse(food == 0, "non-food-item", category))) %>%
#  dplyr::select(customer, transact, upc, spending, total, category) %>%
#  group_by(customer, transact, total, category, upc) %>%
#  dplyr::summarise(spending = sum(spending)) %>%
#  mutate(category = ifelse(is.na(category), "null", category)) # collapse dups
#
#any(duplicated(mini)) # false
#rm(basket.sample)
#mini.baskets <- mini %>%
#  filter(category %!in% c('dept-specific-bottle', 'null', 'non-food-item')) %>%
#  dplyr::group_by(transact) %>%
#  dplyr::summarise(basket = as.vector(list(category)))
#



mini <- basket.sample %>%
  left_join(upc.class, by="upc") %>%
  mutate(category = ifelse(is.na(upc), "dept-specific-bottle", category)) %>%
  mutate(category = ifelse(food == 0, "non-food-item", category)) %>%
  mutate(category = ifelse(is.na(category), "null", category)) %>%
  select(customer, transact, upc, spending, total, category) %>%
  distinct(.) # mutate not working??? check and manually recode

# calculate proportion
# deal with 0 total, spending < 0 and spending > total
mini$proportion <- ifelse(mini$total == 0, 0, 
                          ifelse(mini$spending < 0, 0,
                                 ifelse(mini$total < mini$spending, 1,
                                        abs(mini$spending)/abs(mini$total))))

tr2key <- basket.sample %>%
  select(customer,transact,store,timestamp,register,transaction) %>%
  distinct(.)

### Itemlist data 
'%!in%' <- function(x,y)!('%in%'(x,y))
mini.transactions <- mini %>%
  filter(category %!in% c('dept-specific-bottle', 'null', 'non-food-item')) %>%
  inner_join(tr2key, by=c("customer", "transact")) %>%
  group_by(customer, transact) %>%
  dplyr::summarise(SIZE = n(),
                   items = paste(as.character(category), collapse = ';'),
                   timestamp = unique(timestamp))

### Number of single item transaction 
table(mini.transactions$SIZE)
### Number of duplciated categories within transact 
dupItems <- mini %>%  
  filter(category %!in% c('dept-specific-bottle', 'null', 'non-food-item')) %>% 
  group_by(transact) %>%  
  count(category) 


mini.baskets <- mini %>%
  filter(category %!in% c('dept-specific-bottle', 'null', 'non-food-item')) %>%
#  filter(transact %!in% undup$transact) %>%
  dplyr::group_by(transact) %>%
  dplyr::summarise(basket = as.vector(list(category)))



# compute transactions  ### >>> MBA data uses arrules::as  ---- 
transactions_c <- as(mini.baskets$basket, "transactions")
#saveRDS(transactions, "data_hiroshi/transactions_card.rds")




###########################
### Compare member and non-mermber ts ----------------
###########################
summary(transactions_c)
summary(transactions_nc)













# determine which items are frequent (frequent iff in at least 2.5% of baskets)
item.frequencies <- itemFrequency(transactions_c, type="a")
support <- 0.05
freq.items <- sort(item.frequencies, decreasing = F)
freq.items <- freq.items[freq.items > support*length(transactions_c)]
freq.items

item.frequencies <- itemFrequency(transactions_nc, type="a")
support <- 0.01
freq.items <- sort(item.frequencies, decreasing = F)
freq.items <- freq.items[freq.items > support*length(transactions_nc)]
freq.items


## ARM 
support <- 0.01
backports::import("base", "deparse1") ### Deparse needed to be load
itemsets <- apriori(transactions_nc, 
                    parameter = list(
                    #target = "frequent itemsets", 
                                     supp=0.001, conf=0.05
                                     #, minlen=2
                                     ), 
                    control = list(verbose = FALSE))
summary(itemsets)
itemsets %>% head(n = 3, by = "lift") %>% inspect()


# remove redundant rules
# get subset rules in vector
subsetRules <- which(colSums(is.subset(itemsets, itemsets)) > 1) 
length(subsetRules)  ### 4455 rules subset 
itemsets <- itemsets[-subsetRules]

# sort items by support
par(mar=c(5,18,2,2)+.1)
itemsets_supp <- DATAFRAME(sort(itemsets, by="support", decreasing = F))

# cut braces, str_split and find relevant foods
has.salty <- grepl('salty-snacks', as.character(itemsets_supp$items))
has.sugar <- grepl('sweet-snacks-candy', as.character(itemsets_supp$items))
has.soda <- grepl('soft-drinks', as.character(itemsets_supp$items))
has.juice <- grepl('juices-drinks', as.character(itemsets_supp$items))
itemsets_supp$has.badfoods <- ifelse(has.sugar + has.salty + has.soda + has.juice == 1, T, F)



################################################################################
##### mine association rules ------------------------
# Line 1458
# compute association rules
# we want rules related to given items (soft drinks, juice, soda
# increase minlen of association... which min support and conf to use?
# arem and aval/minval?
apriori.ctl <- list(sort=-1, verbose = FALSE)
apriori.prm <- list(supp=0.001, conf=0.05, minlen=2, maxlen=5, ext=T)



rules.sel <- apriori(transactions, 
                     parameter = apriori.prm, control = apriori.ctl,
                     appearance = list(lhs = "salty-snacks"))
rules.jus <- apriori(transactions,
                     parameter = apriori.prm, control = apriori.ctl,
                     appearance = list(lhs = "juices-drinks"))
rules.suc <- apriori(transactions,
                     parameter = apriori.prm, control = apriori.ctl,
                     appearance = list(lhs = "sweet-snacks-candy"))
rules.veg <- apriori(transactions,
                     parameter = apriori.prm, control = apriori.ctl,
                     appearance = list(lhs = "vegetables"))
rules.frt <- apriori(transactions,
                     parameter = apriori.prm, control = apriori.ctl,
                     appearance = list(lhs = "fruits"))

################################################################################
### linking to households ----
# change all to df, order by lift
# ssb, sel, suc, jus, frt, veg



rules.ssb.df <- data.frame(
  lhs = labels(lhs(rules.ssb)),
  rhs = labels(rhs(rules.ssb)), 
  rules.ssb@quality)
rules.ssb.df <- rules.ssb.df[order(-rules.ssb.df$lift),]
rules.ssb.df <- rules.ssb.df %>%
  mutate(lhs = gsub("[{}]", "", lhs),
         rhs = gsub("[{}]", "", rhs)) %>%
  head(15)

rules.suc.df <- data.frame(
  lhs = labels(lhs(rules.suc)),
  rhs = labels(rhs(rules.suc)), 
  rules.suc@quality)
rules.suc.df <- rules.suc.df[order(-rules.suc.df$lift),]
rules.suc.df <- rules.suc.df %>%
  mutate(lhs = gsub("[{}]", "", lhs),
         rhs = gsub("[{}]", "", rhs)) %>%
  head(15)

rules.sel.df <- data.frame(
  lhs = labels(lhs(rules.sel)),
  rhs = labels(rhs(rules.sel)), 
  rules.sel@quality)
rules.sel.df <- rules.sel.df[order(-rules.sel.df$lift),]
rules.sel.df <- rules.sel.df %>%
  mutate(lhs = gsub("[{}]", "", lhs),
         rhs = gsub("[{}]", "", rhs)) %>%
  head(15)

rules.jus.df <- data.frame(
  lhs = labels(lhs(rules.jus)),
  rhs = labels(rhs(rules.jus)), 
  rules.jus@quality)
rules.jus.df <- rules.jus.df[order(-rules.jus.df$lift),]
rules.jus.df <- rules.jus.df %>%
  mutate(lhs = gsub("[{}]", "", lhs),
         rhs = gsub("[{}]", "", rhs)) %>%
  head(15)

rules.frt.df <- data.frame(
  lhs = labels(lhs(rules.frt)),
  rhs = labels(rhs(rules.frt)), 
  rules.frt@quality)
rules.frt.df <- rules.frt.df[order(-rules.frt.df$lift),]
rules.frt.df <- rules.frt.df %>%
  mutate(lhs = gsub("[{}]", "", lhs),
         rhs = gsub("[{}]", "", rhs)) %>%
  head(15)

rules.veg.df <- data.frame(
  lhs = labels(lhs(rules.veg)),
  rhs = labels(rhs(rules.veg)), 
  rules.veg@quality)
rules.veg.df <- rules.veg.df[order(-rules.veg.df$lift),]
rules.veg.df <- rules.veg.df %>%
  mutate(lhs = gsub("[{}]", "", lhs),
         rhs = gsub("[{}]", "", rhs)) %>%
  head(15)

rules <- rules.ssb.df %>%
  bind_rows(., rules.suc.df) %>%
  bind_rows(., rules.sel.df) %>%
  bind_rows(., rules.jus.df) %>%
  bind_rows(., rules.veg.df) %>%
  bind_rows(., rules.frt.df)




################################################################################
### get CI ---- 
# extract rules for categories of interest as the consequent
# min confidence to be the minimum support of consequent
# alter max len from 2 -> 4 to get altered sizes (272 rules in total for 2)
rules.ssb.rhs <- apriori(transaction_nc, 
                         parameter = list(supp=0.01, conf=0.09, minlen=2, maxlen=2, ext=T),
                         control = apriori.ctl,
                         appearance = list(rhs = "soft-drinks"))

ci <- confint(rules.ssb.rhs, "lift", level = 0.95)
ci
ci <- confint(rules.ssb.rhs, "oddsRatio",  smoothCounts = .5)
quality(rules.ssb.rhs) <- cbind(
  quality(rules.ssb.rhs),
  oddsRatio = interestMeasure(rules.ssb.rhs, "oddsRatio", smoothCounts = .5),
  oddsRatio = ci)
rules.ssb.rhs <- sort(rules.ssb.rhs, by = "oddsRatio")
inspect(rules.ssb.rhs)

rules.sel.rhs <- apriori(transactions, 
                         parameter = list(supp=0.01, conf=0.19, minlen=2, maxlen=2, ext=T),
                         control = apriori.ctl,
                         appearance = list(rhs = "salty-snacks"))
rules.jus.rhs <- apriori(transactions,
                         parameter = list(supp=0.01, conf=0.19, minlen=2, maxlen=2, ext=T),
                         control = apriori.ctl,
                         appearance = list(rhs = "juices-drinks"))
rules.suc.rhs <- apriori(transactions,
                         parameter = list(supp=0.01, conf=0.21, minlen=2, maxlen=2, ext=T),
                         control = apriori.ctl,
                         appearance = list(rhs = "sweet-snacks-candy"))
rules.veg.rhs <- apriori(transactions,
                         parameter = list(supp=0.01, conf=0.42, minlen=2, maxlen=2, ext=T),
                         control = apriori.ctl,
                         appearance = list(rhs = "vegetables"))
rules.frt.rhs <- apriori(transactions,
                         parameter = list(supp=0.01, conf=0.41, minlen=2, maxlen=2, ext=T),
                         control = apriori.ctl,
                         appearance = list(rhs = "fruits"))




funcDfRhs <- function(d){
  dd <- data.frame(
    lhs = labels(lhs(d)),
    rhs = labels(rhs(d)),  
    d@quality)
  out <- dd[order(-dd$lift),]
  out <- dd %>%
    mutate(lhs = gsub("[{}]", "", lhs),
           rhs = gsub("[{}]", "", rhs)) # %>% head(15)
}

rules.ssb.df.rhs <- funcDfRhs(rules.ssb.rhs)
rules.frt.df.rhs <- funcDfRhs(rules.frt.rhs)
rules.sel.df.rhs <- funcDfRhs(rules.sel.rhs)
rules.jus.df.rhs <- funcDfRhs(rules.jus.rhs)
rules.veg.df.rhs <- funcDfRhs(rules.veg.rhs)
rules.suc.df.rhs <- funcDfRhs(rules.suc.rhs)

rules.rhs.full <- rules.ssb.df.rhs %>%
  bind_rows(., rules.suc.df.rhs) %>%
  bind_rows(., rules.sel.df.rhs) %>%
  bind_rows(., rules.jus.df.rhs) %>%
  bind_rows(., rules.veg.df.rhs) %>%
  bind_rows(., rules.frt.df.rhs)

# calculate odds ratio
# use count, lhs.support, nrow(mini.baskets) to calculate 2x2 tables
#     y +   y -
# x + both  ante
# x n cons  none
rules.rhs.full <- rules.rhs.full %>%
  mutate(c11 = count,
         c22 = nrow(mini.baskets) - count,
         c12 = lhs.support*nrow(mini.baskets),
         c21 = confidence / lift * nrow(mini.baskets), # rhs support
         or = (c11*c22) / (c12*c21),
         or.lse = sqrt((1/c11) + (1/c12) + (1/c21) + (1/c22)),
         cil = exp(log(or) - qnorm(0.975)*or.lse),
         ciu = exp(log(or) + qnorm(0.975)*or.lse),
         rpf = support * confidence) %>%
  select(-c(c11,c12,c21,c22,or.lse))

# link n customers below
rules.rhs.full <- rules.rhs.full %>% inner_join(rule.count %>% select(rule, nc), by=c("id" = "rule"))
