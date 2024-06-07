gc()
rm(list=ls())
source("loadFunc.R")

### Load data ----------------------------
mtl_stores <- dbGetQuery(con, "select * from kodyandrew.mtl_stores")

### sample by newly created transact id  
basket.sample.noCard <- readRDS("data_hiroshi/basket_sample_noCard.rds")

# transaction index, using DT and df (ver slow latter)
setDT(basket.sample.noCard)[,transact:=.GRP, by = c("store", "transaction", "timestamp", "register_lane")]
#basket.sample.noCard$transactSlow <- group_indices(basket.sample.noCard, store, transaction, timestamp, register_lane)
length(unique(basket.sample.noCard$transact))
#length(unique(basket.sample.noCard$transactSlow))

set.seed(1)
transaction_sample <- sample(unique(basket.sample.noCard$transact), 1000000, replace= FALSE, prob = NULL)
length(unique(transaction_sample))

bs <- basket.sample.noCard[transact %in% transaction_sample]


### convert to DT and clean 
str(bs)
bs[, spending_basket:=as.character(spending_basket)]
bs[, spending_item_max:=as.character(spending_item_max)]
bs[, food:=factor(food, levels = c(0, 1))]
str(bs)

bs <- bs[order(store, transaction, timestamp)]

bs <- bs[!is.na(category), ]
bs <- bs[!is.na(upc_description), ]
bs <- bs[food == 1, ]
nrow(bs)
bs  %>%  select(department_upctable) %>%  filter(is.na(department_upctable)) %>%  nrow()


### date var
bs[, dateTransact := format(as.Date(timestamp), "%Y-%m")]
# create transaction variable


### re-grouping category, like pizza 
bs[like(category, "pizza"), ] %>%  group_by(category, department_upctable) %>%  dplyr::summarise(count = n())



### import category name 
#Edited in PC manually, with remove flag and merge flag - loading back 
ct2 <- read_csv("data_hiroshi/ProductTable_detailedUnreported_Manuscript.csv")
ct2 <- ct2 %>% dplyr::rename(upcNum = `UPC Num`, ID = `...1`)
# Deaprtment_no dup is modified 
# Remove var indicates suggested remoal of rare category, due to lack of depat name, or contianing only one UPC
# Merge Var contain grouping variable to compress seleval items into one to preevent replication 

# nameVar 
catCommonNames <- read.csv("data_hiroshi/ProductCategoryReportList_Manuscript.csv")
catCommonNames <- catCommonNames %>% 
  mutate(category = tolower(Category)) %>% 
  select(-Category)
#name var indicates common category name made by me 
setDT(catCommonNames)


dim(bs)


setDT(ct2)
bs <- bs %>% 
  dplyr::rename(departmentCode = department) %>% 
  dplyr::rename(department = department_upctable)
head(bs)

bs_merged <- bs[ct2, on = c(category = "category", department = "department")] %>% 
  dplyr::rename(catID = ID, 
                catMerge = Merge,  
                catRemove = Remove) %>%
  select(-TransactNum, -upcNum) %>% 
  mutate(categoryNew = ifelse(is.na(catMerge), category, catMerge)) %>% 
  dplyr::rename(departmentNew = department_noDup) 
# CategoryNew is newly created variable, whose original cat name is replacted by cat merge varaibles 


#### check grouping 
bs_merged[like(category, "frozen"), ] %>%  group_by(category, department, categoryNew, departmentNew) %>%  dplyr::summarise(count = n())
bs_merged[like(category, "fish"), ] %>%  group_by(category, department, categoryNew, departmentNew) %>%  dplyr::summarise(count = n())
bs_merged[like(category, "seaf"), ] %>% group_by(category, department, categoryNew, departmentNew) %>%  dplyr::summarise(count = n())
bs_merged[like(category, "salty-sna"), ] %>% group_by(category, department, categoryNew, departmentNew) %>%  dplyr::summarise(count = n())
bs_merged[like(category, "deli-me"), ] %>%  group_by(category, department, categoryNew, departmentNew) %>%  dplyr::summarise(count = n())

# Check counts 
bs_merged[!is.na(catMerge), ] %>%  
  group_by(category, department, categoryNew, departmentNew) %>%  
  dplyr::summarise(count = n()) %>% 
  dplyr::arrange(departmentNew, categoryNew) %>% 
  data.frame()


# rename category and depat var to modified one, and old one can be kept as "original_"
bs_merged_commonName <- bs_merged %>% 
  left_join(catCommonNames, by = c("categoryNew" = "category")) %>% 
  mutate(categoryCommonName = ifelse(is.na(name), 
                                     str_to_title(gsub('-', ' ', categoryNew)), name)) %>% 
  select(-catMerge)
# Category common name is final variable name used for analysis and reporting 

### Inspect 
dim(bs); dim(bs_merged); dim(bs_merged_commonName)

bs_merged_commonName %>% 
  group_by(categoryNew, categoryCommonName, departmentNew, catRemove) %>%
  dplyr::summarize(count = n()) %>% 
  arrange(departmentNew, categoryNew, categoryCommonName, catRemove) %>% 
  data.frame()

### remove items without department name and product categowith with only 1 upc 
### as it is likely transaction erro 
bs_merged_commonName <- bs_merged_commonName %>% 
  filter(is.na(catRemove)) %>% 
  select(-catRemove)

dim(bs); dim(bs_merged); dim(bs_merged_commonName)


### rename to standard name 
bs_merged_commonName <- bs_merged_commonName %>% 
  dplyr::rename(category_original = category, 
                department_original = department) %>% 
  dplyr::rename(category = categoryCommonName, 
                department = departmentNew) %>% 
  select(-name, -categoryNew)



### Finalize basket data for non-member
bs <- bs_merged_commonName 



# Check number of items in each transaction 
setorder(bs, transact, category)
bs[, .N, by=.(transact)]
a <- bs[, .N, by=.(transact, store, timestamp, transaction, category)]
table(a$N)  #!!!!!!!! Does not match with SIZE below???? 
#!!!!! why is this reaped so much ?bs %>% filter(transact == 63849)
# a %>% filter(transact == 63849)




### Basket descriptive -----------------------------------
bs <- bs %>% 
  dplyr::mutate(quantity = as.numeric(quantity), 
                spending_basket = as.numeric(spending_basket)) 

bs[quantity < 1, .N ]
bs[spending_basket < 0, .N ]
dim(bs)
bs <- bs[quantity > 0 & spending_basket > 0 , ]
dim(bs)


basketSummary <- bs %>%
  dplyr::group_by(transact, timestamp) %>%
  dplyr::summarise(sumQuant = sum(quantity), 
                   totalDollar = unique(spending_basket), 
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

tableShopDetails_ns <- as_tibble(cbind(nms = names(basketDescriptive), t(basketDescriptive)))
names(tableShopDetails_ns) <- c('VAR', 'VALUE')
tableShopDetails_ns <- makeTbl(tableShopDetails_ns)


### Save descriptive basket -----
saveRDS(tableShopDetails_ns, "manuscript_hiroshi/tableShopDetails_noMmembers.rds")


#tableShopDetails_ns <- tableShopDetails
tableShopDetails <-readRDS("manuscript_hiroshi/tableShopDetails_members.rds")

rownames(tableShopDetails_ns) <- tableShopDetails_ns$STATISTIC
rownames(tableShopDetails) <- tableShopDetails$STATISTIC

tns <- tableShopDetails_ns %>%
  select(-STATISTIC) %>% 
  t() %>% data.frame() %>% 
  dplyr::mutate(IQR_ns =  paste(as.character(Q1), as.character(Q3), sep = "-")) %>% 
  select(Median, IQR_ns) %>% 
  dplyr::rename(Median_ns = Median)

t <-tableShopDetails %>%
  select(-STATISTIC) %>% 
  t() %>% data.frame() %>% 
  dplyr::mutate(IQR =  paste(as.character(Q1), as.character(Q3), sep = "-")) %>% 
  select(Median, IQR)

t <- data.frame(cbind(t, tns))
t$basket_summary <- c("Categories", "Item quantities", "Spending")
t <- t %>% dplyr::relocate(basket_summary)

ft <- flextable(data = t) %>% 
  theme_booktabs() %>% 
  autofit

### Save combined basket table 
save_as_docx("basket non-member" = ft, path = "manuscript_hiroshi/basketSize.docx")


### Prepare tranaction data -------------------------------------------------------
### Itemlist data 
bsTransact <- bs %>%
  select(transact, upc, spending_item_max, spending_basket, category, timestamp, store, transaction) %>%
  ### without distinct, there are too mnay strange replicates with quantity == 1!!!!!!!!!
  distinct(.) %>% # small eduction in transactions 
  #inner_join(tr2key, by=c("customer", "transact")) %>%
  group_by(transact, store, timestamp, transaction) %>%
  dplyr::summarise(SIZE = n(),
                   items = paste(as.character(category), collapse = ';'), 
                   basket = as.vector(list(category))
                   #,timestamp = unique(timestamp)
  )


table(bsTransact$SIZE)
# check num of transactions 
length(unique(bs$transact))
nrow(bsTransact)

# number of incidence appearing 
bs[, .N, by=category] %>% arrange(desc(N)) %>% head(30)
bs[, .(sum = sum(as.numeric(quantity)), N = .N), by=category] %>% arrange(desc(N)) %>% head(10)



### Create and save Transactions  
transactions_nc <- as(bsTransact$basket, "transactions")
saveRDS(bs, "data_hiroshi/bs.rds")
saveRDS(transactions_nc, "data_hiroshi/transaction_nonMember.rds")


### DESCRIPTIVE Frequent items   -------------------------------------------------
backports::import("base", "deparse1") ### Deparse needed to be load
class(transactions_nc)
inspect(head(transactions_nc ))
table(size((transactions_nc)))
transactions(transactions_nc)


apriori.ctl <- list(sort=-1, verbose = FALSE)
apriori.prm <- list(supp=0.001, maxlen = 1, ext=T) # picked 3 because of baseline transaction 
freqItem <- eclat(transactions_nc, parameter = apriori.prm)
freqItem <- inspect(head(sort (freqItem, by="support", decreasing=TRUE), 50))

freqItem <- freqItem %>% 
  mutate(Category = gsub("[{}]","", items)) %>% 
  relocate(Category) %>% 
  mutate(Proportion = round(support*100, 2), 
         Count = count) %>% 
  select(-items, -support, -count) 
saveRDS(freqItem, "data_hiroshi/freqItem_nc.rds")




