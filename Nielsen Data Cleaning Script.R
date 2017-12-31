### NIELSEN "PRICING AND PROMOTION" PROJECT

# DATA CLEANING

# MA in Applied Economics (CERGE-EI)
# TEAM in order of alphabet: Kate, Masood, Reynolds, Robert, Sylva

rm(list = ls())

# load packages
library(data.table)
library(dplyr)
library(magrittr)
library(foreign)

#########################
##### Loading data ######
#########################

# working directory
setwd("/Users/macbookair/Google Drive/TSR data")

# Weeks of data
weeks <- 472:536

# brands
brands <- data.table()
for (i in weeks) {
  x <- fread(paste0(i, "/brands.csv"))
  brands <- rbind(brands, x)
  rm(x)
}
dup <- which(duplicated(brands$brand_id))
brands <- brands[-dup,]
#write.csv(brands, "brands.master.csv", row.names = FALSE)


# categories
categories <- data.table()
for (i in weeks) {
  x <- fread(paste0(i, "/categories.csv"))
  categories <- rbind(categories, x)
  rm(x)
}
dup <- which(duplicated(categories$category_id))
categories <- categories[-dup,]
categories <- fread("New Categories.csv") %>% merge(categories, by = "category_name") %>% select(c(6,1,5,2))
#write.csv(categories, "categories.master.csv", row.names = FALSE)


# manufacturers
manufacturers <- data.table()
for (i in weeks) {
  x <- fread(paste0(i, "/manufacturers.csv"), sep = ";")
  manufacturers <- rbind(manufacturers, x)
  rm(x)
}
dup <- which(duplicated(manufacturers$manufacturer_id))
manufacturers <- manufacturers[-dup,]
#write.csv(manufacturers, "manufacturers.master.csv", row.names = FALSE)


# products
products <- data.table()
for (i in weeks) {
  x <- fread(paste0(i, "/products.csv"))
  products <- rbind(products, x)
  rm(x)
}
dup <- which(duplicated(products$product_id))
products <- products[-dup,]
#write.csv(products, "products.master.csv", row.names = FALSE)


# sales
sales <- data.table()
for (i in weeks) {
  x <- fread(paste0(i, "/rawdata.csv"), sep = ";")
  sales <- rbind.fill(sales, x)
  rm(x)
}
sales$period_id <- as.integer(sales$period_id)
sales$price <- as.numeric(sub(",", ".", sales$price))
sales$sales <- as.numeric(sub(",", ".", sales$sales))
sales$volume <- round(sales$sales / sales$price)
sales <- as.data.table(sales)
sales[, "promo" := NULL]


# stores
stores <- fread("stores.csv")


# qbrik and qbrik1
qbrik <- fread("SCT data/qbrik.dsv")
qbrik$NC_REGPR <- as.numeric(sub(",", ".", qbrik$NC_REGPR))
qbrik$NC_SALES <- as.numeric(sub(",", ".", qbrik$NC_SALES))
qbrik$NC_TSBASE <- as.numeric(sub(",", ".", qbrik$NC_TSBASE))
qbrik$NC_VALUE <- as.numeric(sub(",", ".", qbrik$NC_VALUE))
qbrik$NC_XSBASE <- as.numeric(sub(",", ".", qbrik$NC_XSBASE))
qbrik1 <- fread("SCT data/qbrik1.dsv")
qbrik1$NC_REGPR <- as.numeric(sub(",", ".", qbrik1$NC_REGPR))
qbrik1$NC_SALES <- as.numeric(sub(",", ".", qbrik1$NC_SALES))
qbrik1$NC_TSBASE <- as.numeric(sub(",", ".", qbrik1$NC_TSBASE))
qbrik1$NC_VALUE <- as.numeric(sub(",", ".", qbrik1$NC_VALUE))
qbrik1$NC_XSBASE <- as.numeric(sub(",", ".", qbrik1$NC_XSBASE))
qbrik.master <- rbind(qbrik, qbrik1)
write.csv(qbrik, "qbrik.master.csv", row.names = FALSE)


# imdb: problem loading
imdb <- fread("SCT data/IMDB.csv")
qbrik.imdb <- left_join(qbrik, imdb, by = c("AC_PCODE" = "P_CODE")) %>% as.data.table()
write.csv(qbrik.imdb, "qbrik.imdb.csv", row.names = FALSE)


# I want to merge imdb and qbrik
sales.promo <- left_join(sales, qbrik.imdb, by = c("product_id" = "ac_pcode", "store_id" = "ac_nshopid", "period_id" = "nc_periodid")) %>% as.data.table()
sales.promo <- left_join(sales.promo, qbrik.imdb, by = c("product_id" = "ppcode", "store_id" = "ac_nshopid", "period_id" = "nc_periodid")) %>% as.data.table()
# write.csv(sales.promo, "sales.promo.csv")



#########################
##### Sanity Checks #####
#########################
# Sanity check 1: price 
weird_price <- sales[, .(max = max(price), min = min(price), uniq.week = n_distinct(period_id)), by = product_id] %>% 
  .[max > 5*min] %>% merge(products, by = "product_id") %>% select(c(1, 5, 2:4))

#write.csv(weird_price, file = "#Results tables/products with weird prices - max(price) > min(price)*5.csv")

weird_price <- weird_price[, product_id]
sales <- sales[!product_id %in% weird_price] %>% as.data.table()
rm(weird_price)


# Sanity check 2: products
weird_products <- sales[, .(uniq.week = n_distinct(period_id), uniq.store = n_distinct(store_id)), by = .(product_id)] %>% 
  .[uniq.week == 1 & uniq.store == 1] %>% merge(., products, by = "product_id") %>% select(c(1,4,2,3))
# Q: are these from one store? one period? A: No, it comes from all stores without apparent pattern
#write.csv(weird_products, file = "#Results tables/products sold only in one store and one week - weird?.csv")
weird_products <- weird_products[, product_id]
sales <- sales[!product_id == weird_products] %>% as.data.table()


# Sanity check 3: Dummy Bucket Product
sales <- merge(sales, products, by = "product_id") %>% select(c(1,10,2:9)) %>% .[!product_name == "Dummy Bucket Product"] %>% as.data.table()
write.csv(sales, file = "sales.master.csv", row.names = FALSE)
#merge(sales, products, by = "product_id") %>% select(c(1,10,2:9)) %>% .[product_name == "Dummy Bucket Product"] %>% write.csv(file = "#Results tables/dummy bucket prodct.csv")

# Sanity check 4: 
