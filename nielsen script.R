# Nielsen project
rm(list = ls())

# load packages
library(data.table)
library(Rcpp)
library(dplyr)
library(magrittr)
library(plyr)
library(doBy)

# working directory
setwd("/Users/macbookair/Google Drive/TSR data")

# Weeks of data
weeks <- list.dirs("/Users/macbookair/Downloads/TSR data", full.names = F)[-1]

# Importing and joining brands
brands <- data.frame()
for (i in weeks) {
  x <- fread(paste0(i, "/brands.csv"))
  brands <- rbind(brands, x)
  rm(x)
}
dup <- which(duplicated(brands$brand_id))
brands <- brands[-dup,]

# Importing and joining categories
categories <- data.frame()
for (i in weeks) {
  x <- fread(paste0(i, "/categories.csv"))
  categories <- rbind(categories, x)
  rm(x)
}
dup <- which(duplicated(categories$category_id))
categories <- categories[-dup,]

# Importing and joining manufacturers
manufacturers <- data.frame()
for (i in weeks) {
  x <- fread(paste0(i, "/manufacturers.csv"), sep = ";")
  manufacturers <- rbind(manufacturers, x)
  rm(x)
}
dup <- which(duplicated(manufacturers$manufacturer_id))
manufacturers <- manufacturers[-dup,]

# Importing and joining products
products <- data.frame()
for (i in weeks) {
  x <- fread(paste0(i, "/products.csv"))
  products <- rbind(products, x)
  rm(x)
}
dup <- which(duplicated(products$product_id))
products <- products[-dup,]

# Importing and joining sales data
sales <- data.frame()
for (i in weeks) {
  x <- fread(paste0(i, "/rawdata.csv"), sep = ";")
  sales <- rbind(sales, x)
  rm(x)
}
sales$period_id <- as.integer(sales$period_id)
sales$price <- as.numeric(sub(",", ".", sales$price))
sales$sales <- as.numeric(sub(",", ".", sales$sales))
sales$volume <- round(sales$sales / sales$price)

# Importing periods
periods <- fread("periods.csv")

# Importing stores
stores <- fread("stores.csv")

# Looking at the structure of data
str(brands) #"brand_id", "brand_name"
str(categories) #"category_id", "category_name"
str(manufacturers) #"manufacturer_id", "manufacturer_name"
str(products) #"product_id", "product_name"
str(sales) #"period_id", "product_id", "store_id", "category_id", "brand_id", "manufacturer_id", "price", "sales", "week"
str(periods) #"period_id", "period_name", "month", "year"
str(stores) #"store_id", "form", "ret.id"

# creating a sub-sample of sales data
sub_sales <- sample_frac(sales, .01, replace = T)

##########################################
##### Looking at the sub-sample data #####
##########################################

### General
str(sub_sales)
summary(sub_sales)


### Most sold categories of items by value of sales (sorted: highest sum, highest mean, lowest sd)
#here again I notice some of the categories recorded sales only in 1 week out of 65 (sd = NA), which could be problematic. So I filter them out
Most_sold_categories <- sub_sales[, .(sales.sum = sum(sales), sales.mean = mean(sales), sales.median = median(sales), sales.sd = sd(sales), uniq.items = n_distinct(product_id)), by = category_id] %>% 
  filter(!is.na(sales.sd)) %>% arrange(desc(sales.sum), desc(sales.mean), sales.sd) %>% head(50) %>%
  merge(categories, by.x = "category_id", by.y = "category_id") %>% arrange(desc(sales.sum), desc(sales.mean), sales.sd)
#another way to do it
#Most_sold_categories <- summaryBy(sales ~ category_id, FUN = c(sum, mean, median, sd, length), data = sub_sales) %>% arrange(desc(sales.sum), desc(sales.mean), sales.sd) %>% head(200)
Most_sold_categories <- Most_sold_categories[, c(1,7,2:6)] %>% as.data.table()
write.csv(Most_sold_categories, file = "#Results tables/50 Most sold categories")


# most sold products within top 50 categories
top_cat <- Most_sold_categories$category_id
Most_sold_products_by_category <- sub_sales[category_id %in% top_cat] %>% .[, .(sales.sum = sum(sales), sales.mean = mean(sales), sales.median = median(sales), sales.sd = sd(sales), uniq.items = n_distinct(product_id)), by = list(category_id, product_id)]
Most_sold_products_by_category <- split(x, x$category_id) %>% head(5)
write.csv(Most_sold_products_by_category, file = "#Results tables/Most sold products by category")


### Most sold items by volume (sorted: highest sum, highest mean, lowest sd)
summaryBy(volume ~ product_id, FUN = c(sum, mean, median, sd, length), data = sub_sales) %>% arrange(desc(volume.sum), desc(volume.mean), volume.sd) %>% as.tbl()
#here I notice some of the items recorded sales only in 1 week out of 65 (sd = NA), which could be problematic. So I filter them out
summaryBy(volume ~ product_id, FUN = c(sum, mean, median, sd, length), data = sub_sales) %>% filter(!is.na(volume.sd)) %>% arrange(desc(volume.sum), desc(volume.mean), volume.sd) %>% as.tbl()
#another way to do it
sub_sales[, list(volume.sum = sum(volume), volume.mean = mean(volume), volume.median = median(volume), volume.sd = sd(volume), volume.length = length(volume)), by = product_id] %>% filter(!is.na(volume.sd)) %>% arrange(desc(volume.sum), desc(volume.mean), volume.sd) %>% as.tbl()


### Most sold items by value of sales (sorted: highest sum, highest mean, lowest sd)
summaryBy(sales ~ product_id, FUN = c(sum, mean, median, sd, length), data = sub_sales) %>% arrange(desc(sales.sum), desc(sales.mean), sales.sd) %>% as.tbl()
#here again I notice some of the items recorded sales only in 1 week out of 65 (sd = NA), which could be problematic. So I filter them out
summaryBy(sales ~ product_id, FUN = c(sum, mean, median, sd, length), data = sub_sales) %>% filter(!is.na(sales.sd)) %>% arrange(desc(sales.sum), desc(sales.mean), sales.sd) %>% as.tbl()
#another way to do it
sub_sales[, list(sales.sum = sum(sales), sales.mean = mean(sales), sales.median = median(sales), sales.sd = sd(sales), sales.length = length(sales)), by = product_id] %>% filter(!is.na(sales.sd)) %>% arrange(desc(sales.sum), desc(sales.mean), sales.sd) %>% as.tbl()


### Overall value of sales by store (sorted: highest sum, highest mean, lowest sd)
summaryBy(sales ~ store_id, FUN = c(sum, mean, median, sd, length), data = sub_sales) %>% arrange(desc(sales.sum), desc(sales.mean), sales.sd) %>% as.tbl()
#another way to do it
sub_sales[, list(sales.sum = sum(sales), sales.mean = mean(sales), sales.median = median(sales), sales.sd = sd(sales), sales.length = length(sales)), by = store_id] %>% arrange(desc(sales.sum), desc(sales.mean), sales.sd) %>% as.tbl()


### Most sold item by value of sales by store (sorted: highest sum, highest mean, lowest sd)
summaryBy(sales ~ store_id + product_id, FUN = c(sum, mean, median, sd, length), data = sub_sales) %>% filter(!is.na(sales.sd)) %>% arrange(desc(sales.sum), desc(sales.mean), sales.sd) %>% as.tbl() %>% split(., .$store_id)


### Most sold item by volume by store (sorted: highest sum, highest mean, lowest sd)
summaryBy(volume ~ store_id + product_id, FUN = c(sum, mean, median, sd, length), data = sub_sales) %>% filter(!is.na(volume.sd)) %>% arrange(desc(volume.sum), desc(volume.mean), volume.sd) %>% as.tbl() %>% split(., .$store_id)


### Market share of each store in sales of each category
x <- summaryBy(sales ~ store_id + category_id, FUN = c(sum, mean, median, sd, length), data = sub_sales) %>% 
  split(., .$category_id) %>% do.call(rbind.data.frame, .) %>%
  .[, market.share := sales.sum / sum(sales.sum), by = "category_id"] %>% arrange(category_id, desc(market.share))

# Most sold categories by value of sales
y <- sub_sales[, list(sales.sum = sum(sales), sales.mean = mean(sales), sales.median = median(sales), sales.sd = sd(sales), sales.length = length(sales)), by = category_id] %>% filter(!is.na(sales.sd)) %>% arrange(desc(sales.sum), desc(sales.mean), sales.sd) %>% head()
y <- y[1] #I store names of the six most sold categories in y
### voilà --share of each store in total value of sales of the most sold categories ###
x[x$category_id %in% y$category_id,] %>% split(., .$category_id)






