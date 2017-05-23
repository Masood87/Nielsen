### NIELSEN "PRICING AND PROMOTION" PROJECT
# MA in Applied Economics (CERGE-EI)
# TEAM in order of alphabet: Kate, Masood, Reynolds, Robert, Sylva

rm(list = ls())

# load packages
library(data.table)
library(Rcpp)
library(dplyr)
library(magrittr)
library(plyr)

#########################
##### Loading data ######
#########################

# working directory
setwd("/Users/macbookair/Google Drive/TSR data")

# Weeks of data
weeks <- 472:536

# Importing and joining brands
brands <- data.table()
for (i in weeks) {
  x <- fread(paste0(i, "/brands.csv"))
  brands <- rbind(brands, x)
  rm(x)
}
dup <- which(duplicated(brands$brand_id))
brands <- brands[-dup,]

# Importing and joining categories
categories <- data.table()
for (i in weeks) {
  x <- fread(paste0(i, "/categories.csv"))
  categories <- rbind(categories, x)
  rm(x)
}
dup <- which(duplicated(categories$category_id))
categories <- categories[-dup,]

# Importing and joining manufacturers
manufacturers <- data.table()
for (i in weeks) {
  x <- fread(paste0(i, "/manufacturers.csv"), sep = ";")
  manufacturers <- rbind(manufacturers, x)
  rm(x)
}
dup <- which(duplicated(manufacturers$manufacturer_id))
manufacturers <- manufacturers[-dup,]

# Importing and joining products
products <- data.table()
for (i in weeks) {
  x <- fread(paste0(i, "/products.csv"))
  products <- rbind(products, x)
  rm(x)
}
dup <- which(duplicated(products$product_id))
products <- products[-dup,]

# Importing and joining sales data
sales <- data.table()
for (i in weeks) {
  x <- fread(paste0(i, "/rawdata.csv"), sep = ";") %>% as.data.table()
  sales <- rbind.fill(sales, x) %>% as.data.table()
  rm(x)
}
sales$period_id <- as.integer(sales$period_id)
sales$price <- as.numeric(sub(",", ".", sales$price))
sales$sales <- as.numeric(sub(",", ".", sales$sales))
sales$volume <- round(sales$sales / sales$price)
sales <- select(sales, -promo) %>% as.data.table()

# Importing periods
periods <- fread("periods.csv")

# Importing stores
stores <- fread("stores.csv")



#########################
##### Sanity Checks #####
#########################
# Sanity check 1: price 
weird_price <- sales[, .(max = max(price), min = min(price), uniq.week = n_distinct(period_id)), by = product_id] %>% 
  .[max > 5*min] %>% merge(products, by = "product_id") %>% select(c(1, 5, 2:4))

write.csv(weird_price, file = "#Results tables/products with weird prices - max(price) > min(price)*5")

weird_price <- weird_price[, product_id]
sales <- sales[!product_id %in% weird_price] %>% as.data.table()
rm(weird_price)

# Sanity check 2: products
weird_products <- sales[, .(uniq.week = n_distinct(period_id), uniq.store = n_distinct(store_id)), by = .(product_id)] %>% 
  .[uniq.week == 1 & uniq.store == 1] %>% merge(products, by = "product_id") %>% select(c(1,4,2,3))

write.csv(weird_products, file = "#Results tables/products sold only in one store and one week - weird?")
weird_products <- weird_products[, product_id]
sales <- sales[!product_id == weird_products] %>% as.data.table()

# Sanity check 3: Dummy Bucket Product
sales <- merge(sales, products, by = "product_id") %>% select(c(1,10,2:9)) %>% .[!product_name == "Dummy Bucket Product"] %>% as.data.table()


####################
##### Analysis #####
####################
# unique categories
merge(sales, categories, by = "category_id") %>% .[, category_name] %>% unique() %>% write.csv(file = "#Results tables/List of all categories")

### Most sold items
most.sold.products <- sales[, .(total.sales = sum(sales)), by = product_id] %>% merge(products, by = "product_id") %>% select(c(1,3,2)) %>% arrange(desc(total.sales)) %>% head(20)
write.csv(most.sold.products, file = "#Results tables/20 most sold products - all market")

most.sold.products.store <- sales[, .(total.sales = sum(sales)), by = .(store_id, product_id)] %>% merge(products, by = "product_id") %>% select(c(2,1,4,3)) %>% split(., .$store_id)
most.sold.products.store[[1]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.1")
most.sold.products.store[[2]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.2")
most.sold.products.store[[3]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.3")
most.sold.products.store[[4]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.4")
most.sold.products.store[[5]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.5")
most.sold.products.store[[6]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.6")
most.sold.products.store[[7]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.7")
most.sold.products.store[[8]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.8")
most.sold.products.store[[9]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.9")
most.sold.products.store[[10]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.10")
most.sold.products.store[[11]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.11")
most.sold.products.store[[12]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.12")
most.sold.products.store[[13]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.13")
most.sold.products.store[[14]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.14")
most.sold.products.store[[15]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.15")
most.sold.products.store[[16]] %>% arrange(desc(total.sales)) %>% head(5) %>% write.csv(file = "#Results tables/most.sold.products.store.16")

### Most sold categories
most.sold.categories <- sales[, .(total.sales = sum(sales)), by = category_id] %>% merge(categories, by = "category_id") %>% select(c(1,3,2)) %>% arrange(desc(total.sales)) %>% head(10)
write.csv(most.sold.categories, file = "#Results tables/10 most sold categories - all market")



sales[order(-sales)] #sorts data set by sales
sales[order(-sales), .(sum = sum(sales)), by = product_id]
sales[order(-sales), .(sum = sum(sales), uniq.week = n_distinct(period_id)), by = product_id]
sales[order(-sales), .(sum = sum(sales), median.price = median(price), uniq.week = n_distinct(period_id)), by = product_id]
top_sold_products <- sales[order(-sales), .(sum = sum(sales), median.price = median(price), uniq.week = n_distinct(period_id)), by = product_id] %>% 
  head(50) %>% merge(products, by = c("product_id")) %>% arrange(desc(sum))
top_sold_products <- select(top_sold_products, c(1,5,2:4))
write.csv(top_sold_products, file = "#Results tables/50 most sold products - all market")

top_sold_products_store <- sales[order(-sales), .(sum = sum(sales), median.price = median(price), uniq.week = n_distinct(period_id)), by = .(store_id, product_id)] %>% arrange(store_id, desc(sum)) %>% 
  merge(products, by = c("product_id")) %>% arrange(desc(sum)) %>% split(., .$store_id)

top_sold_products_store_1 <- top_sold_products_store[[1]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_1")
top_sold_products_store_2 <- top_sold_products_store[[2]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_2")
top_sold_products_store_3 <- top_sold_products_store[[3]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_3")
top_sold_products_store_4 <- top_sold_products_store[[4]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_4")
top_sold_products_store_5 <- top_sold_products_store[[5]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_5")
top_sold_products_store_6 <- top_sold_products_store[[6]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_6")
top_sold_products_store_7 <- top_sold_products_store[[7]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_7")
top_sold_products_store_8 <- top_sold_products_store[[8]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_8")
top_sold_products_store_9 <- top_sold_products_store[[9]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_9")
top_sold_products_store_10 <- top_sold_products_store[[10]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_10")
top_sold_products_store_11 <- top_sold_products_store[[11]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_11")
top_sold_products_store_12 <- top_sold_products_store[[12]] %>% head(20) %>% select(c(2,1,6,3:5)) %>% write.csv(file = "#Results tables/top_sold_products_store_12")


### market share of each store
store.share <- sales[, .(total.sales = sum(sales)), by = store_id]
store.share$total_sales <- sum(sales$sales)
store.share$market.share <- store.share$total.sales/store.share$total_sales
store.share <- select(store.share, c(1,4))
store.share$market.share <- round(store.share$market.share*100, 2)
store.share <- merge(store.share, stores, by = "store_id")
write.csv(store.share, file = "#Results tables/store.share")
store.retailer <- select(store.share, c(1,4,3,2))
store.retailer[, .(market.share = sum(market.share)), by = ret.id] %>% arrange(desc(market.share)) %>% write.csv(file = "#Results tables/retailer.share")

class(sales)
sales <- as.data.table(sales)


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
Most_sold_products_by_category <- data.table()
for (i in top_cat) {
  x <- sub_sales[category_id == i] %>% .[, .(sales.sum = sum(sales), sales.mean = mean(sales), sales.median = median(sales), sales.sd = sd(sales), uniq.weeks = n_distinct(period_id)), by = list(category_id, product_id)] %>% arrange(desc(sales.sum), desc(sales.mean)) %>% head(5)
  Most_sold_products_by_category <- rbind(Most_sold_products_by_category, x)
}
Most_sold_products_by_category <- merge(Most_sold_products_by_category, categories, by = c("category_id"))
Most_sold_products_by_category <- merge(Most_sold_products_by_category, products, by = c("product_id"))
Most_sold_products_by_category <- setcolorder(Most_sold_products_by_category, c(2,8,1,9,3:7))
write.csv(Most_sold_products_by_category, file = "#Results tables/5 Most sold products within 50 most sold category")

# Total sales by week
totalsales <- sales[, sum(sales), by="period_id"] %>% merge(periods, by = c("period_id")) %>% setcolorder(c(1,3:5,2))
write.csv(totalsales, file = "#Results tables/Total sales by week")
ggplot(aes(period_id, V1)) + geom_line() + labs(title = "Total sales")
dev.off()
ggsave("#Results tables/Plots/Total sales by week.pdf")


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






