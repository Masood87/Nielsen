### NIELSEN "PRICING AND PROMOTION" PROJECT

# DATA ANALYSIS

# MA in Applied Economics (CERGE-EI)
# TEAM in order of alphabet: Kate, Masood, Reynolds, Robert, Sylva

rm(list = ls())

# load packages
library(data.table)
library(dplyr)
library(magrittr)
library(plyr)

#########################
##### Loading data ######
#########################

setwd("/Users/macbookair/Downloads/TSR data")

departments <- fread("departments.master.csv")
products <- fread("products.master.csv")
sales <- fread("sales.master.csv")
stores <- fread("stores.csv")
periods <- fread("periods.csv")
brands <- fread("brands.master.csv")
manufacturers <- fread("manufacturers.master.csv")
qbrik.imdb <- fread("qbrik.imdb.csv")
sales.promo <- fread("sales.promo.csv")


##### ## ##### ##### ## #####
 ## ### ## ANALYSIS ## ### ##
## ##### ## ##### ## ##### ##

# unique categories and departments
merge(sales.promo, departments, by = "category_id") %>% .[, category_name] %>% unique() #%>% write.csv(file = "#Results tables/List of all categories.csv")
merge(sales.promo, departments, by = "category_id") %>% .[, department] %>% unique()

# top departments with 80% of sales
top.sold.dept <- merge(sales.promo, departments, by = "category_id") %>% .[, .(total.sales = sum(sales)), by = department] %>% arrange(desc(total.sales))
top.sold.dept$percent <- (top.sold.dept$total.sales / sum(top.sold.dept$total.sales))*100
top.sold.dept$cum.percent <- cumsum(top.sold.dept$percent)
top.sold.dept <- subset(top.sold.dept, cum.percent <= 80)
#write.csv(top.sold.dept, "#Results tables/top.sold.departments.csv")

# 
dept.kvi <- top.sold.dept[, department]
top.prod.dept <- merge(sales.promo, departments, by = "category_id") %>% subset(department %in% dept.kvi) %>% select(1, 3:6, 9, 11) %>% write.csv("for sylva.csv")
  .[, .(total.sales = sum(sales)), by = .(department, product_id)] %>% merge(products, by = "product_id") %>% 
  select(2,1,4,3) %>% arrange(department, desc(total.sales)) %>% split(., .$department)
for (i in seq_along(top.prod.dept)) {
  top.prod.dept[[i]]$percent <- round((top.prod.dept[[i]]$total.sales / sum(top.prod.dept[[i]]$total.sales)) * 100, 3)
  top.prod.dept[[i]]$cum.percent <- round(cumsum(top.prod.dept[[i]]$percent), 3)
  top.prod.dept[[i]] <- subset(top.prod.dept[[i]], cum.percent < 50)
}
write.csv(do.call(rbind.data.frame, top.prod.dept), "top.prod.dept.csv")


# for sylva
top.prod <- fread("top.prod kvi.csv")
x <- merge(sales.promo, departments, by = "category_id")
top.prod <- select(top.prod, product_id)
y <- left_join(top.prod, x, by = "product_id")
names(y)
write.csv(y, "forsylva.csv")

# for rey
rey <- merge(sales.promo, departments, by = "category_id") %>% 
  .[, .(promo = mean(promo), total.sales = sum(sales), price.change = sd(price)), by = .(store_id, department, product_id)] %>% 
  merge(products, by = "product_id") %>% select(2,3,1,7,4:6) %>%
  arrange(desc(total.sales)) %>% split(., .$department)
for (i in seq_along(rey)) {
  rey[[i]] <- subset(rey[[i]], is.na(price.change))
  rey[[i]] <- subset(rey[[i]], promo == 1)
}
write.csv(do.call(rbind.data.frame, rey), "for rey.csv")


table(sales.promo$promo)
table(sales.promo$nc_promo.y)
table(sales.promo$nc_promo.x, sales.promo$nc_promo.y, exclude = NULL)

# top products with 80% of sales of top.sold.dept
top.sold.dept <- top.sold.dept[, department]
top.sold.prod <- merge(sales.promo, departments, by = "category_id") %>% subset(department %in% top.sold.dept) %>%
  .[, .(total.sales = sum(sales)), by = .(department, product_id)] %>% arrange(department, desc(total.sales)) %>% split(., .$department)

for (i in seq_along(top.sold.prod)) {
  top.sold.prod[[i]]$percent <- round((top.sold.prod[[i]]$total.sales / sum(top.sold.prod[[i]]$total.sales)) * 100, 3)
  top.sold.prod[[i]]$cum.percent <- round(cumsum(top.sold.prod[[i]]$percent), 3)
  top.sold.prod[[i]] <- subset(top.sold.prod[[i]], percent >= 1) #tea has 3 products.. add more products???
}
top.sold.prod <- do.call(rbind.data.frame, top.sold.prod) %>% merge(products, by = "product_id") %>% select(2,1,6,3:5) %>% arrange(department, desc(total.sales))
#write.csv(top.sold.prod, "#Results tables/top.sold.prod.csv")


# top departments within each store
top.dept.store <- merge(sales.promo, departments, by = "category_id") %>% .[, .(total.sales = sum(sales)), by = .(store_id, department)] %>%
  arrange(store_id, desc(total.sales)) %>% split(., .$store_id)

for (i in seq_along(top.dept.store)) {
  top.dept.store[[i]]$percent <- round((top.dept.store[[i]]$total.sales / sum(top.dept.store[[i]]$total.sales))*100, 3)
  top.dept.store[[i]]$cum.percent <- round(cumsum(top.dept.store[[i]]$percent), 3)
  top.dept.store[[i]] <- subset(top.dept.store[[i]], cum.percent <= 80)
}
#write.csv(do.call(rbind.data.frame, top.dept.store), "#Results tables/top.dept.store.csv")


# top products within each store
for (i in names(top.dept.store)) { top.dept.store[[i]] <- top.dept.store[[i]][, department] }

top.prod.store <- merge(sales.promo, departments, by = "category_id") %>% split(., .$store_id)

for (i in names(top.dept.store)) {
  assign(paste("store", i, sep = "."), 
         top.prod.store[[noquote(i)]] %>% subset(department %in% top.dept.store[[noquote(i)]]) %>% 
           .[, .(total.sales = sum(sales)), by = .(store_id, department, product_id)] %>%
           arrange(department, desc(total.sales)) %>% split(., .$department))
}

for (i in seq_along(store.X20800)) {
  store.X20800[[i]]$percent <- round((store.X20800[[i]]$total.sales / sum(store.X20800[[i]]$total.sales))*100, 3)
  store.X20800[[i]]$cum.percent <- round(cumsum(store.X20800[[i]]$percent), 3)
  store.X20800[[i]] <- subset(store.X20800[[i]], percent >= 1)
}
for (i in seq_along(store.X40716)) {
  store.X40716[[i]]$percent <- round((store.X40716[[i]]$total.sales / sum(store.X40716[[i]]$total.sales))*100, 3)
  store.X40716[[i]]$cum.percent <- round(cumsum(store.X40716[[i]]$percent), 3)
  store.X40716[[i]] <- subset(store.X40716[[i]], percent >= 1)
}
for (i in seq_along(store.X40732)) {
  store.X40732[[i]]$percent <- round((store.X40732[[i]]$total.sales / sum(store.X40732[[i]]$total.sales))*100, 3)
  store.X40732[[i]]$cum.percent <- round(cumsum(store.X40732[[i]]$percent), 3)
  store.X40732[[i]] <- subset(store.X40732[[i]], percent >= 1)
}
for (i in seq_along(store.X40800)) {
  store.X40800[[i]]$percent <- round((store.X40800[[i]]$total.sales / sum(store.X40800[[i]]$total.sales))*100, 3)
  store.X40800[[i]]$cum.percent <- round(cumsum(store.X40800[[i]]$percent), 3)
  store.X40800[[i]] <- subset(store.X40800[[i]], percent >= 1)
}
for (i in seq_along(store.X40849)) {
  store.X40849[[i]]$percent <- round((store.X40849[[i]]$total.sales / sum(store.X40849[[i]]$total.sales))*100, 3)
  store.X40849[[i]]$cum.percent <- round(cumsum(store.X40849[[i]]$percent), 3)
  store.X40849[[i]] <- subset(store.X40849[[i]], percent >= 1)
}
for (i in seq_along(store.X40885)) {
  store.X40885[[i]]$percent <- round((store.X40885[[i]]$total.sales / sum(store.X40885[[i]]$total.sales))*100, 3)
  store.X40885[[i]]$cum.percent <- round(cumsum(store.X40885[[i]]$percent), 3)
  store.X40885[[i]] <- subset(store.X40885[[i]], percent >= 1)
}
for (i in seq_along(store.X71200)) {
  store.X71200[[i]]$percent <- round((store.X71200[[i]]$total.sales / sum(store.X71200[[i]]$total.sales))*100, 3)
  store.X71200[[i]]$cum.percent <- round(cumsum(store.X71200[[i]]$percent), 3)
  store.X71200[[i]] <- subset(store.X71200[[i]], percent >= 1)
}
for (i in seq_along(store.X73100)) {
  store.X73100[[i]]$percent <- round((store.X73100[[i]]$total.sales / sum(store.X73100[[i]]$total.sales))*100, 3)
  store.X73100[[i]]$cum.percent <- round(cumsum(store.X73100[[i]]$percent), 3)
  store.X73100[[i]] <- subset(store.X73100[[i]], percent >= 1)
}
for (i in seq_along(store.X78000)) {
  store.X78000[[i]]$percent <- round((store.X78000[[i]]$total.sales / sum(store.X78000[[i]]$total.sales))*100, 3)
  store.X78000[[i]]$cum.percent <- round(cumsum(store.X78000[[i]]$percent), 3)
  store.X78000[[i]] <- subset(store.X78000[[i]], percent >= 1)
}
for (i in seq_along(store.XA0800)) {
  store.XA0800[[i]]$percent <- round((store.XA0800[[i]]$total.sales / sum(store.XA0800[[i]]$total.sales))*100, 3)
  store.XA0800[[i]]$cum.percent <- round(cumsum(store.XA0800[[i]]$percent), 3)
  store.XA0800[[i]] <- subset(store.XA0800[[i]], percent >= 1)
}
for (i in seq_along(store.XA7100)) {
  store.XA7100[[i]]$percent <- round((store.XA7100[[i]]$total.sales / sum(store.XA7100[[i]]$total.sales))*100, 3)
  store.XA7100[[i]]$cum.percent <- round(cumsum(store.XA7100[[i]]$percent), 3)
  store.XA7100[[i]] <- subset(store.XA7100[[i]], percent >= 1)
}
for (i in seq_along(store.XI0134)) {
  store.XI0134[[i]]$percent <- round((store.XI0134[[i]]$total.sales / sum(store.XI0134[[i]]$total.sales))*100, 3)
  store.XI0134[[i]]$cum.percent <- round(cumsum(store.XI0134[[i]]$percent), 3)
  store.XI0134[[i]] <- subset(store.XI0134[[i]], percent >= 1)
}
for (i in seq_along(store.XM0964)) {
  store.XM0964[[i]]$percent <- round((store.XM0964[[i]]$total.sales / sum(store.XM0964[[i]]$total.sales))*100, 3)
  store.XM0964[[i]]$cum.percent <- round(cumsum(store.XM0964[[i]]$percent), 3)
  store.XM0964[[i]] <- subset(store.XM0964[[i]], percent >= 1)
}
for (i in seq_along(store.XP2702)) {
  store.XP2702[[i]]$percent <- round((store.XP2702[[i]]$total.sales / sum(store.XP2702[[i]]$total.sales))*100, 3)
  store.XP2702[[i]]$cum.percent <- round(cumsum(store.XP2702[[i]]$percent), 3)
  store.XP2702[[i]] <- subset(store.XP2702[[i]], percent >= 1)
}
for (i in seq_along(store.XP2703)) {
  store.XP2703[[i]]$percent <- round((store.XP2703[[i]]$total.sales / sum(store.XP2703[[i]]$total.sales))*100, 3)
  store.XP2703[[i]]$cum.percent <- round(cumsum(store.XP2703[[i]]$percent), 3)
  store.XP2703[[i]] <- subset(store.XP2703[[i]], percent >= 1)
}
for (i in seq_along(store.XT3800)) {
  store.XT3800[[i]]$percent <- round((store.XT3800[[i]]$total.sales / sum(store.XT3800[[i]]$total.sales))*100, 3)
  store.XT3800[[i]]$cum.percent <- round(cumsum(store.XT3800[[i]]$percent), 3)
  store.XT3800[[i]] <- subset(store.XT3800[[i]], percent >= 1)
}

write.csv(merge(do.call(rbind.data.frame, store.X20800), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.X20800.csv")
write.csv(merge(do.call(rbind.data.frame, store.X40716), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.X40716.csv")
write.csv(merge(do.call(rbind.data.frame, store.X40732), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.X40732.csv")
write.csv(merge(do.call(rbind.data.frame, store.X40800), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.X40800.csv")
write.csv(merge(do.call(rbind.data.frame, store.X40849), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.X40849.csv")
write.csv(merge(do.call(rbind.data.frame, store.X40885), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.X40885.csv")
write.csv(merge(do.call(rbind.data.frame, store.X71200), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.X71200.csv")
write.csv(merge(do.call(rbind.data.frame, store.X73100), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.X73100.csv")
write.csv(merge(do.call(rbind.data.frame, store.X78000), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.X78000.csv")
write.csv(merge(do.call(rbind.data.frame, store.XA0800), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.XA0800.csv")
write.csv(merge(do.call(rbind.data.frame, store.XA7100), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.XA7100.csv")
write.csv(merge(do.call(rbind.data.frame, store.XI0134), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.XI0134.csv")
write.csv(merge(do.call(rbind.data.frame, store.XM0964), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.XM0964.csv")
write.csv(merge(do.call(rbind.data.frame, store.XP2702), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.XP2702.csv")
write.csv(merge(do.call(rbind.data.frame, store.XP2703), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.XP2703.csv")
write.csv(merge(do.call(rbind.data.frame, store.XT3800), products, by = "product_id") %>% select(2,3,1,7,4:6), "#Results tables/store.XT3800.csv")

# for (i in x) {
#   for (j in seq_along(i)) {
#     i[[j]]$percent <- round((i[[j]]$total.sales / sum(i[[j]]$total.sales)) * 100, 3)
#     i[[j]]$cum.percent <- round(cumsum(i[[j]]$percent), 3)
#     i[[j]] <- subset(i[[j]], percent >= 1)
#     }
# }

glimpse(store.X20800)
summary(sales.promo)
corr(sales.promo$nc_regpr.x)
summary(sales.promo$nc_regpr.x)
summary(sales.promo$nc_regpr.y)



# 









# unique (new) categories
merge(sales, categories, by = "category_id") %>% .[, `New Categories`] %>% unique() #%>% write.csv(file = "#Results tables/List of all categories.csv")

# 1. Top selling products
sales[, .(total.sales = sum(sales), uniq.store = n_distinct(store_id), uniq.week = n_distinct(period_id)), by = product_id] %>% 
  merge(products, by = "product_id") %>% select(c(1,5,2:4)) %>% arrange(desc(total.sales)) %>% head(20)

# 2. Top selling products by retailer
merge(sales, stores, by = "store_id") %>% .[, .(total.sales = sum(sales), uniq.week = n_distinct(period_id), uniq.store = n_distinct(store_id)), by = .(ret.id, product_id)] %>% 
  merge(products, by = "product_id") %>% select(c(2,5,1,6,3,4)) %>% arrange(desc(total.sales)) %>% split(., .$ret.id)

# 3. Top selling products by store
sales[, .(total.sales = sum(sales), uniq.week = n_distinct(period_id)), by = .(store_id, product_id)] %>% 
  merge(products, by = "product_id") %>% select(c(2,1,5,3,4)) %>% arrange(desc(total.sales)) %>% split(., .$store_id)

# 4. Top selling products by category
sales[, .(total.sales = sum(sales), uniq.week = n_distinct(period_id), uniq.store = n_distinct(store_id)), by = .(category_id, product_id)] %>%
  arrange(desc(total.sales)) %>% split(.$category_id)




# 4. Top selling categories
sales[, .(total.sales = sum(sales), uniq.weeks = n_distinct(period_id), uniq.stores = n_distinct(store_id)), by = category_id] %>% 
  merge(categories, by = "category_id") %>% select(c(6,1,3,4,2)) %>% arrange(desc(total.sales)) %>% 
  .[, .(total.sales = sum(total.sales)), by = `New Categories`] %>% arrange(desc(total.sales))

names(categories)



#





# Top selling (new) categories
merge(sales, categories, by = "category_id") %>% .[, category_name] %>% unique() #%>% write.csv(file = "#Results tables/Top selling (new) categories.csv")

### Most sold items
sales[, .(total.sales = sum(sales)), by = product_id] %>% merge(products, by = product_id) %>% select(c(1,3,2)) %>% arrange(desc(total.sales)) %>% tail(20) #%>% write.csv(file = "#Results tables/20 most sold products - all market.csv")

### Most sold categories
sales[, .(total.sales = sum(sales), uniq.products = n_distinct(product_id)), by = category_id] %>% merge(categories, by = "category_id") %>% select(c(1, 4, 3, 2)) %>% arrange(desc(total.sales)) %>% tail(20) #%>% write.csv(file = "#Results tables/20 most sold categories - all market.csv")

### Most sold items per store
sold.prod.store <- sales[, .(total.sales = sum(sales), uniq.products = n_distinct(product_id)), by = .(store_id, product_id)] %>% merge(products, by = "product_id") %>% select(c(2,1,5,4,3)) %>% arrange(store_id, desc(total.sales)) %>% split(., .$store_id)
sold.prod.store <- lapply(sold.prod.store, head, 10)
sold.prod.store <- do.call("rbind", lapply(sold.prod.store, as.data.frame)) 
sold.prod.store$store_id <- rownames(sold.prod.store) %>% sub(".\\d{1,}", "", .)
# write.csv("#Results tables/10 most sold items - by store.csv")
data.frame(sold.prod.store)


most.sold.products.store <- sales[, .(total.sales = sum(sales)), by = .(store_id, product_id)] %>% merge(products, by = "product_id") %>% select(c(2,1,4,3)) %>% split(., .$store_id)
most.sold.products.store[[1]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.1")
most.sold.products.store[[2]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.2")
most.sold.products.store[[3]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.3")
most.sold.products.store[[4]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.4")
most.sold.products.store[[5]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.5")
most.sold.products.store[[6]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.6")
most.sold.products.store[[7]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.7")
most.sold.products.store[[8]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.8")
most.sold.products.store[[9]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.9")
most.sold.products.store[[10]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.10")
most.sold.products.store[[11]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.11")
most.sold.products.store[[12]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.12")
most.sold.products.store[[13]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.13")
most.sold.products.store[[14]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.14")
most.sold.products.store[[15]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.15")
most.sold.products.store[[16]] %>% arrange(desc(total.sales)) %>% head(5) #%>% write.csv(file = "#Results tables/most.sold.products.store.16")

### Most sold categories
most.sold.categories <- sales[, .(total.sales = sum(sales)), by = category_id] %>% merge(categories, by = "category_id") %>% select(c(1,3,2)) %>% arrange(desc(total.sales)) %>% head(10)
# write.csv(most.sold.categories, file = "#Results tables/10 most sold categories - all market")



sales[order(-sales)] #sorts data set by sales
sales[order(-sales), .(sum = sum(sales)), by = product_id]
sales[order(-sales), .(sum = sum(sales), uniq.week = n_distinct(period_id)), by = product_id]
sales[order(-sales), .(sum = sum(sales), median.price = median(price), uniq.week = n_distinct(period_id)), by = product_id]
top_sold_products <- sales[order(-sales), .(sum = sum(sales), median.price = median(price), uniq.week = n_distinct(period_id)), by = product_id] %>% 
  head(50) %>% merge(products, by = c("product_id")) %>% arrange(desc(sum))
top_sold_products <- select(top_sold_products, c(1,5,2:4))
# write.csv(top_sold_products, file = "#Results tables/50 most sold products - all market")

top_sold_products_store <- sales[order(-sales), .(sum = sum(sales), median.price = median(price), uniq.week = n_distinct(period_id)), by = .(store_id, product_id)] %>% arrange(store_id, desc(sum)) %>% 
  merge(products, by = c("product_id")) %>% arrange(desc(sum)) %>% split(., .$store_id)

top_sold_products_store_1 <- top_sold_products_store[[1]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_1")
top_sold_products_store_2 <- top_sold_products_store[[2]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_2")
top_sold_products_store_3 <- top_sold_products_store[[3]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_3")
top_sold_products_store_4 <- top_sold_products_store[[4]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_4")
top_sold_products_store_5 <- top_sold_products_store[[5]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_5")
top_sold_products_store_6 <- top_sold_products_store[[6]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_6")
top_sold_products_store_7 <- top_sold_products_store[[7]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_7")
top_sold_products_store_8 <- top_sold_products_store[[8]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_8")
top_sold_products_store_9 <- top_sold_products_store[[9]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_9")
top_sold_products_store_10 <- top_sold_products_store[[10]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_10")
top_sold_products_store_11 <- top_sold_products_store[[11]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_11")
top_sold_products_store_12 <- top_sold_products_store[[12]] %>% head(20) %>% select(c(2,1,6,3:5)) #%>% write.csv(file = "#Results tables/top_sold_products_store_12")


### market share of each store
store.share <- sales[, .(total.sales = sum(sales)), by = store_id]
store.share$total_sales <- sum(sales$sales)
store.share$market.share <- store.share$total.sales/store.share$total_sales
store.share <- select(store.share, c(1,4))
store.share$market.share <- round(store.share$market.share*100, 2)
store.share <- merge(store.share, stores, by = "store_id")
# write.csv(store.share, file = "#Results tables/store.share")
store.retailer <- select(store.share, c(1,4,3,2))
store.retailer[, .(market.share = sum(market.share)), by = ret.id] %>% arrange(desc(market.share)) #%>% write.csv(file = "#Results tables/retailer.share")

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
# write.csv(Most_sold_categories, file = "#Results tables/50 Most sold categories")


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
# write.csv(Most_sold_products_by_category, file = "#Results tables/5 Most sold products within 50 most sold category")

# Total sales by week
totalsales <- sales[, sum(sales), by="period_id"] %>% merge(periods, by = c("period_id")) %>% setcolorder(c(1,3:5,2))
# write.csv(totalsales, file = "#Results tables/Total sales by week")
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






