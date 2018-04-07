# This is a sample script.
# Adjust accordingly to your input mapping, output mapping
# and desired functionality.
library(tidyverse)
library(dplyr)

cust <- read_csv(file = "in/tables/customers.csv")

# cleaned orders without unknown
orders <- read_csv(file = "in/tables/orders.csv")
orders <- orders %>% 
  filter(customer_web_id != "Unknown-1" & customer_web_id != "Unknown-2")
  

joinedCustomers_Orders <- left_join(cust, orders, by = "customer_web_id") %>% 
  filter(is_paid == "Yes" & state == "distributed")

ordersPerCustomer <- data.frame(table(joinedCustomers_Orders[,c(1,12)]$customer_web_id))
colnames(ordersPerCustomer)[1] <- "customer_web_id" 
colnames(ordersPerCustomer)[2] <- "numberOfOrders" 


#####
#### Customer value obrat
#####
colnames(joinedCustomers_Orders)

cv <- sum()










write.csv(ordersPerCustomer, file = "out/tables/ordersPerCustomer.csv", row.names = FALSE)
