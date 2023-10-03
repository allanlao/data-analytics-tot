
# what is the age distribution of the customers?
# what is the most profitable year?
# what is the most profitable month?
# which Gender has the most orders?
# which country/state generates the highest revenue?
# is there any correlation between the customer’s age and revenue?
# which category/subcategory generates the most profit?


install.packages("tidyverse")


library(dplyr)
library(ggplot2)


setwd("~/lao-analytics/bike")


df <- read.csv("Sales.csv")


str(df)

products_by_category <- df %>%  select(Product_Category) %>% 
  group_by(Product_Category) %>% summarise(Qty = n())
products_by_category


products_by_subcategory <- df %>%  select(Product_Category,Sub_Category) %>% 
  group_by(Product_Category,Sub_Category) %>% summarise(Qty = n())

#sales per month arranged highest to lowest
monthly_sales <- df %>% group_by(Month) %>% summarise(Total = sum(Unit_Price * Order_Quantity)) %>% 
  arrange(desc(Total))

#Qty sold per product

products_sold <- df %>% select(Product_Category,Product,Order_Quantity,Profit) %>%  group_by(Product_Category,Product) %>% summarise(Total = sum(Order_Quantity)) %>%  arrange(desc(Total)) 

#The Bestseller

best_seller <-products_sold %>%  head(1)



