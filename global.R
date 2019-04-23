###
### Katy's Shiny Project - Evidence of Pink Tax in Amazon Toy Dataset (Kaggle) - April 2019
###

### TO DO
# gender price graph
# add dataset thought process & conclusions
# beautify
# json datasets

## global.R ##

#### set up #####################################################

library(shinydashboard)
library(shiny)
library(DT)
library(googleVis)
library(tidyverse)
library(ggplot2)
library(data.table)

#setwd("C:/Users/Katy/Desktop/NYCDSA Bootcamp 17/R Stuff/Shiny Project")

#### clean up ####################################################

# Read rawdata
rawdf = fread("./rawdata_amazon_co-ecommerce_sample.csv") 

# Select relevant columns
rawdf = rawdf %>% select(-uniq_id, -number_available_in_stock, -number_of_answered_questions, 
                           -customers_who_bought_this_item_also_bought, -customer_questions_and_answers,
                           -items_customers_buy_after_viewing_this_item, -sellers)

# Mutating Data
rawdf = rawdf %>% separate(amazon_category_and_sub_category, c("Root", "Category"), extra = "drop", fill = "right", sep = " > ") %>% mutate( 
  average_review_rating = as.numeric(gsub(" out of 5 stars", "", average_review_rating)), # change to number
  number_of_reviews = as.numeric(number_of_reviews), # change to number
  price = as.numeric(substring(price, 3)), # change to number & multiple by sterling/usd conversion
  manufacturer = as.factor(manufacturer), # change to factor for efficiency
  Category = as.factor(Category)) %>% rename(Manufacturer = manufacturer, Reviews = number_of_reviews, Avg_Rating = average_review_rating)

# Filter blanks for PRICES, others ignored - left with ~8.5K data rows
rawdf = rawdf %>% filter(price != "" & !is.na(price)) 

#### Define Gender Value #########################################

# THIS SECTION WILL BE EXPANDED ON LATER, PROVIDING VALUES BASED ON KEYWORD TIERS & COUNTS RATHER THAN T OR F

# core gender keywords
gwords1 = c(" girl ", " girls ", "daughter")
bwords1 = c(" boy "," boys ", " son", "grandson") # space before son to remove any "reaSON" or "lesSON" kind of words

# function takes list of keywords, dataframe, and gender to create new columns girly/boyish with T/F if keywords found
update_cleandf_new_keywords = function(girlwords, boywords, rawdf) {
  
  tempdf = rawdf
  
  # look for any keywords in columns 1, 8:11 - description, product_decription, product_information, customer_reviews
  tempdf$girly = !!rowSums(sapply(tempdf[c(1,11,8,9,10)], grepl, pattern = paste(girlwords, collapse = "|"), ignore.case=TRUE))
  tempdf$boyish = !!rowSums(sapply(tempdf[c(1,11,8,9,10)], grepl, pattern = paste(boywords, collapse = "|"), ignore.case=TRUE))
  
  # creates category column based on T/F values
  tempdf = tempdf %>% mutate(gorb = as.factor(ifelse((girly == T & boyish == F), "girl", ifelse((girly == F & boyish == T), "boy", "neutral"))))

  # remove wordy & unecessary columns
  tempdf = tempdf %>% select(-description, -product_description, -product_information, -customer_reviews, -Root)
  
  return (tempdf)
  
}

# does both Fvar & Ttest. 
# Returns TRUE if VAR is same or girl boy prices are same
gb_stat_test = function(df, testtype) {
  
  girlprices = unlist(df %>% filter(gorb=="girl") %>% select(price))
  boyprices = unlist(df %>% filter(gorb=="boy") %>% select(price))
  
  if (testtype == "Fvar") { test = var.test(girlprices, boyprices)}
  if (testtype == "Ttest") { test = t.test(girlprices, boyprices, alternative = "greater") }
  
  #return(test$p.value)
  return (test$p.value >= 0.05)
  
}

# levels(cleandf$Category) #148
update_subdf = function(df) {
  
  # make subdf
  subdf = df %>% select(Category, price, gorb) %>% filter(!is.na(Category)) %>% mutate(Category = as.character(Category))
  
  # choose only categories that have at least 5 girl & 5 boy items - only 26/148 of core keywords
  gsubdf = subdf %>% filter(gorb == "girl") %>% count(Category) %>% filter(n>5)
  bsubdf = subdf %>% filter(gorb == "boy") %>% count(Category) %>% filter(n>5)
  subdf2 = inner_join(gsubdf, bsubdf, by = "Category") 
  subdf = inner_join(subdf, subdf2, by = "Category")
  
  # for each category, calculate average prices
  subdf3 = subdf %>% group_by(Category, gorb) %>% summarise(avg = mean(price)) %>% spread(gorb, avg) %>% select(-neutral) %>% rename(avg_b_price = boy, avg_g_price = girl)
  subdf2 = inner_join(subdf2, subdf3, by = "Category")
  
  # for each category, calculate if VARsame and if G>B prices
  subdf3 = subdf %>% group_by(Category) %>% do(data.frame(varsame = gb_stat_test(., "Fvar"), g_gtr_b = !gb_stat_test(., "Ttest")))
  subdf2 = inner_join(subdf2, subdf3, by = "Category") %>% rename(b_count = n.y, g_count = n.x) %>% mutate(avg_b_price = round(avg_b_price, 2), avg_g_price = round(avg_g_price, 2))
  
  # return updated subdf
  return (subdf2)
}


# running function to create clean df used for charting
cleandf = update_cleandf_new_keywords(gwords1, bwords1, rawdf)
subdf = update_subdf(cleandf)

#### STATS CHECK & QUICK EDA  #################################################

# cleandf %>% filter(gorb == "girl") %>% summary()
# cleandf %>% filter(gorb == "boy") %>% summary()
# cleandf %>% filter(girly & boyish | !girly & !boyish) %>% summary()

#### TESTING ZONE #################################################