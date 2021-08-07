# initializing packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(plotly)
library(scales)

# reading boston airbnb csv files
df_cal = read.csv('calendar.csv')
df_rev = read.csv('reviews.csv')
df_list = read.csv('listings.csv')

# creating original copies of datasets
copy_cal = df_cal
copy_rev = df_rev
copy_list = df_list

# Alex attempt
# Step 1 - clean price
copy_cal$price = as.numeric(gsub('\\$', '', copy_cal$price))

# Step 2 - nest data
copy_cal_nested = copy_cal %>% nest_by(listing_id)

# Step 3 - define function to fill price values with values above / below(if first is NA)
impute_price = function(df){
  df = df[order(df$date), ]
  df = df %>% fill(price, .direction = 'downup')
  return(df)
}

# Step 4 - apply function to each dataframe
copy_cal_nested[[2]] = lapply(copy_cal_nested[[2]], impute_price)
copy_cal_nested

# Step 5 - unnest
final_df = unnest(copy_cal_nested, cols = c(data))

# Step 6 - remove NA values (listings which never had any prices listed - booked the whole year)
# number of NAs in final_df (all occurring in price column fortunately) - 250755
sum(is.na(final_df$price))
sum(is.na(final_df))

# omit NA 
final_df = na.omit(final_df)
sum(is.na(final_df)) # 0 NAs left
dim(final_df) # 250755 rows omitted - 1058135 left

# Step 7 - find mean price of all listings per day and plot over the course of the year
agg = final_df %>% group_by(date) %>% summarise(mean_price = mean(price))
agg$date = as.Date(agg$date)

# Step 8 - plot of mean prices per day (prices adjusted)
ggplot() + geom_line(data = agg, aes(x = date, y = mean_price, lty = 'All\nListings')) +
  scale_linetype('') +
  labs(title = 'Mean Listing Prices per Day (Prices Adjusted)',
       y = 'Mean Price',
       x = 'Date',
       subtitle = 'Prices of ALL listings between September 2016 and September 2017')

# information RE separate datasets
summary(df_cal)
summary(df_rev)
summary(df_list)

dim(df_cal)
dim(df_rev)
dim(df_list)

# cleaning of df_cal
# df_cal is useful as we would be able to see the pricing increases/decreases vs. time
names(df_cal)
length(df_cal$listing_id) # number of pricing entries for all the different listings
length(unique(df_cal$listing_id)) # matches number of listings included in listings dataframe

# Convert date column to datetime object
df_cal = df_cal %>% mutate(date = as.Date(date))
class(df_cal$date)

# Clean price column and convert to numeric
df_cal$price = gsub('\\$', '', df_cal$price)
df_cal$price = as.numeric(df_cal$price)
df_cal = df_cal %>% filter(price != is.na(price))


# plot of mean listing price per day for the year
by_day = df_cal %>% group_by(date) %>% summarise(mean_price = mean(price))

min(df_cal$date)
max(df_cal$date)

ggplot() + geom_line(data = by_day, aes(x = date, y = mean_price, lty = 'Available\nListings')) + 
  scale_linetype('') +
  labs(title = 'Mean Listing Prices per Day',
       y = 'Mean Price',
       x = 'Date',
       subtitle = 'Prices of AVAILABLE listings between September 2016 and September 2017')

# Clean df_cal to include each airbnb listing price **every day**
# original df only includes the listing price when it becomes available again
# Step 1 - clean price
copy_cal$price = as.numeric(gsub('\\$', '', copy_cal$price))

# Step 2 - nest data
copy_cal_nested = copy_cal %>% nest_by(listing_id)

# Step 3 - define function to fill price values with values above / below(if first is NA)
impute_price = function(df){
  df = df[order(df$date), ]
  df = df %>% fill(price, .direction = 'downup')
  return(df)
}

# Step 4 - apply function to each dataframe
copy_cal_nested[[2]] = lapply(copy_cal_nested[[2]], impute_price)
copy_cal_nested

# Step 5 - unnest
final_df = unnest(copy_cal_nested, cols = c(data))

# Step 6 - remove NA values (listings which never had any prices listed - booked the whole year)
# number of NAs in final_df (all occurring in price column fortunately) - 250755
sum(is.na(final_df$price))
sum(is.na(final_df))

# omit NA 
final_df = na.omit(final_df)
sum(is.na(final_df)) # 0 NAs left
dim(final_df) # 250755 rows omitted - 1058135 left

# Step 7 - find mean price of all listings per day and plot over the course of the year
agg = final_df %>% group_by(date) %>% summarise(mean_price = mean(price))
agg$date = as.Date(agg$date)

# Step 8 - plot of mean prices per day (prices adjusted)
ggplot() + geom_line(data = agg, aes(x = date, y = mean_price), color = 'steelblue3') +
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = 'Mean Listing Prices per Day',
       y = 'Mean Price',
       x = 'Date',
       subtitle = '(September 2016 - August 2017)')

# combining both available and all listings
# plot of mean prices of available listings vs. prices of all listings
by_day$date

avail_all = ggplot() + 
  geom_line(data = by_day, aes(x = date, y = mean_price, color = 'Available')) + 
  geom_line(data = agg, aes(x = date, y = mean_price, color = 'All')) + 
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  scale_color_manual(name = 'Type of\nListing', values=c('steelblue3', 'orangered2')) +
  theme(legend.position = c(0.89, 0.81), axis.text.x = element_text(angle = 45)) +
  labs(title = 'Mean Listing Prices per Day',
  y = 'Mean Price',
  x = 'Date',
  subtitle = '(September 2016 - August 2017)')

avail_all

fig = ggplotly(avail_all)

fig

# cleaning of df_rev
# don't think i'll be doing a lot with reviews dataframe
names(df_rev)

# cleaning of df_list
# getting rid of weird brittish spelling
df_list = df_list %>% rename('neighborhood' = neighbourhood)
df_list = df_list %>% rename('neighborhood_cleansed' = neighbourhood_cleansed)

# selecting relevant columns to be included
df_list = df_list %>% select(id, street, neighborhood, neighborhood_cleansed, zipcode, latitude, 
                                 longitude, is_location_exact, property_type, room_type, 
                                 accommodates, bathrooms, bedrooms, beds, amenities, square_feet, 
                                 price, guests_included, extra_people)

# cleaning price column (what is the best way to handle missing values?)
unique(df_list$price)

# getting rid of '$' sign
df_list$price = gsub('\\$', '', df_list$price)
# getting rid of ',' for prices greater than 1000
df_list$price = gsub(',', '', df_list$price)
# converting to numeric
df_list$price = as.numeric(df_list$price)
# understanding distribution of price
summary(df_list$price)

# histogram and density plots of price
ggplot(df_list, aes(x=price)) + geom_histogram()
ggplot(df_list, aes(x=price)) + geom_density()

# finding which are outliers by definition (>425)
outvals = boxplot(df_list$price)$out
min(outvals)

# Filtering prices to exclude the outliers (going with 600 as the cutoff for round number)
df_list = df_list %>% filter(price<=600)

# distribution of prices
ggplot(df_list, aes(x=price)) + 
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'white', binwidth = 20) + 
  geom_density(alpha = 0.2, fill = 'red') +
  labs(title = 'Distribution of Listing Prices',
       x = 'Price',
       y = 'Density')

# getting rid of observations without neighborhood included
df_list_reduced = df_list %>% filter(neighborhood != '')

# creating data frame with mean neighborhood prices with total count of observations per neighborhood
neigh_price = df_list_reduced %>% group_by(neighborhood) %>% summarise(price = mean(price), count = n())
neigh_price_ord = neigh_price %>% arrange(desc(price))
neigh_price_ord

# plotting mean of price per neighborhood including color scale for count of each neighborhood
ggplot(neigh_price_ord, aes(y=reorder(neighborhood, price), x=price, fill = count)) + 
  scale_fill_continuous(type = "viridis") +
  geom_bar(stat='identity') +
  labs(title = 'Mean Listing Price per Neighborhood',
       x = 'Price',
       y = 'Neighborhood')



  