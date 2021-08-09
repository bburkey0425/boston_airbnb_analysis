# initializing packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(plotly)
library(scales)
library(ggmap)
library(RColorBrewer)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(geosphere)

# reading boston airbnb csv files
df_cal = read.csv('calendar.csv')
df_rev = read.csv('reviews.csv')
df_list = read.csv('listings.csv')

# creating original copy of df_cal
copy_cal = df_cal
copy_list = df_list

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
agg

# Step 8 - plot of mean prices per day (prices adjusted)
avail = ggplot() + geom_line(data = agg, aes(x = date, y = mean_price), color = 'steelblue3') +
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_hline(yintercept = mean(agg$mean_price), color="black", linetype = 'dashed') +
  labs(title = 'Mean Listing Prices per Day',
       y = 'Mean Price',
       x = 'Date',
       subtitle = '(September 2016 - August 2017)')

avail

# plotly
fig_1 = ggplotly(avail)
fig_1

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

# plotly interactive plot
fig_2 = ggplotly(avail_all)
fig_2

# cleaning of df_rev
# don't think i'll be doing a lot with reviews dataframe
names(df_rev)

# cleaning of df_list
# getting rid of weird brittish spelling
df_list = df_list %>% rename('neighborhood' = neighbourhood)
df_list = df_list %>% rename('neighborhood_cleansed' = neighbourhood_cleansed)

# cleaning price column (what is the best way to handle missing values?)

# getting rid of '$' sign
df_list$price = gsub('\\$', '', df_list$price)
# getting rid of ',' in prices greater than 1000
df_list$price = gsub(',', '', df_list$price)
# converting to numeric
df_list$price = as.numeric(df_list$price)
# understanding distribution of price
summary(df_list$price)

# selecting relevant columns to be included
df_list_v1 = df_list %>% select(id, street, neighborhood, neighborhood_cleansed, zipcode, latitude, 
                                 longitude, is_location_exact, property_type, room_type, 
                                 accommodates, bathrooms, bedrooms, beds, amenities, square_feet, 
                                 price, guests_included, extra_people)

# cleaning price column (what is the best way to handle missing values?)
unique(df_list_v1$price)

# histogram plot of price (including outliers)
price_out = ggplot(df_list_v1, aes(x=price)) + 
  geom_histogram(binwidth = 50, color = 'black', fill = 'cornsilk') +
  geom_vline(aes(xintercept = 600), color="black", lty='dashed') +
  labs(title = 'Distribution of Listing Prices',
       subtitle = '(Outliers Included)',
       x = 'Price',
       y = 'Count')

price_out

# plotly
fig_3 = ggplotly(price_out)
fig_3

# finding which are outliers by definition (>425)
outvals = boxplot(df_list_v1$price)$out
min(outvals)

# Filtering prices to exclude the outliers (going with 600 as the cutoff for round number)
df_list_red = df_list_v1 %>% filter(price<=600)

# distribution of prices ($600 and less)
price_no_out = ggplot(df_list_red, aes(x=price)) + 
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'white', binwidth = 20) + 
  geom_density(alpha = 0.2, fill = 'red') +
  labs(title = 'Distribution of Listing Prices',
       subtitle = '(Outliers Excluded)',
       x = 'Price',
       y = 'Density')

price_no_out

# plotly
fig_4 = ggplotly(price_no_out)
fig_4

# getting rid of observations without neighborhood included
df_list_red = df_list_red %>% filter(neighborhood_cleansed != '')

# creating data frame with mean neighborhood prices with total count of observations per neighborhood
neigh_price = df_list_red %>% group_by(neighborhood_cleansed) %>% summarise(price = mean(price), count = n())
neigh_price = neigh_price %>% arrange(desc(price))
neigh_price

# count of listings per neighborhood
neigh_count_plot = ggplot(neigh_price) + 
  geom_bar(aes(x=reorder(neighborhood_cleansed, -count), y=count), color = 'navyblue', fill = 'lightsteelblue1', stat='identity') + 
  geom_hline(aes(yintercept = 30), color="black", linetype = 'dashed') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Number of Listings per Neighborhood',
       subtitle = '(Cutoff: >= 30 listings)',
       x = 'Neighborhood',
       y = 'Count')

neigh_count_plot

# plotly
fig_5 = ggplotly(neigh_count_plot)
fig_5

# calculating 'distance to boston' column for property_type and room_type
sum(is.na(df_list_v1$property_type))
sum(is.na(df_list_v1$room_type))
sum(is.na(df_list_v1$accommodates))

unique(df_list_v1$property_type) # has '' values
unique(df_list_v1$room_type) # good to go
unique(df_list_v1$accommodates) # good to go
class(df_list_v1$longitude) # numeric
class(df_list_v1$latitude) # numeric

# function to gather distance from center of boston to location of each listing
df_list_v1 = df_list_v1 %>% rowwise() %>% 
  mutate(dist_boston = geosphere::distHaversine( c(longitude, latitude), c(-71.059748, 42.360568)))

# convert meters to miles
df_list_v1 = df_list_v1 %>% mutate(dist_boston = dist_boston/1609.344)

# filter outlier prices (different than before for graphing purposes)
# remove null property_type values
df_list_filt = df_list_v1 %>% filter(price<=800)
df_list_filt = df_list_filt %>% filter(property_type != '')

# plotly of distance vs. price vs. number of guests
df_list_filt %>% 
  plot_ly(y = ~price, x = ~dist_boston, type = 'scatter', color = ~accommodates) %>%
  layout(title = 'Distance to Boston vs. Price vs. Number of Guests', 
         xaxis = list(title = 'Distance to Boston (miles)'),
         yaxis = list(title = 'Price ($)'),
         showlegend = FALSE)

# plotly of distance vs. price vs. property type
df_list_filt %>% 
  plot_ly(y = ~price, x = ~dist_boston, type = 'scatter', color = ~property_type) %>%
  layout(title = 'Distance to Boston vs. Price vs. Property Type', 
         xaxis = list(title = 'Distance to Boston (miles)'),
         yaxis = list(title = 'Price ($)'))

# plotly of distance vs. price vs. room type
df_list_filt %>% 
  plot_ly(y = ~price, x = ~dist_boston, type = 'scatter', color = ~room_type) %>%
  layout(title = 'Distance to Boston vs. Price vs. Room Type', 
         xaxis = list(title = 'Distance to Boston (miles)'),
         yaxis = list(title = 'Price ($)'))

# plotting mean of price per neighborhood including color scale for count of each neighborhood
# neigh_count --> excluding neighborhoods with less than 30 listings (to exclude in box plot)
neigh_price
neigh_price_red = neigh_price %>% filter(count>=30) %>% arrange(desc(count)) %>% select(neighborhood_cleansed, price, count)
neigh_price_red

neigh_price_plot = ggplot(neigh_price_red, aes(y=reorder(neighborhood_cleansed, price), x=price, fill = count)) + 
  scale_fill_continuous(type = "viridis") +
  geom_bar(stat='identity') +
  geom_text(aes(label = round(price, 2)), size = 3, color = 'white', nudge_x=-25) +
  labs(title = 'Mean Listing Price per Neighborhood',
       subtitle = '(At least 30 listings in each neighborhood)',
       x = 'Price',
       y = 'Neighborhood')

neigh_price_plot

# plotly
fig_6 = ggplotly(neigh_price_plot)
fig_6

# box plot of middle tendencies of pricing per neighborhood
df_list_box_exp = df_list_red %>% 
  select(price, neighborhood_cleansed) %>% 
  filter(neighborhood_cleansed %in% c('South Boston Waterfront', 'Downtown', 'Chinatown', 'Back Bay', 'West End'))

df_list_box_chp = df_list_red %>% 
  select(price, neighborhood_cleansed) %>% 
  filter(neighborhood_cleansed %in% c('Hyde Park', 'Dorchester', 'Roslindale', 'West Roxbury', 'Allston'))

# most expensive ggplot2 box plot
neigh_price_box_exp = 
  ggplot(df_list_box_exp, aes(x=reorder(neighborhood_cleansed, -price), y=price)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.75, linetype = "dashed") +
  theme(legend.position = 'none') +
  labs(title = 'Price per Neighborhood - Most Expensive',
  subtitle = '(At least 30 listings in each neighborhood)',
  x = 'Neighborhood',
  y = 'Price')

neigh_price_box_exp

# least expensive ggplot2 box plot
neigh_price_box_chp = ggplot(df_list_box_chp, aes(x=reorder(neighborhood_cleansed, -price), y=price)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.75, linetype = "dashed") +
  theme(legend.position = 'none') +
  labs(title = 'Price per Neighborhood - Least Expensive',
       subtitle = '(At least 30 listings in each neighborhood)',
       x = 'Neighborhood',
       y = 'Price')

neigh_price_box_chp

# most expensive plotly box plot
# creating ordered array for box plot
most_exp = df_list_box_exp %>% 
  plot_ly(y = ~price, x = ~reorder(neighborhood_cleansed, -price), color = ~neighborhood_cleansed, 
          type = 'box', boxmean = TRUE, boxpoints = 'all', jitter = 0.3, 
          colors = 'Set1', marker = list(size = 4), line = list(color = 'black', width = 1.2)) %>%
  layout(title = 'Listing Price per Neighborhood - Most Expensive', 
         xaxis = list(title = 'Neighborhood'),
         yaxis = list(title = 'Price ($)', range = c(0, 550)),
         showlegend = FALSE)

most_exp

# least expensive plotly box plot
least_exp = df_list_box_chp %>% 
  plot_ly(y = ~price, x = ~reorder(neighborhood_cleansed, -price), color = ~neighborhood_cleansed, 
          type = 'box', boxmean = TRUE, boxpoints = 'all', jitter = 0.3, 
          colors = 'Set1', marker = list(size = 4), line = list(color = 'black', width = 1.2)) %>%
  layout(title = 'Listing Price per Neighborhood - Least Expensive', 
         xaxis = list(title = 'Neighborhood'),
         yaxis = list(title = 'Price ($)', range = c(0, 350)),
         showlegend = FALSE)

least_exp

# Attempt at making superimposed heatmaps on maps
names(df_list)

df_list_loc = df_list %>% select(id, neighborhood_cleansed, price, latitude, longitude, 
                                 number_of_reviews, reviews_per_month, first_review, 
                                 last_review)

# cleaning columns of df_list_loc
# NA values in reviews_month column where listing has a total of 0 reviews (756 listings)
sum(is.na(df_list_loc$reviews_per_month))
class(df_list_loc$reviews_per_month)
unique(df_list_loc$reviews_per_month)

# first review and last review are '' wherever number_of_reviews = 0 and...
# reviews_per_month = NA
df_list_loc %>% filter(first_review == '') %>% 
  select(first_review, last_review, number_of_reviews, reviews_per_month)

# using reviews_per_month as primary popularity metric (accounts for time)
# imputing 0 for all NA values in reviews_per_month
df_list_loc[is.na(df_list_loc)] = 0
sum(is.na(df_list_loc))  # 0 - good to go
unique(df_list_loc$reviews_per_month)

# removing first_review and last_review
df_list_loc = df_list_loc %>% select(id, neighborhood_cleansed, price, latitude, longitude, 
                                 number_of_reviews, reviews_per_month)

# removing outlier price observations
df_list_loc = df_list_loc %>% filter(price<=600)

# finding max and min lat/long coordinates to set bound for map
max(df_list_loc$longitude)
min(df_list_loc$longitude)

max(df_list_loc$latitude)
min(df_list_loc$latitude)

# Stamen = map for data to be plotted on
bbox = c(left = -71.20000, bottom = 42.27000, right = -70.90000, top = 42.40000)

base_map = ggmap(get_stamenmap(bbox, maptype = "terrain", zoom = 15), extent='device')

coords_map_num = base_map + 
  stat_density2d(data=df_list_loc, 
                 aes(x=longitude, y=latitude, fill = ..level.., alpha = 0.1), 
                 geom = 'polygon')

coords_map_num = coords_map_num + scale_fill_gradientn(colors=rev(brewer.pal(7, "Spectral")))

coords_map_num

# interactive heatmaps using leaflet
# heatmap - number of listings 
heat_count = df_list_loc %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addHeatmap(lng=~longitude, lat=~latitude, radius = 2, blur=3)

heat_count

# heatmap - price of listings 
heat_price = df_list_loc %>% 
  leaflet() %>%
  setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~price, 
             max=600, radius = 3, blur=4)

# for reference when gauging max/min opacity values in heatmap
min(df_list_loc$price)
max(df_list_loc$price)

heat_price

# Analysis of popularity through number of reviews
# heatmap - numbers of reviews for each listing
heat_review_month = df_list_loc %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addHeatmap(lng=~longitude, lat=~latitude, 
             intensity=~reviews_per_month, max = 15, minOpacity = 0, radius = 5, blur=2)

heat_review_month

# for reference when gauging max/min opacity values in heatmap
max(df_list_loc$reviews_per_month)
min(df_list_loc$reviews_per_month)

# reviews per month per neighborhood barplot
df_reviews = df_list %>% select(neighborhood_cleansed, reviews_per_month, 
                                number_of_reviews, review_scores_rating)

dim(df_reviews)
names(df_reviews)

# cleaning values from reviews_per_month plot - NA values --> 0
df_reviews$reviews_per_month[is.na(df_reviews$reviews_per_month)] = 0
class(df_reviews$reviews_per_month)

# finding mean number of monthly ratings per neighborhood
rev_num_score = df_reviews %>% group_by(neighborhood_cleansed) %>% 
  summarise(reviews_per_month = sum(reviews_per_month), num_listings = n()) %>% 
  mutate(month_revs_adj = reviews_per_month/num_listings)

# bar plot of mean monthly reviews per neighborhood
rev_neigh_plot = ggplot(rev_num_score) + 
  geom_bar(aes(x = month_revs_adj, y = reorder(neighborhood_cleansed, month_revs_adj)), 
           color = 'sienna4', fill = 'salmon', stat='identity') +
  labs(title = 'Average Reviews per Month per Neighborhood',
       x = 'Neighborhood',
       y = 'Average Number of Reviews per Month')

