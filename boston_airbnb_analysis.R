# initializing packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# reading boston airbnb csv files
df_cal = read.csv('calendar.csv')
df_rev = read.csv('reviews.csv')
df_list = read.csv('listings.csv')

# creating original copies of datasets
copy_cal = df_cal
copy_rev = df_rev
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
df_cal = df_cal %>% mutate(date = format(date, '%m/%d/%Y'))

# Clean price column and convert to numeric

class(df_cal$price)

work = df_cal %>% mutate(price = gsub('$', '', price))
work$price

num = '$65.00'
sub('$', 'r', num)

# cleaning of df_rev
# don't think i'll be doing a lot with this either...
names(df_rev)

# cleaning of df_list
# selecting relevant columns
names(df_list)

df_list$room_type

df_list = df_list %>% select(id, host_is_superhost,
                   host_listings_count, street, neighbourhood, neighbourhood_cleansed, 
                   city, zipcode, smart_location, latitude, longitude, is_location_exact, 
                   property_type, room_type, accommodates, bathrooms, bedrooms, beds,
                   bed_type, amenities, square_feet, price, weekly_price, monthly_price,
                   security_deposit, cleaning_fee, guests_included, extra_people, minimum_nights,
                   maximum_nights, number_of_reviews, review_scores_rating, review_scores_accuracy, 
                   review_scores_cleanliness, review_scores_checkin, review_scores_communication,
                   review_scores_location, review_scores_value, instant_bookable, cancellation_policy, 
                   reviews_per_month)

# getting rid of weird brittish spelling
df_list = df_list %>% rename('neighborhood' = neighbourhood)
df_list = df_list %>% rename('neighborhood_cleansed' = neighbourhood_cleansed)

df_list$amenities

unique(df_cal$price)


