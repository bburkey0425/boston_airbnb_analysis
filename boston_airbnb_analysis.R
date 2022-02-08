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
library(car)
library(moments)
library(mice)
library(missForest)
library(fastDummies)
library(stringr)
library(lubridate)

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

########## new stuff #########
# playing around
df_cal$year = format(df_cal$date, format="%Y")
df_cal$month = format(df_cal$date, format="%m")
df_cal

df_list = df_list %>% rename(listing_id = id)
df_list

df_list$neighbourhood_cleansed

tst = merge(x = df_cal, y = df_list, by = 'listing_id')
tst
tst = tst[c('listing_id', 'date', 'year', 'month', 'neighbourhood_cleansed', 
            'accommodates', 'bedrooms', 'room_type')]
tst
table(tst$room_type)

tst_grp = tst %>% group_by(year, month, neighbourhood_cleansed) %>% summarise(count = n_distinct(listing_id))
tst_grp


# number of unique listings per month per year per neighborhood
# testing just allston
als = tst_grp %>% filter(neighbourhood_cleansed == 'Allston')
als

als$date = as.Date(paste(als$year, als$month, 01), "%Y %m %d")
als

ggplot(als, aes(x= date, y = count)) + geom_line()

# doing for all individual neighborhoods
tst_grp$date = as.Date(paste(tst_grp$year, tst_grp$month, 01), "%Y %m %d")
tst_grp
ggplot(tst_grp, aes(x= date, y = count)) + geom_line(aes(color = neighbourhood_cleansed))


# same thing but city wide number of Airbnb listings
dim(tst)
all_list = tst %>% group_by(year, month, bedrooms) %>% summarise(count = n_distinct(listing_id))
all_list

all_list$date = as.Date(paste(all_list$year, all_list$month, 01), "%Y %m %d")
all_list

all_list = all_list %>% drop_na()
all_list

all_list$bedrooms = as.factor(all_list$bedrooms)

ggplot(all_list, aes(x= date, y = count)) +
  geom_line(aes(color = bedrooms)) + 
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")

# dataset with only entire home / apartments
tst_filt = tst %>% filter(room_type == 'Entire home/apt')
dim(tst_filt)

tst_filt_grp = tst_filt %>% group_by(year, month, bedrooms) %>% summarise(count = n_distinct(listing_id))
tst_filt_grp
tst_filt_grp = tst_filt_grp %>% drop_na()
tst_filt_grp$bedrooms = as.factor(tst_filt_grp$bedrooms)

tst_filt_grp$date = as.Date(paste(tst_filt_grp$year, tst_filt_grp$month, 01), "%Y %m %d")

ggplot(tst_filt_grp, aes(x= date, y = count)) +
  geom_line(aes(color = bedrooms)) + 
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month") + 
  xlab('Date') + 
  ylab('Number of Listings') + 
  scale_color_discrete(name = "Number of\nBedrooms")
  
# monthly percent change in the number of listings per type of apartment
tst_filt_grp
reshape(tst_filt_grp, direction = 'long', idvar = 'date', timevar = 'bedrooms')

num_list = spread(tst_filt_grp, bedrooms, count)
colnames(num_list) = c('year', 'month', 'date', 'studio', 'one_bed', 'two_bed', 'three_bed', 'four_bed', 'five_bed')
num_list

num_list[ ,c(4:9)] = lapply(num_list[ ,c(4:9)], as.numeric)

num_list = as.data.frame(num_list)
num_list

pct_change_num = num_list %>% mutate(studio_pct_change = ((studio/lag(studio))-1)*100, 
                    one_bed_pct_change = ((one_bed/lag(one_bed))-1)*100, 
                    two_bed_pct_change = ((two_bed/lag(two_bed))-1)*100,
                    three_bed_pct_change = ((three_bed/lag(three_bed))-1)*100,
                    four_bed_pct_change = ((four_bed/lag(four_bed))-1)*100,
                    five_bed_pct_change = ((five_bed/lag(five_bed))-1)*100)

pct_change_num

# average number of listings percent change across all months
# average monthly percent change in rent for STUDIO apartments
num_stud_pct_change_mean = mean(pct_change_num$studio_pct_change, na.rm = TRUE)
num_stud_pct_change_mean

# average monthly percent change in rent for ONE bedroom apartments
num_one_bed_pct_change_mean = mean(pct_change_num$one_bed_pct_change, na.rm = TRUE)
num_one_bed_pct_change_mean

# average monthly percent change in rent for TWO bedroom apartments
num_two_bed_pct_change_mean = mean(pct_change_num$two_bed_pct_change, na.rm = TRUE)
num_two_bed_pct_change_mean

# average monthly percent change in rent for THREE bedroom apartments
num_three_bed_pct_change_mean = mean(pct_change_num$three_bed_pct_change, na.rm = TRUE)
num_three_bed_pct_change_mean

# average monthly percent change in rent for FOUR bedroom apartments
num_four_bed_pct_change_mean = mean(pct_change_num$four_bed_pct_change, na.rm = TRUE)
num_four_bed_pct_change_mean

# average monthly percent change in rent for FIVE bedroom apartments
num_five_bed_pct_change_mean = mean(pct_change_num$five_bed_pct_change, na.rm = TRUE)
num_five_bed_pct_change_mean

# manually creating dataframe with rental prices from zumper
# https://www.zumper.com/rent-research/boston-ma

year = c(2016, 2016, 2016, 2016, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017)
month = c(09, 10, 11, 12, 01, 02, 03, 04, 05, 06, 07, 08, 09)
studio = c(1748, 1750, 1750, 1750, 1750, 1750, 1750, 1750, 1750, 1750, 1750, 1750, 1770)
one_bed = c(2231, 2250, 2221, 2255, 2270, 2250, 2216, 2200, 2200, 2200, 2200, 2200, 2200)
two_bed = c(2562, 2600, 2576, 2600, 2606, 2600, 2600, 2600, 2600, 2612, 2656, 2625, 2615)
three_bed = c(2985, 2920, 2937, 3090, 3250, 3261, 3200, 3149, 3100, 3099, 3055, 3000, 3000)
four_bed = c(3600, 3600, 3600, 3840, 3987, 4000, 3937, 3910, 3837, 3800, 3800, 3800, 3785)

avg_rent = data.frame(year, month, studio, one_bed, two_bed, three_bed, four_bed)
avg_rent
avg_rent$date = as.Date(paste(avg_rent$year, avg_rent$month, 01), "%Y %m %d")
class(avg_rent$studio)
avg_rent

plot_1 = ggplot() +
  geom_line(data = avg_rent, aes(x = date, y = studio, color = "Studio")) + 
  geom_line(data = avg_rent, aes(x = date, y = one_bed, color = "1 Bedroom")) +
  geom_line(data = avg_rent, aes(x = date, y = two_bed, color = "2 Bedroom")) +
  geom_line(data = avg_rent, aes(x = date, y = three_bed, color = "3 Bedroom")) +
  geom_line(data = avg_rent, aes(x = date, y = four_bed, color = "4 Bedroom")) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month") + 
  xlab('Date') + 
  ylab('Average Rent') +
  ggtitle('Average Monthly Rent by Apartment Type') +
  scale_color_manual(name = "Apartment Type", values = c("Studio" = "red", 
                                                         "1 Bedroom" = "black",
                                                         "2 Bedroom" = 'orange',
                                                         "3 Bedroom" = 'blue',
                                                         "4 Bedroom" = 'green'))
  
plot_1 + theme(axis.text.x = element_text(angle = 45, hjust=1))

# percent change over each month for each of the types of apartments
rent_pct_change = avg_rent %>% mutate(pct_change_studio = (studio/lag(studio)-1)*100) %>% 
  mutate(pct_change_one_bed = (one_bed/lag(one_bed)-1)*100) %>% 
  mutate(pct_change_two_bed = (two_bed/lag(two_bed)-1)*100) %>% 
  mutate(pct_change_three_bed = (three_bed/lag(three_bed)-1)*100) %>% 
  mutate(pct_change_four_bed = (four_bed/lag(four_bed)-1)*100)

rent_pct_change

# average monthly percent change in rent for STUDIO apartments
rent_stud_pct_change_mean = mean(rent_pct_change$pct_change_studio, na.rm = TRUE)
rent_stud_pct_change_mean

# average monthly percent change in rent for ONE bedroom apartments
rent_one_bed_pct_change_mean = mean(rent_pct_change$pct_change_one_bed, na.rm = TRUE)
rent_one_bed_pct_change_mean

# average monthly percent change in rent for TWO bedroom apartments
rent_two_bed_pct_change_mean = mean(rent_pct_change$pct_change_two_bed, na.rm = TRUE)
rent_two_bed_pct_change_mean

# average monthly percent change in rent for THREE bedroom apartments
rent_three_bed_pct_change_mean = mean(rent_pct_change$pct_change_three_bed, na.rm = TRUE)
rent_three_bed_pct_change_mean

# average monthly percent change in rent for FOUR bedroom apartments
rent_four_bed_pct_change_mean = mean(rent_pct_change$pct_change_four_bed, na.rm = TRUE)
rent_four_bed_pct_change_mean

# pct change from sept. 2016 to sept. 2017
avg_rent
tst_filt_grp
tst_filt_grp %>% filter(month == 09)

df_test = as.data.frame(tst_filt_grp)
df_test

count_wide = df_test %>% pivot_wider(names_from = bedrooms, values_from = count)
count_wide = as.data.frame(count_wide)
count_wide = count_wide[-9]

count_wide

ann_pct_chge = function(col){
  return(((col[13]-col[1])/col[1])*100)
}

count_pct_change = apply(count_wide[, c(4, 5, 6, 7, 8)], 2, ann_pct_chge)
count_pct_change

avg_rent

rent_pct_change = apply(avg_rent[, c(3, 4, 5, 6, 7)], 2, ann_pct_chge)
rent_pct_change = rent_pct_change[-5]
rent_pct_change

avg_rent
count_wide
colnames(count_wide) = c('year', 'month', 'date', 
                         'studio', 'one_bed', 'two_bed',
                         'three_bed', 'four_bed')
cols.num = c(4, 5, 6, 7, 8)
count_wide[cols.num] = sapply(count_wide[cols.num],as.numeric)
sapply(count_wide, class)

class(count_wide$studio)

count_wide

plot_2 = ggplot() +
  geom_line(data = count_wide, aes(x = date, y = studio, color = "Studio")) + 
  geom_line(data = count_wide, aes(x = date, y = one_bed, color = "1 Bedroom")) +
  geom_line(data = count_wide, aes(x = date, y = two_bed, color = "2 Bedroom")) +
  geom_line(data = count_wide, aes(x = date, y = three_bed, color = "3 Bedroom")) +
  geom_line(data = count_wide, aes(x = date, y = four_bed, color = "4 Bedroom")) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month") + 
  xlab('Date') + 
  ylab('Airbnb Listing Count') +
  ggtitle('Number of Airbnb Listings by Apartment Type') +
  scale_color_manual(name = "Apartment Type", values = c("Studio" = "red", 
                                                         "1 Bedroom" = "black",
                                                         "2 Bedroom" = 'orange',
                                                         "3 Bedroom" = 'blue',
                                                         "4 Bedroom" = 'green'))

plot_2 + theme(axis.text.x = element_text(angle = 45, hjust=1))


# pearson correlation tests between airbnb listing count and avg rent by apartment type
cor.test(avg_rent$studio, count_wide$studio, method = 'pearson') # -0.18
cor.test(avg_rent$one_bed, count_wide$one_bed, method = 'pearson') # 0.78
cor.test(avg_rent$two_bed, count_wide$two_bed, method = 'pearson') # -0.64
cor.test(avg_rent$three_bed, count_wide$three_bed, method = 'pearson') # 0.03

# pearson correlation coeff. for avg listing count and avg rent for all apartment types
cor.test(x$sum_count, y$avg_rent, method = 'pearson') # 0.79


# average count of listings per month across all apartment types (sum)
x = count_wide %>% group_by(date) %>% summarise(sum_count = sum(studio, one_bed, two_bed, three_bed, four_bed))
count_wide
# plot
ggplot() + geom_line(data = x, aes(x = date, y = sum_count)) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month") + 
  xlab('Date') + 
  ylab('Total Monthly Airbnb Listing Count (All apartment types)') +
  ggtitle('Number of Airbnb Listings by Month')


# average rent per month across all apartment types (mean)
y = avg_rent %>% group_by(date) %>% summarise(avg_rent = mean(one_bed, two_bed, three_bed, four_bed))
y



# plot
ggplot() + geom_line(data = y, aes(x = date, y = avg_rent)) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month") + 
  xlab('Date') +
  ylab('Average Rent (All apartment types)') +
  ggtitle('Average Rent by Month')

avg_rent

# cross correlation between the two
ccf(x$sum_count, y$avg_rent)
print(ccf(x$sum_count, y$avg_rent, 12))


# plot of the median rent prices by month by apartment type
f = ggplot(avg_rent) + 
  geom_line(aes(x = date, y = studio, color = 'Studio')) +
  geom_line(aes(x = date, y = one_bed, color = 'One Bedroom')) + 
  geom_line(aes(x = date, y = two_bed, color = 'Two Bedroom')) + 
  geom_line(aes(x = date, y = three_bed, color = 'Three Bedroom')) + 
  geom_line(aes(x = date, y = four_bed, color = 'Four Bedroom'))

f + scale_color_discrete(name = "Apartment\nType") + xlab("Date") + ylab("Average Rent")

# combined dataframe of average monthly percent changes - number of listings & rent prices

### pricing of listings over the course of the year ###
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

df_cal

# Clean df_cal to include each airbnb listing price **every day**
# original df only includes the listing price when it becomes available again
# Step 1 - clean price
copy_cal$price = as.numeric(gsub('\\$', '', copy_cal$price))

# Step 2 - nest data
copy_cal_nested = copy_cal %>% nest_by(listing_id)
copy_cal_nested

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

# investigating seasonal, weekday vs. week, and holiday prices throughout the year
# applied to calendar table with all listings
final_df
final_df$date_time = as.POSIXlt(final_df$date)

final_df$month = format(final_df$date_time, "%m")
final_df$month = as.numeric(final_df$month)
class(final_df$month)

final_df$weekday = weekdays(final_df$date_time)

final_df = final_df %>% mutate(season = case_when(
  month %in% c(9, 10, 11) ~ 'Fall', 
  month %in% c(12, 1, 2) ~ 'Winter',
  month %in% c(3, 4, 5) ~ 'Spring',
  month %in% c(6, 7, 8) ~ 'Summer'))

final_df = final_df %>% mutate(day_type = case_when(
  weekday %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday') ~ 'Week', 
  weekday %in% c('Friday', 'Saturday', 'Sunday') ~ 'Weekend'))

final_df

final_df = final_df %>% mutate(holiday = case_when(
  (date >= '2016-10-08' & date <= '2016-10-10') ~ 'Columbus Day',
  (date >= '2016-11-11' & date <= '2016-11-13') ~ 'Veterans Day',
  (date >= '2016-11-23' & date <= '2016-11-27') ~ 'Thanksgiving',
  (date >= '2016-12-24' & date <= '2017-01-02') ~ 'Christmas',
  (date >= '2017-01-14' & date <= '2017-01-16') ~ 'MLK',
  (date >= '2017-02-18' & date <= '2017-02-26') ~ "Presidents' Day",
  (date >= '2017-04-15' & date <= '2017-04-23') ~ 'Spring Break',
  (date >= '2017-05-27' & date <= '2017-05-29') ~ 'Memorial Day',
  (date >= '2017-06-08' & date <= '2017-08-29') ~ 'Summer Break',
  TRUE ~ 'Non-holiday'))

final_df = final_df %>% mutate(holiday_binary = case_when(
  (date >= '2016-10-08' & date <= '2016-10-10') ~ 'Holiday',
  (date >= '2016-11-11' & date <= '2016-11-13') ~ 'Holiday',
  (date >= '2016-11-23' & date <= '2016-11-27') ~ 'Holiday',
  (date >= '2016-12-24' & date <= '2017-01-02') ~ 'Holiday',
  (date >= '2017-01-14' & date <= '2017-01-16') ~ 'Holiday',
  (date >= '2017-02-18' & date <= '2017-02-26') ~ 'Holiday',
  (date >= '2017-04-15' & date <= '2017-04-23') ~ 'Holiday',
  (date >= '2017-05-27' & date <= '2017-05-29') ~ 'Holiday',
  (date >= '2017-06-08' & date <= '2017-08-29') ~ 'Holiday',
  TRUE ~ 'Non-holiday'))

# average listing prices by season, holidays, weekdays vs. weekend, month
seasons = final_df %>% group_by(season) %>% summarise(mean_price = mean(price))
names(seasons) = c('season', 'mean_price')
holiday = final_df %>% group_by(holiday) %>% summarise(mean_price = mean(price))
week = final_df %>% group_by(day_type) %>% summarise(mean_price = mean(price))
month = final_df %>% group_by(month) %>% summarise(mean_price = mean(price))
holiday_2 = final_df %>% group_by(holiday_binary) %>% summarise(mean_price = mean(price))
holiday_2

### two-way anova for (1) season and (2) weekend ###
sapply(final_df, class)
final_df$season = as.factor(final_df$season)
final_df$day_type = as.factor(final_df$day_type)

table(final_df$season, final_df$day_type)

ggboxplot(final_df, x = "season", y = "price", color = "day_type",
          palette = c("#00AFBB", "#E7B800"))

res.aov2 <- aov(price ~ season + day_type, data = final_df)
summary(res.aov2)

res.aov3 <- aov(price ~ season * day_type, data = final_df)
summary(res.aov3)

TukeyHSD(res.aov3, which = "season")
TukeyHSD(res.aov3, which = "day_type")


plot(res.aov3, 1)
plot(res.aov3, 2)

### one-way anova for holiday ###
# each individual holiday
final_df$holiday = as.factor(final_df$holiday)
res.aov <- aov(price ~ holiday, data = final_df)
summary(res.aov)

TukeyHSD(res.aov)

# holiday vs. non-holiday in general
final_df$holiday_binary = as.factor(final_df$holiday_binary)
res.aov.2 <- aov(price ~ holiday_binary, data = final_df)
summary(res.aov.2)

TukeyHSD(res.aov.2)

# holiday and month table
# getting month names from month numbers
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")

month$month_name = mymonths[month$month]
month_names = month %>% select(mean_price, month_name)
month_names = month_names[, c("month_name", "mean_price")]
month_names

names(holiday) = c('time', 'mean_price')
names(seasons) = c('time', 'mean_price')

holiday_season = rbind(holiday, seasons)

holiday_season = holiday_season %>% mutate(factor = case_when(
  time %in% c('Christmas', 'Winter') ~ 'a',
  time %in% c('Columbus Day', 'Fall') ~ 'b',
  time %in% c('Memorial Day', 'Spring') ~ 'c',
  time %in% c('MLK', 'Winter') ~ 'd',
  time %in% c("Presidents' Day", 'Winter') ~ 'e',
  time %in% c('Spring Break', 'Spring') ~ 'f',
  time %in% c('Summer Break', 'Summer') ~ 'g',
  time %in% c('Thanksgiving', 'Fall') ~ 'h',
  time %in% c('Veterans Day', 'Fall') ~ 'i',
  TRUE ~ 'other'))

holiday_season

season_col = c('Winter', 'Winter', 'Spring', 'Fall', 'Fall', 'Overall')
price_col = c(179.9399, 179.9399, 191.0600, 194.6157, 194.6157, 190.8397)
fac_col = c('d', 'e', 'f', 'h', 'i', 'other')
  
df_add = data.frame(season_col, price_col, fac_col)
df_add

names(df_add) = c('time', 'mean_price', 'factor')

holiday_season = rbind(holiday_season, df_add)
holiday_season
holiday_season = holiday_season[
  with(holiday_season, order(factor)),
]

holiday_season = holiday_season %>% mutate(factor = case_when(
  factor == 'a' ~ 'Christmas',
  factor == 'b' ~ 'Columbus Day',
  factor == 'c' ~ 'Memorial Day',
  factor == 'd' ~ 'MLK Day',
  factor == 'e' ~ "Presidents' Day",
  factor == 'f' ~ 'Spring Break',
  factor == 'g' ~ 'Summer Break',
  factor == 'h' ~ 'Thanksgiving',
  factor =='i' ~ 'Veterans Day',
  factor == 'other' ~ 'Non-holiday'))

holiday_season

holiday_season = holiday_season %>% mutate(time = case_when(
  time %in% c('Christmas', 'Columbus Day', 'Memorial Day', 'MLK', 
              "Presidents' Day", 'Spring Break', 'Summer Break', 'Thanksgiving',
              'Veterans Day', 'Non-holiday') ~ 'Holiday',
  TRUE ~ 'Seasonal'))
  
holiday_season = as.data.frame(holiday_season)

holiday_season = holiday_season %>% filter(factor != 'Non-holiday')
names(holiday_season) = c('Time', 'Mean_Price', 'Holiday')

# overall average price
mean(final_df$price)

# season barplots
# 1
season_plot = ggplot(data=seasons, aes(fill = season, x=season, y=mean_price)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(final_df$price), linetype = 'dashed') +
  coord_cartesian(ylim = c(175, 200)) + 
  ggtitle("Average Listing Prices by Season")
season_plot

# 2
seasons_2 = final_df %>% 
  group_by(season, day_type) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE))

ggplot(data=seasons_2, aes(fill = day_type, x=season, y=mean_price)) +
  geom_bar(stat="identity", position = 'dodge') +
  geom_hline(yintercept = mean(final_df$price), linetype = 'dashed') +
  coord_cartesian(ylim = c(175, 200)) + 
  ggtitle("Average Listing Prices by Season & Day Type")

# holiday barplot
holiday_plot = ggplot(data = holiday_season, mapping = aes(x=Holiday, y=Mean_Price, fill=Time)) +
  geom_bar(stat="identity", position = "dodge") +
  coord_cartesian(ylim = c(175, 202)) +
  geom_hline(yintercept = mean(final_df$price), linetype = 'dashed')
holiday_plot = holiday_plot + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggtitle("Average Listing Prices: \nHoliday vs. Season (of holiday)")
holiday_plot

# week vs. weekday barplot
ggplot(week, aes(fill = day_type, x=day_type, y=mean_price)) + geom_bar(stat='identity') +
  coord_cartesian(ylim = c(180, 200)) + 
  geom_hline(yintercept = mean(final_df$price), linetype = 'dashed') +
  ggtitle("Average Listing Prices: Weekday vs. Weekend")

week

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

df_list

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
df_list_v1 = df_list %>% select(street, neighborhood, neighborhood_cleansed, zipcode, latitude, 
                                 longitude, is_location_exact, property_type, room_type, 
                                 accommodates, bathrooms, bedrooms, beds, amenities, square_feet, 
                                 price, guests_included, extra_people)


df_list_v1

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

lapply(df_list,function(x) { length(which(is.na(x)))})

# plotly
fig_3 = ggplotly(price_out)
fig_3

# finding which are outliers by definition (>425)
outvals = boxplot(df_list_v1$price)$out
min(outvals)

# Filtering prices to exclude the outliers (going with 600 as the cutoff for round number)
df_list_red = df_list_v1 %>% filter(price<=600)
df_list_v1
df_list_red
df_list_check = df_list_v1 %>% filter(price<=425)
df_list_check

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

df_list_red
colSums(is.na(df_list_red))
str(df_list_red)
dim(df_list_red)

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

df_pop = read.csv('total_housing_boston.csv')
df_pop

neigh_price

neigh_price_2 = merge(neigh_price, df_pop, by.x = "neighborhood_cleansed", by.y = "X")

neigh_price_2$Total.Housing.Units = gsub('[,]', '', neigh_price_2$Total.Housing.Units)
neigh_price_2$Total.Housing.Units = as.numeric(neigh_price_2$Total.Housing.Units)

neigh_price_2$abb_units_ratio = (neigh_price_2$count/neigh_price_2$Total.Housing.Units)*100

# barplot of total housing units vs. airbnb listings per neighborhood
count_abb_ratio = ggplot(neigh_price_2) + 
  geom_bar(aes(x=reorder(neighborhood_cleansed, -abb_units_ratio) , y=abb_units_ratio), color = 'springgreen4', fill = 'lightgreen', stat='identity') + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Number of Airbnb Listings vs. Total Housing Units',
       subtitle = '(Percentage)',
       x = 'Neighborhood',
       y = '# of Airbnb Listings / # of Housing Units')

count_abb_ratio

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

df_list_filt %>% group_by(neighborhood_cleansed) %>% summarise(mean_price = mean(price))
            
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

# average rent indices by neighborhood
4141, 4048, 4003, 4003, 3919, 3817, 3760, 3683, 3569, 3566, 3484, 3474, 3278, 3095, 
3027, 3001, 2815, 2810, 2776, 2323, 2011, 1863

neighborhood_cleansed = c('North End', 'Bay Village', 'Chinatown', 'Leather District', 'Back Bay',
                'Downtown', 'South Boston', 'South End', 'Fenway', 'West End', 'Beacon Hill',
                'Mission Hill', 'Charlestown', 'Jamaica Plain', 'Allston', 'Brighton', 'Dorchester',
                'Roslindale', 'East Boston', 'Roxbury', 'West Roxbury', 'Mattapan', 'Hyde Park')

avg_rent = c(4141, 4048, 4003, 4003, 3919, 3817, 3760, 3683, 3569, 3566, 3484, 3474, 3278, 3095, 
             3027, 3027, 3001, 2815, 2810, 2776, 2323, 2011, 1863)

length(neighborhood_list)
length(avg_rent)

avg_rent_neigh = data.frame(neighborhood_cleansed, avg_rent)

avg_rent_neigh %>% arrange(-avg_rent, neighborhood_list)

neigh_price

# does not include south boston waterfront & longwood medical area 
# (not included in average rent by neighborhood list)
avg_rent_list = merge(x = avg_rent_neigh, y = neigh_price, by = "neighborhood_cleansed")
avg_rent_list = avg_rent_list %>% select(-count)

avg_rent_list %>% arrange(-price)

avg_rent_list_red = avg_rent_list %>% filter(price>190)

ggplot(avg_rent_list, aes(x=avg_rent, y=price)) + 
  geom_point() +
  geom_abline(slope = 3.607,
              intercept = 3060.049)

most_exp = c('Downtown', 'Chinatown', 'Back Bay', 'West End')

library(ggrepel)
rent_price_scat = ggplot(avg_rent_list_red, aes(x=avg_rent, y=price, label=neighborhood_cleansed)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  xlab('Avg. Neighborhood Rent') + 
  ylab('Avg. Neighborhood Listing Price')
rent_price_scat + 
  geom_label_repel(aes(label = neighborhood_cleansed),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()

avg_rent_list %>% arrange(-price)

price_rent_rat = avg_rent_list_red %>% mutate(rat = price/avg_rent) %>% arrange(rat)
price_rent_rat$rat

# Attempt at making superimposed heatmaps on maps
names(df_list)

df_list_loc = df_list %>% select(neighborhood_cleansed, price, latitude, longitude, 
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

rev_num_score

# bar plot of mean monthly reviews per neighborhood
rev_neigh_plot = ggplot(rev_num_score) + 
  geom_bar(aes(x = month_revs_adj, y = reorder(neighborhood_cleansed, month_revs_adj)), 
           color = 'sienna4', fill = 'salmon', stat='identity') +
  labs(title = 'Average Reviews per Month per Neighborhood',
       y = 'Neighborhood',
       x = 'Average Number of Reviews per Month')

rev_neigh_plot


### REGRESSION ANALYSIS ###
# filtering variables in df_list for regression
# dropping columns due to majority null values / relevancy

colnames(df_list)

drops = c("id", "listing_url", "scrape_id", "last_scraped", "name", "summary",
          "space", "description", "experiences_offered", "neighborhood_overview",
          "notes", "transit", "access", "interaction", "house_rules", "thumbnail_url",
          "medium_url", "picture_url", "xl_picture_url", "host_id", "host_url", 
          "host_name", "host_location", "host_about", "host_thumbnail_url", "host_picture_url",
          "host_neighbourhood", "host_listings_count", "host_verifications",
          "street", "neighborhood", "neighbourhood_group_cleansed", "state",
          "zipcode", "market", "smart_location", "country_code", "country", "is_location_exact", "square_feet", "weekly_price",
          "monthly_price", "calendar_updated", "has_availability", "availability_30",
          "availability_60", "availability_90", "availability_365", "calendar_last_scraped",
          "first_review", "last_review", "requires_license", "license", "jurisdiction_names",
          "calculated_host_listings_count", 'host_has_profile_pic')

# columns to check:
# security_deposit, cleaning_fee, guests_included, extra_people, minimum_nights, 
# maximum_nights, instant_bookable, require_guest_profile_picture, 
# require_guest_phone_verification, reviews_per_month

df_list_reg = df_list[ , !(names(df_list) %in% drops)]
colnames(df_list_reg)

# continuous variables:
# host_response_rate, host_acceptance_rate (need to re-add these two columns from df_list)
# host_since (duration in days), host_total_listings_count, bathrooms, bedrooms,
# beds, price, security_deposit, cleaning_fee, guests_included, extra_people, minimum_nights, 
# maximum_nights, number_of_reviews, review_scores_rating, review_scores_accuracy,
# review_scores_cleanliness, review_scores_checkin, review_scores_communication, 
# review_scores_location, review_scores_value, reviews_per_month

# categorical variables:
# host_response_time, host_is_superhost
# host_identity_verified, neighborhood_cleansed, city, property_type, 
# room_type, bed_type, amenities,
# instant_bookable, cancellation_policy, require_guest_profile_picture,
# require_guest_phone_verification

### host_response_rate ###
# removing special characters, converting to NA and converting to numeric
df_list_reg$host_response_rate = gsub('[%]', '', df_list_reg$host_response_rate)
df_list_reg$host_response_rate

df_list_reg = df_list_reg %>%
  mutate(host_response_rate = na_if(host_response_rate, "N/A"))

df_list_reg$host_response_rate = as.numeric(df_list_reg$host_response_rate)

sum(is.na(df_list_reg$host_response_rate))

### host_acceptance_rate ###
df_list_reg$host_acceptance_rate = gsub('[%]', '', df_list_reg$host_acceptance_rate)
df_list_reg$host_acceptance_rate

df_list_reg = df_list_reg %>%
  mutate(host_acceptance_rate = na_if(host_acceptance_rate, "N/A"))

df_list_reg$host_acceptance_rate = as.numeric(df_list_reg$host_acceptance_rate)
df_list_reg$host_acceptance_rate

sum(is.na(df_list_reg$host_acceptance_rate))

### host since ###
# converting to date class and calculating the number of days from 2016-09-07
df_list_reg$host_since = as.Date(df_list_reg$host_since, "%Y-%m-%d")
date1 = as.Date("2016-09-07", "%Y-%m-%d")
df_list_reg$host_since = as.numeric(
  difftime(date1,df_list_reg$host_since,units=c("days")))

df_list_reg$host_since

### host_total_listings_count ###
# converting to numeric
df_list_reg$host_total_listings_count = as.numeric(df_list_reg$host_total_listings_count)
sum(is.na(df_list_reg$host_total_listings_count))

## bathrooms ##
# need to impute missing values
class(df_list_reg$bathrooms)
sum(is.na(df_list_reg$bathrooms))
table(df_list_reg$bathrooms)

### bedrooms ###
# converting to numeric
# need to impute missing values
class(df_list_reg$bedrooms)
df_list_reg$bedrooms = as.numeric(df_list_reg$bedrooms)
sum(is.na(df_list_reg$bedrooms))
table(df_list_reg$bedrooms)

### beds ###
class(df_list_reg$beds)
df_list_reg$beds = as.numeric(df_list_reg$beds)
sum(is.na(df_list_reg$beds))
table(df_list_reg$beds)

### security deposit ###
# deleting commas and money sign, converting missing values to NA, converting to numeric
df_list_reg$security_deposit
df_list_reg$security_deposit = gsub('[,$]', '', df_list_reg$security_deposit)

df_list_reg = df_list_reg %>% 
  mutate(security_deposit = if_else(security_deposit == '', '0',
                                    security_deposit))

df_list_reg$security_deposit = as.numeric(df_list_reg$security_deposit)
sum(is.na(df_list_reg$security_deposit))

### cleaning fee ###
# same operation as security_deposit
df_list_reg$cleaning_fee

df_list_reg$cleaning_fee = gsub('[,$]', '', df_list_reg$cleaning_fee)

df_list_reg = df_list_reg %>% 
  mutate(cleaning_fee = if_else(cleaning_fee == '', '0',
                                cleaning_fee))

df_list_reg$cleaning_fee = as.numeric(df_list_reg$cleaning_fee)
sum(is.na(df_list_reg$cleaning_fee))

### guests_included ###
# converting to numeric
sum(is.na(df_list_reg$guests_included))
class(df_list_reg$guests_included)
df_list_reg$guests_included = as.numeric(df_list_reg$guests_included)

### extra_people ###
# getting rid of special characters and converting to numeric
unique(df_list_reg$extra_people)
df_list_reg$extra_people = gsub('[,$]', '', df_list_reg$extra_people)

df_list_reg$extra_people = as.numeric(df_list_reg$extra_people)
df_list_reg_imp$extra_people
sum(is.na(df_list_reg$extra_people))

### minimum_nights ###
table(df_list_reg$minimum_nights)
sum(is.na(df_list_reg$minimum_nights))
class(df_list_reg$minimum_nights)
df_list_reg$minimum_nights = as.numeric(df_list_reg$minimum_nights)
df_list_reg$minimum_nights

### maximum nights ###
table(df_list_reg$maximum_nights)
sum(is.na(df_list_reg$maximum_nights))
class(df_list_reg$maximum_nights)
df_list_reg$maximum_nights = as.numeric(df_list_reg$maximum_nights)
df_list_reg$maximum_nights

### number_of_reviews ###
table(df_list_reg$number_of_reviews)
sum(is.na(df_list_reg$number_of_reviews))
class(df_list_reg$number_of_reviews)
df_list_reg$number_of_reviews = as.numeric(df_list_reg$number_of_reviews)
df_list_reg$number_of_reviews

### review_scores_rating, review_scores_accuracy, ###
### review_scores_cleanliness, review_scores_checkin, ###
### review_scores_communication, review_scores_location, ###
### review_scores_value, reviews_per_month ###
# converting all columns to numeric
# leaving NAs to be imputed by mice
cols.num = c('review_scores_rating', 'review_scores_accuracy',
             'review_scores_cleanliness', 'review_scores_checkin',
             'review_scores_communication', 'review_scores_location',
             'review_scores_value', 'reviews_per_month')

df_list_reg[cols.num] = sapply(df_list_reg[cols.num],as.numeric)

### host_response_time ###
# converting 'N/A' to NA values to be imputed by mice
df_list_reg$host_response_time
df_list_reg = df_list_reg %>%
  mutate(host_response_time = na_if(host_response_time, "N/A"))

### host_is_superhost ###
# converting to 0s and 1s
table(df_list_reg$host_is_superhost)
df_list_reg = df_list_reg %>% 
  mutate(host_is_superhost = 
           if_else(host_is_superhost == 't', 1, 0))
class(df_list_reg$host_is_superhost)

### host_identity_verified ###
# converting binary to 0s and 1s
table(df_list_reg$host_identity_verified)
df_list_reg = df_list_reg %>% 
  mutate(host_identity_verified = 
           if_else(host_identity_verified == 't', 1, 0))
df_list_reg$host_identity_verified

### neighbourhood_cleansed ###
# good to go
table(df_list_reg$neighborhood_cleansed)

### city ###
# universal names for the specific neighborhoods
unique(df_list_reg$city)

table(df_list_reg$city)

df_list_reg = df_list_reg %>% 
  mutate(city = case_when
         (city %in% c('波士顿', 'Boston ', "Boston, Massachusetts, US", "boston", 'Boston') ~ "Boston",
           city %in% c("Boston (Jamaica Plain)", "Jamaica Plain (Boston)", "Jamaica Plain, MA", "Jamaica Plain ", "Jamaica plain ", "Jamaica Plain", 'Jamaica Plain, Boston') ~ "Jamaica Plain",
           city %in% c("ROXBURY CROSSING", "Roxbury Crossing", 'Roxbury') ~ 'Roxbury',
           city == 'Cambridge' ~ 'Cambridge',
           city %in% c("Charlestown", "Boston (Charlestown)") ~ 'Charlestown',
           city == "Watertown" ~ 'Watertown',
           city == "Newton" ~ 'Newton',
           city == "Milton" ~ "Milton",
           city == "South Boston" ~ "South Boston",
           city %in% c("Roslindale", "Roslindale, Boston") ~ 'Roslindale',
           city == "Brookline" ~ 'Brookline',
           city == "Mission Hill, Boston" ~ 'Mission Hill',
           city %in% c("East Boston", "east Boston ") ~ "East Boston",
           city %in% c('Brighton', "Brighton ") ~ 'Brighton',
           city == "Hyde Park" ~ "Hyde Park",
           city %in% c("Dorchester", "dorchester, boston ") ~ "Dorchester",
           city %in% c("Allston", "ALLSTON") ~ "Allston",
           city == "South End, Boston" ~ 'South End',
           city == "Somerville" ~ "Somerville",
           city %in% c("West Roxbury", "") ~ "West Roxbury",
           city == "Mattapan" ~ "Mattapan"))

### property_type ###
# converted null values to apartment
table(df_list_reg$property_type)
sum(is.na(df_list_reg$property_type))

df_list_reg = df_list_reg %>% 
  mutate(property_type = if_else(property_type == '', 
                                'Apartment', 
                                 property_type))

### room_type ###
# good to go
table(df_list_reg$room_type)

### room_type ###
df_list_reg$room_type

### bed_type ###
table(df_list$bed_type)

### amenities ####
# dealing with amenities column
df_list_reg$amenities
class(df_list_reg$amenities)

# creating vector with all unique amenities
amenities_clean = gsub('[{}"]', '', df_list_reg$amenities)
amenities_split = strsplit(amenities_clean, ",")
amenities_unique = unique(unlist(strsplit(amenities_clean, ",")))

# initializing dummy columns for each unique amenity
df_list_reg[amenities_unique] = NA
# checking that dummy columns were made... check
df_list_reg$`Wireless Internet`

# imbededd for loops to iterate through amenities for each listing and ...
# fill 1 in appropriate column if provided and 0 otherwise
for(i in 1:ncol(df_list_reg[amenities_unique])){
  for(j in 1:nrow(df_list_reg)){
    df_list_reg[amenities_unique][j,i] <- 
      ifelse(str_detect(amenities_split[j], names(df_list_reg[amenities_unique][i])), 1, 0)
  }
}

# checking to see if dummy ammennities columns were made... check
df_list_reg$`Wireless Internet`

# dropping original amenities column
df_list_reg = df_list_reg %>% select(-c('amenities'))

### instant_bookable ###
table(df_list_reg$instant_bookable)
# imputing 1s and 0s for t and f values
df_list_reg = df_list_reg %>% 
  mutate(instant_bookable = 
           if_else(instant_bookable == 't', 1, 0))
df_list_reg$instant_bookable

### cancellation_policy ###
# good to go
table(df_list_reg$cancellation_policy)

### require_guest_profile_picture ###
table(df_list_reg$require_guest_profile_picture)
df_list_reg = df_list_reg %>% 
  mutate(require_guest_profile_picture = 
           if_else(require_guest_profile_picture == 't', 1, 0))
df_list_reg$require_guest_profile_picture


### require_guest_phone_verification ###
# dummifying column
table(df_list_reg$require_guest_phone_verification)
df_list_reg = df_list_reg %>% 
  mutate(require_guest_phone_verification = 
           if_else(require_guest_phone_verification == 't', 1, 0))
df_list_reg$require_guest_phone_verification

# checking null values remaining
colSums(is.na(df_list_reg))

# columns with NA values: 
# host_response_time, host_response_rate, host_acceptance_rate, bedrooms,
# bathrooms, beds, review_scores_rating, review_scores_accuracy, 
# review_scores_cleanliness, review_scores_checkin, review_scores_communication,
# review_scores_location,  review_scores_value, reviews_per_month

# investigating host related NA values
# all host related missing values are missing for all the same listings
# most likely means the listing hasnt been booked before
# going to autofill with mode values for each
host = df_list_reg %>% filter(is.na(host_response_time) & 
                                is.na(host_response_rate) & 
                                is.na(host_acceptance_rate))

# function to calculate column mode
calc_mode <- function(x){
  # List the distinct / unique values
  distinct_values <- unique(x)
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

df_list_reg = df_list_reg %>% 
  mutate(host_response_time = if_else(is.na(host_response_time), 
                                      calc_mode(host_response_time), 
                                      host_response_time),
         host_response_rate = if_else(is.na(host_response_rate), 
                                      calc_mode(host_response_rate), 
                                      host_response_rate),
         
         host_acceptance_rate = if_else(is.na(host_acceptance_rate), 
                                        calc_mode(host_acceptance_rate), 
                                        host_acceptance_rate)
         )

table(df_list_reg$host_acceptance_rate)
table(df_list$host_acceptance_rate)


# investigating bedrooms NA values
# assuming NA bedrooms means listing is a studio which seems to be represented as
# 0 beds for the rest of listings
beds = df_list_reg %>% filter(is.na(bedrooms))
beds2 = df_list %>% filter(is.na(bedrooms))

unique(df_list_reg$bedrooms)
stud = df_list_reg %>% filter(bedrooms == 0)

df_list_reg = df_list_reg %>% mutate(bedrooms = ifelse(is.na(bedrooms), 0, bedrooms))

# investigating bathrooms NA values
# 0 bath means shared bathroom the majority of the time
# the listings I was able to access mainly listed shared bathrooms where NA
bath = df_list_reg %>% filter(is.na(bathrooms))
df_list %>% filter(is.na(bathrooms)) %>% select(space)
priv = df_list_reg %>% filter(room_type == 'Private room')
no = df_list_reg %>% filter(bathrooms == 0)
df_list %>% filter(bathrooms == 0) %>% select(summary)

df_list_reg = df_list_reg %>% mutate(bathrooms = ifelse(is.na(bathrooms), 0, bathrooms))

# investigating beds NA values
# human entry error on the author of the listing's end
# imputing 1 as majority are single rooms/apartments that acc. no more than 2 guests
table(df_list_reg$bedrooms)
b = df_list_reg %>% filter(is.na(beds))
b2 = df_list %>% filter(is.na(beds))

df_list_reg = df_list_reg %>% mutate(beds = ifelse(is.na(beds), 1, beds))

# review_scores_rating, review_scores_accuracy, 
# review_scores_cleanliness, review_scores_checkin, review_scores_communication,
# review_scores_location,  review_scores_value, reviews_per_month
# clear that NA in review column means listing doesn't have any reviews yet
# converting ratings per month to 0 for all NA values
df_list_reg = df_list_reg %>% mutate(reviews_per_month = if_else(is.na(reviews_per_month),
                       0, reviews_per_month))

sum(is.na(df_list_reg$reviews_per_month))

# converting all other review columns to mode
revs = c('review_scores_rating', 'review_scores_accuracy', 
         'review_scores_cleanliness', 'review_scores_checkin', 
         'review_scores_communication',
         'review_scores_location', 'review_scores_value')

my_data = mutate_if(df_list_reg, revs, ~replace(revs, is.na(revs), 0))

my_data = mutate_at(df_list_reg, revs, ~replace(., is.na(.), 0))

cf = mutate_at(df_list_reg, revs, ~replace(., is.na(.), calc_mode(revs)))
cf

# inputting median values for the rest of the reviews columns
df_list_reg = df_list_reg %>% 
  mutate(review_scores_rating = if_else(is.na(review_scores_rating), 
                                      median(review_scores_rating, na.rm = TRUE), 
                                      review_scores_rating),
        review_scores_accuracy = if_else(is.na(review_scores_accuracy), 
                                        median(review_scores_accuracy, na.rm = TRUE), 
                                        review_scores_accuracy),
        review_scores_cleanliness = if_else(is.na(review_scores_cleanliness), 
                                        median(review_scores_cleanliness, na.rm = TRUE), 
                                        review_scores_cleanliness),
        review_scores_checkin = if_else(is.na(review_scores_checkin), 
                                        median(review_scores_checkin, na.rm = TRUE), 
                                        review_scores_checkin),
        review_scores_communication = if_else(is.na(review_scores_communication), 
                                        median(review_scores_communication, na.rm = TRUE), 
                                        review_scores_communication),
        review_scores_location = if_else(is.na(review_scores_location), 
                                        median(review_scores_location, na.rm = TRUE), 
                                        review_scores_location),
        review_scores_value = if_else(is.na(review_scores_value), 
                                       median(review_scores_value, na.rm = TRUE), 
                                      review_scores_value))
df_list_reg$longitude
df_list_reg$latitude

colnames(df_list_reg)

### distances to popular tourist locations ###
# distance to freedom trail
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_free_trail = geosphere::distHaversine( c(longitude, latitude), c(-71.054710, 42.366355)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_free_trail = dist_free_trail/1609.344)

# distance to quincy market
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_quincy_market = geosphere::distHaversine( c(longitude, latitude), c(-71.0544953, 42.360653)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_quincy_market = dist_quincy_market/1609.344)

# distance to boston common
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_boston_common = geosphere::distHaversine( c(longitude, latitude), c(-71.065489, 42.354954)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_boston_common = dist_boston_common/1609.344)

# distance to faneuil hall
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_faneuil_hall = geosphere::distHaversine( c(longitude, latitude), c(-71.054749, 42.360031)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_faneuil_hall = dist_faneuil_hall/1609.344)

# distance to USS constitution
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_uss_const = geosphere::distHaversine( c(longitude, latitude), c(-71.053166454, 42.370998516)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_uss_const = dist_uss_const/1609.344)

# distance to fenway
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_fenway = geosphere::distHaversine( c(longitude, latitude), c(-71.095764, 42.346268)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_fenway = dist_fenway/1609.344)

# distance to NE aquarium
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_aquarium = geosphere::distHaversine( c(longitude, latitude), c(-71.049149, 42.359215)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_aquarium = dist_aquarium/1609.344)

# distance to museum of fine arts
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_museum = geosphere::distHaversine( c(longitude, latitude), c(-71.089832974, 42.336831986)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_museum = dist_museum/1609.344)

# distance to public garden
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_public_garden = geosphere::distHaversine( c(longitude, latitude), c(-71.070930, 42.353821)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_public_garden = dist_public_garden/1609.344)

# distance to north end
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_north_end = geosphere::distHaversine( c(longitude, latitude), c(-71.054234, 42.364702)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_north_end = dist_north_end/1609.344)

# distance to old state house
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_state_house = geosphere::distHaversine( c(longitude, latitude), c(-71.0578287, 42.3587086)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_state_house = dist_state_house/1609.344)

# distance to tea party
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_tea_party = geosphere::distHaversine( c(longitude, latitude), c(-71.0494951, 42.3509309)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_tea_party = dist_tea_party/1609.344)

# distance to beacon hill
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_beacon_hill = geosphere::distHaversine( c(longitude, latitude), c(-71.0707389, 42.3587999)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_beacon_hill = dist_beacon_hill/1609.344)

# distance to museum of science
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_museum_science = geosphere::distHaversine( c(longitude, latitude), c(-71.071129, 42.367893)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_museum_science = dist_museum_science/1609.344)

# distance to bunker hill monument
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_bunker_hill = geosphere::distHaversine( c(longitude, latitude), c(-71.0606068, 42.3762083)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_bunker_hill = dist_bunker_hill/1609.344)

# distance to old north church
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(dist_old_north_church = geosphere::distHaversine( c(longitude, latitude), c(-71.0544954, 42.3664863)))
# meters to miles
df_list_reg = df_list_reg %>% mutate(dist_old_north_church = dist_old_north_church/1609.344)

colnames(df_list_reg)

# creating column with sum of all tourist destination distances and...
# taking average of distances (divide by 16)
# 16 total destinations
df_list_reg = df_list_reg %>% rowwise() %>% 
  mutate(avg_tourist_dest_dist = sum(across(starts_with("dist")))/16)

# checking distribution of avg_tourist_dest_dist
ggplot(df_list_reg, aes(x=avg_tourist_dest_dist)) + geom_histogram()

# amenities total
# creating column with sum number of amenities per listing
df_list_reg$total_amenities = rowSums(df_list_reg[, c(40:84)])

# checking distribution of total_amenities
# doesn't matter but is normal looking
ggplot(df_list_reg, aes(x=total_amenities)) + geom_histogram()

# reordering total_amenities to be after last amenity
df_list_reg = df_list_reg %>% relocate(total_amenities, .after = 'Free Parking on Street')

# converting tibble to dataframe (don't know why it changed to tibble)
df_list_reg = as.data.frame(df_list_reg)
sum(is.na(df_list_reg))

# dropping long / lat columns
colnames(df_list_reg)
drops_3 = c('longitude', 'latitude')
df_list_reg = df_list_reg %>% select(-drops_3)

drops_4 = c('listing_id')
df_list_reg = df_list_reg %>% select(!drops_4)

# double checking that there is no null values
sum(is.na(df_list_reg))

# fixing two of the amenity columns
# translation missing: en.hosting_amenity_49 / translation missing: en.hosting_amenity_50
# unnamed amenities that are not translated
# was not able to find translation

### dummying the columns ###
# remaining columns to dummy:
# host_response_time, neighborhood_cleansed, city, property_type,
# room_type, bed_type, cancellation_policy

# checking classes of all the columns
lapply(df_list_reg,class)
#converting accommodates to numeric
df_list_reg$accommodates = as.numeric(df_list_reg$accommodates)

df_list_reg = dummy_cols(df_list_reg, select_columns = c('host_response_time', 
                                            'neighborhood_cleansed',
                                            'city',
                                            'property_type',
                                            'room_type',
                                            'bed_type',
                                            'cancellation_policy'),
                             remove_selected_columns = TRUE)

colnames(df_list_reg)

# removing spaces and special characters from all column names
names(df_list_reg) = gsub(" ", "_", names(df_list_reg))
names(df_list_reg)
names(df_list_reg) = gsub('[(,),/,&]', '', names(df_list_reg))
names(df_list_reg) = gsub('-', '_', names(df_list_reg))
names(df_list_reg) = gsub("24", "twofour", names(df_list_reg))
names(df_list_reg)

# renaming unknown amenity columns
names(df_list_reg)
colnames(df_list_reg)[colnames(df_list_reg) == 'translation_missing:_en.hosting_amenity_49'] = 'unknown_amenity_one'
colnames(df_list_reg)[colnames(df_list_reg) == 'translation_missing:_en.hosting_amenity_50'] = 'unknown_amenity_two'

# dropping categorical, dummy variables which are uniform across all listings
l = sapply(training, function(x) is.factor(x))
m = training[, l]
# other pets, cats, dogs
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")
# dropping columns
drops_5 = c('Cats', 'Dogs', 'Other_pets')
df_list_reg = df_list_reg %>% select(-drops_5)

## transforming features to correct data types ##
# ...in a very inefficient way...
summary(df_list_reg)
sapply(df_list_reg, class)

df_list_reg = transform(df_list_reg, 
        security_deposit = as.integer(security_deposit),                
        host_since = as.integer(host_since),
        host_response_rate = as.integer(host_response_rate),
        host_acceptance_rate = as.integer(host_acceptance_rate),
        host_total_listings_count = as.integer(host_total_listings_count),
        host_is_superhost = as.factor(host_is_superhost),
        host_identity_verified = as.factor(host_identity_verified),
        accommodates = as.integer(accommodates),
        bathrooms = as.integer(bathrooms),
        bedrooms = as.integer(bedrooms),
        beds = as.integer(beds),
        price = as.integer(price),
        cleaning_fee = as.integer(cleaning_fee),
        guests_included = as.integer(guests_included),
        extra_people = as.integer(extra_people),
        minimum_nights = as.integer(minimum_nights),
        maximum_nights = as.integer(maximum_nights),
        number_of_reviews = as.integer(number_of_reviews),
        review_scores_rating = as.integer(review_scores_rating),
        review_scores_accuracy = as.integer(review_scores_accuracy),
        review_scores_cleanliness = as.integer(review_scores_cleanliness),
        review_scores_checkin = as.integer(review_scores_checkin),
        review_scores_communication = as.integer(review_scores_communication),
        review_scores_location = as.integer(review_scores_location),
        review_scores_value = as.integer(review_scores_value),
        instant_bookable = as.factor(instant_bookable),
        require_guest_profile_picture = as.factor(require_guest_profile_picture),
        require_guest_phone_verification = as.factor(require_guest_phone_verification),
        reviews_per_month = as.integer(reviews_per_month),
        TV = as.factor(TV),
        Wireless_Internet = as.factor(Wireless_Internet),
        Kitchen = as.factor(Kitchen),
        Free_Parking_on_Premises = as.factor(Free_Parking_on_Premises),
        Pets_live_on_this_property = as.factor(Pets_live_on_this_property),
        Heating = as.factor(Heating),
        FamilyKid_Friendly = as.factor(FamilyKid_Friendly),
        Washer = as.factor(Washer),
        Dryer = as.factor(Dryer),
        Smoke_Detector = as.factor(Smoke_Detector),
        Fire_Extinguisher = as.factor(Fire_Extinguisher),
        Essentials = as.factor(Essentials),
        Shampoo = as.factor(Shampoo),
        Laptop_Friendly_Workspace = as.factor(Laptop_Friendly_Workspace),
        Internet = as.factor(Internet),
        Air_Conditioning = as.factor(Air_Conditioning),
        Pets_Allowed = as.factor(Pets_Allowed),
        Carbon_Monoxide_Detector = as.factor(Carbon_Monoxide_Detector),
        Lock_on_Bedroom_Door = as.factor(Lock_on_Bedroom_Door),
        Hangers = as.factor(Hangers),
        Hair_Dryer = as.factor(Hair_Dryer),
        Iron = as.factor(Iron),
        Cable_TV = as.factor(Cable_TV),
        First_Aid_Kit = as.factor(First_Aid_Kit),
        Safety_Card = as.factor(Safety_Card),
        unknown_amenity_one = as.factor(unknown_amenity_one),
        unknown_amenity_two = as.factor(unknown_amenity_two),
        Gym = as.factor(Gym),
        Breakfast = as.factor(Breakfast),
        Indoor_Fireplace = as.factor(Indoor_Fireplace),
        twofour_Hour_Check_in = as.factor(twofour_Hour_Check_in),
        Hot_Tub = as.factor(Hot_Tub),
        BuzzerWireless_Intercom = as.factor(BuzzerWireless_Intercom),
        Washer__Dryer = as.factor(Washer__Dryer),
        Smoking_Allowed = as.factor(Smoking_Allowed),
        Suitable_for_Events = as.factor(Suitable_for_Events),
        Wheelchair_Accessible = as.factor(Wheelchair_Accessible),
        Elevator_in_Building = as.factor(Elevator_in_Building),
        Pool = as.factor(Pool),
        Doorman = as.factor(Doorman),
        Paid_Parking_Off_Premises = as.factor(Paid_Parking_Off_Premises),
        Free_Parking_on_Street = as.factor(Free_Parking_on_Street),
        total_amenities = as.integer(total_amenities),
        host_response_time_a_few_days_or_more = as.factor(host_response_time_a_few_days_or_more),
        host_response_time_within_a_day = as.factor(host_response_time_within_a_day),
        host_response_time_within_a_few_hours = as.factor(host_response_time_within_a_few_hours),
        host_response_time_within_an_hour = as.factor(host_response_time_within_an_hour),
        neighborhood_cleansed_Allston = as.factor(neighborhood_cleansed_Allston),
        neighborhood_cleansed_Back_Bay = as.factor(neighborhood_cleansed_Back_Bay),
        neighborhood_cleansed_Bay_Village = as.factor(neighborhood_cleansed_Bay_Village),
        neighborhood_cleansed_Beacon_Hill = as.factor(neighborhood_cleansed_Beacon_Hill),
        neighborhood_cleansed_Brighton = as.factor(neighborhood_cleansed_Brighton),
        neighborhood_cleansed_Charlestown = as.factor(neighborhood_cleansed_Charlestown),
        neighborhood_cleansed_Chinatown = as.factor(neighborhood_cleansed_Chinatown),
        neighborhood_cleansed_Dorchester = as.factor(neighborhood_cleansed_Dorchester),
        neighborhood_cleansed_Downtown = as.factor(neighborhood_cleansed_Downtown),
        neighborhood_cleansed_East_Boston = as.factor(neighborhood_cleansed_East_Boston),
        neighborhood_cleansed_Fenway = as.factor(neighborhood_cleansed_Fenway),
        neighborhood_cleansed_Hyde_Park = as.factor(neighborhood_cleansed_Hyde_Park),
        neighborhood_cleansed_Jamaica_Plain = as.factor(neighborhood_cleansed_Jamaica_Plain),
        neighborhood_cleansed_Leather_District = as.factor(neighborhood_cleansed_Leather_District),
        neighborhood_cleansed_Longwood_Medical_Area = as.factor(neighborhood_cleansed_Longwood_Medical_Area),
        neighborhood_cleansed_Mattapan = as.factor(neighborhood_cleansed_Mattapan),
        neighborhood_cleansed_Mission_Hill = as.factor(neighborhood_cleansed_Mission_Hill),
        neighborhood_cleansed_North_End = as.factor(neighborhood_cleansed_North_End),
        neighborhood_cleansed_Roslindale = as.factor(neighborhood_cleansed_Roslindale),
        neighborhood_cleansed_Roxbury = as.factor(neighborhood_cleansed_Roxbury),
        neighborhood_cleansed_South_Boston = as.factor(neighborhood_cleansed_South_Boston),
        neighborhood_cleansed_South_Boston_Waterfront = as.factor(neighborhood_cleansed_South_Boston_Waterfront),
        neighborhood_cleansed_South_End = as.factor(neighborhood_cleansed_South_End),
        neighborhood_cleansed_West_End = as.factor(neighborhood_cleansed_West_End),
        neighborhood_cleansed_West_Roxbury = as.factor(neighborhood_cleansed_West_Roxbury),
        city_Allston = as.factor(city_Allston),
        city_Boston = as.factor(city_Boston),
        city_Brighton = as.factor(city_Brighton),
        city_Brookline = as.factor(city_Brookline),
        city_Cambridge = as.factor(city_Cambridge),
        city_Charlestown = as.factor(city_Charlestown),
        city_Dorchester = as.factor(city_Dorchester),
        city_East_Boston = as.factor(city_East_Boston),
        city_Hyde_Park = as.factor(city_Hyde_Park),
        city_Jamaica_Plain = as.factor(city_Jamaica_Plain),
        city_Mattapan = as.factor(city_Mattapan),
        city_Milton = as.factor(city_Milton),
        city_Mission_Hill = as.factor(city_Mission_Hill),
        city_Newton = as.factor(city_Newton),
        city_Roslindale = as.factor(city_Roslindale),
        city_Roxbury = as.factor(city_Roxbury),
        city_Somerville = as.factor(city_Somerville),
        city_South_Boston = as.factor(city_South_Boston),
        city_South_End = as.factor(city_South_End),
        city_Watertown = as.factor(city_Watertown),
        city_West_Roxbury = as.factor(city_West_Roxbury),
        property_type_Apartment = as.factor(property_type_Apartment),
        property_type_Bed__Breakfast = as.factor(property_type_Bed__Breakfast),
        property_type_Boat = as.factor(property_type_Boat),
        property_type_CamperRV = as.factor(property_type_CamperRV),
        property_type_Condominium = as.factor(property_type_Condominium),
        property_type_Dorm = as.factor(property_type_Dorm),
        property_type_Entire_Floor = as.factor(property_type_Entire_Floor),
        property_type_Guesthouse = as.factor(property_type_Guesthouse),
        property_type_House = as.factor(property_type_House),
        property_type_Loft = as.factor(property_type_Loft),
        property_type_Other = as.factor(property_type_Other),
        property_type_Townhouse = as.factor(property_type_Townhouse),
        property_type_Villa = as.factor(property_type_Villa),
        room_type_Entire_homeapt = as.factor(room_type_Entire_homeapt),
        room_type_Private_room = as.factor(room_type_Private_room),
        room_type_Shared_room = as.factor(room_type_Shared_room),
        bed_type_Airbed = as.factor(bed_type_Airbed),
        bed_type_Couch = as.factor(bed_type_Couch),
        bed_type_Futon = as.factor(bed_type_Futon),
        bed_type_Pull_out_Sofa = as.factor(bed_type_Pull_out_Sofa),
        bed_type_Real_Bed = as.factor(bed_type_Real_Bed),
        cancellation_policy_flexible = as.factor(cancellation_policy_flexible),
        cancellation_policy_moderate = as.factor(cancellation_policy_moderate),
        cancellation_policy_strict = as.factor(cancellation_policy_strict),
        cancellation_policy_super_strict_30 = as.factor(cancellation_policy_super_strict_30))
        
sapply(df_list_reg, class)

### making separate DFs with and without amenity and tourist_location totals ###
# all features
df_list_reg_all = df_list_reg

# without amenity and tourist location distance totals
totals = c('total_amenities', 'avg_tourist_dest_dist')
df_list_reg_no_tots = df_list_reg %>% select(-totals)

# only totals (no amenities dummy columns or specific tourist location distances)
colnames(df_list_reg)
amen_dist = c("TV", "Wireless_Internet", "Kitchen", "Free_Parking_on_Premises",
              "Pets_live_on_this_property", "Heating", "FamilyKid_Friendly", 
              "Washer", "Dryer", "Smoke_Detector", "Fire_Extinguisher", "Essentials", 
              "Shampoo", "Laptop_Friendly_Workspace", "Internet", "Air_Conditioning",
              "Pets_Allowed", "Carbon_Monoxide_Detector", "Lock_on_Bedroom_Door",
              "Hangers", "Hair_Dryer", "Iron", "Cable_TV", "First_Aid_Kit",
              "Safety_Card", "unknown_amenity_one",
              "unknown_amenity_two", "Gym", "Breakfast",
              "Indoor_Fireplace", "twofour_Hour_Check_in", "Hot_Tub", 
              "BuzzerWireless_Intercom", "Washer__Dryer",
              "Smoking_Allowed", "Suitable_for_Events", "Wheelchair_Accessible", 
              "Elevator_in_Building", "Pool", "Doorman", "Paid_Parking_Off_Premises", 
              "Free_Parking_on_Street", "dist_free_trail", "dist_quincy_market", 
              "dist_boston_common", "dist_faneuil_hall", "dist_uss_const", 
              "dist_fenway", "dist_aquarium", "dist_museum", "dist_public_garden",
              "dist_north_end", "dist_state_house", "dist_tea_party", "dist_beacon_hill",
              "dist_museum_science", "dist_bunker_hill", "dist_old_north_church")
df_list_reg_only_tots = df_list_reg %>% select(-amen_dist)


### target variable analysis ###
# price vs. price per guest
sum(is.na(df_list_reg$price))

# checking normality for the two variables
# distributions of target (price) is not normal
ggplot(df_list_reg, aes(x=price, binwidth = 50)) + geom_histogram()

# getting a better idea of distribution without outliers
df_list_no_out = df_list_reg %>% filter(price<=1000)
df_list_reg %>% filter(price>1000)
ggplot(df_list_no_out, aes(x=price)) + geom_histogram()

# checking the listings above outlier price threshold
out = boxplot.stats(df_list_reg$price)$out
out_ind = which(df_list_reg$price %in% c(out))
length(out_ind) # 124 outliers
outliers = df_list_reg[out_ind, ]

ggplot(outliers, aes(x=price)) + geom_histogram()
j = outliers %>% filter(number_of_reviews == 0)

# outliers - price
boxplot.stats(df_list_reg$price)$out
min(boxplot.stats(df_list_reg$price)$out)
max(boxplot.stats(df_list_reg$price)$out)

### normality of target variable ###
# Shaprio-wilks tests - both are statistically not normal
shapiro.test(df_list_reg$price)

# Kolmogorov-Smirnov Test - both are statistically not normal
ks.test(df_list_reg$price, 'pnorm')

# Q-Q plots
qqPlot(df_list_reg$price)

# skewness - not normal
skewness(df_list_reg$price)

# evaluating set of outliers in price
out_price = df_list_reg %>% filter(price > 425)
out_price

# shapiro test, qq plot, skewness measure for price without outliers
no_out_price = df_list_reg %>% filter(price < 425)
ggplot(no_out_price, aes(x=price)) + geom_histogram()
shapiro.test(no_out_price$price)
qqPlot(no_out_price$price)
skewness(no_out_price$price)

# moving forward with Random Forest model to evaluate feature importance
# target is going to be price


### random forest model with outliers ###
# target variable: price
# WITH OUTLIERS

library(randomForest)
require(caTools)
library(ranger)
library(caret)
library(Metrics)
library(varImp)
library(mlbench)
library(h2o)

# splitting train and test data
# 80 / 20 train, test split
# did not ensure distribution of price variable is similar for both train/test

### 1. 'all' dataset ###
# df_list_reg_all --> splitting data
set.seed(1234)
dt_out_all = sort(sample(nrow(df_list_reg_all), nrow(df_list_reg_all)*.8))

train_out_all = df_list_reg_all[dt_out_all,]
test_out_all = df_list_reg_all[-dt_out_all,]

dim(train_out_all)
dim(test_out_all)

# within $4 of one another - close enough
mean(train_out_all$price) 
mean(test_out_all$price) 

# random forest - outliers - all dataset
rf_out_all = randomForest(
  price ~ .,
  data = train_out_all,
  type='regression',
  importance=TRUE
)

# R-squared on train = 0.349
rf_out_all

# permutation feature importance
# (percent increase in MSE)
varImpPlot(rf_out_all, type=1)

sqrt(min(rf_out_all$mse))

# RMSE (on train data)
sqrt(rf_out_all$mse[length(rf_out_all$mse)])

# RMSE (test) = ~73.34
pred_out_all = predict(rf_out_all, newdata=test_out_all)
actual_values_out_all = test_out_all$price
rmse(pred_out_all, actual_values_out_all)

# R-squared on test set = ~0.583
r_squared_test_1 = 1 - (sum((actual_values_out_all-pred_out_all)^2)/
                        sum((actual_values_out_all-mean(actual_values_out_all))^2))
r_squared_test_1

### 2. 'no totals' dataset ###
# df_list_reg_no_tots --> splitting data
set.seed(1234)
dt_out_no_tots = sort(sample(nrow(df_list_reg_no_tots), nrow(df_list_reg_no_tots)*.8))

train_out_no_tots = df_list_reg_no_tots[dt_out_no_tots,]
test_out_no_tots = df_list_reg_no_tots[-dt_out_no_tots,]

dim(train_out_no_tots)
dim(test_out_no_tots)

# within $7 of one another - close enough
mean(train_out_no_tots$price) 
mean(test_out_no_tots$price) 

# random forest - outliers - all dataset
rf_out_no_tots = randomForest(
  price ~ .,
  data = train_out_no_tots,
  type='regression',
  importance=TRUE
)

# R-squared = 0.381
rf_out_no_tots

# permutation feature importance
# (percent increase in MSE)
varImpPlot(rf_out_no_tots, type=1)

sqrt(min(rf_out_no_tots$mse))

# RMSE = ~80.88
pred_out_no_tots = predict(rf_out_no_tots, newdata=test_out_no_tots)
actual_values_out_no_tots = test_out_no_tots$price
rmse(pred_out_no_tots, actual_values_out_no_tots)

# R-squared on test set = ~0.456
r_squared_test_2 = 1 - (sum((actual_values_out_no_tots-pred_out_no_tots)^2)/
                        sum((actual_values_out_no_tots-mean(actual_values_out_no_tots))^2))
r_squared_test_2

### 3. 'only totals' dataset ###
# df_list_reg_only_tots --> splitting data
set.seed(1234)
df_list_reg_only_tots

dt_out_only_tots = sort(sample(nrow(df_list_reg_only_tots), nrow(df_list_reg_only_tots)*.8))

train_out_only_tots = df_list_reg_only_tots[dt_out_only_tots,]
test_out_only_tots = df_list_reg_only_tots[-dt_out_only_tots,]

dim(train_out_only_tots)
dim(test_out_only_tots)

# within $1 of one another - close enough
mean(train_out_only_tots$price) 
mean(test_out_only_tots$price) 

# random forest - outliers - all dataset
rf_out_only_tots = randomForest(
  price ~ .,
  data = train_out_only_tots,
  type='regression',
  importance=TRUE
)

# R-squared = 0.375
rf_out_only_tots

# permutation feature importance
# (percent increase in MSE)
varImpPlot(rf_out_only_tots, type=1)

sqrt(min(rf_out_only_tots$mse))

# RMSE = ~83.14
pred_out_only_tots = predict(rf_out_only_tots, newdata=test_out_only_tots)
actual_values_out_only_tots = test_out_only_tots$price
rmse(pred_out_only_tots, actual_values_out_only_tots)

# R-squared on test set = ~0.554
r_squared_test_3 = 1 - (sum((actual_values_out_only_tots-pred_out_only_tots)^2)/
                        sum((actual_values_out_only_tots-mean(actual_values_out_only_tots))^2))
r_squared_test_3

### Random Forest WITHOUT OUTLIERS (cutoff --> 600) ###
# checking whether outliers play a large role in bad model
# data WITHOUT OUTLIERS (cutoff being 600 a night and under)
# 1. all, no outliers
set.seed(1234)
df_list_reg_all_no_out = df_list_reg_all %>% filter(price <= 600)
dim(df_list_reg_all_no_out)

# splitting data
dt_no_out_all = sort(sample(nrow(df_list_reg_all_no_out), nrow(df_list_reg_all_no_out)*.8))

train_no_out_all = df_list_reg_all_no_out[dt_no_out_all,]
test_no_out_all = df_list_reg_all_no_out[-dt_no_out_all,]

dim(train_no_out_all)
dim(test_no_out_all)

# checking distribution of target variable in train and test sets
# train
ggplot(train_no_out_all) + geom_histogram(aes(x=price))
# test
ggplot(test_no_out_all) + geom_histogram(aes(x=price))

# within $2 of one another - close enough
mean(train_no_out_all$price) 
mean(test_no_out_all$price) 

# random forest - no outliers, all variables
rf_no_out_all = randomForest(
  price ~ .,
  data = train_no_out_all,
  type='regression',
  importance=TRUE
)

# R-squared = ~0.692
rf_no_out_all

mean(rf_no_out_all$rsq)

# permutation feature importance
# (percent increase in MSE)
varimp(rf_no_out_all, type=1)
varImpPlot(rf_no_out_all, type=1, main=deparse(substitute(rf_no_out_all)))

# RMSE (on train data)
sqrt(rf_no_out_all$mse[length(rf_no_out_all$mse)])

# RMSE (on test data) = ~50.12
pred_no_out_all = predict(rf_no_out_all, newdata=test_no_out_all)
actual_values_no_out_all = test_no_out_all$price
rmse(pred_no_out_all, actual_values_no_out_all)

# R-squared on test set = ~0.730
r_squared_test_4 = 1 - (sum((actual_values_no_out_all-pred_no_out_all)^2)/
                        sum((actual_values_no_out_all-mean(actual_values_no_out_all))^2))
r_squared_test_4

plot(rf_no_out_all)

# 464 trees
sqrt(min(rf_no_out_all$mse))

# 55.1922
sqrt(rf_no_out_all$mse[which.min(rf_no_out_all$mse)])

## Second all_no_out model with mtry set to 33
rf_no_out_all_mtry = randomForest(
  price ~ .,
  data = train_no_out_all,
  type='regression',
  importance=TRUE,
  mtry=33
)

rf_no_out_all_mtry

names(df_list_reg_all_no_out)

# RMSE (on train data) - mtry = 33
sqrt(rf_no_out_all_mtry$mse[length(rf_no_out_all_mtry$mse)])

# RMSE (on test data) - mtry = 33
pred_no_out_all_3 = predict(rf_no_out_all_mtry, newdata=test_no_out_all)
actual_values_no_out_all_3 = test_no_out_all$price
rmse(pred_no_out_all_3, actual_values_no_out_all_3)

# 2. no totals/averages, no outliers
set.seed(1234)
df_list_reg_no_tots_no_out = df_list_reg_no_tots %>% filter(price <= 600)
dim(df_list_reg_no_tots_no_out)

# splitting data
dt_no_out_no_tots = sort(sample(nrow(df_list_reg_no_tots_no_out), nrow(df_list_reg_no_tots_no_out)*.8))

train_no_out_no_tots = df_list_reg_no_tots_no_out[dt_no_out_no_tots,]
test_no_out_no_tots = df_list_reg_no_tots_no_out[-dt_no_out_no_tots,]

dim(train_no_out_no_tots)
dim(test_no_out_no_tots)

# within $3 of one another - close enough
mean(train_no_out_no_tots$price) 
mean(test_no_out_no_tots$price) 

# random forest - no outliers, all variables
rf_no_out_no_tots = randomForest(
  price ~ .,
  data = train_no_out_no_tots,
  type='regression',
  importance=TRUE
)

# R-squared = ~0.692
rf_no_out_no_tots

# permutation feature importance
# (percent increase in MSE)
varImpPlot(rf_no_out_no_tots, type=1)

# RMSE (on train data)
sqrt(rf_no_out_no_tots$mse[length(rf_no_out_no_tots$mse)])
sqrt(min(rf_no_out_no_tots$mse))

# RMSE (on test data) = ~54.67
pred_no_out_no_tots = predict(rf_no_out_no_tots, newdata=test_no_out_no_tots)
actual_values_no_out_no_tots = test_no_out_no_tots$price
rmse(pred_no_out_no_tots, actual_values_no_out_no_tots)

# R-squared on test set = ~0.712
r_squared_test_5 = 1 - (sum((actual_values_no_out_no_tots-pred_no_out_no_tots)^2)/
                        sum((actual_values_no_out_no_tots-mean(actual_values_no_out_no_tots))^2))
r_squared_test_5

# 3. only totals/averages, no outliers
set.seed(1234)
df_list_reg_only_tots_no_out = df_list_reg_only_tots %>% filter(price <= 600)
dim(df_list_reg_only_tots_no_out)

# splitting data
dt_no_out_only_tots = sort(sample(nrow(df_list_reg_only_tots_no_out), nrow(df_list_reg_only_tots_no_out)*.8))

train_no_out_only_tots = df_list_reg_only_tots_no_out[dt_no_out_only_tots,]
test_no_out_only_tots = df_list_reg_only_tots_no_out[-dt_no_out_only_tots,]

dim(train_no_out_only_tots)
dim(test_no_out_only_tots)

# within $2 of one another - close enough
mean(train_no_out_only_tots$price) 
mean(test_no_out_only_tots$price) 

# random forest - no outliers, all variables
rf_no_out_only_tots = randomForest(
  price ~ .,
  data = train_no_out_only_tots,
  type='regression',
  importance=TRUE
)

# R-squared = ~0.696
rf_no_out_only_tots

# permutation feature importance
# (percent increase in MSE)
varImpPlot(rf_no_out_only_tots, type=1)

# RMSE (on train data)
sqrt(rf_no_out_only_tots$mse[length(rf_no_out_only_tots$mse)])
sqrt(min(rf_no_out_only_tots$mse))

# RMSE (on test data) = ~51.73
pred_no_out_only_tots = predict(rf_no_out_only_tots, newdata=test_no_out_only_tots)
actual_values_no_out_only_tots = test_no_out_only_tots$price
rmse(pred_no_out_only_tots, actual_values_no_out_only_tots)

# R-squared on test set = ~0.720
r_squared_test_6 = 1 - (sum((actual_values_no_out_only_tots-pred_no_out_only_tots)^2)/
                        sum((actual_values_no_out_only_tots-mean(actual_values_no_out_only_tots))^2))
r_squared_test_6


### Random Forest Model Tuning w/ RANGER ###
# (without outliers - df_list_reg_all_no_out)
set.seed(123)
samp = createDataPartition(df_list_reg_all_no_out$price, p = 0.8, list = FALSE)
training = df_list_reg_all_no_out[samp,]
testing = df_list_reg_all_no_out[-samp,]

dim(training)
dim(testing)

# distribution of price in both training and testing
ggplot(training) + geom_histogram(aes(x=price))
ggplot(testing) + geom_histogram(aes(x=price))

rf_first = randomForest(
  price ~ .,
  data = training,
  type='regression',
  importance=TRUE
)

# R-squared = 0.6983
rf_first

plot(rf_first)

# 490 trees (ntrees)
which.min(rf_first$mse)

# 490 trees --> train rmse of 55.74
sqrt(rf_first$mse[which.min(rf_first$mse)])

# test rmse of 56.47
pred_first = predict(rf_first, newdata=testing)
actual_values_first = testing$price
rmse(pred_first, actual_values_first)

# mtry tuning - best mtry value is around 33
feats_first = setdiff(names(training), "price")
set.seed(123)
rf_tune <- tuneRF(
  x          = training[feats_first],
  y          = training$price,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)

### grid search of multiple hyperparameters ###
hyper_grid = expand.grid(
  mtry       = seq(22, 30, by = 2),
  node_size  = seq(1, 5, by = 2),
  sampe_size = c(.70, .80, 1),
  ntree      = seq(450, 500, by = 10),
  OOB_RMSE   = 0
)

# total number of combinations
nrow(hyper_grid)
## [1] 120

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  rf_grid = ranger(
    formula         = price ~ ., 
    data            = training,
    num.trees       = hyper_grid$ntree[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123,
    importance = 'permutation'
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] = sqrt(rf_grid$prediction.error)
}

hyper_grid %>% arrange(OOB_RMSE)

# minimum OOB_RMSE value hyperparameters:
# mtry = 26, node_size = 3, sample_size = 1, ntree = 460
hyper_grid %>% filter(OOB_RMSE == min(OOB_RMSE))

# getting a better sense of OOB_RMSE with best hyperparameters
OOB_RMSE = vector(mode = "numeric", length = 100)

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger = ranger(
    formula         = price ~ ., 
    data            = training, 
    num.trees       = 460,
    mtry            = 26,
    min.node.size   = 3,
    sample.fraction = 1,
    importance      = 'permutation'
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

# more cenetered between 56.0 and 56.2
hist(OOB_RMSE, breaks = 20)

# r-squared = 0.6934
optimal_ranger$r.squared

# optimal_ranger model predictions for testing set
pred = predict(optimal_ranger, 
               testing)
# prediction values from pred
pred$predictions

# RMSE of training set - 56.19
sqrt(optimal_ranger$prediction.error)
optimal_ranger$prediction.error

# RMSE of testing set - 56.58
predictions_ranger = pred$prediction 
actual_values_optimal_ranger = testing$price
rmse(predictions_ranger, actual_values_optimal_ranger)

### interpretting model accuracy ###
### plot of actual vs. predicted ###
actualAndPredictedData = data.frame(actualValue = testing$price, 
                                    predictedValue = pred$predictions)
actualAndPredictedData

# raw error column
actualAndPredictedData = actualAndPredictedData %>% mutate(error = actualValue - predictedValue)

# over / under column
# predict is.... (over, under, accurate)
actualAndPredictedData = actualAndPredictedData %>% 
  mutate(over_under = case_when(error < 0 ~ "over", 
                                error > 0 ~ "under",
                                error == 0 ~ "accurate"))

# actual listing price classification
actualAndPredictedData = actualAndPredictedData %>% 
  mutate(actual_listing_price_class = case_when(actualValue <= 50 ~ '1-50', 
                                                actualValue <= 100 & actualValue > 50 ~ '51-100',
                                                actualValue <= 150 & actualValue > 100 ~ '101-150',
                                                actualValue <= 200 & actualValue > 150 ~ '151-200',
                                                actualValue <= 250 & actualValue > 200 ~ '201-250',
                                                actualValue <= 300 & actualValue > 250 ~ '251-300',
                                                actualValue <= 350 & actualValue > 300 ~ '301-350',
                                                actualValue <= 400 & actualValue > 350 ~ '351-400',
                                                actualValue <= 450 & actualValue > 400 ~ '401-450',
                                                actualValue <= 500 & actualValue > 450 ~ '451-500',
                                                actualValue <= 550 & actualValue > 500 ~ '501-550',
                                                actualValue <= 600 & actualValue > 550 ~ '551-600'))
                                                
# absolute error column
actualAndPredictedData = actualAndPredictedData %>% mutate(abs_error = abs(actualValue - predictedValue))
max(actualAndPredictedData$abs_error)


actualAndPredictedData = actualAndPredictedData %>% 
  mutate(absolute_error = case_when(abs_error <= 25 ~ "<=25", 
                                     abs_error <= 50 ~ "<=50",
                                     abs_error <= 75 ~ '<=75',
                                     abs_error <= 100 ~ '<=100',
                                     abs_error <= 150 ~ '<=150',
                                     abs_error <= 200 ~ '<=200',
                                     abs_error > 200 ~ '>200'))

actualAndPredictedData$absolute_error = 
  factor(actualAndPredictedData$absolute_error, 
         levels = c("<=25", "<=50", '<=75', '<=100', '<=150',
                    '<=200', '>200'))

# scatter plot
plt = ggplot(actualAndPredictedData) + 
  geom_point(aes(x = actualValue, y = predictedValue, color = absolute_error))
plt = plt + 
  geom_abline(intercept = 0, slope = 1, color="black", size=0.5, linetype='dashed') +
  geom_abline(intercept = -50, slope = 1, color="black", size=0.1, linetype='dashed') +
  geom_abline(intercept = 50, slope = 1, color="black", size=0.1, linetype='dashed') +
  geom_abline(intercept = -100, slope = 1, color="black", size=0.1, linetype='dashed') +
  geom_abline(intercept = 100, slope = 1, color="black", size=0.1, linetype='dashed') +
  geom_abline(intercept = -150, slope = 1, color="black", size=0.1, linetype='dashed') +
  geom_abline(intercept = 150, slope = 1, color="black", size=0.1, linetype='dashed') +
  geom_abline(intercept = -200, slope = 1, color="black", size=0.1, linetype='dashed') +
  geom_abline(intercept = 200, slope = 1, color="black", size=0.1, linetype='dashed') +
  ggtitle('Test Data: Actual vs. Predicted Values') + 
  xlab('Actual Values') + 
  ylab('Predicted Values') + 
  labs(color = 'Absolute Error')
plt

# histograms
ggplot(actualAndPredictedData) + geom_histogram(aes(x=actualValue))
ggplot(actualAndPredictedData) + geom_histogram(aes(x=predictedValue))

# density plots
dens = ggplot(actualAndPredictedData) +
  geom_density(aes(x=actualValue, fill = 'Actual', alpha=0.1)) + 
  geom_density(aes(x=predictedValue, fill = 'Predicted', alpha=0.1))

dens + ggtitle('Test Data: Actual vs. Predicted') +
  xlab('Listing Prices') +
  ylab('Density') + guides(alpha=FALSE) +
  labs(fill = 'Values')


ggplot(actualAndPredictedData) + geom_histogram(aes(x=abs_error))

# number of predictions over / under per actual listing value group
pred_over_under = actualAndPredictedData %>% 
  group_by(actual_listing_price_class, over_under) %>% 
  count()

level_order = c('1-50', '51-100', '101-150', '151-200', '201-250', 
                '251-300', '301-350', '351-400', '401-450', 
                '451-500', '501-550', '551-600')

p = ggplot(pred_over_under, aes(x=factor(actual_listing_price_class, level = level_order), y=n, fill=factor(over_under, levels=c("under","over")))) + 
  geom_bar(stat="identity", position = "stack") +
  scale_fill_manual(values = c('over' = "darkseagreen3", 'under'  = "cadetblue")) +
  ggtitle('Number of Over/Under Predictions by Actual Listing Price Grouping') +
  ylab('Number of Listings / Predictions') +
  xlab('Actual Listing Price Ranges ($)')
p + labs(fill = "Prediction \n Type") + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25))


### feature importance ###
variable_importance = as.data.frame(optimal_ranger$variable.importance)
feature_importance = rownames_to_column(variable_importance)
names(feature_importance) = c('feature', 'variable_importance')
feature_importance

# top 25 feature importances
feature_importance %>% 
  arrange(desc(variable_importance)) %>%
  top_n(25) %>%
  ggplot(aes(x=reorder(feature, variable_importance), y=variable_importance)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 25 Important Variables") +
  xlab('Permutation Feature Importance') + 
  ylab('Feature')

# tourist location distances feature importances
feature_importance %>% 
  filter(grepl('dist', feature)) %>% 
  arrange(desc(variable_importance)) %>%
  ggplot(aes(x=reorder(feature, variable_importance), y=variable_importance)) +
  geom_col() +
  coord_flip() +
  ggtitle("Variable Importance - Tourist Location Distance") +
  xlab('Permutation Feature Importance') + 
  ylab('Feature')

# neighborhood feature importances
feature_importance %>% 
  filter(grepl('neighborhood', feature)) %>% 
  arrange(desc(variable_importance)) %>%
  ggplot(aes(x=reorder(feature, variable_importance), y=variable_importance)) +
  geom_col() +
  coord_flip() +
  ggtitle("Variable Importance - Neighborhoods") +
  xlab('Permutation Feature Importance') + 
  ylab('Feature')

# amenities feature importance
feature_importance[29:70, ] %>% 
  arrange(desc(variable_importance)) %>%
  ggplot(aes(x=reorder(feature, variable_importance), y=variable_importance)) +
  geom_col() +
  coord_flip() +
  ggtitle("Variable Importance - Amenities") +
  xlab('Permutation Feature Importance') + 
  ylab('Feature')

# host feature importances
host_rel_filt = c('host', 'cancellation', 'security', 'cleaning',
             'guests_included', 'extra', 'minimum', 'maximum', 'instant', 'require')
host_rel = unique(grep(paste(host_rel,collapse="|"), 
            feature_importance$feature, value=TRUE))

feature_importance %>% 
  filter(feature %in% host_rel) %>%
  arrange(desc(variable_importance)) %>%
  ggplot(aes(x=reorder(feature, variable_importance), y=variable_importance)) +
  geom_col() +
  coord_flip() +
  ggtitle("Variable Importance - Host Related") +
  xlab('Permutation Feature Importance') + 
  ylab('Feature')

# review related feature importances
feature_importance %>% 
  filter(grepl('review', feature)) %>% 
  arrange(desc(variable_importance)) %>%
  ggplot(aes(x=reorder(feature, variable_importance), y=variable_importance)) +
  geom_col() +
  coord_flip() +
  ggtitle("Variable Importance - Reviews") +
  xlab('Permutation Feature Importance') + 
  ylab('Feature')

ggplot(df_list_reg_all_no_out) + geom_histogram(aes(x=dist_beacon_hill))
ggplot(df_list_reg_all_no_out) + geom_histogram(aes(x=price))

ggplot(df_list_reg_all_no_out) + geom_histogram(aes(x=dist_boston_common))
ggplot(df_list_reg_all_no_out) + geom_histogram(aes(x=dist_public_garden))
ggplot(df_list_reg_all_no_out) + geom_histogram(aes(x=dist_quincy_market))
ggplot(df_list_reg_all_no_out) + geom_histogram(aes(x=dist_north_end))
ggplot(df_list_reg_all_no_out) + geom_histogram(aes(x=dist_fenway))
ggplot(df_list_reg_all_no_out) + geom_histogram(aes(x=dist_tea_party))
ggplot(df_list_reg_all_no_out) + geom_histogram(aes(x=dist_museum))


ggplot(df_list_reg_all_no_out) + geom_histogram(aes(x=price))

ggplot(df_list_reg_all_no_out) + geom_point(aes(x=price, y=reviews_per_month))

top_10 = feature_importance[29:70, ] %>% 
  arrange(desc(variable_importance)) %>% 
  top_n(10)

top_10

top_10 = top_10$feature
top_10

top_10_df = df_list_reg_all_no_out %>% 
  select(c("Gym", "Washer", "Free_Parking_on_Premises", "TV", "FamilyKid_Friendly", 
           "Elevator_in_Building", "Air_Conditioning", "Cable_TV", "Pool", "Hair_Dryer"))

summary(top_10_df)

colSums(top_10_df, na.rm = TRUE)

nrow(top_10_df)

top_10_df$FamilyKid_Friendly

a %>%
  rowwise() %>%
  mutate(sum = sum(across(starts_with("Col")), na.rm = T))

ggplot(df_list_reg_all_no_out) + geom_point(aes(x=price, y=cancellation_policy_strict))


# listing characterists feature importance
list_rel_filt = c('bed', 'property_type', 'bath', 'accommodates', 'room')
list_rel = unique(grep(paste(list_rel_filt,collapse="|"), 
                       feature_importance$feature, value=TRUE))

feature_importance %>% 
  filter(feature %in% list_rel) %>% 
  arrange(desc(variable_importance)) %>%
  ggplot(aes(x=reorder(feature, variable_importance), y=variable_importance)) +
  geom_col() +
  coord_flip() +
  ggtitle("Variable Importance - Listing Characterists") +
  xlab('Permutation Feature Importance') + 
  ylab('Feature')

## trying something 
top_25 = feature_importance %>% top_n(25) %>% select(feature)
top_25 = top_25$feature

df_try = df_list_reg_all_no_out %>% select(top_25, 'price')

set.seed(123)
samp_try = createDataPartition(df_try$price, p = 0.8, list = FALSE)
training_try = df_try[samp_try,]
testing_try = df_try[-samp_try,]

dim(training_try)
dim(testing_try)

# rf
rf_try = randomForest(
  price ~ .,
  data = training_try,
  type='regression',
  importance=TRUE
)

# R-squared = ~0.692
rf_try

# RMSE (on train data)
sqrt(rf_try$mse[length(rf_try$mse)])

# RMSE (on test data) = ~50.12
pred_no_out_all = predict(rf_no_out_all, newdata=test_no_out_all)
actual_values_no_out_all = test_no_out_all$price
rmse(pred_no_out_all, actual_values_no_out_all)

# Breakdown of the type of features
classes = sapply(df_list_reg_all_no_out, class)
classes = as.data.frame(classes)
classes = tibble::rownames_to_column(classes, "VALUE")
names(classes) = c('Feature', 'Class')

num_feats = classes %>% filter(Class %in% c('numeric', 'integer'))
num_cols = df_list_reg_all_no_out %>% select(all_of(num_feats$Feature), 'price')
num_cols = num_cols %>% relocate(price, .after=last_col())
ncol(num_cols)

cat_feats = classes %>% filter(Class =='factor')
cat_cols = df_list_reg_all_no_out %>% select(all_of(cat_feats$Feature), 'price')
cat_cols = cat_cols %>% relocate(price, .after=last_col())
ncol(cat_cols)

ggplot(df_list_reg) + geom_point(aes(y=price, x=security_deposit))

## Feature Selection ##
# lots of highly correlated features as I originally took shotgun approach 
# 1. Correlation matrix - which numerical / integer columns are highly correlated?
set.seed(7)
# calculate correlation matrix
correlationMatrix = cor(num_cols[, 1:41])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated = findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# columns not highly correlated:
# host_since, host_response_rate, host_acceptance_rate, host_total_listings_count,
# bathrooms, beds, security_deposit, cleaning_fee, guests_included, extra_people, 
# minimum_nights, maximum_nights, number_of_reviews, 
num_cols[, -highlyCorrelated]

## RFE (recursive feature elimination) ###
# feature selection for ***numerical/integer columns only****
# ensure the results are repeatable
set.seed(7)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(num_cols[, 1:41], num_cols[,42], sizes=c(1:41), metric = 'Accuracy',rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
# converting chosen predictors to DF
relevant_feats = as.data.frame(predictors(results))
names(relevant_feats) = 'Features'
relevant_feats

# Df with all factor features and selected numerical/integer features
df_select_num_all_fac = df_list_reg_all_no_out %>% select(all_of(relevant_feats$Features), all_of(fac_feats$Feature))


# experimenting with only variables with top feature importances
dude = perm_var_imp_1 %>% 
  arrange(-Permutation_Variable_Importance) %>%
  top_n(100, Permutation_Variable_Importance)

dude$Feature

df_list_reg_sub = df_list_reg_all_no_out %>% select(all_of(dude$Feature
), 'price')
names(df_list_reg_sub)

dt_list_reg_sub = sort(sample(nrow(df_list_reg_sub), nrow(df_list_reg_sub)*.8))

training_2 = df_list_reg_sub[dt_list_reg_sub,]
testing_2 = df_list_reg_sub[-dt_list_reg_sub,]

dim(training_2)
dim(testing_2)

rf_no_out_sub = randomForest(
  price ~ .,
  data = training_2,
  type='regression'
)

rf_no_out_sub

rf2 = train(price~., data = training_2, method = "rf", tuneLength = 10)
rf2$results

### Permutation feature importance of RF (no outliers, all features) ###
# random forest - no outliers, all variables
rf_no_out_all_2 = randomForest(
  price ~ .,
  data = train_no_out_all,
  type='regression', 
  importance=TRUE
)

# extracting permutation feature importance from the model importance 
# converting to tibble
perm_var_imp_1 = as.data.frame(rf_no_out_all_2$importance[,1])
perm_var_imp_1 = tibble::rownames_to_column(perm_var_imp_1, "VALUE")
names(perm_var_imp_1) = c('Feature', 'Permutation_Variable_Importance')
perm_var_imp_1

# selecting top 15 features based on perm. feat. imp.
perm_var_imp_1_sub = perm_var_imp_1 %>% 
  arrange(-Permutation_Variable_Importance) %>%
  top_n(15, Permutation_Variable_Importance)
perm_var_imp_1_sub

perm_var_imp_1 %>% 
  arrange(-Permutation_Variable_Importance) %>%
  top_n(20, Permutation_Variable_Importance)

# barplot of the features and their perm. feat. imp. 
perm_var_imp = 
  ggplot(data=perm_var_imp_1_sub, 
         aes(x=reorder(Feature, -Permutation_Variable_Importance), 
             y = Permutation_Variable_Importance, fill=Feature)) + 
  geom_bar(stat = 'identity')
perm_var_imp = perm_var_imp + 
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  theme(legend.position = "none") +
  xlab('Feature') + 
  ylab('Permutation Variable Importance') +
  ggtitle('Random Forest- Permutation Variable Importance')
perm_var_imp
