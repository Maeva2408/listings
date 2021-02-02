#########################################################################################
# Cleaning for Maeva's Data Analysis
# Assignment 1 DA3 : Predicting Airbnb Apartment Prices : Sydney
#########################################################################################

# Library
library(tidyverse)
library(data.table)
library(stringr)  
library(fastDummies)
library(ggpubr)
library(scales)
library(stargazer)
library(Hmisc)
library(skimr)


#------------------------------------------------------------------------------

# Import data
#setting working directory
rm(list=ls())

# Directory

setwd("C:/Users/mbrae/OneDrive/Bureau/CEU/DA3/A1/")
dir<-"Data"

#location folders
data_in  <- paste0(dir,"/Raw/")
data_out <- paste0(dir,"/Clean/")

data<-read.csv(paste0(data_in,"listings.csv"))

#########################
## FEATURE ENGINEERING ##
#########################

# ---------------------------- DATA CLEANING ----------------------------------#
 I 

# Drop unnecessary columns
data <- data[grep("^host", colnames(data), invert = TRUE)]
data <- data[grep("^calculated", colnames(data), invert = TRUE)]
data <- data %>% select(-contains("maximum"))
data <- data %>% select(-c("listing_url", "calendar_updated",
                           "picture_url","last_scraped","description", "neighborhood_overview", 
                           "scrape_id","neighbourhood_group_cleansed","bathrooms","number_of_reviews_l30d","first_review","license","last_review","review_scores_checkin"))

#remove dollar signs from price variables
data$price <-gsub("\\$","",as.character(data$price))
data$price <-gsub("\\,","",as.character(data$price))
data$price <- as.numeric(data$price)

#format binary variables
for (binary in c("instant_bookable", "has_availability")){
  data[[binary]][data[[binary]]=="f"] <- 0
  data[[binary]][data[[binary]]=="t"] <- 1
}

# Format amenities column. Remove square [] and convert to vector
#amenities
data$amenities<-gsub("\\[","",data$amenities)
data$amenities<-gsub("\\]","",data$amenities)
data$amenities<-gsub('\\"',"",data$amenities)
data$amenities <- as.list(strsplit(data$amenities, ","))


#define levels and dummies 
levs <- levels(factor(unlist(data$amenities)))
data <- cbind(data, as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), 
                                                       table))))
data <- select(data,-c('amenities'))


# ------------------------------------------------------------------------------


## Aggregate amenities with a loop in order to decrease the number of variables
# Function made by Fasih Atif
# Keywords to find same amenities and categorize it.
column_names <- c("sound", "wifi|ethernet|internet","HDTV|TV",
                  "Paid.*Parking|Paid.*Garage","Free.*Parking|Free.*Garage", "refrigerator", "baby|children|crib","Smart.*Lock|Smoke.*Alarm|Safe|Lockbox", "balcony","stove","oven","fridge", "body|gel", "BBQ", "air", "dryer", "baby", "free.*parking", "paid.*parking", "heating","washer","shampoo|conditioner")

for( word in column_names){
  
  # Subset columns which contains a specific word and save them to another dataframe. Also select 'id' to use for merge later
  new_df <- data %>% select(matches(word),"id")
  
  #Go row by row to see if any of the rows have at least one '1'. If it does, populate new column 'col_name' with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # Save new column and id column to another dataframe. We use this new dataframe to merge with original dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  #merge original dataframe and new_df_merge by 'id'
  data <- merge(data,new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and 'id' column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the subset columns from original dataframe since they have already been aggregated into a new column and merged
  data <- data %>% select(-colnames(new_df))
  
  # Convert from character to integer
  data$col_name <- as.integer(data$col_name)
  
  # Rename new column
  names(data)[names(data) == 'col_name'] <- paste0(word,"_agg")} 

#-------------------------------------------------------------------------------

# Removing the amenities that represents less that 1% of the data

amenities_clean <- data %>% select(32:268, 0:1)
Dummies_less_than_1 <- function(DummyVardf, MaxHowManyTrue = 338) {
  
  return(lapply(1:length(DummyVardf), function(x) {
    tl <- list()
    tl[['Colname']] <- colnames(DummyVardf[c(x)])
    tl[['False']] <- table(DummyVardf[c(x)])[1]
    tl[['True']] <- table(DummyVardf[c(x)])[2]
    return(tl)
  }) %>% rbindlist() %>% filter(True < MaxHowManyTrue) %>% 
    arrange(True))
}
less_than_1 <- Dummies_less_than_1(amenities_clean)
amenities_clean[less_than_1$Colname] <- NULL

# Merge the original and amenities_clean dataframe
data <- data %>% select(-(32:268))
data <- cbind(data,amenities_clean)

#-------------------------------------------------------------------------------

#theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

options(digits = 3)

# keep if property type is Apartment or Condominium
table(data$property_type)
data <- data %>%
  filter(property_type %in% c("Entire apartment", "Private room in condominium" ,"Private room in apartment",
                              "Entire condominium",  "Shared room in condominium","Shared room in apartment" ))

# Rename Property type simple because room type has already the information
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire apartment", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in condominium", "Condominium", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in apartment", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire condominium", "Condominium", data$property_type),
    f_property_type = factor(property_type))

data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Shared room in condominium", "Condominium", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Shared room in apartment", "Apartment", data$property_type),
    f_property_type = factor(property_type))


# Rename room type
data$room_type <- replace(data$room_type,data$room_type == 'Entire home/apt', "Entire_apt")
data$room_type <- replace(data$room_type,data$room_type == 'Hotel room', "Private room")
data$room_type <- replace(data$room_type,data$room_type == 'Private room', "Private_room")
data$room_type <- replace(data$room_type,data$room_type == 'Shared room', "Shared_room")

data <- data %>%
  mutate(f_room_type = factor(room_type))

# Convert neighbourhood_cleansed to factors
data <- data %>%
  mutate(
    f_neighbourhood = factor(data$neighbourhood_cleansed))

# Convert proprety_type to factors
data <- data %>%
  mutate(f_property_type = factor(property_type))

#Room type as factor
table(data$room_type)
data <- data %>%
  mutate(f_room_type = factor(room_type))
 
#-------------------------------------------------------------------
# Type of column
str(data)
# Convert bathrooms into Numerical
data$bathrooms <- gsub("[^0-9.]", "", data$bathrooms_text) %>% factor()
data$bathrooms <- as.numeric(data$bathrooms)

# No need to convert to numerical other variable

#-------------------------------------------------------------------
# Missing values
# with price info only
data <- data %>%
  drop_na(price)

dir<-"Data"

#location folders
data_in  <- paste0(dir,"/Raw/")
data_out <- paste0(dir,"/Clean/")

##################################
# DESCRIBE

#--------------------------------
data$f_neighbourhood_cleansed
data <- data %>%

  
# I choose to analyse only one neighbourhood : Sydney, I will call it SydneyB 
# to avoid confusion
    filter(neighbourhood_cleansed == "Sydney")
write_csv(data, paste0(data_out, "airbnb_SydneyB_neighbour_workfile.csv"))


# Size, we need a normal apartment, 1-7persons
data <- data %>%
  filter(accommodates < 7
  )

N=nrow(data)
N
# N=5479
output <- paste0(dir,"output/")
create_output_if_doesnt_exist(output)

#####################
### look at price ###
#####################
summary(data$price)
describe(data$price)

data <- data %>%
  mutate(ln_price = log(price))

# Remove extreme values + missing from prices
data <- data %>%
  filter(price <1000)

# Histograms
R_F14_h_lnprice <- ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Log price") +
  theme_bg()

R_F14_h_lnprice
ggsave(paste0(output, "R_F14_h_lnprice.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "R_F14_h_lnprice.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
print(R_F14_h_lnprice)
dev.off()

R_F14_h_price <- ggplot(data, aes(price)) +
  geom_histogram(binwidth = 25, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("count") +
  xlab("Price") +
  theme_bg()
R_F14_h_price
ggsave(paste0(output, "R_F14_h_price.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "R_F14_h_price.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 12,
         fallback_resolution = 1200)
print(R_F14_h_price)
dev.off()

#-------------------------------------------------------------------

# Missing values in SydneyB

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# what to do with missing values?
# 1. drop if no target (already did)
data <- data %>%
  drop_na(price)


# 2. imput when few for beds and bedrooms
data <- data %>%
  mutate(
    beds = ifelse(is.na(beds),accommodates, beds), #assume n_beds=n_accomodates 
    bedrooms = ifelse(is.na(bedrooms), accommodates %% 2, bedrooms)) #assume beds=accomodates/2 

# 3. drop columns when many missing not imortant
to_drop <- c("review_scores_accuracy", "review_scores_cleanliness", 
             "review_scores_communication", "review_scores_location", "review_scores_value")
data <- data %>%
  select(-one_of(to_drop))

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# 4. Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    flag_review_scores_rating=ifelse(is.na(review_scores_rating),1, 0),
    review_scores_rating =  ifelse(is.na(review_scores_rating), median(review_scores_rating, na.rm = T), review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(reviews_per_month),1, 0),
    reviews_per_month =  ifelse(is.na(reviews_per_month), median(reviews_per_month, na.rm = T), reviews_per_month)
  )

# Look at data
summary(data$price)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# For beds, sometimes 0 was input
# problem if I want to take ln

data <- data %>%
  mutate(beds = ifelse(beds == 0, round(accommodates / 1.5), beds)) #assume beds= accommodates/1.5 for places with "zero" beds


################################################
# look at some cnts. key vars, functional form #
################################################

## accommodates: look at distribution

data %>%
  group_by(accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

## price increase a lot between 3 and 4 people

R_14_s_accommodates <- ggplot(data = data, aes(x=accommodates, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,1200)+
  xlim(0,7)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()

R_14_s_accommodates
ggsave(paste0(output, "R_14_s_accommodates.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "R_14_s_accommodates.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 12,
         fallback_resolution = 1200)
print(R_14_s_accommodates)
dev.off()

# Squares and further values to create
data <- data %>%
  mutate(accommodates2=accommodates^2, ln_accommodates=log(accommodates) ,
         ln_accommodates2=log(accommodates)^2)

# Regression 1: ln price and num of accomodates and squares
lm(ln_price ~ accommodates + accommodates2, data=data)
# Regression 2: ln price and log num of accomodates
lm(ln_price ~ ln_accommodates , data=data)
# Regression 3: ln price and num of accomodates
lm(ln_price ~ accommodates, data=data)

## hard to interpret, I will not used ln_price

## Beds
data %>%
  group_by(beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())
# maybe best is to have log beds
data <- data %>%
  mutate(ln_beds = log(beds))

# hard to interpret as well, will not used

## Number of reviews
nreview_plot <- data %>%
  filter(number_of_reviews <100)

ggplot(nreview_plot, aes(number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bg()

# number of reviews: use logs as well
data <- data %>%
  mutate(ln_number_of_reviews = log(number_of_reviews+1))

ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log N of reviews") +
  theme_bg()

# better distribution but for consistency, will go with level

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(number_of_reviews, c(0,1,51,max(data$number_of_reviews)), labels=c(0,1,2), right = F))
data %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())

#Regression 1: log-price and number of reviews

reg4<-lm(ln_price ~ f_number_of_reviews, data=data)
summary(reg4)

# not taking log for consistency, will go with level

# Regression 2: log-price and log number of reviews
reg5<-lm(ln_price ~ ln_number_of_reviews, data=data)
summary(reg5)

skimr::skim(data$number_of_reviews)
ggplot(data = data, aes(x=number_of_reviews , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(11,999)+
  xlim(0,514)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_bg()

# not taking log for consistency, will go with level

# Create log of review scores
data <- data %>%
  mutate(ln_review_scores_rating = log(review_scores_rating))
# Regression 1) ln price - num of review scores
lm(ln_price ~ review_scores_rating,data=data)
# Regression 2) ln price - log num of review scores
lm(ln_price ~ ln_review_scores_rating,data=data)

#for consistency, will go with level

## minimum nights
lm(ln_price ~ minimum_nights,data=data)

# Pool and categorize the number of minimum nights: 1,2,3, 3+

data <- data %>%
  mutate(f_minimum_nights= cut(minimum_nights, c(1,2,3,max(data$minimum_nights)), labels=c(1,2,3), right = F))

lm(ln_price ~ f_minimum_nights,data=data)


### after looking at all the plot, I choose to use Price and variables with no log transformation.
### It will be easier for interpretation, I am not looking for difference but to actual prices.

###########################
## look at categoricals  ##
###########################

categoricals <- c("f_property_type", "f_room_type", "f_minimum_nights", "f_number_of_reviews")

for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}
#####################################

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

## Inputing because just one
data <- data %>%
  mutate(f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1,f_number_of_reviews),
    f_minimum_nights =  ifelse(is.na( f_minimum_nights), 1,f_minimum_nights))

write_csv(data, paste0(data_out, "airbnb_sydneyB_neighbour_workfile_adj.csv"))

#------------------------------------------------------------------------------------------------

