# Explore the data

# Explore the `reviews` data eset with `summary()` 
summary(reviews)

# Explore the `users` data set with `summary()` 
summary(users)

# Explore the `businesses` data set with `summary()` 
summary(businesses)

# Combine the data into one

# Make dplry package avaiable to use
library(dplyr)

# Combine the reviews and users data sets
ru  <- inner_join(reviews, users)

# Combine the newly created data set with the businesses data set
rub <- inner_join(ru, businesses)

# Take a look at the combined data frame
summary(rub)

# Isolate Indian Restaurants 

# Create indian review column
rub$is_indian <- grepl("Indian", rub$categories) == TRUE

# Select only reviews for Indian restaurants
indian <- subset(rub, is_indian == TRUE)

# Find number of reviews per user

# The package dplyr is available to use
# Generate a new data frame with the number of reviews by each reviewer
number_reviews_indian <- indian %>% 
  select(user_id, user_name) %>%
  group_by(user_id) %>% 
  summarise(total_reviews = n())

# Print the table of total_reviews
table(number_reviews_indian$total_reviews)

# Pring the average number of reviews per users
mean(number_reviews_indian$total_reviews)

print(number_reviews_indian)
print(total_reviews)

# Add review count to dataset 

# The package dplyr is available to use
# Combine number of Indian reviews with original data frame of Indian restaurant reviews
indian_plus_number <- inner_join(indian,number_reviews_indian)

# Display column names for the new data frame
indian_plus_number
names(indian_plus_number)

# Generate rated star reviews

# Generate weighted_stars variable 
indian_plus_number$weighted_stars <- indian_plus_number$stars * indian_plus_number$total_reviews

# Create a new weighted review for each restaurant (Note: package dplyr is available to use)
new_review_indian <- indian_plus_number %>% 
  select(city, business_name, avg_stars, stars, total_reviews, weighted_stars) %>%
  group_by(city, business_name, avg_stars) %>%
  summarise(count = n(),
            avg = sum(stars) / count,
            new = sum(weighted_stars) / sum(total_reviews),
            diff = new - avg)

# Detect modification defects

# Load the ggplot2 package into the environment
library(ggplot2)

# Plot the distribution of changes to reviews 
hist(new_review_indian$diff, main = "Changes in Star Reviews", xlab = "Change")

# Plot the changes in review per restaurant 
ggplot(new_review_indian, aes(x=1:nrow(new_review_indian), y=diff, fill=city)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_classic() + scale_fill_grey() + xlab("Businesses ID") + ylab("Change in Star Review")

# Display a summary of the 
summary(new_review_indian)

# Exploring the data again
# This url contains the .txt file with Indian names. (Note: Don't change this code)
indian_names_url <-url("http://s3.amazonaws.com/assets.datacamp.com/production/course_1073/datasets/indian_names.txt")

# Read Indian names into a list
indian_names <- scan(indian_names_url, what = character())

# Show the first 10 names from the indian_names list
head(indian_names, 10)

# Clean the names list

# Show the names list
unlist(indian_names)

# Locate the names that you want to eliminate
indian_names_remove <- grep("[A-z]\\.", indian_names, perl = TRUE)

# Check to make sure they are the correct names
indian_names[indian_names_remove]

# Eliminate them from the indian_names list
indian_names_clean <- indian_names[-indian_names_remove]

# Display a table of the cleaned names list
unlist(indian_names_clean)

Find the authentic users

# The package `dplyr` is available to use
# Subset the `indian` data set to just the users with native Indian names
authentic_users <- subset(indian, indian$user_name %in% indian_names_clean)

# Find the number of users in each city
number_authentic_city <- authentic_users %>%
  select(city,user_name) %>%
  group_by(city) %>%
  summarise(users = n())

# Generate the average of authentic reviews

# Print the number of users per city
number_authentic_city

# The package `dplyr` is available to use
# Generate new "immigrant" review
avg_review_indian <- authentic_users %>% 
  select(business_id, business_name, city, stars, 
         avg_stars, is_indian, user_name) %>%
  group_by(city, business_name, avg_stars) %>%
  summarise(count = n(),
            new_stars = sum(stars) / count) %>%
  mutate(diff = new_stars - avg_stars)

# Detecting manipulation effect

# The plotting package `ggplot2` is avaibale to use
# Create a histogram of the avg_stars
hist(avg_review_indian$avg_stars)

# Create a histogram of the new_stars
hist(avg_review_indian$new_stars)

# Plot the distribution of changes to reviews 
hist(avg_review_indian$diff, main = "Changes in Star Reviews", xlab = "Change")

# Plot the changes to per restaurant 
ggplot(avg_review_indian, aes(x=1:nrow(avg_review_indian), y = diff, fill = city)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_classic() + scale_fill_grey() + xlab("Businesses ID") + ylab("Change in Star Review”)
                                                                     
                                                                     
                                                                     

