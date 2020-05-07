# A4 Data Wrangling

# We provide this line to delete all variables in your workspace.
# This will make it easier to test your script.
rm(list = ls())

# Loading and Exploring Data -------------------------------- (**28 points**)

# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package
library(dplyr)

# We provide this code to make sure your locale is in English so that
# files will be read correctly
Sys.setlocale("LC_ALL", "English")

# Load your data, making sure to not interpret strings as factors.
ks_projects_201801 <- read.csv(file
                               = "data/ks-projects-201801.csv",
                               stringsAsFactors = FALSE)

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
colnames(ks_projects_201801)
# - How many rows is the data frame?
nrow(ks_projects_201801)
# - How many columns are in the data frame?
ncol(ks_projects_201801)
# Use the `summary` function to get some summary information
summary(ks_projects_201801)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type
get_col_info <- function(column_name, df) {
  values <- df %>% pull(column_name)
  if (typeof(values) == "double" || typeof(values) == "integer") {
    ks_num <- list()
    min_num <- min(values)
    max_num <- max(values)
    mean_num <- mean(values)
    ks_num[["min_num"]] <- min_num
    ks_num[["max_num"]] <- max_num
    ks_num[["mean_num"]] <- mean_num
    return(ks_num)
  }
  ks_notnum1 <- unlist(df[column_name], use.names = FALSE)
  if (typeof(pull(df, column_name)) != "double" &&
               length(unique(ks_notnum1)) < 10) {
    ks_notnum_list <- list()
    n_values <- length(unique(ks_notnum1))
    unique_values <- unique(ks_notnum1)
    ks_notnum_list[["n_values"]] <- n_values
    ks_notnum_list[["unique_values"]] <- unique_values
    return(ks_notnum1)
  }
  ks_notnum2 <- unlist(df[column_name], use.names = FALSE)
  if (length(unique(ks_notnum2)) >= 10) {
    ks_notnum_list2 <- list()
    n_values <- length(unique(ks_notnum2))
    sample_values <- sample(ks_notnum2, 10)
    ks_notnum_list2[["n_values"]] <- n_values
    ks_notnum_list2[["sample_values"]] <- sample_values
    return(ks_notnum_list2)
  }
}


# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name
name_info <- get_col_info("currency", ks_projects_201801)

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(df) {
  applied <- lapply(colnames(df), get_col_info, df)
  applied
}


# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
name_sum_info <- get_summary_info(ks_projects_201801)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# I thought it was interesting how I can take any columns in this 
# dataframe and be able to construct functions 
# depending on different values of the column
# What category had the most successful?
# Which category had most backing and if so, how much pledged?


# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!

# What was the name of the project(s) with the highest goal?
highest_goal_projects <- ks_projects_201801 %>%
  filter(goal == max(goal)) %>%
  pull("name")


# What was the category of the project(s) with the lowest goal?
lowest_goal_cat <- ks_projects_201801 %>%
  filter(goal == min(goal)) %>%
  pull("category")

# How many projects had a deadline in 2018?
# Hint: start by googling "r get year from date" and then look up more about
# different functions you find
deadline_2018 <- ks_projects_201801 %>%
  select(deadline) %>%
  filter(substr(deadline, 1, 4) == "2018") %>%
  nrow()
# is this the right approach?

# What proportion or projects weren't successful? Your result can be a decimal
unsuccessful <- ks_projects_201801 %>%
  group_by(state) %>%
  filter(state != "successful") %>%
  nrow() / nrow(ks_projects_201801)

# What was the amount pledged for the project with the most backers?
mostbackers <- ks_projects_201801 %>%
  filter(backers == max(backers)) %>%
  pull("usd_pledged_real")


# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
mostfailed <- ks_projects_201801 %>%
  filter(state == "failed") %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  pull("name")
# check this
# is this including failed and cancelled?

# How much total money was pledged to projects that weren't successful?
unsuccess_amount <- ks_projects_201801 %>%
  filter(state != "successful") %>%
  select(pledged) %>%
  sum()

# Performing analysis by *grouped* observations ----------------- (38 Points)

# Which category had the most money pledged (total)?
most_pledged <- ks_projects_201801 %>%
  group_by(category) %>%
  summarize(total = sum(pledged)) %>%
  filter(total == max(total)) %>%
  pull(category)

# Which country had the most backers?
most_backers_country <- ks_projects_201801 %>%
  group_by(country) %>%
  summarize(total_backers = sum(backers)) %>%
  filter(total_backers == max(total_backers)) %>%
  pull(country)


# Which year had the most money pledged (hint: you may have to create a new
# column)?
deadline_year <- ks_projects_201801 %>%
  select(launched, pledged) %>%
  mutate(year_only = substr(launched, 1, 4)) %>%
  group_by(year_only) %>%
  summarize(mostpledged_year = sum(pledged, na.rm = TRUE)) %>%
  filter(mostpledged_year == max(mostpledged_year)) %>%
  pull(year_only)

# What were the top 3 main categories in 2018 (as ranked by number of backers)?
top_3_categories <- ks_projects_201801 %>%
  group_by(main_category) %>%
  mutate(year_only = substr(deadline, 1, 4)) %>%
  filter(year_only == "2018") %>%
  summarise(all_backers = sum(backers)) %>%
  arrange(-all_backers) %>%
  slice(1:3) %>%
  pull(main_category)


# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
most_common_day <- ks_projects_201801 %>%
  mutate(day = weekdays(as.Date(launched))) %>%
  group_by(day) %>%
  summarize(day_count = n()) %>%
  filter(day_count == max(day_count)) %>%
  pull(day)



# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were successful)? This might require some creative problem solving....
# Hint: Try googling "r summarize with condition in dplyr"
least_successful_n <- ks_projects_201801 %>%
  mutate(day = weekdays(as.Date(launched))) %>%
  group_by(day) %>%
  summarise(num_of_days = n(),
            success_rate = sum(state == "successful") / num_of_days) %>%
  filter(success_rate == min(success_rate)) %>%
  pull(day)

library(lintr)
lint("a4.R")