# Assignment 4
# Michelle Starkell

# 1. Read the data into a data frame (make sure that column names do not have spaces in them).

# Read data int a data frame

library(readr)
ufo <- read_csv("ufo_subset.csv")
View(ufo)
# Replace spaces in column names with underscores
colnames(ufo) <- gsub(" ", "_", colnames(ufo))


# 2. Find the rows where Shape information is missing and impute with "unknown".

# View rows where shape information is missing by subsetting
missingshaperows <- subset(ufo, is.na(shape))
# Impute missing shape information with "unknown"
ufo$shape <- replace(ufo$shape, is.na(ufo$shape), "unknown")
# Double check there are no na values leftover
sum(is.na(ufo$shape))


# 3. Remove the rows that do not have Country information.

library(dplyr) #load dplyr package so that the filter function can be used
ufo2 <- ufo %>% filter(!is.na(country))
# Keep rows that meet a certain condition, where there are no NA values under country
# Create new data set "ufo2" so that changes can be tracked


# 4. Convert Datetime and Date_posted columns into appropriate formats

class(ufo2$datetime) # check class of 'datetime'

# 4.1 Separate 'datetime' column into two columns
library(tidyr)
ufo3 <- ufo2 %>% separate(datetime, c("date", "time"), sep = " ", remove = TRUE)

# 4.2 Convert date into appropriate format
# check class of date values
class(ufo3$date)
# load lubridate package to convert date values into date data type
library(lubridate)
# convert to date data type
ufo3$date <- lubridate::ymd(ufo3$date)

# 4.3 Convert time into approriate format
# check class of time values
class(ufo3$time)
# convert time to time data type
ufo3$time <- parse_time(ufo3$time, format = "%H:%M")

# 4.4 Convert date_posted into appropriate format
# check class of date_posted values
class(ufo3$date_posted)
# convert to date data type
ufo3$date_posted <- lubridate::dmy(ufo3$date_posted)


# 5. Create a new boolean column delineating whether a sighting is a possible hoax

ufo4 <- ufo3 %>% mutate(is_hoax = case_when(grepl("(?i)HOAX", comments) ~ "TRUE", .default = "FALSE"))
# Use mutate function to add new column with value "TRUE" when "HOAX" is included in the comments and value "FALSE" when it isn't
# Included (?i) argument to allow all 'hoax' to be identified regardless of case

# 6. Create a table reporting the percentage of hoax sightings per country.

# Group ufo4 data by country, then calculate percentage of TRUE values in is_hoax column over total number of rows per country
# Multiply result by 100 to obtain percentage value
percentages <- ufo4 %>% group_by(country) %>% summarize(percentage = sum(is_hoax == "TRUE") / n() * 100)
# Print table
print(percentages)


# 7. Add another column delineating the time difference between the sighting date and reported date

# Calculate time difference in days between date_posted and date
timedif <- ufo4$date_posted - ufo4$date
# Add another column to report the time difference
ufo5 <- ufo4 %>% mutate(report_delay = timedif)


# 8. Remove the rows where the sighting was reported before it happened.

# Use the filter function to remove rows where the time difference is negative
ufo6 <- ufo5 %>% filter(!timedif < 0)


# 9. Create a table reporting the average report_delay per country.

delaypercountry <- ufo6 %>% group_by(country) %>% summarize(average_delay = mean(report_delay))
print(delaypercountry)
# Groups data by country and calculates average report_delay per country, summarizing the data within a table


# 10. Check the data quality (missingness, format, range etc) of the "duration seconds" column. 

# Explain what kinds of problems you have identified and how you chose to deal with them, in your comments.
class(ufo6$duration_seconds) # Class is numeric - this is good
sum(ufo6$duration_seconds < 0) # Sum of values less than zero means there are no negative values
sum(is.na(ufo6$duration_seconds)) # No NA values - this is good
range(ufo6$duration_seconds) # Very large range of seconds

# The major problem I identified is that the range of values is extremely large, from 2.00e-02 to 8.28e+07
# To account for this, the histogram will use the log of duration_seconds so that the shape of the distribution can be visualized
# If we do not do this, the histogram will only appear to have one bar due to the low number of observations in groups with extreme values


# 11. Create a histogram using the "duration seconds" column.   

hist(log(ufo6$duration_seconds), main = "Histogram of Sighting Durations in Seconds", xlab = "Log of sighting duration (seconds)", ylab = "Frequency", col = "blue")

