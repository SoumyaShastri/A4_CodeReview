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


#*Comments by Soumya Shastri: 

#*Line 8: Nice choice in package; "readr" provides a fast and friendly way to read 
#*csv files in R.  
#*Line 9-10: Great! I would recommend letting the user know to have the file in 
#*their working directory or adding the csv file to your submission. Regardless, 
#*I was able to open the file. 
#*Line 12: Prompt works, and upon reading the dataframe, the space has been effectively 
#*replaced with an underscore. Another way is by using the "make.names" function 
#*which replaces any spaces with "."
#*Line 17-22: Excellent! This works as it should. Double checking that all the missing 
#*shape values have been actually replaced with "unknown" was also a great add. 
#*Line 25-30: This works as it should, there are no N/A values in the country column. One
#*way to make the code a little more efficient and concise would be to make use of the pipe
#*operator more frequently to sequence the operations within one single variable instead of 
#*creating a ufo2 variable. 
#*Line 33-59: both date columns have been formatted and they both respect the same formatting. 
#*I like how you chose to separate the time out of the column entirely as it makes it cleaner for 
#*further analysis. It is also good practice on checking the class of variables before you manipulate
#*the data in any way, great work on that! Another quick way of getting the same results is 
#*by using the "as.date" function which requires no additional packages. 
#*Line 64-66: Great use of regex to capture all the possible values for "Hoax", that is something 
#*I didn't think about. 
#*Line 70-74: Great work again, the table shows all the data outlined in the requirements 
#*Line 79-82: I like how your "report_delay" column specifies that the difference is in days. 
#*Line 85-88: This works as it should. The only comment I would have is that at this point, there are a 
#*lot of data in the Global Environment, which could be avoided if the pipe operator was used a 
#*more throughout the code, condensing it to maybe 1 or 2 data frames. 
#*Line 91-94: Table generated fulfills assignment requirements. Again, it's good that you have units 
#*of measurement within the table as without a unit, the quantity is meaningless to end user.
#*Line 98-104: Great analysis, you looked for missing and negative values. You also correctly pointed out 
#*the large range in duration seconds. And although that is well accounted for in the histogram, a point to 
#*consider is if it even makes sense to keep such large values in your analysis. Some of those times 
#*span years, can a UFO sighting be seen for years on end? 

#*Overall, excellent work! All prompts were answered and they worked as they should. I would recommend 
#*making use of the pipe function more which would avoid repetitive lines 
#*in your code such as creating new variables for sightly changed data and make it overall more concise. 
#*Otherwise, it was a pleasure reviewing your code. Well done!
