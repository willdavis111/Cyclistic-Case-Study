library(tidyverse)
library(ggplot2)
library(dplyr)

#load all 12 months
jan <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202201-divvy-tripdata.csv")
feb <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202202-divvy-tripdata.csv")
mar <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202203-divvy-tripdata.csv")
apr <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202204-divvy-tripdata.csv")
may <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202205-divvy-tripdata.csv")
jun <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202206-divvy-tripdata.csv")
jul <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202207-divvy-tripdata.csv") 
aug <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202208-divvy-tripdata.csv")
sep <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202209-divvy-publictripdata.csv")
oct <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202210-divvy-tripdata.csv")
nov <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202211-divvy-tripdata.csv")
dec <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\google DA\\CS_1\\12Month\\202212-divvy-tripdata.csv")
# create a data frame from all 12
full_df <- rbind(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
#remove duplicates
full_df <- unique(full_df)
#df of the columns I want to analyze
reduced_df <- data.frame(full_df$rideable_type, full_df$started_at, full_df$ended_at, full_df$member_casual)
#dropping null values from my df starts 5667717
reduced_df <- drop_na(reduced_df)
#Adding ride time column
colnames(reduced_df) <- c('type', 'start', 'end', 'status')
work_df <- reduced_df %>%
  mutate(ride_time = case_when(as_datetime(end) > as_datetime(start) ~ (as_datetime(end) - as_datetime(start)),
  as_datetime(end) < as_datetime(start) ~ (as_datetime(start) - as_datetime(end))))
#get some stats on member types
member_df <- subset(work_df, status == 'member')
casual_df <- subset(work_df, status == 'casual')
#average ride time for each 
avg_mem_ride <- mean(member_df$ride_time, na.rm=TRUE)/60
print(avg_mem_ride)
avg_cas_ride <- mean(casual_df$ride_time, na.rm=TRUE)/60
print(avg_cas_ride)
#most common vehicle type
#mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
v <- member_df$type
mode_mem <- getmode(v)
print(mode_mem)

v <- casual_df$type
mode_cas <- getmode(v)
print(mode_cas)

# count by memebr status and month
df1 <- work_df %>% 
  count(format(as.POSIXct(start),"%m"), status)
df1
colnames(df1) <- c('month', 'status', 'count')
colnames(df1)

#visualization of most popular month 
ggplot(df1, aes(x=month, y= count, fill=status)) + 
  geom_bar(stat = "identity")

