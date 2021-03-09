# Modelling Correlation of Mobility
# with rates of Coronavirus

# libraries
library(tidyverse)
library(dplyr)
library(reshape2)
library(lubridate)
library(covid19.analytics)

# initial data download
# google mobility data (422.4 MB, Sys.Date = "2021-03-09")
if (!dir.exists("data")) {
    # create data directory 
    dir.create("data")
}
# url of the csv download 
url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
# download file into data directory
download.file(url, destfile = "data/google_global_mobility_report.csv")


# load the data ...
# covid data from John Hopkins 
covid <- covid19.data(case = "ts-confirmed")
# Mobility data from Google
mobility <- read.csv("data/Global_Mobility_Report.csv")

# Subset mobility data
mobility <- mobility %>% 
    # sub region "" why?
    filter(sub_region_1 == "") %>% 
    mutate(date = ymd(date)) %>% 
    # dates before end of first wave
    filter(date <= "2020-07-01") %>% 
    # select columns
    select(-country_region_code, -sub_region_1, -sub_region_2, -metro_area, -iso_3166_2_code,
           -census_fips_code, -place_id)

# subset the covid data
covid <- covid %>% 
    pivot_longer(col = -c("Province.State", "Country.Region", "Lat", "Long"),
                                names_to = "date", values_to = "cumulative_cases") %>%
    # Province.State == "" means just British mainland
    filter(Province.State == "")
    # dates before the first wave
    mutate(date = ymd(date)) %>%
    filter(date <= "2020-07-01") %>%
    # after first case
    filter(cumulative_cases > 0)
        
# more on data manipulation
covid <- covid %>% mutate(week = week(date)) %>% filter(week > 8)
mobility <- mobility %>% mutate(week = week(date)) %>% filter(week > 8)
mobility_uk <- mobility %>% filter(country_region == "United Kingdom")
covid_uk <- covid %>% filter(Country.Region == "United Kingdom")
covid_uk <- covid_uk %>% filter(Province.State == "")
covid_uk <- covid_uk %>% select(date, cumulative_cases, week)

    
# day by day rate of change function
roc_day <- function(x) {
    y <- vector()
    y[1] <- 1
    for (i in 1:(length(x) - 1)) {
        y[i + 1] <- (x[i + 1] / x[i]) - 1
    }
    y
}
# number of new cases per day
new_cases_day <- function(x) {
    y <- vector()
    y[1] <- 1
    for (i in 1:(length(x) - 1)) {
        y[i + 1] <- x[i + 1] - x[i]
    }
    y
}

# create columns for new cases per day and rate of change of new cases 
# per day
covid_uk <- covid_uk %>% 
    mutate(new_cases_day = new_cases_day(cumulative_cases),
           daily_rate_of_change = roc_day(new_cases_day))


# Exploratory Analysis
# Line plot of new cases per day
ggplot(covid_uk, aes(date, new_cases_day)) + geom_line()
# histogram of new cases per day
ggplot(covid_uk, aes(new_cases_day)) + geom_histogram(binwidth = 1000)

# create covid uk week 
covid_uk_week <- covid_uk %>% 
    group_by(week) %>% 
    # How many new cases per week
    summarise(new_cases_week = sum(new_cases_day)) %>%
    # Rate of change of cases per week 
    mutate(weekly_rate_of_change = roc_day(new_cases_week))
    
# histogram of new cases per week
# shows the same weird pattern as the daily rates
ggplot(covid_uk_week, aes(new_cases_week)) + 
    geom_histogram(binwidth = 5000)
# line plot of cases per week 
ggplot(covid_uk_week, aes(week, weekly_rate_of_change)) + 
    geom_line()

# change the shit names
# keep in mind each column is percent change from baseline
percent_names <- str_detect(names(mobility_uk), "_percent")
names(mobility_uk)[percent_names] <- str_extract(names(mobility_uk)[percent_names], 
                                                 regex("(.*)(?=_percent)"))
mobility_uk <- mobility_uk %>% 
    select(-country_region)











