#Get the current working directory
getwd()
#Set the working directory
setwd("C:/Users/Dilshani/Documents/DA/CW/Supplement files")

library(lubridate)
library(tidyverse)

CovidDataSet <- read.csv("owid-covid-data.csv",  stringsAsFactors = FALSE)
head(CovidDataSet,5)
sum(is.na(CovidDataSet))
names(CovidDataSet)  = c("iso_code","continent","location","date","total_cases","new_cases","new_cases_smoothed","total_deaths","new_deaths","new_deaths_smoothed","total_cases_per_million","new_cases_per_million","new_cases_smoothed_per_million","total_deaths_per_million","new_deaths_per_million","new_deaths_smoothed_per_million","reproduction_rate","icu_patients","icu_patients_per_million","hosp_patients","hosp_patients_per_million","weekly_icu_admissions","weekly_icu_admissions_per_million","weekly_hosp_admissions","weekly_hosp_admissions_per_million","new_tests","total_tests","total_tests_per_thousand","new_tests_per_thousand","new_tests_smoothed","new_tests_smoothed_per_thousand","positive_rate","tests_per_case","tests_units","total_vaccinations","people_vaccinated","people_fully_vaccinated","new_vaccinations","new_vaccinations_smoothed","total_vaccinations_per_hundred","people_vaccinated_per_hundred","people_fully_vaccinated_per_hundred","new_vaccinations_smoothed_per_million","stringency_index","population","population_density","median_age","aged_65_older","aged_70_older","gdp_per_capita","extreme_poverty","cardiovasc_death_rate","diabetes_prevalence","female_smokers","male_smokers","handwashing_facilities","hospital_beds_per_thousand","life_expectancy",'human_development_index')
colSums(is.na(CovidDataSet))
CovidDataSet <- CovidDataSet[complete.cases(CovidDataSet[,5:17]), ]
summary(CovidDataSet)
str(CovidDataSet)

install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr) 

columns <-c("total_cases", "new_cases", "total_deaths", "new_deaths", "total_cases_per_million", "total_deaths_per_million", "new_deaths_per_million", "reproduction_rate")
CovidDataSet[, columns] <- lapply(columns, function(x) as.numeric(CovidDataSet[[x]]))

CovidDataSet <- CovidDataSet %>% group_by(location)

#Get a summary for each location
CovidDataSet2 <- CovidDataSet %>% summarise(
  total_cases = max(total_cases),
  new_cases = mean(new_cases),
  total_deaths = max(total_deaths),
  new_deaths = mean(new_deaths),
  total_cases_per_million = mean(total_cases_per_million),
  new_cases_per_million = mean(new_cases_per_million),
  total_deaths_per_million = mean(total_deaths_per_million),
  new_deaths_per_million = mean(new_deaths_per_million),
  reproduction_rate= max(reproduction_rate)
)


CovidDataSet3 <- CovidDataSet2[order(CovidDataSet2$total_cases, CovidDataSet2$total_deaths, CovidDataSet2$total_cases_per_million, CovidDataSet2$total_deaths_per_million),]

#Drawing a single plot
summary((CovidDataSet))
data =CovidDataSet[!(CovidDataSet$total_cases==""), ]

data$date <-  as.Date(data$date, format="%Y-%m-%d")
data$test_percentage <- data$total_cases/ data$population

Ghana <- data[data$location == "Ghana",]
Montenegro <- data[data$location == "Montenegro",]
Kosovo <- data[data$location == "Kosovo",]
China <- data[data$location == "China",]
sl <- data[data$location == "Sri Lanka",]

install.packages('ggplot2')
library(ggplot2)

Slplot <- ggplot(sl, 
                 aes(date, as.numeric(test_percentage))) +
  geom_col(fill = "grey0", alpha = 0.3) + 
  theme_minimal(base_size = 14) +
  xlab("Date Range") + ylab("Growth of total cases propotinately to the population") + 
  scale_x_date(date_labels = "%Y/%m/%d")
Sl_Gh  <- Slplot + geom_col(data=Ghana, 
                               aes(date, as.numeric(test_percentage)),
                               fill="red2",
                               alpha = 0.1)
Sl_Gh_Mon <- Sl_Gh + geom_col(data=Montenegro, 
                                aes(date, as.numeric(test_percentage)),
                                fill="yellow",
                                alpha = 0.2)
Sl_Gh_Mon_Kos <- Sl_Gh_Mon + geom_col(data=Kosovo, 
                                    aes(date, as.numeric(test_percentage)),
                                    fill="blue3",
                                    alpha = 0.3)
Sl_Gh_Mon_Kos_Ch <- Sl_Gh_Mon_Kos + geom_col(data=China, 
                                    aes(date, as.numeric(test_percentage)),
                                    fill="orchid4",
                                    alpha = 0.2)
Sl_Gh_Mon_Kos_Ch + labs(title="Ghana, Montenegro, Sri Lanka, Kosovo, & China")
Sl_Gh_Mon_Kos_Ch + theme(legend.position = "bottom")

legend(Sl_Gh_Mon_Kos_Ch, 95, legend=c("Ghana", "Montenegro", "Sri Lanka", "Kosovo","China"),
       fill = c("red", "#009E73", "blue", "yellow", "orchid4"))


Slplot1 <- ggplot(sl, 
                 aes(date, as.numeric(total_cases))) +
  geom_col(fill = "grey0", alpha = 0.3) + 
  theme_minimal(base_size = 14) +
  xlab("Date Range") + ylab("Growth of total cases") + 
  scale_x_date(date_labels = "%Y/%m/%d")
Sl_Gh1  <- Slplot1 + geom_col(data=Ghana, 
                            aes(date, as.numeric(total_cases)),
                            fill="red2",
                            alpha = 0.1)
Sl_Gh_Mon1 <- Sl_Gh1 + geom_col(data=Montenegro, 
                              aes(date, as.numeric(total_cases)),
                              fill="yellow",
                              alpha = 0.2)
Sl_Gh_Mon_Kos1 <- Sl_Gh_Mon1 + geom_col(data=Kosovo, 
                                      aes(date, as.numeric(total_cases)),
                                      fill="blue3",
                                      alpha = 0.3)
Sl_Gh_Mon_Kos_Ch1 <- Sl_Gh_Mon_Kos1 + geom_col(data=China, 
                                             aes(date, as.numeric(total_cases)),
                                             fill="orchid4",
                                             alpha = 0.2)
Sl_Gh_Mon_Kos_Ch1 + labs(title="Ghana, Montenegro, Sri Lanka, Kosovo, & China")
Sl_Gh_Mon_Kos_Ch + theme(legend.position = "bottom")
