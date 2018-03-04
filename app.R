library("dplyr")
library("ggplot")
library("maps")
library("tidyr")
library("sp")
library("shiny")
library("ggplot2")

##########################
# DATA WRANGLING SECTION #
##########################

# Reading in data files
deaths <- read.csv('data/Deaths_in_122_US_cities.csv', stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
population <- read.csv('data/population_info.csv', stringsAsFactors = FALSE)

# Creating a new dataframe with the needed columns for analysis. 
pneuomnia.vs.all <- deaths %>% select(Year, WEEK, City, Pneumonia.and.Influenza.Deaths, All.Deaths) %>% 
  filter(Year > 2009)

# Changing of column name called "NAME" to "City" to help innerjoin the two dataframes (deaths & population)"                          
colnames(population)[9] <- "City"

# Spreading of years in "population" dataframe.

colnames(population)[13:19] <- 2010:2016

estimate.long <- gather(population, 
                        key = Year, 
                        value = ESTIMATE, "2010", "2011", "2012",
                        "2013", "2014", "2015", "2016"
                        )

# Making a new dataframe that joins by city 
city.join <- inner_join(pneuomnia.vs.all, estimate.long, by = "City")
View(city.join)


# Filtering for lowest and highest death rates 

lowest.and.highest <- filter(pneuomnia.vs.all, All.Deaths == Max
            )



