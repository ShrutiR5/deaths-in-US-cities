library("dplyr")
library("maps")
library("tidyr")
library("sp")
library("shiny")
library("ggplot2")


source("spatial_utils.R")

##########################
# DATA WRANGLING SECTION #
##########################

# Reading in data files
deaths <- read.csv('data/Deaths_in_122_US_cities.csv', stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
population <- read.csv('data/census_info.csv', stringsAsFactors = FALSE)

# Creating a new dataframe with the needed columns for analysis. 
pneumonia.vs.all <- deaths %>% select(Year, WEEK, City, Pneumonia.and.Influenza.Deaths, All.Deaths) %>% 
  filter(Year > 2009)

# Getting the city names
cities <- (unique(pneumonia.vs.all$City))


# Changing of column name called "NAME" to "City" to help innerjoin the two dataframes (deaths & population)"                          
colnames(population)[9] <- "City"

# Spreading of years in "population" dataframe.

colnames(population)[13:19] <- 2010:2016

estimate.long <- gather(population, 
                        key = Year, 
                        value = ESTIMATE, "2010", "2011", "2012",
                        "2013", "2014", "2015", "2016")

estimates <- estimate.long %>% select(Year, City, STNAME, ESTIMATE)

# Making a new dataframe that joins by city 
city.join <- inner_join(pneumonia.vs.all, estimate.long, by = "City")


# Filtering for lowest and highest death rates 

deaths.2010 <- filter(pneumonia.vs.all, Year == 2010)
deaths.2011 <- filter(pneumonia.vs.all, Year == 2011)
deaths.2012 <- filter(pneumonia.vs.all, Year == 2012)
deaths.2013 <- filter(pneumonia.vs.all, Year == 2013)
deaths.2014 <- filter(pneumonia.vs.all, Year == 2014)
deaths.2015 <- filter(pneumonia.vs.all, Year == 2015)
deaths.2016 <- filter(pneumonia.vs.all, Year == 2016)

# Max & Min Deaths for each year

#2010
max.2010 <- deaths.2010 %>% filter(All.Deaths == max(All.Deaths, na.rm = TRUE)) 
min.2010 <- deaths.2010 %>% filter(All.Deaths == min(All.Deaths, na.rm = TRUE))
 
maxmin.2010 <- full_join(max.2010, min.2010)


# 2011
max.2011 <- deaths.2011 %>% filter(All.Deaths == max(All.Deaths, na.rm = TRUE)) 
min.2011 <- deaths.2011 %>% filter(All.Deaths == min(All.Deaths, na.rm = TRUE))

maxmin.2011 <- full_join(max.2011, min.2011)

# 2012
max.2012 <- deaths.2012 %>% filter(All.Deaths == max(All.Deaths, na.rm = TRUE)) 
min.2012 <- deaths.2012 %>% filter(All.Deaths == min(All.Deaths, na.rm = TRUE))

maxmin.2012 <- full_join(max.2012, min.2012)

# 2013
max.2013 <- deaths.2013 %>% filter(All.Deaths == max(All.Deaths, na.rm = TRUE)) 
min.2013 <- deaths.2013 %>% filter(All.Deaths == min(All.Deaths, na.rm = TRUE))

maxmin.2013 <- full_join(max.2013, min.2013)

# 2014
max.2014 <- deaths.2014 %>% filter(All.Deaths == max(All.Deaths, na.rm = TRUE)) 
min.2014 <- deaths.2014 %>% filter(All.Deaths == min(All.Deaths, na.rm = TRUE))

maxmin.2014 <- full_join(max.2014, min.2014)

# 2015
max.2015 <- deaths.2015 %>% filter(All.Deaths == max(All.Deaths, na.rm = TRUE)) 
min.2015 <- deaths.2015 %>% filter(All.Deaths == min(All.Deaths, na.rm = TRUE))

maxmin.2015 <- full_join(max.2015, min.2015)

# 2016
max.2016 <- deaths.2016 %>% filter(All.Deaths == max(All.Deaths, na.rm = TRUE)) 
min.2016 <- deaths.2016 %>% filter(All.Deaths == min(All.Deaths, na.rm = TRUE))

maxmin.2016 <- full_join(max.2016, min.2016)


# Which age/year group had the most deaths combined?

age.groups <- deaths %>% filter(Year > 2009) %>% group_by(Year)

colnames(age.groups)[9:13] <- c("<1", "1-24", "25-44", "45-64", "65+")

# UI Code

ui<- fluidPage(
  
  titlePanel("Mortality Rates in the United States from 2010-2016"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'city',
        label = 'Cities',
        choices = cities
      ),
      radioButtons(
        inputId = "age",
        label = "Age Range",
        choices = 1
      )
    ),
    
     mainPanel(
      tabsetPanel(
        id = "tabset",
        type = "tabs",
        tabPanel("Background Information", textOutput("BackInfo")),
        tabPanel("Table", tableOutput("table")),
        tabPanel( "Map", plotOutput(
          "map",
          width = "100%",
          height = 400,
          hover = "map.hover",
          click = "map.click"
        ),
        verbatimTextOutput("country.info")
      
        )
    )
    )))
  


# Server Code

server <- function(input, output){
  
  output$country.info <- renderPrint({
    return(GetCountryAtPoint(input$map.click$x, input$map.click$y))
  })
  
  output$map <- renderPlot({
    usa <- map_data("state")
   
 # Layout Of Map
    
    p <- ggplot()
    p <- p + geom_polygon(data=usa, aes(x=long, y=lat, group = group),colour="white") + 
      scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
    p <-  geom_map(map = usa,
             aes(map_id = region))
    return(p)
  })
  
  
  
}

shinyApp(ui, server)
