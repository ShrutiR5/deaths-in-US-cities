library("dplyr")
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

city.join <- left_join(pneumonia.vs.all, estimate.long, by = "City")

city.population <- city.join %>% filter(Year.x == Year.y)  

city.names <- unique(city.population$City)

city.range <- range(city.population$Year.x)

city.population <- mutate(city.population, "Pneumonia.and.Influenza.Ratio" = 
                            paste0(Pneumonia.and.Influenza.Deaths / 
                                     All.Deaths * 100, "%" ))

# Filtering for lowest and highest death rates 

pneumonia.deaths <- city.population %>% select(Year.x, WEEK, City,
                                               Pneumonia.and.Influenza.Deaths,
                                               All.Deaths, STNAME, ESTIMATE,
                                               Pneumonia.and.Influenza.Ratio)

YearlyDeaths <- function(year){
  deaths <- pneumonia.deaths %>% filter(Year.x == year) %>% 
    group_by(City) %>% mutate("Mortality.Rate" = 
                                sum(All.Deaths) / ESTIMATE * 100)
  
  table <- deaths %>% group_by(City) %>% filter(Mortality.Rate == max(Mortality.Rate,
                                                                  na.rm = TRUE)) 
  min <- table %>% 
    group_by(City) %>%
    filter(WEEK == 1) %>% 
    na.omit() %>% 
    select(Year.x, City, STNAME, Mortality.Rate) %>%
    ungroup(City) %>% filter(Mortality.Rate != 0) %>%
    filter(Mortality.Rate == min(Mortality.Rate))
  
  max <- table %>% 
    group_by(City) %>%
    filter(WEEK == 1) %>% 
    na.omit() %>% 
    select(Year.x, City, STNAME, Mortality.Rate) %>%
    ungroup(City) %>% filter(Mortality.Rate != 0) %>%
    filter(Mortality.Rate == max(Mortality.Rate))
  
  min.max <- full_join(min, max)
  
  return(min.max)
  
}

deaths.2010 <- YearlyDeaths(2016)

# Which age/year group had the most deaths combined?

age.groups <- deaths %>% filter(Year > 2009) %>% group_by(Year)

colnames(age.groups)[9:13] <- c("<1", "1-24", "25-44", "45-64", "65+")

age.group <- gather(age.groups,
                    key = age.group,
                    value = agegroup.deaths, "<1", "1-24", "25-44", "45-64", "65+")

age.ranges <- unique(age.group$age.group)

GetAgeDeaths <- function(year){
  g <- age.group %>% filter(Year == year) %>% group_by(age.group) %>%
    summarize(sum = sum(agegroup.deaths, na.rm = TRUE))
}

AgeGroupDeaths <- function(ages){
  t <- age.group %>% filter(age.group == ages) %>% group_by(Year) %>% 
    summarize(sum = sum(agegroup.deaths, na.rm = TRUE))
}

frist.ages <- AgeGroupDeaths('<1')


# UI Code

ui<- fluidPage(
  
  titlePanel("Mortality Rates in the United States from 2010-2016"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = 'year', label = 'Year', min = 2010, 
        max = 2016, value = 2010),
      selectInput(
        inputId = 'city',
        label = 'Cities',
        choices = cities
      ),
      radioButtons(
        inputId = "age",
        label = "Age Range",
        choices = age.ranges
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabset",
        type = "tabs",
        tabPanel("Background Information", htmlOutput("heading"), textOutput("BackInfo")),
        tabPanel("Table", tableOutput("table")),
        tabPanel( "Map", plotOutput(
          "map",
          width = "100%",
          height = 400,
          hover = "map.hover",
          click = "map.click"
        )),
        verbatimTextOutput("country.info"),
        tabPanel("Deaths by city", tableOutput("CityDeaths")),
        tabPanel("Deaths by city Map", plotOutput("CityDeathsMap")),
        tabPanel("Age Group Deaths", tableOutput("AgeGroup")),
        tabPanel("Age Groups Trends", plotOutput("Trends")),
        tabPanel("Age Groups Bar Graphs", plotOutput("Graphs"))
        
        ))
      )
    )



# Server Code

server <- function(input, output){
  
  output$heading <- renderUI({
    return(h1("Introduction"))
  
    
  })
  
  output$BackInfo <- renderText({
    
  return("This data set consists of information that was reported to 120 
         Cities Mortality Reporting System.
         This data set reflects information based on 120 cities across 
         the United States, it shows the mortality rates and this was created 
         by looking at the number of death certificates processed 
         as well as the number of people who died by pneumonia or influenza.
         This system ran every week for years. 
         This was grouped by various age ranges, i.e: Under 28 days, 
         28 days -1 year, 1 - 14 years, 15 - 24 years,
         25 - 44 years, 45 - 64 years, 65 - 74 years, 75 - 84 years, and 85+ 
         years. We also have another dataset that consists of information 
         based of the US population census 2010, it gives an estimate population
         for each of the cities from 2010-2016. " 

    
  )})

  
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
  
  output$Trends <- renderPlot({
    
  })
  
  }



shinyApp(ui, server)

