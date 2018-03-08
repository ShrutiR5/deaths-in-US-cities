library("dplyr")
library("maps")
library("tidyr")
library("sp")
library("shiny")
library("ggplot2")
library("plotly")

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
  
  single.max <- head(max, 1)
  
  min.max <- full_join(min, single.max)
  
  return(min.max)
  
}

deaths.2010 <- YearlyDeaths(2014)

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
  return(g)
}

AgeGroupDeaths <- function(ages){
  t <- age.group %>% filter(age.group == ages) %>% group_by(Year) %>% 
    summarize(sum = sum(agegroup.deaths, na.rm = TRUE))
  return(t)
}

# UI Code

ui<- fluidPage(
  theme = "bootstrap.css",
  
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
        tabPanel("Pneumonia and Influenza Deaths", htmlOutput('table.header'), textOutput('table.text'),tableOutput("table")),
        tabPanel( "Map", htmlOutput("mapshead"), textOutput("mapstext"), plotlyOutput("plot")),
        tabPanel("Deaths by city", htmlOutput("dbchtml"), textOutput("dbctext"), tableOutput("CityDeaths")),
        tabPanel("Age Group Deaths", htmlOutput("agdhtml"), textOutput("agdtext"), tableOutput("AgeGroup")),
        tabPanel("Age Groups Trends", htmlOutput("trendshtml"), textOutput("trendstext"), plotOutput("Trends")),
        tabPanel("Age Groups Bar Graphs",htmlOutput("bghtml"), textOutput("bgtext"), plotOutput("Graphs"))

        
        ))))
      
    



# Server Code

server <- function(input, output){
  
  output$heading <- renderUI({
    return(h1("Introduction"))
  
    
  })

  
  output$AgeGroup <- renderPlot({
    gg <- ggplot(data = GetAgeDeaths(input$year), mapping = aes(x = age.group, y = sum, fill = age.group)) +
      geom_bar(stat = "identity")+
      scale_y_continuous(labels =  scales::comma)
    
    
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
  
  
  output$table <- renderTable({
    o <- pneumonia.deaths %>% filter(Year.x == input$year, City == input$city)
    return(o)
    
    
  })

  
  output$plot <- renderPlotly({
    year.input <- input$year
    
    df <- read.csv('data/deaths_in_122_US.cities.csv')
    Sys.setenv('MAPBOX_TOKEN' = "pk.eyJ1IjoicmFuaWkyMiIsImEiOiJjamVnNDQ3NnExcDZiMzNvN3dtemNkdmF3In0.9xXW1i3CBnuYr4N6TrbJXA")
    df <- df %>% 
      filter(State != "", Year == year.input) %>% 
      group_by(State,City, Year) %>% 
      na.omit() %>% 
      summarise(all_death = sum(All.Deaths)) %>% 
      mutate(name = paste(City, State),
        hover = paste(City, State,", Death: ",all_death)) %>% 
      left_join(us.cities) 
    
    p <- 
      df %>%
      plot_mapbox(lat = ~lat, lon = ~long, split=~City,mode = 'scattermapbox', text = ~hover, hoverinfo = "text", showlegend = FALSE) %>% 
        add_markers(size=~all_death, text = ~hover, hoverinfo = "text") %>%
      layout(title = 'Deaths by Cities',
             font = list(color='white'),
             plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
             mapbox = list(style = 'dark',
                           zoom = 3,
                           center = list(lat = 37.0902,lon =-95.7129)),
             legend = list(orientation = 'h',
                           font = list(size = 8)),
             margin = list(l = 25, r = 25,
                           b = 25, t = 25,
                           pad = 2))
    
   
    
    return(p)
  })

  
  output$Trends <- renderPlotly({
    AgeGroupDeaths(input$age) %>% 
    plot_ly( x = ~Year, y = ~sum, type = "scatter") %>% 
      add_trace(mode = "lines", showlegend = FALSE) %>% 
      layout(title = 'Age Group ',
             yaxis = list(zeroline = FALSE),
             xaxis = list(zeroline = FALSE))
  })

  output$Trends <- renderPlot({
    
  })
  
  output$table.header <- renderUI({
    return(h1("Pneuomonia vs. Influenza Ratio"))
  })
  
  output$table.text <- renderText({
    return("In this tab the rates of deaths caused by Pneumonia and Influenza can be seen for each year and city in comparison to the total number of deaths.
")
  })
  output$mapshead <- renderUI({
    return(h1("Deaths By City Map"))
  })
  
  output$mapstext <- renderText({
    return("This tab shows a map of the United States with information based on the number of deaths in each city per year.
     You can hover over the dot to see more information.")
    })
  output$dbchtml <- renderUI({
    return(h1("Deaths By City Table"))

  })
 output$dbctext <- renderText({
    return("This tab shows a table of the United States with information based on the number 
         of deaths in each city per year.")
  })
 output$agdhtml <- renderUI({
   return(h1("Deaths By Age Group Table"))
 })
 output$agdtext <- renderText({
   return("In this tab you can see a table of the deaths in each city based on different age groups.")
 })
 output$trendshtml <- renderUI({
   return(h1("Death Trends In Age Groups"))
 })
 output$trendstext <- renderText({
   return("A line graph is shown here that represents a specific age group over the last 7 years.")
 })
 output$bghtml <- renderUI({
   return(h1("Age Group Bar Graphs"))
 })
 output$bgtext <- renderText({
   return("A bar graph will be represented for all age groups in a given year. ")
})
}



shinyApp(ui, server)

