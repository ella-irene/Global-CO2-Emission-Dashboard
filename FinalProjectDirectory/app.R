library(tidyverse)
library(stringr)
library(purrr)
library(ggthemes)
library(rvest)
library(polite)
library(shiny)
library(maps)
library(maptools)
library(sp)
library(leaflet)
library(tidyverse)
library(dplyr)
library(tidyr)
library(shinythemes)
library(rgeos)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(polite)
library(tidymodels)
library(stringr)
library(rlang)
library(data.table)
library(shinythemes)
library(probably)
library(factoextra)

# Load CO2 data
owid_co2_data <- read_csv("owid-co2-data.csv")
#View(owid_co2_data)
# Load in second data set
countryGDPperCapita <- read_csv("countryGDPperCapita.csv")
#View(countryGDPperCapita)
# Load in third data set
world_population <- read_csv("world_population.csv")
#View(world_population)


# years in countryGDPPercapita: 1990, 1995, 2000, 2005, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018
Mgmt <- c(1990, 1995, 2000, 2005, 2010, 2015)
co2NewData <- filter(owid_co2_data, is.element(year, Mgmt))
WorldPopNew <- world_population %>%
  mutate(`2022 Population` = NULL, `1970 Population` = NULL, `1980 Population` = NULL, `2020 Population` = NULL)
GDPperCap <- countryGDPperCapita %>%
  mutate(`2011` = NULL, `2012` = NULL, `2013` = NULL, `2014` = NULL, `2016` = NULL, `2017` = NULL, `2018` = NULL)

library(DataEditR)
#data_edit(WorldPopNew)
# used the above library package to perform mass edits on our data sets. Needs to be commented out to allow for knitting after the changes are made. I first merged our initial two data sets, GDP with co2NewData, by changing the rows to columns in GDP. This allowed for us to see the 6 ideal years of GDP values along with our co2 data. I was then able to see which countries were included in our co2NewData but did not have GDP values and was able to delete those from our data set as they were unnecessary.

co2CleanDataV1 <- read_csv("co2CleanDataV1.csv")
# This was loading in the initial data set from the DataEditr package after the above changes. 

GDP_p_c_Clean <- co2CleanDataV1 %>%
  select(country, year, gdp_p_cap)

#data_edit(co2CleanDataV1)
co2CleanDataFinal <- read_csv("co2CleanDataFinal.csv")

# Map data preparation
world <- maps::map("world", plot = FALSE, fill=TRUE) 
mapNames <- c(world$names) #accesses country labels from map data
mapData <- co2CleanDataFinal[ co2CleanDataFinal$country %in% mapNames, ] #drops countries that are not in the world map country labels
country_list <- mapData$country
co2Data <- co2CleanDataFinal
year_list<- Mgmt
year_to_plot <- setNames(Mgmt, c("1990", "1995", "2000", "2005", "2010", "2015")) #Used this to fix named issue

# Coxcolmb Data Preparation
coxcolmbData <- owid_co2_data %>%
  select(cumulative_cement_co2, 
         cumulative_coal_co2,
         cumulative_flaring_co2,
         cumulative_gas_co2,
         cumulative_oil_co2, 
         cumulative_other_co2,
         year, country, co2_per_capita, co2) %>%
  filter(!country %in% c( "Asia", "North America", "South America", "Australia", "Europe", "Africa", "World", "Upper-middle-income-countries", "High-income-countries", "European Union (28)", "European Union (27)", "Europe (excl. EU-27)", "Europe (excl. EU-28)", "North America (excl. USA)", "International transport", "Oceania", "Upper-middle-income countries", "High-income countries", "Asia (excl. China & India)", "Lower-middle-income countries", "Low-income countries"))%>%
  pivot_longer(names_to = "source",
               values_to = "emissions",
               cols = cumulative_cement_co2:cumulative_other_co2)

# Time-series Data Preparation
timeSeriesData <- owid_co2_data %>%
  select(year, country, co2_per_capita, co2) %>%
  filter(!country %in% c( "Asia", "North America", "South America", "Australia", "Europe", "Africa", "World", "Upper-middle-income-countries", "High-income-countries", "European Union (28)", "European Union (27)", "Europe (excl. EU-27)", "Europe (excl. EU-28)", "North America (excl. USA)", "International transport", "Oceania", "Upper-middle-income countries", "High-income countries", "Asia (excl. China & India)", "Lower-middle-income countries", "Low-income countries"))


# Clustering by High, low, or medium emissions
justCo2Data<- co2CleanDataFinal %>%
  select(country, population, gdp, co2) %>%
  mutate(emission_rate = case_when(co2 <= 9.376  ~ 'low', co2 >= 1000 ~ 'high'))

justCo2Data$emission_rate <- justCo2Data$emission_rate %>% replace_na('medium')

# Organizing data
mldata <- as_tibble(justCo2Data, rownames = "emission_rate") %>% drop_na() %>%
  column_to_rownames("emission_rate") %>%
  select(population, gdp)

# Standardizing data
standardize <- function(x, na.rm = FALSE) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
}
mldata <- mldata %>% mutate(across(where(is.numeric), standardize))

# Ac
set.seed(1234)
k.means <- kmeans(mldata, centers = 3, nstart = 25)
k.means %>% tidy()




# Shiny Integration 
ui <- fluidPage(theme = shinytheme("yeti"),
            titlePanel("CO2 Emissions Worldwide"),
            navbarPage("About",
                       tabPanel(icon("home"), uiOutput('markdown')),
                       tabPanel("Map",
                                sidebarLayout(
                                  sidebarPanel(selectInput("Year", 
                                                           label = "Choose Year:",
                                                           choices = year_to_plot, 
                                                           selected = 1990,
                                                           multiple = FALSE),
                                               selectInput("Country",
                                                           "Choose Countries:",
                                                           choices = list(`country_to_plot` = country_list),
                                                           selected = c("Ethiopia","Germany", "United States", "Mexico", "China", "Somalia", "Uganda", "Tanzania"),
                                                           multiple = TRUE)),
                                               
                                  
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("Map", leafletOutput(outputId = "mymap")),
                                      tabPanel("Data", DT::dataTableOutput(outputId = "table"))
                                    )
                                  ))),
                          
                       tabPanel("Source Breakdown",
                                  sidebarLayout(
                                  sidebarPanel(selectInput("Date",  "Choose Year",
                                                           choices = year_to_plot,
                                                           selected = 2015)),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("Breakdown of CO2 Emission by Source", plotOutput("coxcolmb")),
                                      tabPanel("Data", DT::dataTableOutput(outputId = "table2"))
                                    )) 
                                )), 
                                
                        tabPanel("Timescale",
                                 sidebarLayout(
                                   sidebarPanel(
                                                selectInput("Region",
                                                            "Choose Countries:",
                                                            choices = list(`country_to_plot` = country_list),
                                                            selected = c("Germany", "United States", "Mexico", "China", "Japan"),
                                                            multiple = TRUE)),
                                   mainPanel("Emissions Over Time", plotOutput("timeseries")) 
                                  )
                   
                     ),
                     tabPanel("Emission Rate Prediction",
                              sidebarLayout(
                                sidebarPanel(p("In this classifier, co2 emissions were split into medium, low, and high based on the distribution of the global data. These labelled data were then put into a k-means clustering model which was able to accurately predict 95% of countries' emission rates based on GDP and population. The clusters are displayed here on the right. ")
                              ),
                                mainPanel("",
                                        plotOutput("mlPlot"))
                     )
                  ))
)
            
          
server <- function(input, output, session) {
  thematic::thematic_shiny() 
  
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML('homePage.md'))
  })
    
  data_filtered <- reactive({
    mapData  %>% janitor::clean_names() %>% 
      filter(country %in% input$Country) %>%
      filter(year == input$Year)
  })
  
  output$mymap <- renderLeaflet({
    worldMap <- maps::map("world", regions = input$Country, plot = FALSE, fill=TRUE)
    worldMap$names <- str_extract(worldMap$names, "\\w+") #
    regionMap <- map2SpatialPolygons(worldMap, IDs = worldMap$names)
    
    map <- SpatialPolygonsDataFrame(regionMap, data_filtered(), match.ID = FALSE)
    bins <- seq(min(map$co2_per_capita), max(map$co2_per_capita),length.out = 6)
    pal <- colorBin("YlOrRd", domain = map$co2_per_capita, bins = bins)
    labels <- sprintf("<strong> %s </strong> <br/> Co2 per capita %s", str_to_upper(map$country), map$co2_per_capita) %>%
      lapply(htmltools::HTML)
    
    l <- leaflet(map) %>% addTiles() %>% setView(lng = 10, lat = 0, zoom = 2) 
    l %>% addPolygons(color = "grey", weight = 1,
                      fillColor = ~pal(co2_per_capita), fillOpacity = 0.7,
                      highlightOptions = highlightOptions(weight = 5),
                      label = labels) %>%
      addLegend(pal = pal, values = ~co2_per_capita, opacity = 0.5, 
                title = "C02 emissions per capita", 
                position = "bottomleft")
  })
  
  output$table <- DT::renderDataTable({
    data_filtered()})
  
  
  coxcolmbData_filtered <- reactive({coxcolmbData %>% 
      janitor::clean_names() %>% 
      filter(year == input$Date) %>% 
      arrange(desc(co2))%>%
      top_n(150, co2) 
       })
  
  output$coxcolmb <- renderPlot({
    #Coxcolmb plot
      ggplot(coxcolmbData_filtered(), aes(fill=source, y=co2, x=country)) + 
      geom_bar(position="stack", stat="identity") + 
      coord_polar() + 
      scale_y_continuous(trans = "log10") +
      aes(x=reorder(country, co2)) +
      labs(y = "Emissions", x = "") +
      theme(axis.text.x = element_text(angle=-20))+
      theme_bw() +
      theme(legend.position="bottom")
  })
  
  output$table2 <- DT::renderDataTable({
    coxcolmbData_filtered()
    })
  
  timeSeriesData_filtered <- reactive({timeSeriesData %>%
      janitor::clean_names() %>% 
      filter(country %in% input$Region)
  })
  
  output$timeseries <- renderPlot({
    ggplot(data= timeSeriesData_filtered(), 
                              aes(x=year, y=co2, group = country,
                                  colour = country)) + 
    geom_line() +
    labs(y= "CO2 emissions", x = "Year") + 
    geom_point() +
    theme_bw()
  })
  
  output$mlPlot <- renderPlot({
    fviz_cluster(kmeans(mldata, centers = 3, nstart = 25), labelsize = 8, data = mldata, ggtheme = theme_bw())})
}    


finalApp <- shinyApp(ui = ui, server = server)
finalApp
