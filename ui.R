## ui.R ##

#Calling the required libraries
library(shinydashboard)
library(shiny)
require(ggplot2)
library(dplyr)
library(plotly)
library("tidyverse")
library(maps)
library(ggthemes)
library(gganimate)
library(gifski)
library(tibble)
library(readxl)
theme_set(theme_bw())


#Creating a dashboard with 3 menu items
dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Olympic History of 1896 to 2016", titleWidth = 450),
  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      # 1st menu for Participation and Performance
      menuItem("Participation and Performance", tabName = "Tab1", icon = icon("bar-chart-o")),
      #2nd menu for Overall Participation Trend
      menuItem("Overall Participation Trend", tabName = "Tab2", icon = icon("bar-chart-o")),
      #3rd menu for Overall Performance Trend
      menuItem("Overall Performance Trend", tabName = "Tab3", icon = icon("bar-chart-o"))
    )
  ),
  # Body content
  dashboardBody(
    # Setting up the color of the main header of the dashboard
    tags$head(tags$style(HTML('
      .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
    # Setting up a suitable color for the slider input which is defined later
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #A020F0}")),
    # Defining the contents for each menu items
    tabItems(
      # First tab content
      tabItem(tabName = "Tab1",
          # Giving a suitable title for the 1st tab
          helpText(h2("Participation and Performance of Athletes from Athens 1896 to Rio 2016 
                      Olympic Games")),
          sidebarLayout(
            # Defining a side bar Panel and a filter for Sex
            sidebarPanel(selectInput("Sex", label= h3("Sex"), 
                                     c("M", 
                                       "F")),
                         #Defining a slider for Year
                         sliderInput("Year_2", label= h3("Year"),
                                     min=1896, max=2016, step= 2, 
                                     value = c(1896, 2016))),
            # Defining the graph to be plotted on the main Panel
            mainPanel (fluidRow(plotlyOutput("plotOne", width = 800, height = 275),
                                 htmlOutput("x_value")))),
          sidebarLayout( 
            # Defining a side bar Panel for Source
            sidebarPanel(         
                 helpText(h3("Source: ")),
                 helpText( a("The Olympics dataset is taken from Kaggle",
                 href=  "https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results#noc_regions.csv",
                 "for the analysis and building these visualisations. Click here to view the source.")),
                 helpText(h3("Instructions:")),
                 helpText("Hover over all the graphs")),
           # Defining the graph to be plotted on the main Panel
            mainPanel(fluidRow(plotlyOutput("plotTwo", width = 800, height = 275)))),
            br(),
          # Defining a side bar Panel for the filters      
          sidebarLayout(
            sidebarPanel( 
              # Defining a filter for Year
              selectInput("Year", label= h3("Year"),
                          c("1896",
                            "1936",
                            "1976",
                            "2016")),
              # Defining a filter for Medal Type
              selectInput("Medal", label = h3("Medal Type"),
                          c("Gold",
                            "Silver",
                            "Bronze")), width = "4"),
            # Defining the graph to be plotted on the main Panel
            mainPanel(fluidRow(plotlyOutput("plotFour", width = 800, height = 300)))
            )
          ),
      
      # Second tab content
      tabItem(tabName = "Tab2",
          # Giving a title for the 2nd menu
          helpText(h2("Trend of Athletes Count in different Regions from Athens 1896 to Rio 2016 
                      Olympic Games")),
          # Defining a sidebar
          fluidRow(sidebarLayout(
            sidebarPanel(
            # Defining a filter for Year
            selectInput("Year_1", label = h3("Year"),
                        c("1896",
                          "1936",
                          "1976",
                          "2016"))),
            # Defining the graph to be plotted on the main Panel
            mainPanel(fluidRow(plotlyOutput("plotFive", width = 800, height = 300)
                               ))),
            # Giving a space
            br(),
            br(),
            # Defining a sidebar
            sidebarLayout(
              sidebarPanel(
                # Defining a filter for Year
                helpText(h3("Source:")),
                helpText(a("For analysis and building these visualisation, 2 datasets are merged, 
                namely,Olympics dataset and NOC dataset, which is taken from Kaggle.",
                href=  "https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results#noc_regions.csv",
                "Click here to view the source.")),
                helpText(h3("Instructions:")),
                helpText("Hover over all the graphs")),
              # Defining the graph to be plotted on the main Panel    
              mainPanel(fluidRow(plotlyOutput("plotSix", width = 800, height = 300))))
              )
          ),
      
      # Third tab content
      tabItem(tabName = "Tab3",
          # Giving a title for the 2nd menu   
          helpText(h2("Participant's Performance Trend across all Countries from Athens 1896 to 
                      Rio 2016 Olympic Games")),
          # Defining a side bar layout
          sidebarLayout(
            sidebarPanel(
              # PRoviding information about the graph
              helpText(h3("About Graph: ")),
              helpText("This is a GIF representing the Trend of Medal Count across all the 
                          countries from Athens 1896 to Rio 2016."),
              helpText("This graph takes few seconds to load as the dataset is huge. Please wait."),
              helpText(h3("Legend: ")),
              helpText("Size of the Bubbles: Medal Counts"),
              helpText("Color of the Bubbles: Different Countries")
            ),
         # Defining the graph to be plotted on the main Panel     
         mainPanel(imageOutput("plotThree")) ),
         # Defining a side bar layout
         sidebarLayout(
           sidebarPanel(
             # Providing information about the source
             helpText(h3("Source:")),
             helpText(a("The GIF is obtained by merging 2 datasets,namely, Olympic dataset 
             which is taken from Kaggle and world co-ordinates dataset, which is taken from developers.google.com ",
             href=  "https://developers.google.com/public-data/docs/canonical/countries_csv",
             "This is used for analysis and building this GIF visualisation. Click here to view the source."))
           ),
           mainPanel("")
         )
      )
    )
  )
)