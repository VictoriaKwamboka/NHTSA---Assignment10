library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(usmap)
#library(plotly)
library(graphics)
library(DT)


### data sets used
setwd("C://Users//perup//OneDrive - South Dakota State University - SDSU/Desktop//2. Fall 2023//STA 542//Assignments//Assignment 10//trial10")
state_crashes<- read.csv('state_crashes.csv')
violations<- read.csv('violations.csv')
drugs<- read.csv('state_drugs.csv')
states_regions<- read.csv("state_regions.csv")

# Define UI for application
ui <- navbarPage("NHTSA 2021", theme = shinytheme("flatly"),
                 setBackgroundColor(
                   color = "#e6e6ff",
                   gradient = c("linear", "radial"),
                   direction = c("bottom", "top", "right", "left"),
                   shinydashboard = FALSE
                 ),
                 tabPanel("MAPPING",
                          sidebarLayout(
                            sidebarPanel(
                              h3("This page contains data about the number of crashes by region"),
                              # Input: Drop down-------------------------------------
                              selectInput("regions", "Regions", choices = state_crashes$Regions, selected = as.factor(levels(state_crashes$Regions)[1])),
                              
                              
                              
                              # br() element to introduce extra vertical spacing ----
                              br(),
                              width = 3
                            ),
                            mainPanel(
                              shinydashboard::box(title = "Map By Region", solidHeader = TRUE,
                                  plotOutput("usmap", height = 350)),
                              shinydashboard::box(title = "Bar Plot", solidHeader = TRUE,
                                  plotOutput("barchart", height = 350)),
                            )
                            
                          )
                 ),
                 tabPanel("Data Explorer", 
                          radioButtons("vio", "Choose a factor:",
                                       choices = c('Drugs', 'Violations'),
                                       selected = "Drugs"
                          ),
                          textOutput('selected_option'),
                          br(),
                          
                          DT::dataTableOutput("tbl"))

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$selected_option<- renderText({
    paste("The table below shows data about crashes that were due to ", input$vio, ".")
  })
  
  output$usmap <- renderPlot({
    
    ## Use built in regions of USMAP to get plots by region
    if (input$regions == 'South'){
      plot_usmap(data = state_crashes, values = "count", include = .south_region, labels = TRUE, color = "black") +
        scale_fill_continuous(low ="#9be396", high = "red", name = "Count", label = scales::comma) +
        labs(title = "Count of Crashes - South Region") +
        theme(legend.position = "right",
              panel.background = element_rect(color = "NA",fill='transparent'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }else if(input$regions == 'West'){
      plot_usmap(data = state_crashes, values = "count", include = .west_region, labels = TRUE, color = "black") +
        scale_fill_continuous(low ="#9be396", high = "red", name = "Count", label = scales::comma) +
        labs(title = "Count of Crashes - West Region")+
        theme(legend.position = "right",
              panel.background = element_rect(color = "NA",fill='transparent'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    }else if(input$regions == 'Midwest'){
      plot_usmap(data = state_crashes, values = "count", include = .midwest_region, labels = TRUE, color = "black") +
        scale_fill_continuous(low ="#9be396", high = "red", name = "Count", label = scales::comma) +
        labs(title = "Count of Crashes - Midwest Region")+
        theme(legend.position = "right",
              panel.background = element_rect(color = "NA",fill='transparent'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    }else{
      plot_usmap(data = state_crashes, values = "count", include = .northeast_region, labels = TRUE, color = "black") +
        scale_fill_continuous(low ="#9be396", high = "red", name = "Count", label = scales::comma) +
        labs(title = "Count of Crashes - Northeast Region") +
        theme(legend.position = "right",
              panel.background = element_rect(color = "NA",fill='transparent'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    
  })
  
  output$barchart<- renderPlot({
    
    dat<- state_crashes%>%
      filter(Regions == input$regions)
    
    
    if (input$regions == 'South'){
      dat<- state_crashes%>%
        filter(Regions == "South")
      p<<- ggplot(dat, aes(x=state)) + 
        geom_bar(stat = "identity", fill = "red", aes(y=deaths_per_100k)) +
        labs(title = "Bar Plot of Crashes in South Region") +
        coord_flip()+
        theme(plot.background = element_rect(fill = "#f0f5f5"),
              panel.background = element_rect(fill='transparent'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      p
      
    }else if(input$regions == 'West'){
      ggplot(dat, aes(x=state, y=deaths_per_100k)) + 
        geom_bar(stat = "identity", fill = "red") +
        labs(title = "Bar Plot of Crashes in West Region") +
        coord_flip()+
        theme(plot.background = element_rect(fill = "#f0f5f5"),
              panel.background = element_rect(fill='transparent'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    }else if(input$regions == 'North East'){
      ggplot(dat, aes(x=state, y=deaths_per_100k)) + 
        geom_bar(stat = "identity", fill = "red") +
        labs(title = "Bar Plot of Crashes in North East Region") +
        coord_flip()+
        theme(plot.background = element_rect(fill = "#f0f5f5"),
              panel.background = element_rect(fill='transparent'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    }else{
      ggplot(dat, aes(x=state, y=deaths_per_100k)) + 
        geom_bar(stat = "identity", fill = "red") +
        labs(title = "Bar Plot of Crashes in Midwest Region") +
        coord_flip()+
        theme(plot.background = element_rect(fill = "#f0f5f5"),
              panel.background = element_rect(fill='transparent'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    }
    
    
  })
  
  ## left join DRUGS data and regions data
  drugs<- drugs%>%
    left_join(states_regions, by='STATENAME', relationship = "many-to-many")
  #View(drugs)
  drugs <- drugs %>%
    select(1:2, (ncol(drugs) - 3):(ncol(drugs)), 3:(ncol(drugs) - 4))
  ### left join violations and regions
  violations<- violations%>%
    left_join(states_regions, by='STATENAME', relationship = "many-to-many")
  #View(violations)
  violations <- violations %>%
    select(1:2, (ncol(violations) - 2):(ncol(violations)), 3:(ncol(violations) - 3))
  
  
  output$tbl = DT::renderDataTable(
    if(input$vio == "Drugs"){
      if(input$regions == 'South'){
        ds<- drugs%>%
          filter(regions == 'South')
        datatable(ds, options = list(pageLength = 50))
        
      }else if(input$regions == 'West'){
        dw<- drugs%>%
          filter(regions == 'West')
        datatable(dw, options = list(pageLength = 50))
        
      }else if(input$regions == 'North East'){
        dne<- drugs%>%
          filter(regions == 'North East')
        datatable(dne, options = list(pageLength = 50))
      }else{
        dmw<- drugs%>%
          filter(regions == 'Midwest')
        datatable(dmw, options = list(pageLength = 50))
      }
      
    }
    else{
      if(input$regions == 'South'){
        vs<- violations%>%
          filter(regions == 'South')
        datatable(vs, options = list(pageLength = 50))
        
      }else if(input$regions == 'West'){
        vw<- violations%>%
          filter(regions == 'West')
        datatable(vw, options = list(pageLength = 50))
        
      }else if(input$regions == 'North East'){
        vne<- violations%>%
          filter(regions == 'North East')
        datatable(vne, options = list(pageLength = 50))
      }else{
        vmw<- violations%>%
          filter(regions == 'Midwest')
        datatable(vmw, options = list(pageLength = 50))
      }
      
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server,options = list(height = 1300))
