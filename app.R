library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(plotly)
library(usmap)
library(shinythemes)
library(DT)


### data sets used
setwd("C://Users//perup//OneDrive - South Dakota State University - SDSU/Desktop//2. Fall 2023//STA 542//Assignments//Assignment 10")
state_crashes<- read.csv('state_crashes.csv')
violations<- read.csv('violations.csv')
# View(violations)
drugs<- read.csv('state_drugs.csv')
states_regions<- read.csv("state_regions.csv")

# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
                tags$style('.container-fluid {
                             background-color: #cadbda;
              }'),
  div(
    style = "margin-left:30%;",
    titlePanel(h2("Snap shot of Crash Rate in 2021 by Region",style="color:teal;") ),
    br(),
    # div(style = "margin-left:2%;
    #     margin-top:0",
    #   h4("    The plots below show the summary of crash rate by state in 2021.",style="color:DodgerBlue;")
    # )
  ),
  br(),
  br(),
  # Sidebar layout with input and output definitions -----------------
div(style = "padding-bottom: 6em;
   margin-top:-2em;",
  sidebarLayout(
    # Sidebar panel for inputs ---------------------------------------

     sidebarPanel(
       width = 2,
       # Input: Drop down
       selectInput("regions", "Regions", choices = state_crashes$Regions, selected = as.factor(levels(state_crashes$Regions)[1])),
       
       div(style = "font-size: 15px",
           fluidRow(
             column(12,
                    h3("Sources"),
                    a("1. NHTSA 2021",
                      href = "https://www.nhtsa.gov/file-downloads?p=nhtsa/downloads/FARS/2021/National/"),
                    br(),
                    a("2. 2021 Population Estimates",
                      href = "http://eadiv.state.wy.us/pop/st-22est.htm")))),
       br(),
       br(),
       br(),
       br(),
       br(),
       br(),
       br()
       
       
     ),
    
    # Main panel for displaying outputs ----
    # Output: Tabset w/ plots and table ----
    
    mainPanel(
      # width=14,
      tabsetPanel(id = "tabs",type = "tabs",
                  tabPanel(id = "m","Mapping", 
                           
                           
                          div(
                            style = "margin-right:0",
                            column(width = 7,plotlyOutput('usmap')),
                            column(width = 5,plotlyOutput('barchart'))
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
      
    )
    
  )
)
)





# Define server logic required to draw a histogram
server <- function(input, output) {
 
  output$selected_option<- renderText({
    paste("The table below shows data about crashes that were due to ", input$vio, ".")
  })
  
  output$usmap <- renderPlotly({
    
    ## Use built in regions of USMAP to get plots by region
    if (input$regions == 'South'){
      plot_usmap(data = state_crashes, values = "crashes_per_100k",include = .south_region, labels = TRUE, color = "black") +
        scale_fill_continuous(low ="#9be396", high = "red", name = "Count", label = scales::comma) +
        labs(title = "Crash Rate - South Region") +
        theme(legend.position = "right",plot.background = element_rect(fill = "#cadbda"),
              panel.background = element_rect(fill='transparent'))
    }else if(input$regions == 'West'){
      plot_usmap(data = state_crashes, values = "crashes_per_100k", include = .west_region, labels = TRUE, color = "black") +
        scale_fill_continuous(low ="#9be396", high = "red", name = "Count", label = scales::comma) +
        labs(title = "Crash Rate - West Region") +
        theme(legend.position = "right",plot.background = element_rect(fill = "#cadbda"),
              panel.background = element_rect(fill='transparent'))
      
    }else if(input$regions == 'Midwest'){
      plot_usmap(data = state_crashes, values = "crashes_per_100k", include = .midwest_region, labels = TRUE, color = "black") +
        scale_fill_continuous(low ="#9be396", high = "red", name = "Count", label = scales::comma) +
        labs(title = "Crash Rate - Midwest Region") +
        theme(legend.position = "right",plot.background = element_rect(fill = "#cadbda"),
              panel.background = element_rect(fill='transparent'))
      
    }else{
      plot_usmap(data = state_crashes, values = "crashes_per_100k", include = .northeast_region, labels = TRUE, color = "black") +
        scale_fill_continuous(low ="#9be396", high = "red", name = "Count", label = scales::comma) +
        labs(title = "Crash Rate - Northeast Region") +
        theme(legend.position = "right",plot.background = element_rect(fill = "#cadbda"),
              panel.background = element_rect(fill='transparent'))
    }
    
  })
  
  output$barchart<- renderPlotly({
    
    if (input$regions == 'South'){
      dat<- state_crashes%>%
        filter(Regions == 'South')
      ggplot(dat, aes(x=state, y=crashes_per_100k)) + 
        geom_bar(stat = "identity", fill = "#254d4a") +
        labs(title = "Bar Plot of Crash Rate - South") +
        coord_flip()+
        theme(plot.background = element_rect(fill = "#cadbda"),
              panel.background = element_rect(fill='transparent'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    }else if(input$regions == 'West'){
      dat<- state_crashes%>%
        filter(Regions == 'South')
      ggplot(dat, aes(x=state, y=crashes_per_100k)) + 
        geom_bar(stat = "identity", fill = "#254d4a") +
        labs(title = "Bar Plot of Crash Rate - West ") +
        coord_flip()+
        theme(plot.background = element_rect(fill = "#cadbda"),
              panel.background = element_rect(fill='transparent'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    }else if(input$regions == 'North East'){
      dat<- state_crashes%>%
        filter(Regions == 'North East')
      ggplot(dat, aes(x=state, y=crashes_per_100k)) + 
        geom_bar(stat = "identity", fill = "#254d4a") +
        labs(title = "Bar Plot of Crash Rate - North East") +
        coord_flip()+
        theme(plot.background = element_rect(fill = "#cadbda"),
              panel.background = element_rect(fill='transparent'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    }else{
      dat<- state_crashes%>%
        filter(Regions == 'Midwest')
      ggplot(dat, aes(x=state, y=crashes_per_100k)) + 
        geom_bar(stat = "identity", fill = "#254d4a") +
        labs(title = "Bar Plot of Crash Rate - Midwest") +
        coord_flip()+
        theme(plot.background = element_rect(fill = "#cadbda"),
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
shinyApp(ui = ui, server = server)
