#=========================
library(tidyverse)
library(plotly)
library(shiny)
library(shinyWidgets)
library(DT)
library(readxl)
library(rsconnect)
#=========================

#===================================================
# Load data (Datasaurus dozen "DatasaurusDozen.tsv")
#===================================================

URL <- "https://www.autodesk.com/content/dam/autodesk/www/autodesk-reasearch/Publications/pdf/The%20Datasaurus%20Dozen.zip"

temp <- tempfile()

download.file(URL, temp, method = "libcurl", mode = "wb")

unzip(temp, files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)

datasauraus <- read_tsv("The Datasaurus Dozen/DatasaurusDozen.tsv")


ds <- datasauraus %>% 
  group_by(dataset) %>% 
  tally()

ds <- as.list(ds$dataset)


#---Colours-----
abscol <- c("#336699", "#669966", "#99CC66", "#993366", "#CC9966", "#666666", 
            "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
            "#B3DE69", "#FCCDE5","#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

#==================
# UI 
#==================  
  
ui <- fluidPage(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    titlePanel("Datasauraus Explorer"),
    
    sidebarPanel(
      
      radioButtons('Plot_type', 'Plot type:', choices = c("scatter", "frequency", "violin", "box"), selected = "scatter", inline = TRUE),
      
      pickerInput('dataset', 'Select one or multiple datasets to compare', choices = c(ds), 
                  selected = c("dino", "h_lines", "circle", "bullseye", "away", "dots"), multiple = TRUE),
      
      radioButtons('facet_row', 'Single or multiple plots:', choices = c("multiple", "single"), selected = "multiple", inline = TRUE),
    
      # adding the div tag to the sidepanel
      
      tags$div(class="header", checked=NA, br(),
               tags$strong("Data source:")),
      tags$div(class="header", checked=NA,
               tags$a(href="https://www.autodesk.com/research/publications/same-stats-different-graphs",
                      target="_blank", "Same Stats, Different Graphs: Generating Datasets with Varied Appearance and Identical Statistics through Simulated Annealing"), 
               br(),
               br(),
               tags$a(href="https://github.com/Atyepa/ACSF-Australia-ShinyApp/blob/d19521291c7442584deb0e54c29644dd91547d0e/ACSF2019-20.R",
                      target="_blank", "See the ShinyApp code at GitHub"))
      
      
        
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                tabPanel("Plot", plotlyOutput('plot')),
                tabPanel("Summary statistics", DT::dataTableOutput('Table')))
      )
  )
  
  ## Server
  server <- function(input, output) {
    
    DS <- reactive({
      list(dataset=input$dataset)
    })
    
    data <- reactive({ datasauraus %>% 
        filter(dataset %in% DS()$dataset)
    })
      
    Table <-   reactive({ datasauraus %>% 
        filter(dataset %in% DS()$dataset) %>% 
        group_by(dataset) %>% 
        summarise(
        
        `mean (x)` = round(mean(x),2),
        `mean (y)` = round(mean(y),2), 
        `SD (x)` = round(sd(x),2),
        `SD (y)` = round(sd(y),2),
        `Correl (x,y)` = round(cor(x,y, method = "pearson"),2),
        `min (x)` = round(min(x),2),
        `min (y)` = round(min(y),2),
        `max (x)` = round(max(x),2),
        `max (y)` = round(max(y),2)
        )
        })
    
    output$plot <- renderPlotly({
      
      if(input$Plot_type == "scatter"){
        p <- ggplot(data(), aes(x= x, y= y, colour = dataset)) + geom_point() 
          }
      
      if(input$Plot_type == "frequency"){
        p <- ggplot(data(), aes(x= x,  colour = dataset)) + geom_density() 
      }
      
      if(input$Plot_type == "violin"){
        p <- ggplot(data(), aes(x= dataset, y= y, colour = dataset)) + geom_violin() 
      }
      
      if(input$Plot_type == "box"){
        p <- ggplot(data(), aes(x= dataset, y= y, colour = dataset)) + geom_boxplot() 
      }
      
      
      if (input$facet_row == "multiple" & (input$Plot_type == "scatter" | input$Plot_type == "frequency")){
      p <- p + facet_wrap(~ dataset)
      }
      
      if (input$facet_row == "multiple" & input$Plot_type == "box"){
        p <- p }
      
     
      
      ggplotly(p, height = 800, width = 950, autosize= TRUE)
    
  })
  output$Table <- DT::renderDataTable({
    Table()
  })  
    
  }

  #========================================  
  shinyApp(ui, server)
  #========================================
