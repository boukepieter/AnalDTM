setwd("c:/Users/REACH-IRQ-GIS/Documents/Impact R packages/AnalDTM")
library(shiny)
library(xlsx)
library(spatial)
library(sp)
library(rgdal)
library(raster)
library(plotKML)
library(plyr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(rgeos)
library(plotly)
source("functions.R")
source("functions_shiny.R")

ui <- fluidPage( 
  tags$style(type = "text/css", 
             "html, body {width:100%;height:100%}",
             ".leaflet .legend i{
                   width: 10px;
                   height: 10px;
                   margin-top: 4px;
                   }
                   "
  ),
  navbarPage("DTM Analysis tool",
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
             tabPanel("Input",
                      fluidRow(column(1,br()),
                               column(3,radioButtons(inputId="inputType","Data Type:",c("DTM - Returnee"="Returnee",
                                                                                        "DTM - IDP (NOT OPERATIONAL YET)"="IDP"))),
                               column(3,fileInput(inputId="target_file", "Choose target file"))),
                      fluidRow(
                        column(1,br()),
                        column(3,
                               radioButtons(inputId="useDatabase","Make use of database?",c("yes"="yes","no"="no"))),
                        column(3,
                               conditionalPanel(condition = "input.useDatabase == 'yes'",
                                                fileInput(inputId="databasefile", "Select database file")),
                               conditionalPanel(condition = "input.useDatabase == 'no'",
                                                fileInput(inputId="bline_file", "Select baseline file")))),
                      fluidRow(actionButton("run","RUN!",width ="300",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                               align = "center")),
             tabPanel("Output",
                      radioButtons(inputId="mapType","Map Type:",c("Points"="points","Districts"="districts",
                                                                   "Subdistricts"="subdistricts")),
                      conditionalPanel("condition=input.mapType=='points'",fluidRow(
                        column(12, leafletOutput("map_points",height="600px")))),
                      conditionalPanel("condition=input.mapType=='districts'",fluidRow(
                        column(7, leafletOutput("map_districts",height="600px")),
                        column(5, plotlyOutput("plot_districts", height="300px")))),
                      conditionalPanel(condition="input.mapType=='subdistricts'",fluidRow(
                        column(7, leafletOutput("map_subdistricts",height="600px")),
                        column(5, plotlyOutput("plot_subdistricts", height="300px")))),
                      conditionalPanel(condition="input.mapType=='points'",
                                       fluidRow(DT::dataTableOutput("table_points"))),
                      conditionalPanel(condition="input.mapType=='districts'",
                                       fluidRow(DT::dataTableOutput("table_districts"))),
                      conditionalPanel(condition="input.mapType=='subdistricts'",
                                       fluidRow(DT::dataTableOutput("table_subdistricts")))), selected="Input"
  ))
server <- function(input, output){
  rv <- reactiveValues(Clicks=list())
  
  
  
  observeEvent(input$run,{
    
    
    withProgress(message = 'Running analysis', style="notification", detail = "", value = 0,{
      target_file <- input$target_file$name
      target_sheet <- "Sheet"
      Family_column <- ifelse(input$inputType=="IDP", 10, 12)
      
      ##### Prepare environment #####
      date <- substr(target_file,nchar(target_file)-16,nchar(target_file)-5)
      date <- as.Date(date, format="%b %d, %Y")
      if (dir.exists(sprintf('%s/%s',"output/shiny_output",date))==F){dir.create(sprintf('%s/%s',"output/shiny_output",date))}
      
      ##### READ IN DATA #####
      target <- read.DTM(target_file, target_sheet, columns=Family_column)
      incProgress(1/5)
      ### Baseline file
      compareWithDatabase <- ifelse(input$useDatabase=="yes", TRUE,FALSE)
      if (compareWithDatabase){
        databasefile <- input$databasefile$datapath
        bline <- read.csv(databasefile, stringsAsFactors = F)
      } else {
        bline_file <- input$bline_file$name
        bline_sheet <- "Sheet"
        bline <- read.DTM(bline_file, bline_sheet, columns=Family_column)
      }
      incProgress(2/5)
      ##### LOCATION LEVEL ANALYSIS #####
      result_point <- DTM.point.analysis.shiny(target, bline, date, target_file,  
                                               bline.is.dbase=compareWithDatabase)
      incProgress(3/5)
      
      ##### DISTRICT LEVEL ANALYSIS #####
      district_shapes <- readOGR("IAU_DIBs_SubDistricts/districts.shp","districts")
      projection(district_shapes) <- WGS84
      
      result_district <- DTM.area.analysis.shiny(target, bline, district_shapes, sumname="A2NameEn",date, target_file, 
                                                     bline.is.dbase=compareWithDatabase)
      incProgress(4/5)
      
      ##### SUB-DISTRICT LEVEL ANALYSIS #####
      # load in shapes
      subdistricts <- readOGR("IAU_DIBs_SubDistricts/irq_polbnda_adm3_500k_UNAMI_PA.shp","irq_polbnda_adm3_500k_UNAMI_PA")
      projection(subdistricts) <- WGS84
      
      result_subdistrict <- DTM.area.analysis.shiny(target, bline, subdistricts, sumname="ADM3_Eng_n",date, target_file, 
                                                     bline.is.dbase=compareWithDatabase) 
    })
    
    
    #### Make map reactive:
    dist_click <- reactiveValues(clickedShape=NULL)
    subdist_click <- reactiveValues(clickedShape=NULL)
    
    output$map_districts <- renderLeaflet(result_district$m)
    observeEvent(input$map_districts_shape_click,{
      dist_click$clickedShape <- input$map_districts_shape_click
      print(dist_click$clickedShape$id)
    })
    output$table_districts <- DT::renderDataTable(result_district$table)
    output$map_points <- renderLeaflet(result_point$m)
    output$table_points <- DT::renderDataTable(result_point$table)
    output$map_subdistricts <- renderLeaflet(result_subdistrict$m)
    observeEvent(input$map_subdistricts_shape_click,{
      subdist_click$clickedShape <- input$map_subdistricts_shape_click
      print(subdist_click$clickedShape$id)
    })
    output$table_subdistricts <- DT::renderDataTable(result_subdistrict$table)
    
    output$plot_districts=renderPlotly({
      my_place=dist_click$clickedShape$id
      if(is.null(my_place)){my_place="place1"}
      if(my_place=="place1"){
      }else{
        no <- match(my_place,names(result_district$graphs))
        print(ggplotly(result_district$graphs[[no]]))
      }
    })
    output$plot_subdistricts=renderPlotly({
      my_place=subdist_click$clickedShape$id
      if(is.null(my_place)){my_place="place1"}
      if(my_place=="place1"){
      }else{
        no <- match(my_place,names(result_subdistrict$graphs))
        print(ggplotly(result_subdistrict$graphs[[no]]))
      }
    })
    
    # output$downloadData <- downloadHandler(
    #   filename = function() {
    #     paste(input$dataset, ".csv", sep = "")
    #   },
    #   content = function(file) {
    #     write.csv(datasetInput(), file, row.names = FALSE)
    #   }
    # )
  })
  
}
shinyApp(ui=ui, server=server)