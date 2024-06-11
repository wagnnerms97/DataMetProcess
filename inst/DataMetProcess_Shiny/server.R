#load packages-------------------------------------
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinybusy)
library(shinyalert)
library(shinyWidgets)
library(reactable)
library(plotly)
library(tidyverse)
library(zoo)
library(lubridate)
library(janitor)


#server--------------------------------
server <-
  function(input, output,session){

    js$disableTabMenu('Plots')
    js$disableSubMenu('process')
    js$disableSubMenu('proc_evapo')

    #images
    output$logoabout <- renderImage({
      list(src = "./materials/Logo.png")
    },deleteFile = FALSE)

    output$ufrpe <- renderImage({
      list(src = "./materials/UFRPE2.png",
           height = "70px",width= "43px")
    },deleteFile = FALSE)

    output$uast <- renderImage({
      list(src = "./materials/UAST2.png",
           height = "70px",width= "70px")
    },deleteFile = FALSE)

    output$pgea <- renderImage({
      list(src = "./materials/PGEA.png",
           height = "70px",width= "90px")
    },deleteFile = FALSE)

    output$capes <- renderImage({
      list(src = "./materials/CAPES.png",
           height = "70px",width= "76px")
    },deleteFile = FALSE)

    output$cnpq <- renderImage({
      list(src = "./materials/CNPQ.png",
           height = "70px",width= "227px")
    },deleteFile = FALSE)

    output$facepe <- renderImage({
      list(src = "./materials/FACEPE.png",
           height = "70px",width= "196px")
    },deleteFile = FALSE)



    serverDownInmet("downinmet")



    #process
    TableInput_ex <- serverUpload("tableupload")
    TablesInputs <- serverProcess("processtabs",TableInput_ex)
    serverCharts("ui_grafic",TablesInputs[[1]],TablesInputs[[2]],TablesInputs[[3]])

    observeEvent(TablesInputs[[3]](),{
      if(nrow(TablesInputs[[3]]())>0){
        js$enableTabMenu("Plots")
      }
    })

    #evapo
    Table_ETO <- serverUpETo("upload_evapo")
    serverEvapo("etocalc",Table_ETO)

  }
