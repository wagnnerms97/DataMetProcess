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

#load partitions-----------------------------------
#ui

#interface-----------------------------------------
ui <-
  dashboardPage(
    dashboardHeader(
      title = "DataMetProcess"
    ),#Header
    dashboardSidebar(
      useShinyjs(),
      shinyjs::extendShinyjs(text = jscode,functions = c('disableTab','disableTabMenu','disableSubMenu','enableTab','enableTabMenu','enableSubMenu')),
      sidebarMenu(
        menuItem(
          text = "About",
          tabName = "about-menu",
          icon = icon("circle-info")
        ),#menu

        menuItem(
          text = "Download Inmet",
          tabName = "Down-Inmet",
          icon = icon("download")
        ),#menu

        menuItem(
          text = "Data Processing",
          tabName = "Data-Process",
          icon = icon("gears"),
          menuSubItem("Load File",
                      tabName = "load-file",
                      icon = icon("upload")),
          menuSubItem("Process",
                      tabName = "process",
                      icon = icon("gear"))
        ),#menu

        menuItem(
          text = "Evapotranspiration",
          tabName = "evapo-pm",
          icon = icon("water"),
          menuSubItem("Load File",
                      tabName = "load-evapo",
                      icon = icon("upload")),
          menuSubItem("Evapotranspiration calculation",
                      tabName = "proc_evapo",
                      icon = icon("droplet")
          ))#menu
      )#Sidemenu
    ),#Sidebar
    dashboardBody(
      tags$head(includeCSS("./materials/styles.css")),
      tabItems(
        tabItem(
          tabName = "about-menu",
          ui_about()
        ),#tabItem

        tabItem(
          tabName = "Down-Inmet",
          ui_down_inmet("downinmet")
        ),#tabItem

        tabItem(
          tabName = "load-file",
          ui_load_file("tableupload")
        ),#tabItem

        tabItem(
          tabName = "process",
          ui_proc()
        ),#tabItem
        tabItem(
          tabName = 'load-evapo',
          ui_eto_load("upload_evapo")
        ),#tabItem
        tabItem(
          tabName = 'proc_evapo',
          ui_eto_calc("etocalc")
        )
      )#tabItems
    )#Body
  )#Page

