
## BIBLIOTHEQUE ----
# ####################### #

library(shiny)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(data.table)

# ####################### #
## UI ----
# ####################### # 
rm(list = ls())

ui <- dashboardPage(
  dashboardHeader(title = "ELAN INFO", titleWidth = 300),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(id = "tabs",
                               menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
                               menuItem("Lecture de donnees", tabName = "tab_readData", icon = icon("bar-chart-o")),
                               menuItem("Visualiser les donnees", tabName = "tab_visualization", icon = icon("eye")),
                               menuItem("Gestion des familles", tabName = "tab_familles", icon = icon("fas fa-cog"))
                   )
  ),
body <- dashboardBody(
    tabItems(
      tabItem(tabName = "tab_visualization", 
              fluidRow(
                column(width = 9, offset = -1,
                       h2("visualiser les donnees"),
                       fluidRow(
                         column(width = 9, offset = -1,
                                dateRangeInput("DateRange", 
                                               label = "Selectionner une plage de date :",
                                               # start = as.Date("),
                                               separator = " / ")))
                       
                )),
              DT::DTOutput("dataFile")),
              DT::DTOutput(`SomData`),
       DT::DTOutput(`SomData`), # class="cell-border compact stripe",width = "100%", height = "auto"),
    tabItem(tabName = "tab_dashboard", 
            h2("Tableau de bord")),
    tabItem(tabName = "tab_readData",
            h3("Importation des donnees"),
            fileInput("dataFile",label = NULL,
                      buttonLabel = "Navigateur...",
                      placeholder = "Selectionner un fichier"),
            fluidRow(
              column(width = 3, offset = -1,
                     h3("Parametres"),
                     radioButtons(inputId = "header",
                                  label = "Entête",
                                  choices = c("oui"= TRUE, "non"= FALSE),
                                  selected = TRUE, inline=T)),
              fluidRow(                    
                column(width = 5, offset = 1,
                       h2(""),
                       radioButtons(inputId = "sep", 
                                    label = "Separateur",
                                    choices = c(Virgule = ",",
                                                'Point Virgule' = ";",
                                                Tabulation = "\t"),
                                    selected = ";", inline=T)),
                fluidRow(
                  column(width = 8, offset = -2, 
                         h2(""),
                         radioButtons(inputId = "quote",
                                      label = "Guillemet",
                                      choices = c(Aucun = "",
                                                  "Double Guillemets" = '"',
                                                  "Single Guillemet" = "'"),
                                      selected = "", inline=T)),
                  tags$br(),
                  (actionButton(inputId = "visualisation", label = "visualiser", icon = icon("play") )
                  )),
                fluidRow(
                  column(width = 9, offset = 1,
                         h3("Fichier previsualise"),
                         fluidRow(
                           column(width = 1, offset = -1,
                                  DTOutput("tab_preview"))))
                )
              )
            )
    ),
    tabItem(tabName = "tab_familles",
            fluidRow(
              column(width = 9, offset = -1,
                     h3("Gestion des familles")
              )
            )
    )
  ) 
),

skin = c("black")
)
