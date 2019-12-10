
## BIBLIOTHEQUE ----
# ####################### #

library(shiny)
library(shinythemes)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(data.table)
library(shinyWidgets)

# ####################### #
## UI ----
# ####################### # 
rm(list = ls())

ui <- dashboardPage(
  dashboardHeader(title = "ELAN INFO", titleWidth = 300),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(id = "tabs",
                               menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
                               menuItem("Lecture des informations ", tabName = "tab_readData", icon = icon("bar-chart-o")),
                               menuItem("Visualiser des informations", tabName = "tab_visualization", icon = icon("eye")),
                               menuItem("Gestion des familles", tabName = "tab_familles", icon = icon("fas fa-cog"))
                   )
  ),
  
  body <- dashboardBody(
    
    tabItems(
      tabItem(tabName = "tab_visualization",
              
              fluidRow(
                column(
                  width = 2, offset = -1,
                  h3("Filtre des ventes"), DTOutput("dataFileSum")
                ),
                column(
                  width = 3, offset = 1,
                  tags$br(),
                  dateRangeInput("DateRange",label = "Selectionner une plage de date :", start = now(), separator = " / "),
                  selectizeInput('SelectCode', 'Selectionner Code Article', choices = " ", multiple = TRUE)
                ),
                column(
                  width = 3, offset = 1,
                  tags$br(),
                  selectizeInput('SelectFamilles', 'Selectionner les familles', choices = " ", multiple = TRUE),
                  selectizeInput('SelectTVA', 'Selectionner la TVA', choices = " ", multiple = TRUE)
                )
                
              ),
              DTOutput("dataFile")
      ),
      
      tabItem(
        tabName = "tab_dashboard", 
        h2("Tableau de bord"),
        shinythemes::themeSelector()
      ),
      
      tabItem(
        tabName = "tab_readData",
        fluidRow(
          column(width = 4, offset = -1,
                 h3("Informations de codage"),
                 fileInput("dataFile",label = NULL,buttonLabel = "Navigateur...", placeholder = "Selectionner un fichier"),
                 fluidRow(
                   column(width = 5, offset = 11,
                          radioButtons(
                            inputId = "header",
                            label = "Entete",
                            choices = c("oui"= TRUE, "non"= FALSE),
                            selected = TRUE, inline=T
                          )
                   )
                 )
          ),
          fluidRow(                    
            column(
              width = 4, offset = 3,
              h2(""),
              radioButtons(
                inputId = "sep", 
                label = "Separateur",
                choices = c(Virgule = ",",'Point Virgule' = ";",Tabulation = "\t"),
                selected = ";", 
                inline=T
              )
            ),
            fluidRow(
              column(
                width = 4, offset = 1,
                h2(""),
                radioButtons(
                  inputId = "quote",
                  label = "Guillemet",
                  choices = c(Aucun = "","Double Guillemets" = '"',"Single Guillemet" = "'"),
                  selected = "", inline=T
                )
              ),
              fluidRow(
                column(
                  width = 11, offset = 1,
                  (actionButton(inputId = "visualisation", label = "visualiser", icon = icon("play"))),
                  # tags$br(),
                  h3("Fichier d'informations"),
                  fluidRow(
                    column(
                      width = 11, offset = -1,
                      DTOutput("tab_preview")
                    )
                  )
                )
              )
            )
          )
        )),
      tabItem(
        tabName = "tab_familles",
        fluidRow(
          column(
            width = 9, 
            offset = -1,
            h3("Gestion des familles"),
            fileInput("dataFamilles",label = NULL,buttonLabel = "Navigateur...", placeholder = "Selectionner un fichier"),
            mainPanel(
              tabsetPanel(
                tabPanel("Table des familles"),
                DTOutput("dataFamilles"),
                tabPanel("Table des rayons"),
                DTOutput("dataCodeRayons")
              )
            ),
            actionButton(inputId = "merging", label = "utiliser", icon = icon("upload"))
          )
        )
      )
    )
  ),
  
  skin = c("green")
)
