
# ####################### # 
## SERVER ----
# ####################### #

server = function(input, output, session) {
  
  MyData <- reactiveValues(df_data = NULL)
  
  output$tab_preview <- DT::renderDataTable(filter='none',
                                            colnames = c('Taux de TVA' = 'Ts %', 'Code article' = 'Code'), {
                                              options(DT.options = list(autoWidth = TRUE, dom = 'tip',  
                                                                        columnDefs = list(list(className = 'dt-center', targets = "")),
                                                                        pageLength = 6,
                                                                        lengthMenu = c(6, 10, 50, 100)
                                              ))
                                              req(input$dataFile)
                                              
                                              dataFile <- read_delim(input$dataFile$datapath,
                                                                     ";", escape_double = FALSE,
                                                                     col_types = cols(
                                                                       `Date` = col_date(format = "%Y-%m-%d"), 
                                                                      `Heure`= col_time(format = ""),
                                                                      `Code article` = col_factor(),
                                                                     `DÃ©signation` = col_character(),
                                                                      `QtÃ©` = col_number(),
                                                                      `Taux de TVA` = col_factor(levels = c()),
                                                                       `Mont.Total` = col_number()),
                                                                     locale = locale(decimal_mark = ",", 
                                                                                     encoding = "ISO-8859-1"), na = "null", 
                                                                     comment = "//", trim_ws = TRUE)
                                              
                                              dataFile <- dataFile[,-3] 
                                            },  options = list(pageLength = 6))
  
  output$dataFile <- DT::renderDataTable(extension = "Buttons", filter='top', class = "hover cell-border compact",
                                         # formatStyle('Prix', color = 'red', backgroundColor = 'yellow', fontWeight = 'bold'),
                                         options = list(
                                           #  targets = c(3, 6, 8:10), searchable = FALSE,
                                           dom = "lftiprB",
                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
                                         ## caption = "Rapport des ventes",
                                         colnames = c('Taux de TVA' = 'Ts %', 'Code article' = 'Code'),
                                         # formatCurrency(8:10, '\U20AC', 2), # ???
                                         {
                                           
                                MyData$data
                                         }
  )
  output$SomData <- DT::renderDataTable(
                                         {
                                           colnms=c("Prix", "Mont.Soumis")
                                           df = rowSums( MyData$data[,colnms])
                                         }
  )
  #                                          options(DT.options = list(autoWidth = TRUE, 
  #                                                                    columnDefs = list(list(className = 'dt-center', targets = ""
  #                                                                                           
  #                                                                    )),
  #                                                                    pageLength = 8,
  #                                                                    lengthMenu = c(8, 10, 50, 1000)
  #                                          ))
  #                                          
  #                                          dataFile <- read_delim(input$dataFile$datapath,
  #                                                                 ";", escape_double = FALSE,
  #                                                                 col_types = cols(
  #                                                                   `Date` = col_date(format = "%Y-%m-%d"), 
  #                                                                  `Heure`= col_time(format = ""),
  #                                                                    `Code article` = col_factor(),
  #                                                                    `DÃ©signation` = col_character(),
  #                                                                   `QtÃ©` = col_number(),
  #                                                                   `Taux de TVA` = col_factor(levels = c()),
  #                                                                   `Mont.Total` = col_number()),
  #                                                                 locale = locale(decimal_mark = ",", 
  #                                                                 encoding = "ISO-8859-1"), na = "null", 
  #                                                                 comment = "//", trim_ws = TRUE 
  #                                          )
  #                                          
  #                                          dataFile <- dataFile[,-3] 
  #                                          dataFile %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
  #                                        },
  # )
  
  # =========================================================================== =
  ## Preview ----
  # =========================================================================== =
  
  observe(input$tab_preview, {

    if(!is.null(input$dataFile$datapath)){
      data$table = read.csv(input$dataFile$datapath,
                            header = as.logical(input$header),
                            sep = input$sep,
                            quote = input$quote)
      updateTabItems(session, "tabs", selected = "tab_preview")
    }
  }
  )
  
  
     observeEvent(input$visualisation, {
  ### Modal a garder ou modifier ...
  if(!is.null(input$dataFile ))
  {
    showModal(modalDialog(title = "Fichier OK","On va à la page suivante"))
    
    ################################
    updateTabItems(session, "tabs", selected = "tab_visualization")
    options(DT.options = list(autoWidth = TRUE, 
                              columnDefs = list(list(className = 'dt-center', targets = ""
                                                     
                              )),
                              pageLength = 8,
                              lengthMenu = c(8, 10, 50, 1000)
    ))
    
    MyData$data <- read_delim(input$dataFile$datapath,
                              ";", escape_double = FALSE,
                              col_types = cols(
                                Date = col_date(format = "%Y-%m-%d"), 
                                Heure = col_time(format = ""),
                                Code = col_factor(),
                                Désignation = col_character(),
                                `Qté` = col_number(),
                                `Ts %` = col_factor(levels = c()),
                                Mont.Total = col_number()),
                              locale = locale(decimal_mark = ",", 
                                              encoding = "ISO-8859-1"), na = "null", 
                              comment = "//", trim_ws = TRUE 
    )
    
    MyData$data <- MyData$data[,-3] 
    MyData$data %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
  }
})

  observeEvent(input$visualisation, {

    if(!is.null(choose.files(default = ""))){
      sendSweetAlert(
        session = session,
        title = "Done !",
        text = "Le fichier a bien Ã©tÃ© lu !",
        type = "success"
      )
      updateTabItems(session, "tabs", selected = "tab_visualization")
    }
  })
}






