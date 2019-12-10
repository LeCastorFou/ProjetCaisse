
# ####################### # 
## SERVER ----
# ####################### #

server = function(input, output,session) {
  MyData <- reactiveValues()
  MyDataSum <- reactiveValues()
  
  output$tab_preview <- DT::renderDataTable(filter='none', rownames = F,
                                            colnames = c('Taux de TVA' = 'Ts %', 'Code article' = 'Code'), 
                                            {
                                            options(
                                                    DT.options = list(
                                                                      autoWidth = TRUE, dom = 'tip',  
                                                                      columnDefs = list(list(className = 'dt-center', targets = "")),
                                                                      pageLength = 6,
                                                                      lengthMenu = c(6, 10, 50, 100)
                                                                      )
                                                    )
                                            req(input$dataFile)
                                            
                                            dataFile <- read_delim(input$dataFile$datapath,
                                                                   ";", escape_double = FALSE,
                                                                    col_types = cols(
                                                                     Date = col_date(format = "%Y-%m-%d"), 
                                                                     Heure = col_time(""),
                                                                     Code = col_factor(),
                                                                     Designation = col_character(),
                                                                     `Qte` = col_number(),
                                                                     `Ts %` = col_factor(levels = c()),
                                                                     Mont.Total = col_number()),
                                                                   locale = locale(decimal_mark = ",", 
                                                                   encoding = "ISO-8859-1"), na = "null", 
                                                                   comment = "//", trim_ws = TRUE)
                                            
                                           dataFile <- dataFile[,-3] 
                                           },  options = list(pageLength = 6)
                                           )
  
  output$dataFile <- DT::renderDataTable(class = "hover cell-border compact",
                                         ## caption = "Rapport des ventes",
                                         # formatCurrency(8:10, '\U20AC', 2), # ???
                                         {
                                         
                                          df <- datatable(MyData$data %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2]),
                                                          extension = "Buttons",
                                                          filter='top',
                                                          colnames = c('Taux de TVA' = 'Ts %', 'Code article' = 'Code'),
                                                          options = list(
                                                                         autoWidth = TRUE,
                                                                         dom = "lftiprB", 
                                                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                                                                         columnDefs = list(list(className = 'dt-center', targets = "",
                                                                      # targets = c(3, 6, 8:10), searchable = FALSE,
                                                                targets = 5,
                                                                render = JS(
                                                                "function(data, type, row, meta) {",
                                                                "return type === 'display' && data.length > 14 ?",
                                                            "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                                                                          "}")
                                                                            )),
                                                                        
                                                                         pageLength = 8,
                                                                         lengthMenu = c(8, 10, 50, 1000)
                                             )) %>% formatStyle('Prix', color = 'red', backgroundColor = 'yellow', fontWeight = 'bold')

                                          }
                                          )
  
  output$dataFileSum <- DT::renderDataTable(
                                         {
                                           options(
                                                    DT.options = list(autoWidth = FALSE, 
                                                                     dom = "none",
                                                                     columnDefs = list(list(className = 'dt-center', targets = "")),
                                                                     lengthMenu = c(8, 10, 50, 1000)
                                           ))
                                    MyDataSum$data 
                                         }
  )
  output$dataFamilles <- DT::renderDataTable(filter='none', {
                                             
                  req(input$dataFamilles)
                  
                              dataFamilles  <- read.csv2(input$dataFamilles$datapath)
   }                             
  )
 
  # =========================================================================== =
  ## Preview ----
  # =========================================================================== =
 
  observeEvent(input$visualisation, {

    if(!is.null(input$dataFile)){
      
      MyData$data <- read_delim(input$dataFile$datapath,
                             ";", escape_double = FALSE,
                             col_types = cols(
                               Date = col_date(format = "%Y-%m-%d"), 
                               Heure = col_time(format = ""),
                               Code = col_factor(),
                               Designation = col_character(),
                               `Qte` = col_number(),
                               `Ts %` = col_factor(levels = c()),
                               Mont.Total = col_number()),
                             locale = locale(decimal_mark = ",", 
                                             encoding = "ISO-8859-1"), na = "null", 
                             comment = "//", trim_ws = TRUE 
      )
      
      MyData$data <- MyData$data[,-3] 
      MyData$data %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
      
      MyDataSum$data <- MyData$data[,c('Prix','Mont.Soumis')]
      MyDataSum$data <- data.frame(Sommes=colSums(dataFile$Data))
      
      sendSweetAlert(
        session = session,
        title = "Le fichier a bien été chargé !",
        text = "Les informations sont disponibles",
        type = "success"
      )

              updateTabItems(session,"tabs",selected= "tab_visualization")
    }
    else
    {

      sendSweetAlert( 
        session  =  session , 
        title  =  "Attention !!" , 
        text  =  "Veuillez sélectionner un fichier ..." , 
        type  =  "erreur" 
      )
    }
  })
  
  # observeEvent(input$DateRange, {
  # 
  #   # MyDataSum$data <- MyData$data[,c('Prix','Mont.Soumis')]
  #   # MyDataSum$data <- data.frame(Sommes=colSums(MyDataSum$data))
  #   # MyDataSum$data < - apply(MyDataSum$data, 2, sum)
  #   update(session,"tabs", selectded = "MyDataSum$Data")
  # })
}








