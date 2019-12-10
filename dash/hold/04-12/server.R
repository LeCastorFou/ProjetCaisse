
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
  
  output$dataFile <- DT::renderDataTable(class = "hover cell-border compact flotter", selection = "none",
                                         
                                         #  targets = c(3, 6, 8:10), searchable = FALSE,
                                         ## caption = "Rapport des ventes",
                                         # formatCurrency(8:10, '\U20AC', 2), # ???
                                         {
                                          
                                          df_expose = MyData$data
                                          # Gerer la selection des codes articles
                                          if(is.null(input$SelectCode)){df_expose = df_expose}
                                          else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
                                          
                                          df <- datatable(df_expose %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2]), 
                                                          extension = "Buttons",
                                                          filter='top',
                                                          colnames = c('Taux de TVA' = 'Ts %', 'Code article' = 'Code'),
                                                          options = list(
                                                                         #"columnDefs":  {"targets": c(3, 6, 8:10) , "searchable": false} 
                                                                         autoWidth = TRUE,
                                                                         dom = "lftiprB", 
                                                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                                                                         columnDefs = list(list(className = 'dt-center', targets = "") ),
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
                                           df_expose = MyData$data %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
                                           # Gerer la selection des codes articles
                                           if(is.null(input$SelectCode)){df_expose = df_expose}
                                           else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
                                           df_expose <- df_expose[,c('Prix','Mont.Soumis')]
                                           df_expose <- data.frame(Sommes=colSums(df_expose))
                                           
                                           df <- datatable(df_expose,
                                                           options=list(
                                                                        autoWidth = FALSE, 
                                                                        dom = "none",
                                                                        columnDefs = list(list(className = 'dt-center', targets = "")),
                                                                        lengthMenu = c(8, 10, 50, 1000)
                                                             )
                                                           )
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
      updateSelectizeInput(session, 'SelectCode', choices = unique( MyData$data[c("Code")] ) )
      
      MyDataSum$data <- MyData$data[,c('Prix','Mont.Soumis')]
      MyDataSum$data <- data.frame(Sommes=colSums(MyDataSum$data))
      
      
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
}








