
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
                                                                      # `Ts %` = col_factor(levels = c()),
                                                                       Mont.Total = col_number()),
                                                                     locale = locale(decimal_mark = ",", 
                                                                                     encoding = "ISO-8859-1"), na = "null", 
                                                                     comment = "//", trim_ws = TRUE)
                                              
                                              dataFile <- dataFile[,-3] 
                                            },  options = list(pageLength = 6)
  )
  
  output$dataFile <- DT::renderDataTable(class = "hover cell-border compact flotter", selection = "none",
                                         
                                         ## caption = "Rapport des ventes",
                                         # formatCurrency(8:10, '\U20AC', 2), # ???
                                         {
                                           
                                           df_expose = MyData$data
                                           # Gerer la selection des codes articles
                                           if(is.null(input$SelectCode)){df_expose = df_expose}
                                           else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
                                           # Gerer la selection des codes TVA
                                           if(is.null(input$SelectTVA)){df_expose = df_expose}
                                           else{df_expose = df_expose[df_expose$'Ts %' %in% input$SelectTVA, ]}
                                           
                                           df <- datatable(df_expose %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2]), 
                                                           extension = "Buttons",
                                                           filter='none',
                                                           colnames = c('Taux de TVA' = 'Ts %', 'Code article' = 'Code'),
                                                           options = list(
                                                             #"columnDefs":  {"targets": c(3, 6, 8:10) , "searchable": false} 
                                                             autoWidth = TRUE,
                                                             dom = "lftiprB", 
                                                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                                                             columnDefs = list(list(className = 'dt-center', targets = "") ),
                                                             pageLength = 8,
                                                             lengthMenu = c(8, 200, 500, 1000)
                                                           )) %>% formatStyle('Prix', color = 'red', backgroundColor = 'yellow', fontWeight = 'bold')
                                           
                                         }
  )
  
  output$dataFileSum <- DT::renderDataTable(
    {
      
      df_expose = MyData$data %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
      # Gerer la selection des codes articles
      if(is.null(input$SelectCode)){df_expose = df_expose}
      else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
      # Gerer la selection des TVA
      if(is.null(input$SelectTVA)){df_expose = df_expose}
      else{df_expose = df_expose[df_expose$'Ts %' %in% input$SelectTVA, ]}
      
      df_expose <- df_expose[,c('Prix','Mont.TVA')]
      df_expose <- data.frame(Sommes=colSums(df_expose))
      
      df <- datatable(df_expose,
                      options=list(
                        autoWidth = FALSE, 
                        dom = "none",
                        columnDefs = list(list(className = 'dt-center') 
                        )
                      )
      )
    }
  )
  output$dataCod.Rayons<- DT::renderDataTable(filter='none', {
    
    req(input$dataCod.Rayons)
    
    dataCod.Rayons <- read.csv2(input$dataCod.Rayons$datapath) 
  } 
  )
  output$dataFamilles<- DT::renderDataTable(filter='none', {
    
    req(input$dataFamilles)
    
    dataFamilles <- read.csv2(input$dataFamilles$datapath)
  } 
  )
  
  # =========================================================================== =
  ## Preview ----
  # =========================================================================== =
  
  observeEvent(input$merging, {
    
      req(input$dataCod.Rayons)
      
 dataCod.Rayons <- read.csv2(input$dataCod.Rayons$datapath)
 dataCod.Rayons<- dataCod.Rayons[,c('Code', 'Code.barres', 'Désignation', 'Désignation.2', 'Famille')]
      MyData$data$Code <- as.numeric(as.character(MyData$data$Code))
      dataCod.Rayons$Code <- as.numeric(as.character(dataCod.Rayons$Code))
      
      total <- merge(MyData$data,dataCod.Rayons,by="Code",all = TRUE)
      total$Code <- as.factor(total$Code)
      MyData$data <- total
      write.csv2(dataCod.Rayons, file = "dataCod.Rayons.csv")
      
      sendSweetAlert(
        session  =  session , 
        title  =  "Succes !!" , 
        text  =  "Ce fichier sera dorénavant utiliser pour libeller les articles ..." , 
        type  =  "success" 
      )
     
     })
  
  observeEvent(input$merging, {

    req(input$dataFamilles)

    dataFamilles <- read.csv2(input$dataFamilles$datapath)
    dataFamilles<- dataFamilles[,c('Code', 'Désignation')]
    MyData$data$Code <- as.numeric(as.character(MyData$data$Code))
    dataFamilles$Code <- as.numeric(as.character(dataFamilles$Code))

    total <- merge(MyData$data,dataFamilles,by="Code",all = TRUE)
    total$Code <- as.factor(total$Code)
    MyData$data <- total
    write.csv2(dataFamilles, file = "dataFamilles.csv")

    sendSweetAlert(
      session  =  session ,
      title  =  "Succes !!" ,
      text  =  "Ce fichier sera dorénavant utiliser pour libeller les codes familles ..." ,
      type  =  "success"
    )

  })
  
  
  
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
                                  # `Ts %` = col_factor(levels = c()),
                                  Mont.Total = col_number()),
                                locale = locale(decimal_mark = ",", 
                                                encoding = "ISO-8859-1"), na = "null", 
                                comment = "//", trim_ws = TRUE 
      )
      
      MyData$data <- MyData$data[,-3,]
    MyData$data <- MyData$data[, c("Date", "Heure", "Réf.Doc.", "Code", "Désignation", "Qté", "Ts %", "Prix", "Mont.Soumis","Mont.TVA","Mont.Total")]
      MyData$data %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
      
      # Merge avec le fichier des codes articles si il existe dans le répertoire courant
      if (file.exists("dataCod.Rayons.csv"))
      { 
        print("file exist")
   dataCod.Rayons <- read.csv2("dataCod.Rayons.csv")
   dataCod.Rayons<- dataCod.Rayons[,c('Code', 'Code.barres', 'Désignation', 'Désignation.2', 'Famille')]
        MyData$data$Code <- as.numeric(as.character(MyData$data$Code))
        dataCod.Rayons$Code <- as.numeric(as.character(dataCod.Rayons$Code))
        
        total <- merge(MyData$data,dataCod.Rayons,by="Code",all = TRUE)
        total$Code <- as.factor(total$Code)
        MyData$data <- total
      }
      
    #  Merge avec le fichier des codes familles si il existe dans le répertoire courant
      # if (file.exists("dataFamilles.csv"))
      # {
      #   print("file exist")
      #   dataFamilles <- read.csv2("dataFamilles.csv")
      #  # dataFamilles<- dataFamilles[,c('Code', 'Désignation')]
      #   MyData$data$Code <- as.numeric(as.character(MyData$data$Code))
      #   dataFamilles$Code <- as.numeric(as.character(dataFamilles$Code))
      # 
      #   total <- merge(MyData$data,dataFamilles,by="Code",all = TRUE)
      #   total$Code <- as.factor(total$Code)
      #   MyData$data <- total
      # }
      
      updateSelectizeInput(session, 'SelectCode', choices = unique( MyData$data[c("Code")] ) )
      updateSelectizeInput(session, 'SelectTVA', choices = unique( MyData$data[c("Ts %")] ) )
      updateSelectInput(session, 'SelectFamilles', choices = unique( MyData$data[c("Famille")] ) )
      
      MyDataSum$data <- MyData$data[,c('Prix','Mont.TVA')]
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








