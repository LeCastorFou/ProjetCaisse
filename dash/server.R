# ####################### # 
## SERVER ----
# ####################### #

server = function(input, output,session) {
  MyData <- reactiveValues()
  MyDataSum <- reactiveValues()
  MyDataBis <- reactiveValues()
  MyDataTVA <- reactiveValues()
  MyDataBisGraph <- reactiveValues()
  MYdataFamilles <- reactiveValues()
  MYdataCod.Rayons <- reactiveValues()
  dataPay <- reactiveValues()
  TabMod2Paiement <- reactiveValues()
  TabMod2P <- reactiveValues()
  tabDataPay <- reactiveValues()
 
 # if(input$marque$Marques == "1") {
    
  output$tab_preview <- DT::renderDataTable(filter='none', rownames = F, selection = 'none',
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
                                                                       Mont.Total = col_number()),
                                                                     locale = locale(decimal_mark = ",", 
                                                                                     encoding = "ISO-8859-1"), na = "null", 
                                                                     comment = "//", trim_ws = TRUE)
                                              
                                              dataFile <<- dataFile[,-3] 
                                              
                                            },  options = list(pageLength = 6)
  )
  
  output$dataFile <- DT::renderDataTable(class = "hover cell-border compact flotter", selection = "none", ## caption = "Rapport des ventes",
                                         {
                                           df_expose = MyData$data # %>% arrange(desc("Date", "Heure")) 
                                           # Gerer la selection des codes articles
                                           if(is.null(input$SelectCode)){df_expose = df_expose}
                                           else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
                                           # Gerer la selection des codes TVA
                                           if(is.null(input$SelectTVA)){df_expose = df_expose}
                                           else{df_expose = df_expose[df_expose$'Ts %' %in% input$SelectTVA, ]}
                                           # Gerer la selection des codes familles
                                           if(is.null(input$SelectFamilles)){df_expose = df_expose}
                                           else{
                                             df_expose = df_expose[df_expose$Désignation.Famille %in% input$SelectFamilles, ]
                                           } 
                                           if (file.exists("dataCodeRayons.rds"))
                                           { 
                                             df_expose = df_expose[,c("Date", "Heure", "Réf.Doc.", "Famille", "Code", "Désignation", "Qté", "Ts %", "Prix", "Mont.Soumis", "Mont.TVA", "Mont.Total")]
                                           }
                                           else if (file.exists("dataFamilles.rds"))
                                           {
                                             df_expose = df_expose[,c("Date", "Heure", "Réf.Doc.", "Famille", "Code", "Désignation", "Désignation.Famille", "Qté", "Ts %", "Prix", "Mont.Soumis", "Mont.TVA", "Mont.Total")]
                                           }
                                           else
                                           {
                                             df_expose = df_expose[,c("Date", "Heure", "Réf.Doc.", "Code", "Désignation", "Qté", "Ts %", "Prix", "Mont.Soumis", "Mont.TVA", "Mont.Total")]
                                           }
                                           
                                           df <- datatable(df_expose %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2]),
                                                           extension = "Buttons",
                                                           selection = 'none',
                                                           filter='none',
                                                           colnames = c('Taux de TVA' = 'Ts %', 'Code article' = 'Code'),
                                                           options = list(
                                                             autoWidth = TRUE,
                                                             dom = "lftiprB", 
                                                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                                                             columnDefs = list(list(className = 'dt-center', targets = "") ),
                                                             pageLength = 8,
                                                             lengthMenu = c(8, 200, 500, 1000)
                                                           )) %>% formatCurrency("Mont.Total", currency = "  \U20AC  ", digits = 2, interval = 3, mark = " ")
                                           # %>% formatStyle('Prix', color = 'red', backgroundColor = 'yellow', fontWeight = 'bold')
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
      # Gerer la selection des codes familles
      if(is.null(input$SelectFamilles)){df_expose = df_expose}
      else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
      
      df_expose <- df_expose[,c('Mont.Total','Mont.TVA')]
      df_expose <- data.frame(Sommes=colSums(df_expose))
      
      df <- datatable(df_expose,
                      selection = 'none',
                      options=list(
                        autoWidth = FALSE, 
                        dom = "none",
                        columnDefs = list(list(className = 'dt-center')))
      ) %>% formatCurrency("Sommes", currency = "  \U20AC  ", digits = 2, interval = 3, mark = " ")
    }) 
  
  output$dataCod.Rayons<- DT::renderDataTable(filter='none', rownames = F, editable = F, {
    
    req(input$dataCod.Rayons)
    
    dataCod.Rayons <- read_delim(input$dataCod.Rayons$datapath, delim = ";",
                                 escape_double = FALSE, quote = '"',
                                 locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null", 
                                 comment = "//", trim_ws = TRUE) 
    dataCod.Rayons <- dataCod.Rayons[,c(-6:-53)]
  })
  
  output$dataFamilles <- DT::renderDataTable(filter='none', rownames = F, editable = F, selection = 'none', {
    
    req(input$dataFamilles)
    
    dataFamilles <- read_delim(input$dataFamilles$datapath, delim = ";",
                               escape_double = FALSE, quote = '"',
                               locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null", 
                               comment = "//", trim_ws = TRUE)
    dataFamilles <- dataFamilles[,c(-3:-14)]
    
  })
  
  output$MyDataBis <- DT::renderDataTable(filter='none', rownames = F, selection = 'none',
                                          {
                                            df_expose = MyData$data 
                                            df_expose <- df_expose %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
                                            
                                            # Gerer la selection des codes articles
                                            if(is.null(input$SelectCode)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
                                            # Gerer la selection des TVA
                                            if(is.null(input$SelectTVA)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$'Ts %' %in% input$SelectTVA, ]}
                                            # Gerer la selection des codes familles
                                            if(is.null(input$SelectFamilles)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
                                            
                                            df_expose$Mont.Soumis <- as.numeric(df_expose$Mont.Soumis)
                                            df_expose$Mont.TVA <- as.numeric(df_expose$Mont.TVA) 
                                            df_expose$Mont.Total <- as.numeric(df_expose$Mont.Total)
                                            
                                            df_expose = aggregate(cbind(df_expose$Mont.Soumis,df_expose$Mont.TVA,df_expose$Mont.Total),
                                                                  by=list(Désignation.Famille=df_expose$Désignation.Famille), FUN=sum)
                                          
                                            df <- datatable(df_expose, rownames = F,
                                                            colnames = c('Montant soumis' = 'V1', 'Montant TVA' = 'V2', 'Montant Total' = 'V3'),
                                                            extension = "Buttons",
                                                            filter='none',
                                                            options = list(
                                                              # autoWidth = TRUE,
                                                              dom = "lftiprB",
                                                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                              pageLength = 15,
                                                              lengthMenu = c(15, 20, 25, 30)
                                                            )) %>% formatCurrency("Montant Total", currency = "  \U20AC  ", digits = 2, interval = 3, mark = " ")
                                          }
  )
  
  output$MyDataTVA <- DT::renderDataTable(filter='none', rownames = F,
                                          {
                                            df_expose = MyData$data
                                            df_expose <- df_expose %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
                                            
                                            # Gerer la selection des codes articles
                                            if(is.null(input$SelectCode)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
                                            # Gerer la selection des TVA
                                            if(is.null(input$SelectTVA)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$'Ts %' %in% input$SelectTVA, ]}
                                            # Gerer la selection des codes familles
                                            if(is.null(input$SelectFamilles)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
                                            
                                            df_expose$Mont.Soumis <- as.numeric(df_expose$Mont.Soumis)
                                            df_expose$Mont.TVA <- as.numeric(df_expose$Mont.TVA)
                                            df_expose$Mont.Total <- as.numeric(df_expose$Mont.Total)
                                            
                                            df_expose <- aggregate(cbind(df_expose$Mont.Soumis,df_expose$Mont.TVA,df_expose$Mont.Total),
                                                                   by=list('Ts %'=df_expose$'Ts %'), FUN=sum)
                                            
                                        
                                            df <- datatable(df_expose, rownames = F,
                                                            colnames = c('Taux de TVA' = 'Ts %', 'Montant TVA' = 'V2', 'Montant HT'= 'V1', 'Montant TTC' = "V3"),
                                                            extension = "Buttons",
                                                            filter='none',
                                                            options = list(
                                                              # autoWidth = TRUE,
                                                              dom = "lftiprB",
                                                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                              pageLength = 5,
                                                              lengthMenu = c(5, 6, 7)
                                                            )) %>% formatCurrency("Montant TTC", currency = "  \U20AC  ", digits = 2, interval = 3, mark = " " )
                                          }
  )
  
  output$MyDataBisGraph <- renderPlot( {
    df_expose = MyData$data
    df_expose <- df_expose %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
    
    # Gerer la selection des codes articles
    if(is.null(input$SelectCode)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
    # Gerer la selection des TVA
    if(is.null(input$SelectTVA)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose$'Ts %' %in% input$SelectTVA, ]}
    # Gerer la selection des codes familles
    if(is.null(input$SelectFamilles)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
    
    df_expose$Mont.Soumis <- as.numeric(df_expose$Mont.Soumis)
    df_expose$Mont.TVA <- as.numeric(df_expose$Mont.TVA)
    df_expose$Mont.Total <- as.numeric(df_expose$Mont.Total)
    
    df_expose = aggregate(cbind(df_expose$Mont.Soumis,df_expose$Mont.TVA,df_expose$Mont.Total),
                          by=list(Désignation.Famille=df_expose$Désignation.Famille), FUN=sum)
    colnames(df_expose)
    colnames(df_expose) = c('Désignation.Famille','Montant soumis' , 'Montant TVA', 'Montant.Total' )
    ggplot(df_expose[,c('Désignation.Famille','Montant.Total')], aes(x=Désignation.Famille, y=Montant.Total))+geom_bar(stat="identity")   
    
  },height = 'auto',width = 'auto'
  )
  
  output$TabMod2Paiement <- renderRHandsontable( {
    attach(TabMod2Paiement)
    TabMod2Paiement <- read_delim("Rapports_Mode_de_paiement.RData", delim = ";",
                                  escape_double = FALSE, quote = '"',
                                  locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null", 
                                  comment = "//", trim_ws = TRUE) 
    
    TabMod2Paiement$Mont.Total <- as.numeric(TabMod2Paiement$Mont.Total)
    TabMod2Paiement$T.Règlement <- as.integer(TabMod2Paiement$T.Règlement)
    rhandsontable(TabMod2Paiement)
  }  
   )
  
  output$tabDataPay <- DT::renderDataTable(filter='none', rownames = F, editable = F, selection = 'none', {
    
    tabDataPay <- read_delim(input$dataPay$datapath, delim = ";",
                             escape_double = FALSE, quote = '"',
                             locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null",
                             comment = "//", trim_ws = TRUE)
  }  
    )
  
  # =========================================================================== =
  ## Preview ----
  # =========================================================================== =
  
  observeEvent(input$TabMod2Paiement, {
    
    saveRDS(hot_to_r(input$TabMod2Paiement), file = "Rapports_Mode_de_paiement.rds")
  }
    )
  observeEvent(input$UploadFile, {
    updateTabItems(session,"tabs",selected= "tab_readData")
  })
  observeEvent(input$ModFile, {
    updateTabItems(session, "tabs", selected = "tab_visualization")
  })
  
  observeEvent(input$Dashboard, {
    updateTabItems(session, "tabs", selected = "tab_dashboard")
  })
  
  observeEvent(input$merging, {
    
    req(input$dataCod.Rayons)
    
    dataCod.Rayons <- read_delim(input$dataCod.Rayons$datapath, delim = ";",
                                 escape_double = FALSE, quote = '"',
                                 locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null", 
                                 comment = "//", trim_ws = TRUE) 
    dataCod.Rayons <- dataCod.Rayons[,c('Code', 'Famille')]
    if (file.exists("dataFamilles.rds"))
    {
      dataFamilles <- readRDS(file = "dataFamilles.rds")
     # dataFamilles <- read.table("dataFamilles.csv", header = T, sep = ";", quote = '"', dec = ".")
      dataFamilles = subset(dataFamilles, select = -c(X) )
      dataFamilles<- dataFamilles[,c('Famille', 'Désignation.Famille')]
      MyData$data$Famille <- as.numeric(as.character(MyData$data$Famille))
      colnames(dataFamilles) <- c("Famille", "Désignation.Famille")
      total <- merge(MyData$data,dataFamilles,by="Famille",all = TRUE)
      total$Famille <- as.factor(total$Famille)
      MyData$data <- total
    }
    else
    {
      MyData$data$Code <- as.numeric(as.character(MyData$data$Code))
      dataCod.Rayons$Code <- as.numeric(as.character(dataCod.Rayons$Code))
      total <- merge(MyData$data,dataCod.Rayons,by="Code",all = TRUE)
      total$Code <- as.factor(total$Code)
      MyData$data <- total
    }
    saveRDS(dataCod.Rayons, file = "dataCodeRayons.rds")
    
    sendSweetAlert(
      session  =  session , 
      title  =  "Succes !!" , 
      text  =  "Ce fichier sera dorénavant utiliser pour libeller les articles ..." , 
      type  =  "success" 
    )
  })
  
  observeEvent(input$mergingF, {
    
    req(input$dataFamilles)
    
    dataFamilles <- read_delim(input$dataFamilles$datapath, delim = ";",
                               escape_double = FALSE, quote = '"',
                               locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null", 
                               comment = "//", trim_ws = TRUE)
    dataFamilles<- dataFamilles[,c('Code', 'Désignation')]
    if (file.exists("dataCodeRayons.rds"))
    {    
      dataCod.Rayons <- readRDS(file = "dataCodeRayons.rds")
     # dataCod.Rayons <- read.table("dataCodeRayons.csv",header = T, sep = ";", quote = '"', dec = ".")
      dataCod.Rayons <- dataCod.Rayons[,c('Code', 'Famille')]
      colnames(dataFamilles) <- c("Famille", "Désignation.Famille")
      MyData$data$Code <- as.numeric(as.character(MyData$data$Code))
      dataCod.Rayons$Code <- as.numeric(as.character(dataCod.Rayons$Code))
      
      total <- merge(MyData$data,dataCod.Rayons,by="Code",all = TRUE)
      total$Code <- as.factor(total$Code)
      MyData$data <- total   
      
      saveRDS(dataFamilles, file = "dataFamilles.rds")
     # write.csv2(dataFamilles, file = "dataFamilles.csv")
      sendSweetAlert(
        session  =  session ,
        title  =  "Succes !!" ,
        text  =  "Ce fichier sera dorénavant utiliser pour libeller les codes familles ..." ,
        type  =  "success"
      )
    }
    else 
    {
      sendSweetAlert(
        session  =  session ,
        title  =  "attention !!" ,
        text  =  "Veuillez charger la table codes rayons en premier" ,
        type  =  "warning"
      )
    }
  })
  
  observeEvent(input$visualisation, {
    
    if(!is.null(input$dataFile)){
      
      MyData$data <- read_delim(input$dataFile$datapath,
                                ";", escape_double = FALSE,
                                col_types = cols(
                                  `Date` = col_date(format = "%Y-%m-%d"), 
                                  `Heure` = col_time(format = ""),
                                  `Code` = col_factor(),
                                  `Désignation` = col_character(),
                                  `Qte` = col_number(),
                                  # `Ts %` = col_factor(levels = c()),
                                  Mont.Total = col_number()),
                                locale = locale(decimal_mark = ",", 
                                                encoding = "ISO-8859-1"), na = "null", 
                                comment = "//", trim_ws = TRUE 
      )
      
      MyData$data <- MyData$data[,-3,]

      MyData$data %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
      
      # Merge avec le fichier des codes articles si il existe dans le répertoire courant
      if (file.exists("dataCodeRayons.rds"))
      { 
        print("file dataCodeRayons exist")
        # dataCod.Rayons <- read.csv2("dataCodeRayons.csv")  # dataCod.Rayons = subset(dataCod.Rayons, select = -c(X) )
        dataCod.Rayons <- readRDS(file = "dataCodeRayons.rds")
        
        MyData$data$Code <- as.numeric(as.character(MyData$data$Code))
        dataCod.Rayons$Code <- as.numeric(as.character(dataCod.Rayons$Code))
        
        total <- merge(MyData$data,dataCod.Rayons,by="Code",all = TRUE)
        total$Code <- as.factor(total$Code)
        MyData$data <- total
      }
      
      # Merge avec le fichier des codes familles si il existe dans le répertoire courant
      if (file.exists("dataFamilles.rds"))
      {
        print("file dataFamilles exist")
        dataFamilles <- readRDS("dataFamilles.rds")
        MyData$data$Famille <- as.numeric(as.character(MyData$data$Famille))
        dataFamilles$Famille <- as.numeric(as.character(dataFamilles$Famille))
        # dataFamilles = subset(dataFamilles, select = -c(X) )
        thecolname  = colnames( dataFamilles)[-1]
        
        total <- merge(MyData$data,dataFamilles,by="Famille",all = TRUE)
        total$Famille <- as.factor(total$Famille)
        MyData$data <- total
        
        myu <- na.omit(unique( MyData$data[c('Désignation.Famille')] ))
        colnames(myu) <- c('thecolname')
        myu <- myu[order(myu$thecolname),]
        print(myu)
        updateSelectizeInput(session, 'SelectFamilles', choices = myu )
        # updateSelectInput(session, 'SelectFamilles', choices = unique( MyData$data[c(thecolname)] ) )
      }
      MyData$data <- MyData$data[order( MyData$data$Heure),]
      MyData$data <- MyData$data[order( MyData$data$Date),]
      # updateSelectizeInput(session, 'SelectCode', choices = unique( MyData$data[c("Code")] ) )
      myu <- na.omit(unique( MyData$data[c("Code")] ))
      colnames(myu) <- c('Code')
      myu <- myu[order(myu$Code),]
      updateSelectizeInput(session, 'SelectCode', choices = myu )
      
      # updateSelectizeInput(session, 'SelectTVA', choices = unique( MyData$data[c("Ts %")] ) )
      myu <- na.omit(unique( MyData$data[c("Ts %")] ))
      colnames(myu) <- c('TS')
      myu <- myu[order(myu$TS),]
      updateSelectizeInput(session, 'SelectTVA', choices = myu )
      
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
  
  observeEvent(input$saveRBtn, {
    
    dataCod.Rayons <- read_delim(input$dataCod.Rayons$datapath, delim = ";",
                                 escape_double = FALSE, quote = '"',
                                 locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null", 
                                 comment = "//", trim_ws = TRUE) 
    dataCod.Rayons <- dataCod.Rayons[,c(-6:-53)]
    dataCod.Rayons <- dataCod.Rayons[,c(-2:-4)]
    saveRDS(dataCod.Rayons, file = "dataCod.Rayons.rds"
  )
    sendSweetAlert(
      session = session,
      title = "Le fichier a bien été sauvegarder !",
      text = "Les informations sont disponibles",
      type = "success"
    )
   }
  )
  
  observeEvent(input$saveFBtn, {
    
    dataFamilles <- read_delim(input$dataFamilles$datapath, delim = ";",
                               escape_double = FALSE, quote = '"',
                               locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null",
                               comment = "//", trim_ws = TRUE)
    dataFamilles <- dataFamilles[,c(-4:-14)]
    dataFamilles <- dataFamilles[,-3]
    dataFamilles <- dataFamilles[,c('Code', 'Désignation')]
    colnames(dataFamilles) <- c("Famille", "Désignation.Famille")
    saveRDS(dataFamilles, file = "dataFamilles.rds")
    
    sendSweetAlert(
      session = session,
      title = "Le fichier a bien été sauvegarder !",
      text = "Les informations sont disponibles",
      type = "success"
    )
   }
  )
 }
# else {
   # output$tab_preview <- renderDataTable(filter='none',{
   # 
   # 
   #   req(input$dataFile)
   # 
   #   df <- read.csv2(input$dataFile$datapath,
   #                   header = as.logical(input$header),
   #                   sep = input$sep,
   #                   quote = input$quote,
   #                   nrows=5
   #   )
   # },  options = list(scrollX = TRUE , dom = 't'))
   # 
   # output$dataFile <- renderDataTable(filter='top',{
   #   req(input$dataFile)
   #   dataFile <- read_delim(input$dataFile$datapath,
   #                          ";", quote = "\\\"", escape_double = FALSE,
   #                          locale = locale(date_names = "fr",
   #                                          decimal_mark = ",",
   #                                          encoding = "ISO-8859-1"),
   #                          trim_ws = TRUE)
   # 
   #   dataFile %>% filter('Date' >= input$dateRange[1] & 'Date' <= input$dateRange[2])
   # 
   # },  options = list(scrollX = TRUE , dom = 't')
   # )
# }
 # observe(input$tab_preview, {
 # 
 #   if(!is.null(input$dataFile$datapath)){
 #     data$table = read.csv(input$dataFile$datapath,
 #                           header = as.logical(input$header),
 #                           sep = input$sep,
 #                           quote = input$quote)
 #     updateTabItems(session, "tabs", selected = "tab_preview")
 #   }
 # })
    
#}
