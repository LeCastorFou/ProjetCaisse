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
  MyDataNbClients <- reactiveValues()
  hot = reactiveValues()
  MasterData <- reactiveValues()
  PreviewData <- reactiveValues()
  
  output$tab_preview <- DT::renderDataTable(filter='none', rownames = F, selection = 'none',
                                            {
                                              options(
                                                DT.options = list(
                                                  autoWidth = TRUE, dom = 'tip',  
                                                  columnDefs = list(list(className = 'dt-center', targets = "")),
                                                  pageLength = 6,
                                                  lengthMenu = c(6, 10, 50, 100)
                                                )
                                              )
                                              req(input$MasterData) 
                                              if(input$marque=='1') {  
                                   ########  Bouton corriger les dates             
                                                 input$btDate 
      
                                                PreviewData <- readr::read_delim(input$MasterData$datapath
                                                                                 ,delim = ";", escape_double = FALSE
                                                                                 ,locale = locale(decimal_mark = ",", encoding = "ISO-8859-1")
                                                                                 , na = "null", comment = "//", trim_ws = TRUE)
                                              } 
                                              else if(input$marque=='2') {
                                                
                                                df <- readxl::read_excel(input$dataFile$datapath,
                                                )  }
                                              
                                              else if(input$marque=='0') {
                                                
                                                df <- read.csv2(input$dataFile$datapath,
                                                                header = as.logical(input$header),
                                                                sep = input$sep,
                                                                quote = input$quote,
                                                                nrows=5
                                                ) 
                                                df <- df[,-2:-4]
                                                df <- select(df, 1, 2 ,6  )
                                                df=df
                                                # boucle for sur la list (reactive value tout les nom de colonnes) - #if "nom " isin colname(df) : -  # pass - #else - #  df[,"nom colonne"] <- NA -   # View(df)
                                              }
                                            },  options = list(pageLength = 6)
  )
  
  output$MasterData <- DT::renderDataTable(class = "hover cell-border compact flotter", selection = "none", ## caption = "Rapport des ventes",
                                         {
                                           df_expose = MyData$data %>% filter(X1 >= input$DateRange[1] & X1 <= input$DateRange[2])
                                           dataRange_rmark <<- input$DateRange[1] 
                                           dataRangeF_rmark <<-  input$DateRange[2]
                                           # Gerer la selection des codes articles
                                           if(is.null(input$SelectCode)){df_expose = df_expose}
                                           else{df_expose = df_expose[df_expose$X5 %in% input$SelectCode, ]}
                                           # Gerer la selection des codes TVA
                                           if(is.null(input$SelectTVA)){df_expose = df_expose}
                                           else{df_expose = df_expose[df_expose[['X8']] %in% input$SelectTVA, ]}
                                           # Gerer la selection des codes familles
                                           if(is.null(input$SelectFamilles)){df_expose = df_expose}
                                           else{
                                             # print(input$SelectFamilles)
                                             # print(df_expose[c("Désignation.Famille")])
                                             df_expose = df_expose[df_expose$Désignation.Famille %in% input$SelectFamilles, ]
                                           } 
                                         
                                           if (file.exists("dataCodeRayons.rds"))
                                           { 
                                             df_expose = df_expose[,c("X1", "X2", "X4", "Famille", "X5", "X6", "X7", "X8", "Prix", "Mont.Soumis", "Mont.TVA", "Mont.Total")]
                                           }
                                           else if (file.exists("dataFamilles.rds"))
                                           {
                                             df_expose = df_expose[,c("X1", "X2", "X4", "Famille", "X5", "X6", "Désignation.Famille", "X7", "X8", "Prix", "Mont.Soumis", "Mont.TVA", "Mont.Total")]
                                           }
                                           else
                                           {
                                             df_expose = df_expose[,c("X1", "X2", "X4", "X5", "X6", "X7", "X8", "Prix", "Mont.Soumis", "Mont.TVA", "Mont.Total")]
                                           }

                                           df_expose_dataFile <<- df_expose
                                         
                                           df <- datatable(df_expose %>% filter(X1 >= input$DateRange[1] & X1 <= input$DateRange[2]),
                                                           extension = "Buttons",
                                                           selection = 'none',
                                                           filter='none',
                                                           options = list(
                                                             autoWidth = TRUE,
                                                             dom = "lftiprB", 
                                                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                                                             columnDefs = list(list(className = 'dt-center', targets = "") ),
                                                             pageLength = 8,
                                                             lengthMenu = c(8, 200, 500, 1000)
                                                           )) %>% formatCurrency("Mont.Total", currency = "  \U20AC  ", digits = 2, interval = 3, mark = " ") # %>% formatStyle('Prix', color = 'red', backgroundColor = 'yellow', fontWeight = 'bold')
                                         } )
  
  output$dataFileSum <- DT::renderDataTable(class = "hover cell-border compact flotter",
                                            {
                                              df_expose = MyData$data %>% filter(X1 >= input$DateRange[1] & X1 <= input$DateRange[2])
                                              # Gerer la selection des codes articles
                                              if(is.null(input$SelectCode)){df_expose = df_expose}
                                              else{df_expose = df_expose[df_expose$X5 %in% input$SelectCode, ]}
                                              # Gerer la selection des TVA
                                              if(is.null(input$SelectTVA)){df_expose = df_expose}
                                              else{df_expose = df_expose[df_expose$'X7' %in% input$SelectTVA, ]}
                                              # Gerer la selection des codes familles
                                              if(is.null(input$SelectFamilles)){df_expose = df_expose}
                                              else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
                                              
                                              df_expose_rmark <- df_expose[,c('Mont.Total','Mont.TVA', 'Mont.Soumis')]
                                              df_expose <- df_expose[,c('Mont.Total','Mont.TVA')]
                                       
                                              df_expose <- data.frame(Sommes=colSums(df_expose))
                                              df_expose_rmark <- data.frame(Sommes=colSums(df_expose_rmark))
                                              
                                              df_expose_SumT <<- df_expose_rmark
                                              
                                              df <- datatable(df_expose,
                                                              selection = 'none',
                                                              options=list(
                                                                autoWidth = FALSE, 
                                                                dom = "none",
                                                                columnDefs = list(list(className = 'dt-center')))
                                              ) %>% formatCurrency("Sommes", currency = "  \U20AC  ", digits = 2, interval = 3, mark = " ")
                                            })
  
  output$dataNbClients <- DT::renderDataTable(
    {
      df_expose = MyData$data %>% filter(X1 >= input$DateRange[1] & X1 <= input$DateRange[2])
      # Gerer la selection des codes articles
      if(is.null(input$SelectCode)){df_expose = df_expose}
      else{df_expose = df_expose[df_expose$X5 %in% input$SelectCode, ]}
      # Gerer la selection des TVA
      if(is.null(input$SelectTVA)){df_expose = df_expose}
      else{df_expose = df_expose[df_expose[['X8']] %in% input$SelectTVA, ]}
      # Gerer la selection des codes familles
      if(is.null(input$SelectFamilles)){df_expose = df_expose}
      else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
      
      df_expose = df_expose
      df_expose <- data.frame(length(unique( df_expose[,c('X4')] )) )
      colnames(df_expose) = c("  ")
      rownames(df_expose) = c('Nbr_de_tickets' ) 
      df_expose_NbClients <<- df_expose
      df <- datatable(df_expose, filter = 'none', 
                      selection = 'none',
                      options=list(
                        autoWidth = FALSE,
                        dom = "none",
                        columnDefs = list(list(className = 'dt-center')))
      )
    })
  
  output$dataCod.Rayons<- DT::renderDataTable(filter='none', rownames = F, editable = F, selection = 'none', {
    
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
                               print(MyData$data)
                               View(df_expose)
                                            df_expose <- df_expose %>% filter(X1 >= input$DateRange[1] & X1 <= input$DateRange[2])
                                            df_exposeDate <<- input$DateRange[1]
                                            df_exposeDateF <<- input$DateRange[2]
                                            df_Date <<- data.frame(df_exposeDate, df_exposeDateF)
                                            colnames(df_Date)=c('Début', 'Fin')
                                            # Gerer la selection des codes articles
                                            if(is.null(input$SelectCode)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$X5 %in% input$SelectCode, ]}
                                            # Gerer la selection des TVA
                                            if(is.null(input$SelectTVA)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$X8 %in% input$SelectTVA, ]}
                                            # Gerer la selection des codes familles
                                            if(is.null(input$SelectFamilles)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
                                            
                                            df_expose$Mont.Soumis <- as.numeric(df_expose$Mont.Soumis)
                                            df_expose$Mont.TVA <- as.numeric(df_expose$Mont.TVA) 
                                            df_expose$Mont.Total <- as.numeric(df_expose$Mont.Total)
                                            df_expose$X8 <- as.numeric(df_expose$X8)
                                            df_expose = aggregate(cbind(df_expose$Mont.Soumis,df_expose$Mont.TVA,df_expose$Mont.Total),
                                                                  by=list(Désignation.Famille=df_expose$Désignation.Famille), FUN=sum)
                                            
                                            df_expose$ColPcent <- df_expose$X8/sum(df_expose$X8)
                                            colnames(df_expose) = c('Désignation.Famille','Montant soumis', 'Montant TVA', 'Montant Total', 'Répartition des ventes')
                                            
                                            df_expose_dataBis <<- df_expose               
                                            df_expose <- datatable(df_expose, rownames = F, selection = 'none',
                                                                   extension = "Buttons",
                                                                   filter='none',
                                                                   options = list(
                                                                     # autoWidth = TRUE,
                                                                     dom = "lftiprB",
                                                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                                     pageLength = 15,
                                                                     lengthMenu = c(15, 20, 25, 30)
                                                                   )) %>% formatCurrency("Montant Total", currency = "  \U20AC  ", digits = 2, interval = 3, mark = " ") %>% formatPercentage('Répartition des ventes', digits = 3
                                                                   )
                                          } )
  
  output$MyDataTVA <- DT::renderDataTable(filter='none', rownames = F, selection = 'none',
                                          {
                                            df_expose = MyData$data
                                            df_expose <- df_expose %>% filter(X1 >= input$DateRange[1] & X1 <= input$DateRange[2])
                                        str(df_expose)    
                                            # Gerer la selection des codes articles
                                            if(is.null(input$SelectCode)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$X5 %in% input$SelectCode, ]}
                                            # Gerer la selection des TVA
                                            if(is.null(input$SelectTVA)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$X8 %in% input$SelectTVA, ]}
                                            # Gerer la selection des codes familles
                                            if(is.null(input$SelectFamilles)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
                                            
                                            df_expose$Mont.Soumis <- as.numeric(df_expose$Mont.Soumis)
                                            df_expose$Mont.TVA <- as.numeric(df_expose$Mont.TVA)
                                            df_expose$Mont.Total <- as.numeric(df_expose$Mont.Total)
                                            df_expose$X8 <- as.numeric(df_expose$X8)
                                            
                                            df_expose <- aggregate(cbind(df_expose$Mont.Soumis,df_expose$Mont.TVA,df_expose$Mont.Total),
                                                                   by=list(X8=df_expose$X8), FUN=sum)
                                            colnames(df_expose)=c('Taux de TVA','Montant HT','Montant TVA', 'Montant TTC') 
                                 print(df_expose)
                                     df_expose_dataTVA <<- df_expose                                            
                                            df <- datatable(df_expose, rownames = F, selection = 'none',
                                                            extension = "Buttons",
                                                            filter='none',
                                                            options = list(
                                                              # autoWidth = TRUE,
                                                              dom = "lftiprB",
                                                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                              pageLength = 5,
                                                              lengthMenu = c(5, 6, 7)
                                                            )) %>% formatCurrency("Montant TTC", currency = "  \U20AC  ", digits = 2, interval = 3, mark = " " )
                                          } )
  
  output$MyDataBisGraph <- renderPlot( {
    df_expose = MyData$data
    df_expose <- df_expose %>% filter(X1 >= input$DateRange[1] & X1 <= input$DateRange[2])
    
    # Gerer la selection des codes articles
    if(is.null(input$SelectCode)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose$X5 %in% input$SelectCode, ]}
    # Gerer la selection des TVA
    if(is.null(input$SelectTVA)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose[['X8']] %in% input$SelectTVA, ]}
    # Gerer la selection des codes familles
    if(is.null(input$SelectFamilles)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
    
    df_expose$Mont.Soumis <- as.numeric(df_expose$Mont.Soumis)
    df_expose$Mont.TVA <- as.numeric(df_expose$Mont.TVA)
    df_expose$Mont.Total <- as.numeric(df_expose$Mont.Total)
    
    df_expose = aggregate(cbind(df_expose$Mont.Soumis,df_expose$Mont.TVA,df_expose$Mont.Total),
                          by=list(Désignation.Famille=df_expose$Désignation.Famille), FUN=sum)
    df_expose$ColPcent <- df_expose$V3/sum(df_expose$V3/100)
    
    colnames(df_expose)
    colnames(df_expose) = c('Désignation.Famille','Montant soumis' , 'Montant TVA', 'Montant.Total', 'Répartition des ventes' )
    ggplot_Graph <<- df_expose
    ggplot(df_expose, aes(x=reorder(df_expose$Désignation.Famille, df_expose$Montant.Total), fill=df_expose$'Désignation.Famille', y=df_expose$Montant.Total))+geom_histogram(color="#4EDB1B", stat = 'identity')+theme_light() %>%
      +theme(axis.title = element_blank(), axis.text.x = element_text(color = 'steelblue', size = 8, hjust = 1, face = "italic") )+theme(axis.line.y = element_line(colour = "#50BFC9", linetype = "dotted", size = 1))+theme(legend.position = 'bottom', legend.title = element_blank()) %>%
      +theme(axis.line.x = element_line(colour = "lightblue", linetype = "dotdash"))+scale_fill_brewer(palette = "Set3")+scale_x_discrete(labels = abbreviate)#+scale_y_continuous(labels = scales::percent)#+scale_y_continuous(limits = c(0,20000))
  },height = 'auto',width = 'auto'
  )
  
  output$MyDataGraph2 <- renderPlot( {
    df_expose = MyData$data
    df_expose <- df_expose %>% filter(X1 >= input$DateRange[1] & X1 <= input$DateRange[2])
    
    # Gerer la selection des codes articles
    if(is.null(input$SelectCode)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose$X5 %in% input$SelectCode, ]}
    # Gerer la selection des TVA
    if(is.null(input$SelectTVA)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose[['X8']] %in% input$SelectTVA, ]}
    # Gerer la selection des codes familles
    if(is.null(input$SelectFamilles)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
    
    df_expose$Mont.Soumis <- as.numeric(df_expose$Mont.Soumis)
    df_expose$Mont.TVA <- as.numeric(df_expose$Mont.TVA)
    df_expose$Mont.Total <- as.numeric(df_expose$Mont.Total)
    
    df_expose = aggregate(cbind(df_expose$Mont.Soumis,df_expose$Mont.TVA,df_expose$Mont.Total),
                          by=list(Désignation.Famille=df_expose$Désignation.Famille), FUN=sum)
    df_expose$ColPcent <- df_expose$V3/sum(df_expose$V3) 
    
    colnames(df_expose)
    colnames(df_expose) = c('Désignation.Famille','Montant soumis' , 'Montant TVA', 'Montant.Total', 'Répartition des ventes' )
    ggplot_Graph2 <<- df_expose
    
    ggplot(df_expose, aes(x=reorder(df_expose$Désignation.Famille, df_expose$'Répartition des ventes'), fill=df_expose$'Désignation.Famille', y=df_expose$'Répartition des ventes')) %>%
      +geom_bar( color="#9BCD9B", stat="identity")+theme_light() %>% #+geom_text(aes(label = df_expose$'Répartition des ventes', df_expose$Désignation.Famille, df_expose$'Répartition des ventes'), vjust=1.6, color='white', size = 0.5) %>%
      +theme(axis.title = element_blank(), axis.text.x = element_text(color = 'steelblue', size = 8, hjust = 1, face = "italic") ) %>%
      +theme(axis.line.y = element_line(colour = "#50BFC9", linetype = "dotted", size = 1))+theme(legend.position = 'bottom', legend.title = element_blank() ) %>%
      +theme(axis.line.x = element_line(colour = "lightblue", linetype = "dotdash"))+scale_fill_brewer(palette = 'Set3')+scale_y_continuous(labels = scales::percent)+scale_x_discrete(labels = abbreviate)
    
  },height = 'auto',width = 'auto'
  )
  
  output$hot <- renderRHandsontable({
    if(is.null(input$hot)) {
      df <- readRDS("Rapports_Mode_de_paiement.rds")  
      df$Mont.Total <- as.numeric(df$Mont.Total)
      df_expose_TabMod2Paiement <<- df
    } else df <- hot_to_r(input$hot)
    df_expose_TabMod2Paiement <<- df 
    rhandsontable(df)
  })
  output$table <- renderTable(hot_to_r(req(input$hot)))
  output$table <- renderTable(if(is.null(input$hot)) return(df0) else return(hot_to_r((input$hot)))) 
  
  output$tabDataPay <- DT::renderDataTable(filter='none', rownames = F, editable = F, selection = 'none', {
    Info <- data.frame("Information"=c("Veuillez charger vos données d'encaissement"))
    # if(is.null) { 
    #   Info
    #   }
    # else{
    tabDataPay <- read_delim(input$dataPay$datapath, delim = ";",
                             escape_double = FALSE, quote = '"',
                             locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null", comment = "//", trim_ws = TRUE)
    # }
  } )
  # =========================================================================== =
  ## Preview ----
  # =========================================================================== =
  
  observeEvent(input$UploadFile, {
    updateTabItems(session,"tabs",selected= "tab_readData")
  } )
  observeEvent(input$ModFile, {
    updateTabItems(session, "tabs", selected = "tab_visualization")
  } )
  
  observeEvent(input$Dashboard, {
    updateTabItems(session, "tabs", selected = "tab_dashboard")
  } )
  
  # observeEvent(input$report, {
  #   updateTabItems(session, "tabs", selected = "tab_dashboard")
  #   
  # })
########################  
  observeEvent(input$merging, {
    
    req(input$dataCod.Rayons)
    
    dataCod.Rayons <- read_delim(input$dataCod.Rayons$datapath, delim = ";", col_names = F,
                                 escape_double = FALSE, quote = '"',
                                 locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null", 
                                 comment = "//", trim_ws = TRUE) 

    dataCod.Rayons <- dataCod.Rayons[-c(1),]
    
 #    if(input$marque=='1') {  
 #    dataCod.Rayons <- dataCod.Rayons[,c('X1','X2','X3','X4','X5', 'X6')]
 # colnames(dataCod.Rayons)=c('X5', 'Code barres','X6', 'Désignation 2', 'Famille')
 #     dataCod.Rayons <- dataCod.Rayons[,c('X5', 'Famille')]
                 #    }
     
    if (file.exists("dataFamilles.rds"))
    {
      dataFamilles <- readRDS(file = "dataFamilles.rds")
      dataFamilles<- dataFamilles[,c('Famille', 'Désignation.Famille')]
      MyData$data$Famille <- as.numeric(as.character(MyData$data$Famille))
      colnames(dataFamilles) <- c("Famille", "Désignation.Famille")
      total <- merge(MyData$data,dataFamilles,by="Famille",all = TRUE)
      total$Famille <- as.factor(total$Famille)
      MyData$data <- total
    }
    else
    {
      MyData$data$X5 <- as.numeric(as.character(MyData$data$X5))
      dataCod.Rayons$X5 <- as.numeric(as.character(dataCod.Rayons$X5))
      total <- merge(MyData$data,dataCod.Rayons,by="X5",all = TRUE)
      total$X5 <- as.factor(total$X5)
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
##########################  
  observeEvent(input$mergingF, {
    
    req(input$dataFamilles)
    
    dataFamilles <- read_delim(input$dataFamilles$datapath,
                               delim = ";", escape_double = FALSE, quote = '"',
                               locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null", 
                               comment = "//", trim_ws = TRUE) 
 
    dataFamilles=dataFamilles[-c(1),]
    dataFamilles <- dataFamilles[,c('X1','X2')]
    colnames(dataFamilles)=c('X5', 'Famille')
    
    if (file.exists("dataCodeRayons.rds"))
    {    
      dataCod.Rayons <- readRDS(file = "dataCodeRayons.rds")
      colnames(dataCod.Rayons)=c('X5', 'Famille')
      dataCod.Rayons <- dataCod.Rayons[,c('X5', 'Famille')]
     # colnames(dataFamilles) <- c("Famille", "Désignation.Famille")
      MyData$data$X5 <- as.numeric(as.factor(MyData$data$X5))
      dataCod.Rayons$X5 <- as.numeric(as.factor(dataCod.Rayons$X5))
      
      total <- merge(MyData$data,dataCod.Rayons,by='X5',all = TRUE)
      total$X5 <- as.factor(total$X5)
      MyData$data <- total   
   str(MyData$data)
   View(MyData$data)
      saveRDS(dataFamilles, file = "dataFamilles.rds")
      
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
    
    if(input$marque=='0' | input$marque=='2') {  
      sendSweetAlert(
        session  =  session , 
        title  =  "Attention !!" , 
        text  =  "Le programme ne permet pas l'affichage de ses informations ..." , 
        type  =  "erreur" 
      ) }
    
    else if(!is.null(input$MasterData)){
      
      MyData$data <- readr::read_delim(input$MasterData$datapath
                                       ,col_names = F, skip = 2
                                       ,delim = ";", escape_double = FALSE
                                       ,locale = locale(decimal_mark = ",", encoding = "ISO-8859-1"), na = "null" 
                                       ,comment = "//", trim_ws = TRUE 
      )
       if(input$btDate=='1') { 
        MyData$data$X1 <- as.Date(MyData$data$X1, "%d/%m/%Y")
       } 

      MyData$data <- MyData$data[,-3,]
      colnames(MyData$data )=c('X1', 'X2', 'X4', 'X5', 'X6', 'X7', 'X8', 'Prix', 'Mont.Soumis', 'Mont.TVA', 'Mont.Total') 
      str(MyData$data)
      MyData$data %>% filter(X1 >= input$DateRange[1] & X1 <= input$DateRange[2])
      
      # Merge avec le fichier des codes articles si il existe dans le répertoire courant
      if (file.exists("dataCodeRayons.rds"))
      { 
        #  print("file dataCodeRayons exist")
        dataCod.Rayons <- readRDS(file = "dataCodeRayons.rds")
        str(dataCod.Rayons)
       # colnames(dataCod.Rayons)=c('X5', "Famille")
        # MyData$data$X5 <- as.numeric(as.character(MyData$data$X5))
        # dataCod.Rayons$X5 <- as.numeric(as.character(dataCod.Rayons$X5))
        # MyData$data$X8 <- as.numeric(as.character(MyData$data$X8))
        #  MyData$data$Famille <- as.factor(as.character(MyData$data$Famille) )

        total <- merge(MyData$data,dataCod.Rayons,by="X5",all = TRUE)
        total$X5 <- as.factor(total$X5)
        MyData$data <- total
      }
      
      # Merge avec le fichier des codes familles si il existe dans le répertoire courant
      if (file.exists("dataFamilles.rds"))
      {
        dataFamilles <- readRDS("dataFamilles.rds")
        thecolname  = colnames( dataFamilles)[-1]

        total <- merge(MyData$data,dataFamilles,by="Famille",all = TRUE)
        total$Famille <- as.factor(total$Famille)
        MyData$data <- total

        myu <- na.omit(unique( MyData$data[c('Désignation.Famille')] ))
        colnames(myu) <- c('thecolname')
        myu <- myu[order(myu$thecolname),]
        updateSelectizeInput(session, 'SelectFamilles', choices = myu )
     View(myu)
         }
      MyData$data <- MyData$data[,c(3,4,5,2,6,7,8,9,10,11,12,1,13)]

      MyData$data <- MyData$data[order( MyData$data$X2),]
      MyData$data <- MyData$data[order( MyData$data$X1),]
      MyData$data$X8 <- as.numeric(as.character(MyData$data$X8))
      MyData$data$Famille <- as.numeric(as.character(MyData$data$Famille))
      
      myu <- na.omit(unique( MyData$data[c('X5')] ))
      colnames(myu) <- c('X5')
      myu <- myu[order(myu$X5),]
      updateSelectizeInput(session, 'SelectCode', choices = myu )
      
      myu <- na.omit(unique( MyData$data[c('X8')] ))
      colnames(myu) <- c('X8')
      myu <- myu[order(myu$X8),]
      updateSelectizeInput(session, 'SelectTVA', choices = myu )
      
      MyDataSum$data <- MyData$data[,c('Prix','Mont.TVA')]
      MyDataSum$data <- data.frame(Sommes=colSums(MyDataSum$data))
      df_MyData_data <<- MyData$data

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
      ) }
  })
  
  observeEvent(input$saveRBtn, {
    
    dataCod.Rayons <- read_delim(input$dataCod.Rayons$datapath, delim = ";",
                                 escape_double = FALSE, quote = '"',
                                 locale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null", 
                                 comment = "//", trim_ws = TRUE) 
    dataCod.Rayons <- dataCod.Rayons[,c(-6:-53)]
    dataCod.Rayons <- dataCod.Rayons[,c(-2:-4)]
    colnames(dataCod.Rayons )=c('X5', 'Famille')
    dataCod.Rayons$Famille <- as.numeric(dataCod.Rayons$Famille)
    dataCod.Rayons$X5 <- as.numeric(dataCod.Rayons$X5)

    saveRDS(dataCod.Rayons, file = "dataCod.Rayons.rds"
    )
    
    sendSweetAlert(
      session = session,
      title = "Le fichier a bien été sauvegarder !",
      text = "Les informations sont disponibles",
      type = "success"
    ) }
  )
  
  observeEvent(input$saveFBtn, {
    
    dataFamilles <- read_delim(input$dataFamilles$datapath, delim = ";"
                               ,escape_double = FALSE, quote = '"'
                               ,ocale = locale(decimal_mark = ",",  encoding = "ISO-8859-1"), na = "null"
                               ,comment = "//", trim_ws = TRUE)
    dataFamilles <- dataFamilles[,c(-4:-14)]
    dataFamilles <- dataFamilles[,-3]
    dataFamilles <- dataFamilles[,c('Code', 'Désignation')]
    colnames(dataFamilles)=c('Famille', 'Désignation.Famille')
    saveRDS(dataFamilles, file = "dataFamilles.rds")
    
    sendSweetAlert(
      session = session,
      title = "Le fichier a bien été sauvegarder !",
      text = "Les informations sont disponibles",
      type = "success"
    ) }
  )
  
  observeEvent(input$SelectFamilles, {
    df <- MyData$data
    df = df[df$Désignation.Famille %in% input$SelectFamilles, ]
    updateSelectizeInput(session, 'SelectCode', choices = unique(df[c('X5')] ) )
  } )
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(""), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      params <- list(n = input$df_expose_dataFile, o = input$df_expose_SumT, p = input$df_expose_dataBis, q = input$df_expose_dataTVA, s = input$df_expose_NbClients, t = input$hot,
                     a = input$ggplot_Graph2, b=input$ggplot_Graph, c=input$ggplot_Graph,  d = df_Date, e = input$df_expose_TabMod2Paiement)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}
