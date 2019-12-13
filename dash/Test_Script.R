data <- read_delim("~/Dropbox/Cours_R/ProjetCaisse/Rapports_Articles-20191124131340.csv",
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
data <- data[,-3] 
dataFamilles  <- read.csv2("~/GitHub/R_caisse/dash/dataCodeRayons.csv")
colnames( dataFamilles )[-1]
dataFamilles[c('Famille')]
dataFamilles = subset(dataFamilles, select = -c(X) )
data$Code <- as.numeric(as.character(data$Code))
dataFamilles$Code <- as.numeric(as.character(dataFamilles$Code))

total = merge(data,dataFamilles,by="Code",all = TRUE)
total$Code <- as.factor(total$Code)
