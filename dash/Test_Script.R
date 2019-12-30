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

myu <- na.omit(unique( data[c("Ts %")] ))
colnames(myu) <- c('TS')
myu[order(myu$TS),]



saveRDS(data, file = "my_data.rds")
df <- readRDS(file = "my_data.rds")


data <- data[,c("Mont.Soumis","TD")]
data %>% group_by(TD) %>% summarise(Mont.Soumis = sum(Mont.Soumis))


data <- data[,-3] 
dataFamilles  <- read.csv2("~/GitHub/R_caisse/dash/dataCodeRayons.csv")
colnames( dataFamilles )
df = dataFamilles[,c('Code','Famille')]
colnames(df) = c('A','B')
df
dataFamilles = subset(dataFamilles, select = -c(X) )
data$Code <- as.numeric(as.character(data$Code))
dataFamilles$Code <- as.numeric(as.character(dataFamilles$Code))

library(data.table)
dataFamilles <- data.table(dataFamilles)
dataFamilles %>% group_by("Code")


total = merge(data,dataFamilles,by="Code",all = TRUE)
total$Code <- as.factor(total$Code)
