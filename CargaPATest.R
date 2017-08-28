library(RMySQL)
library(DBI)
library(stringr)
library(openxlsx)
#Establish an SQL conection Function
sqlQuery <- function (query) {
  # creating DB connection object with RMysql package
  DB <- dbConnect(MySQL(),
                  user = "root",
                  password = "",
                  dbname = "parque_arauco_test")
  #dbSendQuery(DB, 'set character set "utf8"')
  dbSendQuery(DB, 'SET NAMES utf8')
  # send Query to btain result set
  rs <- dbSendQuery(DB, query)
  # get elements from result sets and convert to dataframe
  result <- fetch(rs, -1)
  # close db connection
  dbDisconnect(DB)
  # return the dataframe
  return(result)
}

facturas <- read.csv2("C:\\Users\\Neobis\\Downloads\\BI PA\\Facturas.csv")

rows_facturas <- NROW(facturas)

y <- c("ene. "="01","feb. "="02","mar. "="03","abr. "="04","may. "="05","jun. "="06","jul. "="07","ago. "="08","sept."="09","oct. "="10","nov. "="11","dic. "="12")
for(i in 1:12){
  facturas$Mes.de.facturaciÃ³n <- gsub(names(y[i]),y[[i]],facturas$Mes.de.facturaciÃ³n)
}
facturas$Fecha <- paste(str_sub(facturas$Mes.de.facturaciÃ³n ,-4),str_sub(facturas$Mes.de.facturaciÃ³n ,1,2),"01",sep = "/")

for (i in 1:rows_facturas) {
  sqlQuery(
    paste(
      "INSERT INTO `facturas`(`Numero de factura`, `PROVEEDOR`, `FECHA`, `Total sin impuestos UF`, `Total sin impuestos COL`, `Total sin impuestos PE`)
      VALUES (",
      facturas$ï..NÃºmero.de.factura[i],
      ",",
      facturas$`Proveedor`[i],
      ",",
      facturas$Fecha[i],
      ",",
      facturas$Total.sin.impuestos..UF.[i],
      ",",
      facturas$Total.sin.impuestos..COP.[i],
      ",",
      facturas$Total.sin.impuestos..PEN.[i],
      ")",
      sep = "'"
      )
    )
}


uso_por_acc_201702 <- read.csv2("C:\\Users\\Neobis\\Downloads\\BI PA\\201702_uso_por_acc.csv")
uso_por_acc_201703 <- read.csv2("C:\\Users\\Neobis\\Downloads\\BI PA\\201703_uso_por_acc.csv")
uso_por_acc_201704 <- read.csv2("C:\\Users\\Neobis\\Downloads\\BI PA\\201704_uso_por_acc.csv")
uso_por_acc_201705 <- read.csv2("C:\\Users\\Neobis\\Downloads\\BI PA\\201705_uso_por_acc.csv")

rows_201702 <- NROW(uso_por_acc_201702)
rows_201703 <- NROW(uso_por_acc_201703)
rows_201704 <- NROW(uso_por_acc_201704)
rows_201705 <- NROW(uso_por_acc_201705)


uso_por_acc <- write.table(rbind(uso_por_acc_201702,uso_por_acc_201703,uso_por_acc_201704,uso_por_acc_201705), paste("uso_por_acc.csv", sep = "-"), 
            sep = ";", col.names = TRUE, row.names = FALSE, append = T)

uso_por_acc <- read.csv2("uso_por_acc.csv", stringsAsFactors=FALSE)
rows_uso <- NROW(uso_por_acc)

y <- c("ene. "="01","feb. "="02","mar. "="03","abr. "="04","may. "="05","jun. "="06","jul. "="07","ago. "="08","sept."="09","oct. "="10","nov. "="11","dic. "="12")
for(i in 1:12){
  uso_por_acc$PerÃ.odo.de <- gsub(names(y[i]),y[[i]],uso_por_acc$PerÃ.odo.de)
}
uso_por_acc$Fecha <- paste(str_sub(uso_por_acc$PerÃ.odo.de ,-4),str_sub(uso_por_acc$PerÃ.odo.de ,1,2),"01",sep = "/")

uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010143479"] <- "02250010143479"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010143479"] <- "02250010143479"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010143481"] <- "02250010143481"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010143484"] <- "02250010143484"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010143486"] <- "02250010143486"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010143487"] <- "02250010143487"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010143488"] <- "02250010143488"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010143489"] <- "02250010143489"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010143490"] <- "02250010143490"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010143491"] <- "02250010143491"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010149623"] <- "02250010149623"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010158652"] <- "02250010158652"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010158653"] <- "02250010158653"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010158654"] <- "02250010158654"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010158655"] <- "02250010158655"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010158656"] <- "02250010158656"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010158657"] <- "02250010158657"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010158658"] <- "02250010158658"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010158659"] <- "02250010158659"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010158660"] <- "02250010158660"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010158661"] <- "02250010158661"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010163710"] <- "02250010163710"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010168618"] <- "02250010168618"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010169420"] <- "02250010169420"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010171924"] <- "02250010171924"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2250010180715"] <- "02250010180715"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2370010143480"] <- "02370010143480"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2370010143485"] <- "02370010143485"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "2430010169548"] <- "02430010169548"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "6010010157551"] <- "06010010157551"
uso_por_acc$ï..Acceso[uso_por_acc$ï..Acceso == "6420010157682"] <- "06420010157682"


for (i in 1:rows_uso) {
  sqlQuery(
    paste(
      "INSERT INTO `uso_por_acceso_simple`(`ACCESO`, `PROVEEDOR`, `PAIS`, `Total (UF)`, `Total (COP)`, `Total (PEN)`, `FECHA`)
      VALUES (",
      uso_por_acc$ï..Acceso[i],
      ",",
      uso_por_acc$Proveedor[i],
      ",",
      uso_por_acc$PaÃ.s[i],
      ",",
      uso_por_acc$Total..UF.[i],
      ",",
      uso_por_acc$Total..COP.[i],
      ",",
      uso_por_acc$Total..PEN.[i],
      ",",
      uso_por_acc$Fecha[i],
      ")",
      sep = "'"
      )
    )
}

sqlQuery("update `uso_por_acceso_simple` set
         ACCESO = replace(ACCESO, ' ', '')")


# Abrir y guardar Archivo export soluciona bug

file <- "C:\\Users\\Neobis\\Downloads\\BI PA\\Exp.xlsx"


ACCESSES <- read.xlsx(
  file,
  sheet = "ACCESSES",
  startRow = 1
)

ACCESSES$ACTIVATION.DATE <- as.Date(ACCESSES$ACTIVATION.DATE, origin = "1899-12-30")
ACCESSES$EXPIRATION.DATE <- as.Date(ACCESSES$EXPIRATION.DATE, origin = "1899-12-30")

rows_ACCESSES <-NROW(ACCESSES)


for (i in 1:rows_ACCESSES) {
  
  sqlQuery(
    paste(
      "INSERT INTO `export_access`(`ACCESO`, `TIPO`, `ESTADO`, `FECHA EXPIRACION`, `MANAGEMENT_ORG_1`, `MANAGEMENT_ORG_2`, `MANAGEMENT_ORG_3`,
`MANAGEMENT_ORG_4`, `PROVEEDOR`, `CARRIER_ORG_2`, `CARRIER_ORG_3`, `FECHA ACTIVACION`, `Tipo de Servicio 1`, `Tipo de Servicio 2`, `Tipo de Servicio 3`) 
      VALUES (",
      ACCESSES$ACCESS.NUMBER[i],
      ",",
      ACCESSES$TYPE[i],
      ",",
      ACCESSES$STATUS[i],
      ",",
      ACCESSES$EXPIRATION.DATE[i],
      ",",
      ACCESSES$`MANAGEMENT_ORG:1`[i],
      ",",
      ACCESSES$`MANAGEMENT_ORG:2`[i],
      ",",
      ACCESSES$`MANAGEMENT_ORG:3`[i],
      ",",
      ACCESSES$`MANAGEMENT_ORG:4`[i],
      ",",
      ACCESSES$`CARRIER_ORG:1`[i],
      ",",
      ACCESSES$`CARRIER_ORG:2`[i],
      ",",
      ACCESSES$`CARRIER_ORG:3`[i],
      ",",
      ACCESSES$ACTIVATION.DATE[i],
      ",",
      ACCESSES$`CUSTOM_ORG:Tipo.de.Servicio:1`[i],
      ",",
      ACCESSES$`CUSTOM_ORG:Tipo.de.Servicio:2`[i],
      ",",
      ACCESSES$`CUSTOM_ORG:Tipo.de.Servicio:3`[i],
      ")",
      sep = "'"
      )
    )
}

presupuesto <- "C:\\Users\\Neobis\\Downloads\\BI PA\\presupuesto.xlsx"

presupuesto <- read.xlsx(
  presupuesto,
  sheet = "PTO",
  startRow = 1
)


y <- c("1"="01","2"="02","3"="03","4"="04","5"="05","6"="06","7"="07","8"="08","9"="09","10"="10","11"="11","2"="12")
for(i in 1:12){
  presupuesto$PERIODO <- gsub(paste0('\\<', names(y[i]), '\\>'),y[[i]],presupuesto$PERIODO)
}
presupuesto$Fecha <- paste("2017",str_sub(presupuesto$PERIODO ,1,2),"01",sep = "/")

rows_ppto <- NROW(presupuesto)

for (i in 1:rows_ppto) {
  
  sqlQuery(
    paste(
      "INSERT INTO `presupuesto`(`MANAGEMENT_ORG_2`, `MANAGEMENT_ORG_3`, `MANAGEMENT_ORG_4`, `Tipo de Servicio 2`, `Cuenta Contable`, 
`Item Servicio`, `PROVEEDOR`, `Ppto Anual 2017`, `Presupuesto mensual 2017`,`FECHA`)
      VALUES (",
      presupuesto$`MANAGEMENT_ORG:2`[i],
      ",",
      presupuesto$`MANAGEMENT_ORG:3`[i],
      ",",
      presupuesto$`MANAGEMENT_ORG:4`[i],
      ",",
      presupuesto$`CUSTOM_ORG:Tipo.de.Servicio:2`[i],
      ",",
      presupuesto$Cuenta.Contable[i],
      ",",
      presupuesto$Item.Servicio[i],
      ",",
      presupuesto$`CARRIER_ORG:1`[i],
      ",",
      presupuesto$Ppto.Anual.2017[i],
      ",",
      presupuesto$Presupuesto.mensual.2017[i],
      ",",
      presupuesto$Fecha[i],
      ")",
      sep = "'"
    )
  )
}

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}
killDbConnections()
