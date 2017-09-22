{
  ###########################CARGAR EL DATASET FINAL IGM
  
  library(shiny)      #Dashboard
  library(RMySQL)     #Secundary MySQL connection, Kill connections
  library(DBI)        #Primary MySQL connection
  library(openxlsx)   #Read xlsx files
  library(data.table) #For merge, rbind and dataframe works
  
  DB <- dbConnect(
    MySQL(),
    user = "root",
    password = "",
    dbname = paste0("igm")
  )
  print("Base de datos conectada")
  #####Tabla accesosunicos#####
  dbWriteTable(
    DB,
    "accesosunicos",
    accesosunicos,
    field.types = list(
      `Acceso` = "varchar(255)",
      `Ptotal` = "double(15,2)",
      `Pplanotarifario` = "double(15,2)",
      `Pusos` = "double(15,2)",
      `Pservicios` = "double(15,2)",
      `Pdescuentos` = "double(15,2)",
      `Pvoz` = "double(15,2)",
      `Pdatos` = "double(15,2)",
      `Psmsmms` = "double(15,2)",
      `Cantidad Meses` = "INT(10)"
    ),
    row.names = FALSE,
    overwrite = TRUE,
    append = FALSE,
    allow.keywords = FALSE
  )
  print("accesosunicos subidos")
  #####Tabla UAA_users #####
  UAA_users2<-UAA_users
  UAA_users2$UUI<-NULL
  UAA_users2$`Acceso fix`<-NULL
  UAA_users2$Acceso.y<-NULL
  UAA_users2$Proveedor.y<-NULL
  UAA_users2$Acceso<-NULL
  UAA_users2$IMEI<-NULL
  UAA_users2$REFNUM<-NULL
  
  
  dbWriteTable(
    DB,
    "UAA_users",
    UAA_users2,
    field.types = list(
      
      `Acceso.x` = "varchar(255)",
      `Proveedor.x` = "varchar(255)",
      `Total (CLP)` = "double(15,2)",
      `Plano tarifario (CLP)` = "double(15,2)",
      `Uso (CLP)` = "double(15,2)",
      `Servicios (CLP)` = "double(15,2)",
      `Descuentos (CLP)` = "double(15,2)",
      `Voz (CLP)` = "double(15,2)",
      `Datos (CLP)` = "double(15,2)",
      `SMS/MMS (CLP)` = "double(15,2)",
      `Proveedor Nivel 2` = "varchar(255)",
      `Proveedor Nivel 3` = "varchar(255)",
      `Nombre` = "varchar(255)",
      `Estado` = "varchar(255)",
      `Fecha` = "varchar(255)",
      `Mes` = "INT(10)"
    ),
    row.names = FALSE,
    overwrite = TRUE,
    append = FALSE,
    allow.keywords = FALSE
  )
  print("UAA_accesses subido")
  ######Tabla UTP_accesses######
 UTP_accesses2<-UTP_accesses
 UTP_accesses2$`Acceso fix`<-NULL
 UTP_accesses2$duplicado<-NULL
 UTP_accesses2$Acceso.y<-NULL
 UTP_accesses2$Proveedor.y<-NULL
 UTP_accesses2$Proveedor.x<-NULL
 UTP_accesses2$Acceso<-NULL
 UTP_accesses2$Proveedor.y<-NULL
  
  dbWriteTable(
    DB,
    "UTP_Accesses",
    UTP_accesses2,
    field.types = list(
      
      `Acceso.x` = "varchar(255)",
      `Producto` = "varchar(255)",
      `Proveedor.x` = "varchar(255)",
      `Total (CLP)` = "double(15,2)",
      `Plano tarifario (CLP)` = "double(15,2)",
      `Uso (CLP)` = "double(15,2)",
      `Servicios (CLP)` = "double(15,2)",
      `Descuentos (CLP)` = "double(15,2)",
      `Voz (CLP)` = "double(15,2)",
      `Datos (CLP)` = "double(15,2)",
      `SMS/MMS (CLP)` = "double(15,2)",
      `Tipo de producto` = "varchar(255)",
      `Centro de facturacion` = "varchar(255)",
      `Importe de las opciones descontadas (CLP)` = "double(15,2)",
      `Descripcion Plan` = "varchar(255)",
      `Tipo` = "varchar(255)",
      `Proveedor Nivel 2` = "varchar(255)",
      `Proveedor Nivel 3` = "varchar(255)",
      `Fecha` = "varchar(255)",
      `Mes` = "INT(10)"
    ),
    row.names = FALSE,
    overwrite = TRUE,
    append = FALSE,
    allow.keywords = FALSE
  )
  
  print("UTP_Accesses subido")
  
  
  
  
  
}
