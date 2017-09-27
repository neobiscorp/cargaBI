{
  ###########################CARGAR EL DATASET FINAL IGM

  library(shiny)      #Dashboard
  library(RMySQL)     #Secundary MySQL connection, Kill connections
  library(DBI)        #Primary MySQL connection
  library(openxlsx)   #Read xlsx files
  library(data.table) #For merge, rbind and dataframe works
  killDbConnections <- function () {
    all_cons <- dbListConnections(MySQL())
    print(all_cons)
    for (con in all_cons)
      +  dbDisconnect(con)
    print(paste(length(all_cons), " connections killed."))
  }
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
  UAAD_users2<-UAAD_users
  UAAD_users2[["UUI"]]<-NULL
  UAAD_users2[["Acceso fix"]]<-NULL
  UAAD_users2[["Acceso.y"]]<-NULL
  UAAD_users2[["Proveedor.y"]]<-NULL
  UAAD_users2[["Acceso"]]<-NULL
  UAAD_users2[["IMEI"]]<-NULL
  UAAD_users2[["REFNUM"]]<-NULL
  UAAD_users2[,'Usuario']<- UAAD_users2[["Nombre"]]
  UAAD_users2[["Nombre"]]<-NULL
  UAAD_users2[,'Acceso']<- UAAD_users2[["Acceso.x"]]
  UAAD_users2[["Acceso.x"]]<-NULL
  UAAD_users2[,'Proveedor']<- UAAD_users2[["Proveedor.x"]]
  UAAD_users2[["Proveedor.x"]]<-NULL
  
  if (is.null(UAAD_users[["MANAGEMENTORG1"]])==TRUE){
  dbWriteTable(
    DB,
    "usos",
    UAAD_users2,
    field.types = list(
      
      `Acceso` = "varchar(255)",
      `Usuario` = "varchar(255)",
      `Modelo` = "varchar(255)",
      `Proveedor` = "varchar(255)",
      `Total (CLP)` = "double(15,2)",
      `Plano tarifario (CLP)` = "double(15,2)",
      `Uso (CLP)` = "double(15,2)",
      `Servicios (CLP)` = "double(15,2)",
      `Descuentos (CLP)` = "double(15,2)",
      `Descuento de Plano tarifario (CLP)` = "double(15,2)",
      `Voz (CLP)` = "double(15,2)",
      `Datos (CLP)` = "double(15,2)",
      `SMS/MMS (CLP)` = "double(15,2)",
      `Proveedor Nivel 2` = "varchar(255)",
      `Proveedor Nivel 3` = "varchar(255)",
      `Estado` = "varchar(255)",
      `Fecha` = "varchar(255)",
      `Mes` = "INT(10)"
    ),
    row.names = FALSE,
    overwrite = TRUE,
    append = FALSE,
    allow.keywords = FALSE
  )
  }
  if (is.null(UAAD_users[["MANAGEMENTORG2"]])==TRUE&
      is.null(UAAD_users[["MANAGEMENTORG1"]])==FALSE){
    dbWriteTable(
      DB,
      "usos",
      UAAD_users2,
      field.types = list(
        
        `Acceso` = "varchar(255)",
        `Usuario` = "varchar(255)",
        `Modelo` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
        `Voz (CLP)` = "double(15,2)",
        `Datos (CLP)` = "double(15,2)",
        `SMS/MMS (CLP)` = "double(15,2)",
        `Proveedor Nivel 2` = "varchar(255)",
        `Proveedor Nivel 3` = "varchar(255)",
        `MANAGEMENTORG1` = "varchar(255)",
        `Estado` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )  
  }
  if (is.null(UAAD_users[["MANAGEMENTORG3"]])==TRUE&
      is.null(UAAD_users[["MANAGEMENTORG2"]])==FALSE){
    dbWriteTable(
      DB,
      "usos",
      UAAD_users2,
      field.types = list(
        
        `Acceso` = "varchar(255)",
        `Usuario` = "varchar(255)",
        `Modelo` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
        `Voz (CLP)` = "double(15,2)",
        `Datos (CLP)` = "double(15,2)",
        `SMS/MMS (CLP)` = "double(15,2)",
        `Proveedor Nivel 2` = "varchar(255)",
        `Proveedor Nivel 3` = "varchar(255)",
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `Estado` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )  
  }
  if (is.null(UAAD_users[["MANAGEMENTORG4"]])==TRUE&
      is.null(UAAD_users[["MANAGEMENTORG3"]])==FALSE){
    dbWriteTable(
      DB,
      "usos",
      UAAD_users2,
      field.types = list(
        
        `Acceso` = "varchar(255)",
        `Usuario` = "varchar(255)",
        `Modelo` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
        `Voz (CLP)` = "double(15,2)",
        `Datos (CLP)` = "double(15,2)",
        `SMS/MMS (CLP)` = "double(15,2)",
        `Proveedor Nivel 2` = "varchar(255)",
        `Proveedor Nivel 3` = "varchar(255)",
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `Estado` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )  
  }
  if (is.null(UAAD_users[["MANAGEMENTORG5"]])==TRUE&
      is.null(UAAD_users[["MANAGEMENTORG4"]])==FALSE){
    dbWriteTable(
      DB,
      "usos",
      UAAD_users2,
      field.types = list(
        
        `Acceso` = "varchar(255)",
        `Usuario` = "varchar(255)",
        `Modelo` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
        `Voz (CLP)` = "double(15,2)",
        `Datos (CLP)` = "double(15,2)",
        `SMS/MMS (CLP)` = "double(15,2)",
        `Proveedor Nivel 2` = "varchar(255)",
        `Proveedor Nivel 3` = "varchar(255)",
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `MANAGEMENTORG4` = "varchar(255)",
        `Estado` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )  
  }
  if (is.null(UAAD_users[["MANAGEMENTORG6"]])==TRUE&
      is.null(UAAD_users[["MANAGEMENTORG5"]])==FALSE){
    dbWriteTable(
      DB,
      "usos",
      UAAD_users2,
      field.types = list(
        
        `Acceso` = "varchar(255)",
        `Usuario` = "varchar(255)",
        `Modelo` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
        `Voz (CLP)` = "double(15,2)",
        `Datos (CLP)` = "double(15,2)",
        `SMS/MMS (CLP)` = "double(15,2)",
        `Proveedor Nivel 2` = "varchar(255)",
        `Proveedor Nivel 3` = "varchar(255)",
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `MANAGEMENTORG4` = "varchar(255)",
        `MANAGEMENTORG5` = "varchar(255)",
        `Estado` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )  
  }
  if (is.null(UAAD_users[["MANAGEMENTORG7"]])==TRUE&
      is.null(UAAD_users[["MANAGEMENTORG6"]])==FALSE){
    dbWriteTable(
      DB,
      "usos",
      UAAD_users2,
      field.types = list(
        
        `Acceso` = "varchar(255)",
        `Usuario` = "varchar(255)",
        `Modelo` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
        `Voz (CLP)` = "double(15,2)",
        `Datos (CLP)` = "double(15,2)",
        `SMS/MMS (CLP)` = "double(15,2)",
        `Proveedor Nivel 2` = "varchar(255)",
        `Proveedor Nivel 3` = "varchar(255)",
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `MANAGEMENTORG4` = "varchar(255)",
        `MANAGEMENTORG5` = "varchar(255)",
        `MANAGEMENTORG6` = "varchar(255)",
        `Estado` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )  
  }
  if (is.null(UAAD_users[["MANAGEMENTORG8"]])==TRUE&
      is.null(UAAD_users[["MANAGEMENTORG7"]])==FALSE){
    dbWriteTable(
      DB,
      "usos",
      UAAD_users2,
      field.types = list(
        
        `Acceso` = "varchar(255)",
        `Usuario` = "varchar(255)",
        `Modelo` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
        `Voz (CLP)` = "double(15,2)",
        `Datos (CLP)` = "double(15,2)",
        `SMS/MMS (CLP)` = "double(15,2)",
        `Proveedor Nivel 2` = "varchar(255)",
        `Proveedor Nivel 3` = "varchar(255)",
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `MANAGEMENTORG4` = "varchar(255)",
        `MANAGEMENTORG5` = "varchar(255)",
        `MANAGEMENTORG6` = "varchar(255)",
        `MANAGEMENTORG7` = "varchar(255)",
        `Estado` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )  
  }
  if (is.null(UAAD_users[["MANAGEMENTORG8"]])==FALSE){
    dbWriteTable(
      DB,
      "usos",
      UAAD_users2,
      field.types = list(
        
        `Acceso` = "varchar(255)",
        `Usuario` = "varchar(255)",
        `Modelo` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
        `Voz (CLP)` = "double(15,2)",
        `Datos (CLP)` = "double(15,2)",
        `SMS/MMS (CLP)` = "double(15,2)",
        `Proveedor Nivel 2` = "varchar(255)",
        `Proveedor Nivel 3` = "varchar(255)",
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `MANAGEMENTORG4` = "varchar(255)",
        `MANAGEMENTORG5` = "varchar(255)",
        `MANAGEMENTORG6` = "varchar(255)",
        `MANAGEMENTORG7` = "varchar(255)",
        `MANAGEMENTORG8` = "varchar(255)",
        `Estado` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )  
  }
  print("UAA_users subido")
  
  ######Tabla UTP_accesses######
 UTP_accesses2<-UTP_accesses
 UTP_accesses2[["Acceso fix"]]<-NULL
 UTP_accesses2[["duplicado"]]<-NULL
 UTP_accesses2[["Acceso.y"]]<-NULL
 UTP_accesses2[["Proveedor.y"]]<-NULL
 UTP_accesses2[["Proveedor.x"]]<-NULL
 UTP_accesses2[["Acceso"]]<-NULL
 UTP_accesses2[["Proveedor.y"]]<-NULL
 UTP_accesses2[,'Acceso']<- UTP_accesses2[["Acceso.x"]]
 UTP_accesses2[["Acceso.x"]]<-NULL
 UTP_accesses2[,'Proveedor']<- UTP_accesses2[["Proveedor.x"]]
 UTP_accesses2[["Proveedor.x"]]<-NULL
 
 
  if (is.null(UTP_accesses2[["MANAGEMENTORG1"]])==TRUE){
    
    dbWriteTable(
      DB,
      "UTP_Accesses",
      UTP_accesses2,
      field.types = list(
        `Acceso` = "varchar(255)",
        `Producto` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
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
  }
  if (is.null(UTP_accesses2[["MANAGEMENTORG2"]])==TRUE&
      is.null(UTP_accesses2[["MANAGEMENTORG1"]])==FALSE){
    dbWriteTable(
      DB,
      "UTP_Accesses",
      UTP_accesses2,
      field.types = list(
        `Acceso` = "varchar(255)",
        `Producto` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
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
        `MANAGEMENTORG1` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )
    }
  if (is.null(UTP_accesses2[["MANAGEMENTORG3"]])==TRUE&
      is.null(UTP_accesses2[["MANAGEMENTORG2"]])==FALSE){    
    dbWriteTable(
        DB,
        "UTP_Accesses",
        UTP_accesses2,
        field.types = list(
          `Acceso` = "varchar(255)",
          `Producto` = "varchar(255)",
          `Proveedor` = "varchar(255)",
          `Total (CLP)` = "double(15,2)",
          `Plano tarifario (CLP)` = "double(15,2)",
          `Uso (CLP)` = "double(15,2)",
          `Servicios (CLP)` = "double(15,2)",
          `Descuentos (CLP)` = "double(15,2)",
          `Descuento de Plano tarifario (CLP)` = "double(15,2)",
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
          `MANAGEMENTORG1` = "varchar(255)",
          `MANAGEMENTORG2` = "varchar(255)",
          `Fecha` = "varchar(255)",
          `Mes` = "INT(10)"
        ),
        row.names = FALSE,
        overwrite = TRUE,
        append = FALSE,
        allow.keywords = FALSE
      )
    }
  if (is.null(UTP_accesses2[["MANAGEMENTORG4"]])==TRUE&
      is.null(UTP_accesses2[["MANAGEMENTORG3"]])==FALSE){
    dbWriteTable(
      DB,
      "UTP_Accesses",
      UTP_accesses2,
      field.types = list(
        `Acceso` = "varchar(255)",
        `Producto` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
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
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )
  }
  if (is.null(UTP_accesses2[["MANAGEMENTORG5"]])==TRUE&
      is.null(UTP_accesses2[["MANAGEMENTORG4"]])==FALSE){
    dbWriteTable(
      DB,
      "UTP_Accesses",
      UTP_accesses2,
      field.types = list(
        `Acceso` = "varchar(255)",
        `Producto` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
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
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `MANAGEMENTORG4` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )
  }
  if (is.null(UTP_accesses2[["MANAGEMENTORG6"]])==TRUE&
      is.null(UTP_accesses2[["MANAGEMENTORG5"]])==FALSE){
    dbWriteTable(
      DB,
      "UTP_Accesses",
      UTP_accesses2,
      field.types = list(
        `Acceso` = "varchar(255)",
        `Producto` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
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
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `MANAGEMENTORG4` = "varchar(255)",
        `MANAGEMENTORG5` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )
  }
  if (is.null(UTP_accesses2[["MANAGEMENTORG7"]])==TRUE&
      is.null(UTP_accesses2[["MANAGEMENTORG6"]])==FALSE){
    dbWriteTable(
      DB,
      "UTP_Accesses",
      UTP_accesses2,
      field.types = list(
        `Acceso` = "varchar(255)",
        `Producto` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
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
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `MANAGEMENTORG4` = "varchar(255)",
        `MANAGEMENTORG5` = "varchar(255)",
        `MANAGEMENTORG6` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )
  }
  if (is.null(UTP_accesses2[["MANAGEMENTORG8"]])==TRUE&
      is.null(UTP_accesses2[["MANAGEMENTORG7"]])==FALSE){
    dbWriteTable(
      DB,
      "UTP_Accesses",
      UTP_accesses2,
      field.types = list(
        `Acceso` = "varchar(255)",
        `Producto` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
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
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `MANAGEMENTORG4` = "varchar(255)",
        `MANAGEMENTORG5` = "varchar(255)",
        `MANAGEMENTORG6` = "varchar(255)",
        `MANAGEMENTORG7` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )
  }
  if (is.null(UTP_accesses2[["MANAGEMENTORG8"]])==FALSE){
    dbWriteTable(
      DB,
      "UTP_Accesses",
      UTP_accesses2,
      field.types = list(
        `Acceso` = "varchar(255)",
        `Producto` = "varchar(255)",
        `Proveedor` = "varchar(255)",
        `Total (CLP)` = "double(15,2)",
        `Plano tarifario (CLP)` = "double(15,2)",
        `Uso (CLP)` = "double(15,2)",
        `Servicios (CLP)` = "double(15,2)",
        `Descuentos (CLP)` = "double(15,2)",
        `Descuento de Plano tarifario (CLP)` = "double(15,2)",
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
        `MANAGEMENTORG1` = "varchar(255)",
        `MANAGEMENTORG2` = "varchar(255)",
        `MANAGEMENTORG3` = "varchar(255)",
        `MANAGEMENTORG4` = "varchar(255)",
        `MANAGEMENTORG5` = "varchar(255)",
        `MANAGEMENTORG6` = "varchar(255)",
        `MANAGEMENTORG7` = "varchar(255)",
        `MANAGEMENTORG8` = "varchar(255)",
        `Fecha` = "varchar(255)",
        `Mes` = "INT(10)"
      ),
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      allow.keywords = FALSE
    )
  }
  print("UTP_Accesses subido")
  
  
  
  
  
}
