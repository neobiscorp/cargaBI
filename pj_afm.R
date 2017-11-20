SFOpciones<-subset(SFACTURADOS,
                   SFACTURADOS[["Tipo de producto"]]=="Option")
SFPlanesA<-subset(SFACTURADOS,
                  SFACTURADOS[["Tipo de producto"]]=="Plano tarifario" & 
                    SFACTURADOS[["Estado acceso"]]=="Activo")
SFPlanesDb<-subset(SFACTURADOS,
                   SFACTURADOS[["Tipo de producto"]]=="Plano tarifario" & 
                     SFACTURADOS[["Estado acceso"]]=="Dado de baja")
SFPlanes<-rbind(SFPlanesDb,SFPlanesA)
SFPlanes2<-SFPlanes
SFPlanes<-subset(SFPlanes,SFPlanes[["Producto"]]!="T1P")
  Fact<-merge(uso,SFPlanes,by = c("Acceso fix","Acceso","Centro de facturacion"),all.x = TRUE)
  facturas2<-facturas
  facturas2[,'Proveedor']<-NULL
  facturas2[,'Total sin impuestos']<-NULL
  facturas2[,'Total imp. incluidos']<-NULL
  facturas2[,'Importe IVA']<-NULL
  facturas2[,'Divisa']<-NULL
  facturas2[,'N. accesos facturados']<-NULL
  facturas2[,'Fecha']<-NULL
  Fact<<-merge(Fact,facturas2,by = "Centro de facturacion", all.x = TRUE)
  Fact[["Acceso fix"]]<-NULL
  #########################Consolidado######
  dbWriteTable(
    DB,
    "Consolidado",
    Fact,
    field.types = list(
      
      `Acceso` = "varchar(255)",
      `Estado acceso` = "varchar(255)",
      `Producto` = "varchar(255)",
      `Tipo de producto` = "varchar(255)",
      `Centro de facturacion` = "varchar(255)",
      `Tipo` = "varchar(255)",
      `Proveedor` = "varchar(255)",
      `Factura` = "varchar(255)",
      `Cuenta cliente` = "varchar(255)",
      `Total (CLP)` = "double(15,2)",
      `Plano tarifario (CLP)` = "double(15,2)",
      `Uso rebajado (CLP)` = "double(15,2)",
      `Servicios (CLP)` = "double(15,2)",
      `Servicios opciones (CLP)` = "double(15,2)",
      `Servicios otros (CLP)` = "double(15,2)",
      `Descuentos (CLP)` = "double(15,2)",
      `Descuentos opciones (CLP)` = "double(15,2)",
      `Descuentos otros (CLP)` = "double(15,2)",
      `Voz (CLP)` = "double(15,2)",
      `Voz nacional (CLP)` = "double(15,2)",
      `Voz roaming (CLP)` = "double(15,2)",
      `Datos (CLP)` = "double(15,2)",
      `Datos nacional (CLP)` = "double(15,2)",
      `Datos inter (CLP)` = "double(15,2)",
      `Voz nacional (seg)` = "double(15,2)",
      `Voz roaming (seg)` = "double(15,2)",
      `N. Voz nacional` = "double(15,2)",
      `Datos inter (KB)` = "double(15,2)",
      `Importe de las opciones facturadas (CLP)` = "double(15,2)",
      `Importe descuentos sobre plano tarifario (CLP)` = "double(15,2)",
      `Importe de las opciones descontadas (CLP)` = "double(15,2)",
      `Fecha` = "varchar(255)"
    ),
    row.names = FALSE,
    overwrite = TRUE,
    append = FALSE,
    allow.keywords = FALSE
  )
  ######################Planes Antiguos################
  Productos_Contratados<-as.character(MOVISTAR_PLANES[["Producto"]])
  a<-duplicated(SFPlanes2[["Acceso"]])
  SFPlanes2[["Duplicados"]]<-a
  SFduplicados<-subset(SFPlanes2,SFPlanes2[["Duplicados"]]==TRUE)
  contador<-length(SFduplicados[["Acceso"]])
  accesosunicos<-as.list(unique(SFPlanes2["Acceso"]))
summary(cdr[["Servicio llamado"]])
  for(i in 1:lengths(accesosunicos)){
    SFUnico<-subset(SFPlanes2,SFPlanes2[["Acceso"]] == as.character(accesosunicos[["Acceso"]][i]))
    if (length(SFUnico[["Acceso"]]) != 1){
      c<-length(SFUnico["Acceso"])
      
    }
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
rm(Fact)
