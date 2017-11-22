{
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
  cdr2<-subset(cdr,(cdr[["Servicio llamado"]]=="Números especiales" | (cdr[["Pais emisor"]]!= "Chile" | cdr[["Pais destinatario"]]!="Chile"))&cdr[["Tipo de llamada"]]!="SMS")
  #########################Consolidado######
  # dbWriteTable(
  #   DB,
  #   "Consolidado",
  #   Fact,
  #   field.types = list(
  #     
  #     `Acceso` = "varchar(255)",
  #     `Estado acceso` = "varchar(255)",
  #     `Producto` = "varchar(255)",
  #     `Tipo de producto` = "varchar(255)",
  #     `Centro de facturacion` = "varchar(255)",
  #     `Tipo` = "varchar(255)",
  #     `Proveedor` = "varchar(255)",
  #     `Factura` = "varchar(255)",
  #     `Cuenta cliente` = "varchar(255)",
  #     `Total (CLP)` = "double(15,2)",
  #     `Plano tarifario (CLP)` = "double(15,2)",
  #     `Uso rebajado (CLP)` = "double(15,2)",
  #     `Servicios (CLP)` = "double(15,2)",
  #     `Servicios opciones (CLP)` = "double(15,2)",
  #     `Servicios otros (CLP)` = "double(15,2)",
  #     `Descuentos (CLP)` = "double(15,2)",
  #     `Descuentos opciones (CLP)` = "double(15,2)",
  #     `Descuentos otros (CLP)` = "double(15,2)",
  #     `Voz (CLP)` = "double(15,2)",
  #     `Voz nacional (CLP)` = "double(15,2)",
  #     `Voz roaming (CLP)` = "double(15,2)",
  #     `Datos (CLP)` = "double(15,2)",
  #     `Datos nacional (CLP)` = "double(15,2)",
  #     `Datos inter (CLP)` = "double(15,2)",
  #     `Voz nacional (seg)` = "double(15,2)",
  #     `Voz roaming (seg)` = "double(15,2)",
  #     `N. Voz nacional` = "double(15,2)",
  #     `Datos inter (KB)` = "double(15,2)",
  #     `Importe de las opciones facturadas (CLP)` = "double(15,2)",
  #     `Importe descuentos sobre plano tarifario (CLP)` = "double(15,2)",
  #     `Importe de las opciones descontadas (CLP)` = "double(15,2)",
  #     `Fecha` = "varchar(255)"
  #   ),
  #   row.names = FALSE,
  #   overwrite = TRUE,
  #   append = FALSE,
  #   allow.keywords = FALSE
  # )
  ######################Planes Antiguos################
  Productos_Contratados<-as.character(MOVISTAR_PLANES[["Producto"]])
  a<-duplicated(SFPlanes2[["Acceso"]],fromLast = FALSE)
  b<-duplicated(SFPlanes2[["Acceso"]],fromLast = TRUE)
  SFPlanes2[["Duplicados"]]<-a
  SFPlanes2[["Duplicados2"]]<-b
  SFduplicados<-subset(SFPlanes2,SFPlanes2[["Duplicados"]]=="TRUE"|SFPlanes2[["Duplicados2"]]=="TRUE")
  SF_no_duplicados<-subset(SFPlanes2,SFPlanes2[["Duplicados"]]=="FALSE"&SFPlanes2[["Duplicados2"]]=="FALSE")
  SF_a_evaluar<- merge(SFduplicados,MOVISTAR_PLANES,by = "Producto",all.x = TRUE)
  SF_fueradecontratoSC<-subset(SF_a_evaluar,is.na(SF_a_evaluar[["Tipo"]])==TRUE & SF_a_evaluar[["Importe de las opciones descontadas (CLP)"]]==0)
  SF_fueradecontratoCC<-subset(SF_a_evaluar,is.na(SF_a_evaluar[["Tipo"]])==TRUE & SF_a_evaluar[["Importe de las opciones descontadas (CLP)"]]!=0)
  SF_en_contrato<-subset(SF_a_evaluar,is.na(SF_a_evaluar[["Tipo"]])==FALSE)
  SF_CPduplicados<-rbind(SF_en_contrato,SF_fueradecontratoCC)
  SF_CPduplicados[["Duplicado"]]<-NULL
  SF_CPduplicados[["Duplicado2"]]<-NULL
  a<-duplicated(SF_CPduplicados[["Acceso"]],fromLast = FALSE)
  b<-duplicated(SF_CPduplicados[["Acceso"]],fromLast = TRUE)
  SF_CPduplicados[["Duplicados"]]<-a
  SF_CPduplicados[["Duplicados2"]]<-b
SFduplicados2<-subset(SF_CPduplicados,SF_CPduplicados[["Duplicados"]]=="TRUE"|SF_CPduplicados[["Duplicados2"]]=="TRUE")
if(length(SFduplicados2[["Acceso"]])>0){
  accesosunicos<-as.list(unique(SFduplicados2["Acceso"]))
  SF_unicos<-subset(SFPlanes,SFPlanes[["Acceso"]]==0&SFPlanes[["Acceso"]]!=0)
  for(i in 1:lengths(accesosunicos)){
    SFUnico<-subset(SFPlanes2,SFPlanes2[["Acceso"]] == as.character(accesosunicos[["Acceso"]][i]))
    
    
    SF_unicos[["Acceso"]][i]<-SFUnico[["Acceso"]][1]
    for(j in 1:length(SFUnico[["Acceso"]]))
    SF_unicos[["Estado acceso"]][i]<-
      SF_unicos[["Producto"]][i]<-
      SF_unicos[["Tipo de producto"]][i]<-
      SF_unicos[["Centro de facturacion"]][i]<-
      SF_unicos[["Importe de las opciones facturadas (CLP)"]][i]<-
      SF_unicos[["Importe descuentos sobre plano tarifario (CLP)"]][i]<-
      SF_unicos[["Importe de las opciones descontadas (CLP)"]][i]<-
      SF_unicos[["Acceso fix"]][i]<-
  
  }
  
}
  
  
  
  
  
  
  
  
  
  
  
}
