SFOpciones<-subset(SFACTURADOS,
                   SFACTURADOS[["Tipo de producto"]]=="Option")
SFPlanesA<-subset(SFACTURADOS,
                  SFACTURADOS[["Tipo de producto"]]=="Plano tarifario" & 
                    SFACTURADOS[["Estado acceso"]]=="Activo")
SFPlanesDb<-subset(SFACTURADOS,
                   SFACTURADOS[["Tipo de producto"]]=="Plano tarifario" & 
                     SFACTURADOS[["Estado acceso"]]=="Dado de baja")
SFPlanes<-rbind(SFPlanesDb,SFPlanesA)
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
  Fact<-merge(Fact,facturas2,by = "Centro de facturacion", all.x = TRUE)
  
rm(Fact)
