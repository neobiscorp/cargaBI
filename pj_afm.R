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
  
  
rm(Fact)
