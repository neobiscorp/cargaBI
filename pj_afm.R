{
  SFOpciones<-subset(SFACTURADOS,
                   SFACTURADOS[["Tipo de producto"]]=="Option")
  SFOpciones<<-SFOpciones
SFPlanesA<-subset(SFACTURADOS,
                  SFACTURADOS[["Tipo de producto"]]=="Plano tarifario" & 
                    SFACTURADOS[["Estado acceso"]]=="Activo")
SFPlanesDb<-subset(SFACTURADOS,
                   SFACTURADOS[["Tipo de producto"]]=="Plano tarifario" & 
                     SFACTURADOS[["Estado acceso"]]=="Dado de baja")
SFPlanes<-rbind(SFPlanesDb,SFPlanesA)
SFPlanes<<-SFPlanes
SFPlanes2<-SFPlanes
#SFPlanes<-subset(SFPlanes,SFPlanes[["Producto"]]!="T1P")
  
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
  ######################Ajuste de Servicios Facturados################
  Productos_Contratados<-as.character(MOVISTAR_PLANES[["Producto"]])
  a<-duplicated(SFPlanes2[["Acceso"]],fromLast = FALSE)
  b<-duplicated(SFPlanes2[["Acceso"]],fromLast = TRUE)
  SFPlanes2[["Duplicados"]]<-a
  SFPlanes2[["Duplicados2"]]<-b
  SFduplicados<-subset(SFPlanes2,SFPlanes2[["Duplicados"]]=="TRUE"|
                         SFPlanes2[["Duplicados2"]]=="TRUE")
  SF_no_duplicados<-subset(SFPlanes2,SFPlanes2[["Duplicados"]]=="FALSE"&
                             SFPlanes2[["Duplicados2"]]=="FALSE")
  if(length(SFduplicados[["Acceso"]])>0){
  SF_a_evaluar<- merge(SFduplicados,
                       MOVISTAR_PLANES,
                       by = "Producto",
                       all.x = TRUE)
  SF_fueradecontratoSC<-subset(SF_a_evaluar,
                               is.na(SF_a_evaluar[["Tipo"]])==TRUE &
                                 SF_a_evaluar[["Importe de las opciones descontadas (CLP)"]]==0)
  SF_fueradecontratoCC<-subset(SF_a_evaluar,
                               is.na(SF_a_evaluar[["Tipo"]])==TRUE &
                                 SF_a_evaluar[["Importe de las opciones descontadas (CLP)"]]!=0)
  SF_en_contrato<-subset(SF_a_evaluar,
                         is.na(SF_a_evaluar[["Tipo"]])==FALSE)
  SF_CPduplicados<-rbind(SF_en_contrato,SF_fueradecontratoCC)
  SF_CPduplicados[["Duplicado"]]<-NULL
  SF_CPduplicados[["Duplicado2"]]<-NULL
  a<-duplicated(SF_CPduplicados[["Acceso"]],fromLast = FALSE)
  b<-duplicated(SF_CPduplicados[["Acceso"]],fromLast = TRUE)
  SF_CPduplicados[["Duplicados"]]<-a
  SF_CPduplicados[["Duplicados2"]]<-b
SFduplicados2<-subset(SF_CPduplicados,SF_CPduplicados[["Duplicados"]]=="TRUE"|SF_CPduplicados[["Duplicados2"]]=="TRUE")
SFduplicadosbuenos<-subset(SF_CPduplicados,SF_CPduplicados[["Duplicados"]]=="FALSE"&SF_CPduplicados[["Duplicados2"]]=="FALSE")
if(length(SFduplicados2[["Acceso"]])>0){
  accesosunicos<-as.list(unique(SFduplicados2["Acceso"]))
  i<-1
  
  Acc<-c()
  EstAcc<-c()
  Prod<-c()
  TdProd<-c()
  CdF<-c()
  IOF<-c()
  IDPT<-c()
  IOD<-c()
  Accf<-c()
  for(i in 1:lengths(accesosunicos)){
    SFUnico<-subset(SFPlanes2,SFPlanes2[["Acceso"]] == as.character(accesosunicos[["Acceso"]][i]))
    
    Acc[i]<-SFUnico[["Acceso"]][1]
    EstAcc[i]<-"No determinado"
    Prod[i]<-"Multi producto"
    TdProd[i]<-"Plano tarifario"
    CdF[i]<-SFUnico[["Centro de facturacion"]][1]
    IOF[i]<-sum(SFUnico[["Importe de las opciones facturadas (CLP)"]][1:length(SFUnico[["Acceso"]])])
    IDPT[i]<-sum(SFUnico[["Importe descuentos sobre plano tarifario (CLP)"]][1:length(SFUnico[["Acceso"]])])
    IOD[i]<-sum(SFUnico[["Importe de las opciones descontadas (CLP)"]][1:length(SFUnico[["Acceso"]])])
    Accf[i]<-SFUnico[["Acceso fix"]][1]
   
  }
  Accesos<-(unique(SFduplicados2["Acceso"]))
  Accesos["Estado acceso"]<-EstAcc
    Accesos["Producto"]<-Prod
    Accesos["Tipo de producto"]<-TdProd
    Accesos["Centro de facturacion"]<-CdF
    Accesos["Importe de las opciones facturadas (CLP)"]<-IOF
    Accesos["Importe descuentos sobre plano tarifario (CLP)"]<-IDPT
    Accesos["Importe de las opciones descontadas (CLP)"]<-IOD
    Accesos["Acceso fix"]<-Accf

    SFUnicos<-Accesos
    
}
else {
  SFUnicos<-subset(SFACTURADOS,SFACTURADOS[["Acceso"]]==1&SFACTURADOS[["Acceso"]]!=1)

  }
  SF_no_duplicados[["Duplicados"]]<-NULL
  SF_no_duplicados[["Duplicados2"]]<-NULL
  SFduplicadosbuenos2<-subset(SFduplicadosbuenos,select = c("Acceso","Estado acceso","Producto","Tipo de producto","Centro de facturacion","Importe de las opciones facturadas (CLP)",
                                                           "Importe descuentos sobre plano tarifario (CLP)","Importe de las opciones descontadas (CLP)","Acceso fix"))
  SF_Final<-rbind(SFUnicos,SF_no_duplicados,SFduplicadosbuenos2)
  SFduplicados2<-subset(SFduplicados2,select = c("Acceso","Estado acceso","Producto","Tipo de producto","Centro de facturacion","Importe de las opciones facturadas (CLP)",
                                                            "Importe descuentos sobre plano tarifario (CLP)","Importe de las opciones descontadas (CLP)","Acceso fix"))
  SF_fueradecontratoSC<-subset(SF_fueradecontratoSC,select = c("Acceso","Estado acceso","Producto","Tipo de producto","Centro de facturacion","Importe de las opciones facturadas (CLP)",
                                                 "Importe descuentos sobre plano tarifario (CLP)","Importe de las opciones descontadas (CLP)","Acceso fix"))
  if(length(SFduplicados2[["Acceso"]])>0){
  SFduplicados2<-SFduplicados2[order(x = SFduplicados2[["Acceso"]]),]
  SFduplicados2["Revisar"]<-1
  }
  if(length(SF_fueradecontratoSC[["Acceso"]])>0){
  SF_fueradecontratoSC["Revisar"]<-2
  }
  SF_Apartados<-rbind(SFduplicados2,SF_fueradecontratoSC)
  }
  else{
    SF_Final<-SF_no_duplicados
    SFPlanes2["Revisar"]<-0
    SF_Apartados<-subset(SFPlanes2,SFPlanes2[["Duplicados"]]=="TRUE"|
                           SFPlanes2[["Duplicados2"]]=="TRUE")
   
  }
  rm(SF_a_evaluar,SF_CPduplicados,SF_en_contrato,SF_fueradecontratoCC,SF_no_duplicados,SFduplicados,SFduplicados2,SFduplicadosbuenos,SFduplicadosbuenos2,SFPlanes2,SF_fueradecontratoSC,SFUnicos,SFPlanesA,SFPlanesDb)
  SF_Final<<-SF_Final
  SF_Apartados<<-SF_Apartados
################Consolidado############
  Fact<<-merge(uso,SF_Final,by = c("Acceso fix","Acceso","Centro de facturacion"),all.x = TRUE)
  facturas2<-facturas
  facturas2[,'Proveedor']<-NULL
  facturas2[,'Total sin impuestos']<-NULL
  facturas2[,'Total imp. incluidos']<-NULL
  facturas2[,'Importe IVA']<-NULL
  facturas2[,'Divisa']<-NULL
  facturas2[,'N. accesos facturados']<-NULL
  facturas2[,'Fecha']<-NULL
  facturas2<<-facturas2
  Fact<<-merge(Fact,facturas2,by = "Centro de facturacion", all.x = TRUE)
  Fact[["Acceso fix"]]<-NULL
  Contratoplanes<-subset(MOVISTAR_PLANES,select = c("Producto","Tipo","Precio (CLP)","Voz (min)","Datos (KB)","Precio/min (CLP)","PrecioSC/min (CLP)","Precio/SMS (CLP)","Precio/KB (CLP)"))
  Contratoplanes[,'Tipo Contrato']<-Contratoplanes[,'Tipo']
  Contratoplanes[["Tipo"]]<-NULL
  Consolidado<<-merge(Fact,Contratoplanes,by = "Producto",all.x = TRUE)
  Consolidado[,'Voz nac. (min)']<-Consolidado[,'Voz nacional (seg)']/60
####################MIN ADICIONAL#################

  MIN_ADICIONAL1<<-subset(Consolidado,Consolidado[["Voz nac. (min)"]]>Consolidado[["Voz (min)"]]&Consolidado[["Voz (min)"]]>0)
  MIN_ADICIONAL1[,'Delta minutos']<-MIN_ADICIONAL1[,'Voz nac. (min)']-MIN_ADICIONAL1[,'Voz (min)']
  MIN_ADICIONAL1[,'Precio Real']<- (MIN_ADICIONAL1[,'Voz nac. (min)']-MIN_ADICIONAL1[,'Voz (min)'])*MIN_ADICIONAL1[,'PrecioSC/min (CLP)']
  MIN_ADICIONAL1[,'Delta']<-MIN_ADICIONAL1[,'Voz nacional (CLP)']-MIN_ADICIONAL1[,'Precio Real']
  MIN_ADICIONAL2<-subset(Consolidado,Consolidado[["Voz nac. (min)"]]<Consolidado[["Voz (min)"]]&Consolidado[["Voz nacional (CLP)"]]>0&Consolidado[["PrecioSC/min (CLP)"]]>0)
  MIN_ADICIONAL2[,'Delta minutos']<-MIN_ADICIONAL2[,'Voz nac. (min)']-MIN_ADICIONAL2[,'Voz (min)']
  MIN_ADICIONAL2[,'Precio Real']<- 0
  MIN_ADICIONAL2[,'Delta']<-MIN_ADICIONAL2[,'Voz nacional (CLP)']-MIN_ADICIONAL2[,'Precio Real']
  cdr4<-subset(cdr3,select = c("Numero de llamada","Servicio llamado"))
  
  MIN_ADICIONAL<<-rbind(MIN_ADICIONAL1,MIN_ADICIONAL2)
  
  MIN_ADICIONAL<<-merge(MIN_ADICIONAL,cdr3,by.x = "Acceso",by.y = "Numero de llamada",all.x = TRUE)
  a<-duplicated(MIN_ADICIONAL[["Acceso"]],fromLast = FALSE)
  MIN_ADICIONAL[["Duplicados"]]<-a
  MIN_ADICIONAL<-subset(MIN_ADICIONAL,MIN_ADICIONAL[["Duplicados"]]=="FALSE")
  MIN_ADICIONAL[["Duplicados"]]<-NULL
  MIN_ADICIONAL<<-MIN_ADICIONAL
  print("Total min adicional")
  print(sum(MIN_ADICIONAL[["Delta"]]))
##########################DENTRO DEL PLAN##############
  PlanContrato<-subset(Consolidado,Consolidado[["Importe de las opciones descontadas (CLP)"]]!=Consolidado[["Precio (CLP)"]]&Consolidado[["Precio (CLP)"]]!=0&Consolidado[["Estado acceso"]]=="Activo")
  PlanContrato[,'Delta']<-PlanContrato[,'Importe de las opciones descontadas (CLP)']-PlanContrato[,'Precio (CLP)']
  PlanContrato<<-subset(PlanContrato,PlanContrato[["Delta"]]>0)
  print("Total Plan Contrato")
  print(sum(PlanContrato[["Delta"]]))
  PlanContratoGranel<-subset(Consolidado,Consolidado[["Precio (CLP)"]]==0&Consolidado[["Estado acceso"]]=="Activo")
  PlanContratoGranel[,'Delta']<-PlanContratoGranel[["Voz nacional (CLP)"]]+PlanContratoGranel[["Importe de las opciones descontadas (CLP)"]]-((PlanContratoGranel[,'Voz nacional (seg)']/60)*PlanContratoGranel[,'Precio/min (CLP)'])
  PlanContratoGranel<<-subset(PlanContratoGranel,PlanContratoGranel[["Delta"]]>0)
  print("Total Plan Granel")
  print(sum(PlanContratoGranel[["Delta"]]))
#################################Voz Nacional################## 
  Consolidado2<-subset(Consolidado,Consolidado[["Tipo"]]=="MÃ³vil")
  if(length(Consolidado2[["Acceso"]])>0){
  Consolidado2[["Tipo2"]]<-"Móvil"
  Consolidado2[["Tipo"]]<-NULL
  Consolidado2[["Tipo"]]<-Consolidado2[["Tipo2"]]
  Consolidado2[["Tipo2"]]<-NULL
  Consolidado3<-subset(Consolidado,Consolidado[["Tipo"]]!="MÃ³vil")
  Consolidado2<-rbind(Consolidado2,Consolidado3)
}
  VozNacional<-subset(Consolidado2,Consolidado2[["Estado acceso"]]=="Activo" & Consolidado2[["Tipo Contrato"]]!="Móvil"& Consolidado2[["Voz (CLP)"]]>0)
  VozNacional[,'Delta']<-VozNacional[,'Voz (CLP)']
  VozNacional<-subset(VozNacional,VozNacional[["Delta"]]>0)
  VozNacional<<-VozNacional
  }
