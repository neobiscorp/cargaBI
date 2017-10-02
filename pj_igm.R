{


    ACCESSES2<-subset(ACCESSES,
                       ACCESSES[["Proveedor"]]== "Movistar CL" |
                       ACCESSES[["Proveedor"]] == "Entel PCS (CL)"|
                        ACCESSES[["Proveedor"]]== "Claro CL"
                     )
  ASSOCIATIONS["Acceso fix"]<-substr(ASSOCIATIONS[["Acceso"]],3,1000000L)

  U_accesses <- merge(uso,
                      ACCESSES2,
                      by.x = "Acceso fix",
                      by.y = "Acceso fix",
                      all.x = TRUE)
  UA_associations<- merge(U_accesses,
                          ASSOCIATIONS,
                          by.x = "Acceso fix",
                          by.y = "Acceso fix",
                          all.x = TRUE
                          )
  UAA_users <- merge(UA_associations,
                     USERS,
                     by.x = "UUI",
                     by.y = "UUI",
                     all.x = TRUE)
DEVICES[,'Null']<-is.na(DEVICES[,'IMEI'])
  DEVICES <- subset(DEVICES,
                    DEVICES[["Null"]] == FALSE)
  DEVICES["Tipo"]<-NULL
  DEVICES["REFNUM"]<-NULL
  DEVICES["Estado"]<-NULL
  DEVICES["Null"]<-NULL
  UAAD_users <-merge(UAA_users,
                     DEVICES,
                     by.x = "IMEI",
                     by.y = "IMEI",
                     all.x = TRUE)
  rm(U_accesses,UA_associations)
  
  # 
  # uso2 <- uso
  # a<-duplicated(uso2[["Acceso"]],fromLast = TRUE )
  # uso2[["duplicado"]] <-a
  # uso2<-subset(uso2,uso2[["duplicado"]] == FALSE)
  # PLAN2 <-subset(PLAN,PLAN["Tipo de producto"]=="Plano tarifario")
  # usounicojoinPLANt <-
  #   merge(
  #     uso2,
  #     PLAN2,
  #     by.x = "Acceso fix",
  #     by.y = "Acceso fix",
  #     all.x = TRUE
  #   )
  # usoplantjointipo <-
  #   merge(
  #     usounicojoinPLANt,
  #     TIPO,
  #     by.x = "Producto",
  #     by.y = "Producto",
  #     all.x = TRUE
  #   )
  # usoplantjointipo2<-usoplantjointipo
  # usoplantjointipo2["Proveedor.x"] <- NULL
  # usoplantjointipo2["Proveedor.y"] <- NULL
  # 
  # UTP_accesses<-
  #   merge (
  #     usoplantjointipo2,
  #     ACCESSES,
  #     by.x = "Acceso fix",
  #     by.y = "Acceso fix",
  #     all.x = TRUE
  #   )
  # rm(uso2,usounicojoinPLANt,a,PLAN2,usoplantjointipo,usoplantjointipo2)

#Se quiere generar los consumos promedios mensuales por acceso
# 
# accesosunicos<-as.list(unique(UTP_accesses[["Acceso.x"]]))
# i<-2
# Ptotal<-c()
# Pplanotarifario<-c()
# Pusos<-c()
# Pservicios<-c()
# Pdescuentos<-c()
# Pvoz<-c()
# Pdatos<-c()
# Psmsmms<-c()
# nmeses<-c()
# as.character(accesosunicos[6])
# 
# for(i in 1:as.numeric(length(accesosunicos))){
#   AccMes<-subset(uso,uso[["Acceso"]] == as.character(accesosunicos[[i]])) #Generamos un uso unico al cual le agregamos todos los datos de los distintos meses
#   nmeses[i]<-as.numeric(length(AccMes[["Acceso"]])) #se ve cuantos meses ha sido ocupado el acceso correspondiente
#   #Se obtienen los datos necesarios desde AccMes que contiene los mismos titulos de uso junto con los que se agregaron de 557 al 562
#   Ptotal[i]<-sum(AccMes[["Total (CLP)"]])/nmeses[[i]]
#   Pplanotarifario[i]<- sum(AccMes[["Plano tarifario (CLP)"]])/nmeses[[i]]
#   Pusos[i]<-sum(AccMes[["Uso (CLP)"]])/nmeses[[i]]
#   Pservicios[i]<-sum(AccMes[["Servicios (CLP)"]])/nmeses[[i]]
#   Pdescuentos[i]<-sum(AccMes[["Descuentos (CLP)"]])/nmeses[[i]]
#   Pvoz[i]<-sum(AccMes[["Voz (CLP)"]])/nmeses[[i]]
#   Pdatos[i]<-sum(AccMes[["Datos (CLP)"]])/nmeses[[i]]
#   Psmsmms[i]<-sum(AccMes[["SMS/MMS (CLP)"]])/nmeses[[i]]
# }
# Acceso<-unique(UTP_accesses[["Acceso.x"]])
# accesosunicos<-as.data.frame(Acceso)
# #Se unen los datos recopilados a la nueva tabla accesosunicos
# accesosunicos["Ptotal"]<-Ptotal
# accesosunicos["Pplanotarifario"]<-Pplanotarifario
# accesosunicos["Pusos"]<-Pusos
# accesosunicos["Pservicios"]<-Pservicios
# accesosunicos["Pdescuentos"]<-Pdescuentos
# accesosunicos["Pvoz"]<-Pvoz
# accesosunicos["Pdatos"]<-Pdatos
# accesosunicos["Psmsmms"]<-Psmsmms
# accesosunicos["Cantidad Meses"]<-nmeses
# accesosunicos<-accesosunicos[order(-accesosunicos[["Ptotal"]]),]
# 
# rm(Pvoz,Pusos,Ptotal,Psmsmms,Pservicios,Pplanotarifario,Pdescuentos,Pdatos,nmeses,Acceso,i)
# print("LISTO")


############Anomalias
PLAN2<-subset(PLAN,
              PLAN[["Tipo de producto"]] == "Plano tarifario")
UAAD_users[["Acceso.y"]]<-NULL
UAAD_users[["Proveedor.y"]]<-NULL
UAAD_users[["Acceso.x"]]<-NULL
UAAD_users[["Proveedor.x"]]<-NULL
UAADP_usos <- merge(UAAD_users,
                    PLAN2,
                    by.x = "Acceso fix",
                    by.y = "Acceso fix",
                    all.x = TRUE)
UAADPT_usos <- merge(UAADP_usos,
                     TIPO,
                     by.x = "Producto",
                     by.y = "Producto",
                     all.x = TRUE)
rm(ACCESSES2,UAAD_users,UAA_users,UAADP_usos)






}

