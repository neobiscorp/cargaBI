cdr_accesses <-
  merge (cdr, ACCESSES, by.x = "Numero de llamada fix", by.y = "Acceso fix")

cdr_accesses <-
  merge (
    cdr_accesses,
    ACCESSES,
    by.x = "Numero llamado",
    by.y = "Acceso fix",
    all.x = TRUE
  )

cdr_accesses[, "NET"] <-
  ifelse (cdr_accesses["Proveedor Nivel 2.x"] == cdr_accesses["Proveedor Nivel 2.y"] &
            cdr_accesses["Proveedor Nivel 3.x"] == cdr_accesses["Proveedor Nivel 3.y"],
          1,
          0)

mes1 <- sapply(cdr_accesses["Fecha de llamada"], substr, 6, 7)
mes <- as.numeric(mes1)
cdr_accesses["Mes"] <- mes
n <- (length(unique(mes))) - 1 #número de meses
rm(mes1, mes)

cdr_accesses <-
  subset(cdr_accesses, cdr_accesses["Mes"] != min(cdr_accesses["Mes"]))
cdr_accesses[,"PdivD"] <- cdr_accesses[,"Precio"]/cdr_accesses[,"Duracion"]*60
cdr_accesses[,"PdivV"] <- cdr_accesses[,"Precio"]/cdr_accesses[,"Volumen"]*1024
#Eleccion de Centro de Facturacion
{
  cdr_accesses2 <-
    merge (
      cdr_accesses,
      CUENTAS,
      by.x = "Proveedor Nivel 3.x",
      by.y = "Cuenta Cliente",
      all.x = TRUE
    )
     
       
  
 summary(ACCESSES$`Proveedor Nivel 3`)
  summary(cdr_accesses$`Proveedor Nivel 3.x`)
summary(CUENTAS$`Cuenta Cliente`)
}
#CREACION usos con accesos unicos
{
  uso2 <- uso
  a<-duplicated(uso2["Acceso"],fromLast = TRUE)
  uso2["duplicado"] <-a
  uso2<-subset(uso2,uso2$duplicado == FALSE)
  PLAN2 <-subset(PLAN,PLAN["Tipo de producto"]=="Plano tarifario")
  usounicojoinPLANt <-
    merge(
      uso2,
      PLAN2,
      by.x = "Acceso fix",
      by.y = "Acceso fix",
      all.x = TRUE
    )
  usoplantjointipo <-
    merge(
      usounicojoinPLANt,
      TIPO,
      by.x = "Producto",
      by.y = "Producto",
      all.x = TRUE
    ) 
  rm(uso2,usounicojoinPLANt,a,PLAN2)
}
#MOVISTAR
{
  cdr_movistar <-
    subset(cdr_accesses, cdr_accesses$Proveedor.x == "Movistar CL")
  
  #CONSUMO TOTAL VOZ
  movistarvoz <-
    subset(cdr_movistar,
           cdr_movistar["Tipo de llamada"] == "Voz" &
             cdr_movistar["Duracion"] > 0)
  
  MovTotMin <- (sum(movistarvoz["Duracion"]) / 60) / n
  print(MovTotMin)
  
  #CONSUMO VOZ ENTRE USUARIOS SAAM SA
  movistarvozonnet <-
    subset(movistarvoz,
           movistarvoz["NET"] == 1)
  
  MovVozOnNet <- (sum(movistarvozonnet["Duracion"]) / 60) / n
  rm(movistarvozonnet)
  print(MovVozOnNet)
  
  #CONSUMO VOZ A TODO DESTINO
  MovATodDes <- MovTotMin - MovVozOnNet
  print(MovATodDes)
  
  #SMARTPHONES GAMA ALTA
  #SMARTPHONES GAMA MEDIA
  #---
  #BAM O SERVICIOS DE TELEMETRÍA 
  MovBAMm <- subset(usoplantjointipo,usoplantjointipo["Tipo"] == "BAM"
                    &usoplantjointipo["Proveedor"]=="Movistar CL"
  )
  MovBAM <-lengths(MovBAMm["Acceso fix"])
  rm(MovBAMm)
  #MENSAJERÍA SMS
  movistarSMS <-
    subset(
      cdr_movistar,
      cdr_movistar["Tipo de llamada"] == "SMS"
      &
        cdr_movistar["Precio"] > 0 &
        (
          cdr_movistar["Geografia"] == "Local" |
            cdr_movistar["Geografia"] == "Nacional desconocido"
        )
    )
  MovSms<-(sapply(movistarSMS["Precio"], median))
  print(MovSms)
  rm (movistarSMS)
  
  #MENSAJERÍA MMS
  movistarMMS <-
    subset(
      cdr_movistar,
      cdr_movistar["Tipo de llamada"] == "MMS"
      &
        cdr_movistar["Precio"] > 0 &
        (
          cdr_movistar["Geografia"] == "Local" |
            cdr_movistar["Geografia"] == "Nacional desconocido"
        )
    )
  
  MovMms<-(sapply(movistarMMS["Precio"], median))
  print(MovMms)
  rm (movistarMMS)
  
  #USUARIOS ROAMING ON DEMAND
  #ROAMING VOZ
  mroam <-
    subset(
      cdr_movistar,
      cdr_movistar["Geografia"] == "Roaming saliente" |
        cdr_movistar["Geografia"] == "Roaming entrante"
    )
  mroamvoz <-
    subset(mroam,
           mroam["Tipo de llamada"] == "Voz" &
             mroam["Duracion"] > 0)
  
  MovRoaVoz<-(sum(mroamvoz["Duracion"]) / 60 / n)
  print(MovRoaVoz)
  rm(mroamvoz)
  #ROAMING DATOS
  mroamdat <- subset(mroam,
                     mroam["Tipo de llamada"] == "Datos" &
                       mroam["Volumen"] > 0)
  
  MovRoaDat<-(sum(mroamdat["Volumen"]) / 1024 / n)
  print(MovRoaDat)
  rm(mroamdat)
  #ROAMING MENSAJES
  mroamsms <- subset(mroam,
                     mroam["Tipo de llamada"] == "SMS" &
                       mroam["Precio"] > 0)
  
  MovRoaSms<-(sapply(mroamsms["Precio"], median))
  print(MovRoaSms)
  rm(mroamsms)
  
  #---
  #$/minuto actual
  movistarvoz <-
    subset(
      movistarvoz,
      movistarvoz["Precio"] > 0  &
        #movistarvoz["Mes"] == max(movistarvoz["Mes"]) &
        (
          movistarvoz$Geografia == "Nacional desconocido" |
            movistarvoz$Geografia == "Local"
        )
    )
  
  #MovMinAct<-(sum(movistarvoz["Precio"]) / (sum(movistarvoz["Duracion"]) / 60))
  #movistarvoz[,"PdivD"] <- movistarvoz[,"Precio"]/(movistarvoz[,"Duracion"]/60)
  MovMinAct<-sapply(movistarvoz["PdivD"],mean)
  print(MovMinAct)
  
  #$/Mb Actual
  movistardatos <- subset(cdr_movistar,
                          cdr_movistar["Tipo de llamada"] == "Datos"
                          & (cdr_movistar["Geografia"] == "Nacional Desconocido" |
                               cdr_movistar["Geografia"] == "Local")
                          & cdr_movistar["Volumen"] > 0
                          & cdr_movistar["Precio"] > 0)
  
  movistardatos[, "Volumen"] <- movistardatos["Volumen"] / 1024
  MovMbAct <- sapply(movistardatos["PdivV"],mean)
  #MovMbAct <-
  # (sum(movistardatos["Precio"]) / (sum((movistardatos["Volumen"])) / n))
  print(MovMbAct)
  #Remoción tablas y variables
  rm(cdr_movistar, mroam, movistarvoz,movistardatos)
}

#ENTEL
{
  cdr_entel <-
    subset(cdr_accesses, cdr_accesses$Proveedor.x == "Entel PCS (CL)")
  
  #CONSUMO TOTAL VOZ
  entelvoz <-
    subset(cdr_entel,
           cdr_entel["Tipo de llamada"] == "Voz" &
             cdr_entel["Duracion"] > 0)
  
  EntTotMin <- (sum(entelvoz["Duracion"]) / 60) / n
  print(EntTotMin)
  
  #CONSUMO VOZ ENTRE USUARIOS SAAM SA
  entelvozonnet <-
    subset(entelvoz,
           entelvoz["NET"] == 1)
  
  EntVozOnNet <- (sum(entelvozonnet["Duracion"]) / 60) / n
  rm(entelvozonnet)
  print(EntVozOnNet)
  
  #CONSUMO VOZ A TODO DESTINO
  EntATodDes <- EntTotMin - EntVozOnNet
  print(EntATodDes)
  
  #SMARTPHONES GAMA ALTA
  #SMARTPHONES GAMA MEDIA
  #---
  #BAM O SERVICIOS DE TELEMETRÍA
  EntBAMm <- subset(usoplantjointipo,usoplantjointipo["Tipo"] == "BAM"
                    &usoplantjointipo["Proveedor"]=="Entel PCS (CL)")
  EntBAM <-lengths(EntBAMm["Acceso fix"])
  rm(EntBAMm)
  #MENSAJERÍA SMS
  entelSMS <-
    subset(
      cdr_entel,
      cdr_entel["Tipo de llamada"] == "SMS"
      &
        cdr_entel["Precio"] > 0 &
        (cdr_entel["Geografia"] == "Local" |
           cdr_entel["Geografia"] == "Nacional desconocido")
    )
  EntSms <- (sapply(entelSMS["Precio"], median))
  print(EntSms)
  rm (entelSMS)
  
  #MENSAJERÍA MMS
  entelMMS <-
    subset(
      cdr_entel,
      cdr_entel["Tipo de llamada"] == "MMS"
      &
        cdr_entel["Precio"] > 0 &
        (cdr_entel["Geografia"] == "Local" |
           cdr_entel["Geografia"] == "Nacional desconocido")
    )
  
  EntMms <- (sapply(entelMMS["Precio"], median))
  print(EntMms)
  rm (entelMMS)
  
  #USUARIOS ROAMING ON DEMAND
  #ROAMING VOZ
  mroam <-
    subset(cdr_entel,
           cdr_entel["Geografia"] == "Roaming saliente" |
             cdr_entel["Geografia"] == "Roaming entrante")
  mroamvoz <-
    subset(mroam,
           mroam["Tipo de llamada"] == "Voz" &
             mroam["Duracion"] > 0)
  
  EntRoaVoz <- (sum(mroamvoz["Duracion"]) / 60 / n)
  print(EntRoaVoz)
  rm(mroamvoz)
  #ROAMING DATOS
  mroamdat <- subset(mroam,
                     mroam["Tipo de llamada"] == "Datos" &
                       mroam["Volumen"] > 0)
  
  EntRoaDat <- (sum(mroamdat["Volumen"]) / 1024 / n)
  print(EntRoaDat)
  rm(mroamdat)
  #ROAMING MENSAJES
  mroamsms <- subset(mroam,
                     mroam["Tipo de llamada"] == "SMS" &
                       mroam["Precio"] > 0)
  
  EntRoaSms <- (sapply(mroamsms["Precio"], median))
  print(EntRoaSms)
  rm(mroamsms)
  
  #Internacional Voz
  entelvozint <-
    subset(entelvoz, entelvoz["Geografia"] == "A internacional")
  entelvozint["Duracion"]<-entelvozint["Duracion"]/60
  EntIntVoz <- sapply(entelvozint["Duracion"],sum)
  print (EntIntVoz)
  
  #---
  #$/minuto actual
  entelvoz <-
    subset(entelvoz,
           entelvoz["Precio"] > 0  &
             #entelvoz["Mes"] == max(entelvoz["Mes"]) &
             (entelvoz["Geografia"] == "Nacional desconocido" |
                entelvoz["Geografia"] == "Local"))
  EntMinAct<-sapply(entelvoz["PdivD"],mean)
  #EntMinAct <-
  # (sum(entelvoz["Precio"]) / (sum(entelvoz["Duracion"]) / 60))
  print(EntMinAct)
  
  #$/Mb Actual
  enteldatos <- subset(cdr_entel,
                       cdr_entel["Tipo de llamada"] == "Datos"
                       & (cdr_entel["Geografia"] == "Nacional Desconocido" |
                            cdr_entel["Geografia"] == "Local")
                       & cdr_entel["Volumen"] > 0
                       & cdr_entel["Precio"] > 0)
  
  #enteldatos[, "Volumen"] <- enteldatos["Volumen"] / 1024
  EntMbAct <- sapply(enteldatos["PdivV"],mean)
  #EntMbAct <-
  #(sum(enteldatos["Precio"]) / (sum((enteldatos["Volumen"])) / n))
  print(EntMbAct)
  #Remoción tablas y variables
  rm(cdr_entel, mroam, entelvoz, enteldatos,entelvozint)
}

