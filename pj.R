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
#CREACION usos con accesos unicos


  uso2 <- uso
  a<-duplicated(uso2[["Acceso"]],fromLast = TRUE )
  uso2[["duplicado"]] <-a
  uso2<-subset(uso2,uso2[["duplicado"]] == FALSE)
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
  UTP_accesses<-
  merge (
    usoplantjointipo,
    ACCESSES,
    by.x = "Acceso fix",
    by.y = "Acceso fix",
    all.x = TRUE
  )
 
  rm(uso2,usounicojoinPLANt,a,PLAN2)
  #Datos a sacar
  nM<-c()
  nE<-c()
  datEn<-c()
  datM<-c()
  EntATodDes<-c()
  EntBAM<-c()
  EntIntVoz<-c()
  EntMbAct<-c()
  EntMinAct<-c()
  EntMms<-c()
  EntRoaDat<-c()
  EntRoaSms<-c()
  EntRoaVoz<-c()
  EntSms<-c()
  EntTotMin<-c()
  EntVozOnNet<-c()
  
  MovATodDes<-c()
  MovBAM<-c()
  #MovIntVoz<-c() solo esta en entel
  MovMbAct<-c()
  MovMinAct<-c()
  MovMms<-c()
  MovRoaDat<-c()
  MovRoaSms<-c()
  MovRoaVoz<-c()
  MovSms<-c()
  MovTotMin<-c()
  MovVozOnNet<-c()
  
  nomemp<-unique(CUENTAS["Empresa"])
  m<-lapply(nomemp,as.character)
  CC<-as.numeric(lengths(nomemp))
  

  
  UTP_accesses["Proveedor.x"] <- NULL
  UTP_accesses["Proveedor.y"] <- NULL
  
  UTP_accesses <-
    merge (
      UTP_accesses,
      CUENTAS,
      by.x = "Proveedor Nivel 3",
      by.y = "Cuenta Cliente",
      all.x = TRUE
    )
  UTP_accesses2<-UTP_accesses
  for(i in 0:CC+1){
    UTP_accesses<-UTP_accesses2
    if (i==CC+1){
      print("datos generales")
    }else{
      print(i)
      print(m[["Empresa"]][i])
      UTP_accesses<-
        subset(UTP_accesses,
               UTP_accesses["Empresa"]==m[["Empresa"]][i]
        )
    }
  #BAM O SERVICIOS DE TELEMETRÍA MOVISTAR
  MovBAMm <- subset(UTP_accesses,UTP_accesses[["Tipo"]] == "BAM"
                    &UTP_accesses[["Proveedor.x"]]=="Movistar CL"
  )
  MovBAM[i] <-as.numeric(length(MovBAMm[["Acceso fix"]]))
  rm(MovBAMm)
  #BAM O SERVICIOS DE TELEMETRÍA ENTEL
  EntBAMm <- subset(UTP_accesses,UTP_accesses[["Tipo"]] == "BAM"
                    &UTP_accesses[["Proveedor.x"]]=="Entel PCS (CL)")
  EntBAM[i] <-as.numeric(length(EntBAMm[["Acceso fix"]]))
  rm(EntBAMm)
  }
  print(MovBAM)
  print(EntBAM)
  
#Eleccion de Centro de Facturacion
cdr_accesses <-
  merge (
    cdr_accesses,
    CUENTAS,
    by.x = "Proveedor Nivel 3.x",
    by.y = "Cuenta Cliente",
    all.x = TRUE
  )
cdr_accesses2<-cdr_accesses
nomemp<-unique(CUENTAS["Empresa"])

m<-lapply(nomemp,as.character)

CC<-as.numeric(lengths(nomemp))





for(i in 0:CC+1){
  cdr_accesses<-cdr_accesses2
if (i==CC+1){
  print("datos generales")
}else{
  print(i)
  print(m[["Empresa"]][i])
     cdr_accesses<-
       subset(cdr_accesses,
         cdr_accesses["Empresa"]==m[["Empresa"]][i]
       )
}

#MOVISTAR
  
{
  cdr_movistar <-
    subset(cdr_accesses, cdr_accesses[["Proveedor.x"]] == "Movistar CL")
  datM[i]<-as.numeric(lengths(cdr_movistar["Tipo de llamada"]))
  if(datM[[i]]!=0){
  #CONSUMO TOTAL VOZ
    print("Datos Movistar")
    print(datM[[i]])
  movistarvoz <-
    subset(cdr_movistar,
           cdr_movistar["Tipo de llamada"] == "Voz" &
             cdr_movistar["Duracion"] > 0)
  nM[i]<-as.numeric(length(unique(cdr_movistar[["Mes"]])))
  MovTotMin[i] <- (sum(movistarvoz["Duracion"]) / 60) / nM[[i]]
  print("MovTotMin")
  print(MovTotMin[[i]])
  #CONSUMO VOZ ENTRE USUARIOS SAAM SA
  movistarvozonnet <-
    subset(movistarvoz,
           movistarvoz["NET"] == 1&
             (movistarvoz["Geografia"] == "Local"|
                movistarvoz["Geografia"]=="Nacional desconocido")
           )
  
  MovVozOnNet[i] <- (sum(movistarvozonnet["Duracion"]) / 60) / nM[[i]]
  rm(movistarvozonnet)
  print("MovVozOnNet")
  print(MovVozOnNet[[i]])
  
  #CONSUMO VOZ A TODO DESTINO
  MovATodDes[i] <- MovTotMin[[i]] - MovVozOnNet[[i]]
  print("MovATodDes")
  print(MovATodDes[[i]])
 
  #SMARTPHONES GAMA ALTA
  #SMARTPHONES GAMA MEDIA
  #---
  
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
  MovSms[i]<-as.numeric(sapply(movistarSMS["Precio"], median))
  print("MovSms")
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
  
  MovMms[i]<-as.numeric(sapply(movistarMMS["Precio"], median))
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
  datM2<-as.numeric(lengths(mroamvoz["Tipo de llamada"]))
  if(datM2!=0){
  MovRoaVoz[i]<-(sum(mroamvoz["Duracion"]) / 60 / nM[[i]])
  print(MovRoaVoz)
  }
  rm(mroamvoz)
  #ROAMING DATOS
  mroamdat <- subset(mroam,
                     mroam["Tipo de llamada"] == "Datos" &
                       mroam["Volumen"] > 0)
  datM2<-as.numeric(lengths(mroamdat["Tipo de llamada"]))
  if(datM2!=0){
  MovRoaDat[i]<-(sum(mroamdat["Volumen"]) / 1024 / nM[[i]])
  print(MovRoaDat)
  }
  rm(mroamdat)
  #ROAMING MENSAJES
  mroamsms <- subset(mroam,
                     mroam["Tipo de llamada"] == "SMS" &
                       mroam["Precio"] > 0)
  
  MovRoaSms[i]<-as.numeric(sapply(mroamsms["Precio"], median))
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
          movistarvoz["Geografia"] == "Nacional desconocido" |
            movistarvoz["Geografia"] == "Local"
        )
    )
  
  #MovMinAct<-(sum(movistarvoz["Precio"]) / (sum(movistarvoz["Duracion"]) / 60))
  #movistarvoz[,"PdivD"] <- movistarvoz[,"Precio"]/(movistarvoz[,"Duracion"]/60)
  MovMinAct[i]<-as.numeric(sapply(movistarvoz["PdivD"],mean))
  print(MovMinAct)
  
  #$/Mb Actual
  movistardatos <- subset(cdr_movistar,
                          cdr_movistar["Tipo de llamada"] == "Datos"
                          & (cdr_movistar["Geografia"] == "Nacional Desconocido" |
                               cdr_movistar["Geografia"] == "Local")
                          & cdr_movistar["Volumen"] > 0
                          & cdr_movistar["Precio"] > 0)
  
  movistardatos[, "Volumen"] <- movistardatos["Volumen"] / 1024
  MovMbAct <- as.numeric(sapply(movistardatos["PdivV"],mean))
  #MovMbAct <-
  # (sum(movistardatos["Precio"]) / (sum((movistardatos["Volumen"])) / n))
  print("MovMbAct")
  print(MovMbAct)
  #Remoción tablas y variables
  rm(cdr_movistar, mroam, movistarvoz,movistardatos)
}
}

#ENTEL
{
  cdr_entel <-
    subset(cdr_accesses, cdr_accesses[["Proveedor.x"]] == "Entel PCS (CL)")
  datEn[i]<-as.numeric(lengths(cdr_entel["Tipo de llamada"]))
  if(datEn[[i]]!=0){
  #CONSUMO TOTAL VOZ
    print("Datos entel")
    print(datEn[[i]])
  entelvoz <-
    subset(cdr_entel,
           cdr_entel["Tipo de llamada"] == "Voz" &
             cdr_entel["Duracion"] > 0)
  nE[i]<-as.numeric(length(unique(cdr_entel[["Mes"]])))
  EntTotMin[i] <- (sum(entelvoz["Duracion"]) / 60) / nE[[i]]
  print("EntTotMin")
  print(EntTotMin)
  
  #CONSUMO VOZ ENTRE USUARIOS SAAM SA
  entelvozonnet <-
    subset(entelvoz,
           entelvoz["NET"] == 1)
  
  EntVozOnNet[i] <- (sum(entelvozonnet["Duracion"]) / 60) / nE[[i]]
  rm(entelvozonnet)
  print(EntVozOnNet)
  
  #CONSUMO VOZ A TODO DESTINO
  EntATodDes[i] <- EntTotMin[[i]] - EntVozOnNet[[i]]
  print(EntATodDes)
  
  #SMARTPHONES GAMA ALTA
  #SMARTPHONES GAMA MEDIA
  #---
  
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
  EntSms[i] <- as.numeric(sapply(entelSMS["Precio"], median))
  print("EntSms")
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
  
  EntMms[i] <- as.numeric(sapply(entelMMS["Precio"], median))
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
  
  EntRoaVoz[i] <- (sum(mroamvoz["Duracion"]) / 60 / nE[[i]])
  print(EntRoaVoz)
  rm(mroamvoz)
  #ROAMING DATOS
  mroamdat <- subset(mroam,
                     mroam["Tipo de llamada"] == "Datos" &
                       mroam["Volumen"] > 0)
  datM2<-as.numeric(lengths(mroamdat["Tipo de llamada"]))
  if(datM2!=0){
  EntRoaDat[i] <- (sum(mroamdat["Volumen"]) / 1024 / nE[[i]])
  print(EntRoaDat)
  }
  rm(mroamdat)
  #ROAMING MENSAJES
  mroamsms <- subset(mroam,
                     mroam["Tipo de llamada"] == "SMS" &
                       mroam["Precio"] > 0)
  datM2<-as.numeric(lengths(mroamsms["Tipo de llamada"]))
  if(datM2!=0){
  EntRoaSms[i] <- as.numeric(sapply(mroamsms["Precio"], median))
  print(EntRoaSms)
  }
  rm(mroamsms)
  
  #Internacional Voz
  entelvozint <-
    subset(entelvoz, entelvoz["Geografia"] == "A internacional")
  entelvozint["Duracion"]<-entelvozint["Duracion"]/60
  EntIntVoz[i] <- as.numeric(sapply(entelvozint["Duracion"],sum))
  print (EntIntVoz)
  
  #---
  #$/minuto actual
  entelvoz <-
    subset(entelvoz,
           entelvoz["Precio"] > 0  &
             #entelvoz["Mes"] == max(entelvoz["Mes"]) &
             (entelvoz["Geografia"] == "Nacional desconocido" |
                entelvoz["Geografia"] == "Local"))
  EntMinAct[i]<-as.numeric(sapply(entelvoz["PdivD"],mean))
  #EntMinAct <-
  # (sum(entelvoz["Precio"]) / (sum(entelvoz["Duracion"]) / 60))
  print("EntMinAct")
  print(EntMinAct)
  
  #$/Mb Actual
  enteldatos <- subset(cdr_entel,
                       cdr_entel["Tipo de llamada"] == "Datos"
                       & (cdr_entel["Geografia"] == "Nacional Desconocido" |
                            cdr_entel["Geografia"] == "Local")
                       & cdr_entel["Volumen"] > 0
                       & cdr_entel["Precio"] > 0)
  
  #enteldatos[, "Volumen"] <- enteldatos["Volumen"] / 1024
  EntMbAct[i] <- as.numeric(sapply(enteldatos["PdivV"],mean))
  #EntMbAct <-
  #(sum(enteldatos["Precio"]) / (sum((enteldatos["Volumen"])) / n))
  print(EntMbAct)
  #Remoción tablas y variables
  rm(cdr_entel, mroam, entelvoz, enteldatos,entelvozint)
  }
 print(i)
}
}

print(m)
print(datM)
print(datEn)
print(MovVozOnNet)
