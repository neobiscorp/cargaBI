#ajustando la base de datos
cdr_accesses <-
  merge(cdr, ACCESSES, by.x = "Numero de llamada fix", by.y = "Acceso fix")
mes1 <- sapply(cdr_accesses["Fecha de llamada"],substr,6,7)
mes <- as.numeric(mes1)
rm(mes1)
cdr_accesses["Mes"] <- mes
cdr_accesses <-
  subset(cdr_accesses, cdr_accesses["Mes"] != min(cdr_accesses["Mes"]))

cdr_accesses <-sapply(cdr_accesses,subset,cdr_accesses,cdr_accesses["Proveedor"] == "Movistar CL")
cdr_accesses <-subset(cdr_accesses,cdr_accesses["Proveedor.x"] == "Movistar CL")
rm(mes)

n <-length(unique(cdr_accesses["mes"]))
n1 <-sapply(cdr_accesses["Mes"],unique)
n <-length(n1)
rm(n1)
#BAM o Servicios de Telemetria ->plan datos por dispositivos
mes1 <- substr(uso["Fecha"], 6, 7)
mes <- as.numeric(mes1)
uso <- data.frame(uso,mes)
usoM <- subset(uso,uso["Proveedor.x"] == "Movistar CL")
n2 <-length(unique(usoM["mes2"]))
lineas <- length(unique(usoM["Acceso"]))



#Precio por SMS

MensajeriaSMS <-
  subset(
    cdr_accesses["Precio"],
    (cdr_accesses["Geografia"] == "Local" |
       cdr_accesses["Geografia"] == "Nacional desconocido")
    &cdr_accesses["Precio"] > 0
    &cdr_accesses["Precio"] < 100
    &cdr_accesses["Tipo de llamada"] == "SMS")
MensajeriaSMS2 <- as.double(MensajeriaSMS)
a <- sapply(MensajeriaSMS,mean)



#Precio por MMS -> no aplica para  SAAM
MensajeriaMMS <-
  subset(
    cdr_accesses["Precio"],
    (cdr_accesses["Geografia"] == "Local" |
       cdr_accesses["Geografia"] == "Nacional desconocido")
    &cdr_accesses["Precio"] > 0
    &cdr_accesses["Tipo de llamada"] == "MMS")
b <- sapply(MensajeriaMMS,mean)
#Usuarios Roaming On Demand




#Roaming Voz
RoamingVoz3 <-
  subset(
    cdr_accesses["Duracion"],
    (cdr_accesses["Geografia"] == "Roaming entrante" |
       cdr_accesses["Geografia"] == "Roaming saliente")
    & cdr_accesses["Tipo de llamada"] == "Voz"
    & cdr_accesses["Duracion"] <= 60
    & cdr_accesses["Duracion"] > 0
  )

i <- 01
prommesroamingVoz <- c()
while (i <= 12) {
  RoamingVoz2 <-
    subset(
      cdr_accesses["Duracion"],
      (cdr_accesses["Geografia"] == "Roaming entrante" |
         cdr_accesses["Geografia"] == "Roaming saliente")
      & cdr_accesses["Tipo de llamada"] == "Voz"
      & cdr_accesses["Duracion"] > 60
      & cdr_accesses["Mes"] == i
    )
  prommesroamingVoz[i] <- sapply(RoamingVoz2,sum)
  i <- i + 1
}
print(prommesroamingVoz)
VRmin1<-subset(prommesroamingVoz,prommesroamingVoz>0)
VRmin <- mean(VRmin1/60)

#USAR VRminajustado para presentar RoamingVoz
ajuste<-lengths(RoamingVoz3)
VRminajustado <-
  VRmin + (lengths(RoamingVoz3) / n)




#Roaming Datos
RoamingDatos <-
  subset(
    cdr_accesses["Volumen"],
    cdr_accesses["Geografia"] == "Roaming saliente"
    & cdr_accesses["Precio"] > 0
    & cdr_accesses["Tipo de llamada"] == "Datos"
  )

d <- sum(RoamingDatos) / 1024

maxRD <-max(RoamingDatos/1024)

#Roaming Mensajes
RoamingSMS <-
  subset(
    cdr_accesses["Precio"],
    (cdr_accesses["Geografia"] == "Roaming entrante" |
       cdr_accesses["Geografia"] == "Roaming saliente")
    & cdr_accesses["Precio"] > 0
    & cdr_accesses["Tipo de llamada"] == "SMS"
  )
e <- mean(RoamingSMS)

print(c(a, b, c, d, e))