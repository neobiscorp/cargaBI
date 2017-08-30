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

mes1 <- substr(cdr_accesses$`Fecha de llamada`, 6, 7)
mes <- as.numeric(mes1)
rm(mes1)
cdr_accesses <- data.frame(cdr_accesses, mes)

cdr_accesses <-
  subset(cdr_accesses, cdr_accesses$mes != min(cdr_accesses$mes))

#n <- número de meses
n <- length(unique(cdr_accesses$mes))

#MOVISTAR
cdr_movistar <-
  subset(cdr_accesses, cdr_accesses$Proveedor.x == "Movistar CL")

#CONSUMO TOTAL VOZ
movistarvoz <-
  subset(cdr_movistar,
         cdr_movistar$Tipo.de.llamada == "Voz" &
           cdr_movistar$Duracion > 0)

<<<<<<< HEAD
x <- (sum(movistarvoz["Duracion"]) / 60) / n
print(x)
=======
  x <- (sum(movistarvoz$Duracion) / 60) / n
#print(x)
>>>>>>> e4a710391ec70476295529a6bef573aefd248b24

#CONSUMO VOZ ENTRE USUARIOS SAAM
movistarvozonnet <-
  subset(movistarvoz,
         movistarvoz$"NET" == 1)

<<<<<<< HEAD
y <- (sum(movistarvozonnet["Duracion"]) / 60) / n
rm(movistarvozonnet)
print(y)
=======
  y <- (sum(movistarvozonnet$Duracion) / 60) / n
#print(y)
>>>>>>> e4a710391ec70476295529a6bef573aefd248b24

#CONSUMO VOZ A TODO DESTINO
z <- x - y
print(z)

#SMARTPHONES GAMA ALTA
#SMARTPHONES GAMA MEDIA
#---
#BAM O SERVICIOS DE TELEMETRÍA
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

print(sapply(movistarSMS["Precio"], median))
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

print(sapply(movistarMMS["Precio"], median))
rm (movistarMMS)

#USUARIOS ROAMING ON DEMAND
#ROAMING VOZ
mroam <-
  subset(cdr_movistar,
         cdr_movistar["Geografia"] == "Roaming saliente" |
           cdr_movistar["Geografia"] == "Roaming entrante")
mroamvoz <-
  subset(mroam,
         mroam["Tipo de llamada"] == "Voz" &
           mroam["Duracion"] > 0)

print(sum(mroamvoz["Duracion"]) / 60 / n)
rm(mroamvoz)
#ROAMING DATOS
mroamdat <- subset(mroam,
                   mroam["Tipo de llamada"] == "Datos" &
                     mroam["Volumen"] > 0)

print(sum(mroamdat["Volumen"]) / 1024 / n)
rm(mroamdat)
#ROAMING MENSAJES
mroamsms <- subset(mroam,
                   mroam["Tipo de llamada"] == "SMS" &
                     mroam["Precio"] > 0)

print(sapply(mroamsms["Precio"], median))
rm(mroamsms)

#---
#$/minuto actual
movistarvoz <-
  subset(
    movistarvoz,
    <<<<<<< HEAD
    movistarvoz["Precio"] > 0  &
      #movistarvoz["Mes"] == max(movistarvoz["Mes"]) &
      =======
      movistarvoz$Precio > 0  &
      movistarvoz$mes == max(movistarvoz$mes) &
      >>>>>>> e4a710391ec70476295529a6bef573aefd248b24
    (
      movistarvoz$Geografia == "Local" |
        movistarvoz$Geografia == "Nacional desconocido"
    )
  )

print(sum(movistarvoz$Precio) / (sum(movistarvoz$Duracion) / 60))

#$/Mb Actual

#Remoción tablas y variables
rm(cdr_movistar, mroam, movistarvoz, x, y, z)


#ENTEL
cdr_entel <-
  subset(cdr_accesses, cdr_accesses$Proveedor.x == "Entel PCS (CL)")

#CONSUMO TOTAL VOZ
entelvoz <-
  subset(cdr_entel,
         cdr_entel["Tipo de llamada"] == "Voz" &
           cdr_entel["Duracion"] > 0)

x <- (sum(entelvoz["Duracion"]) / 60) / n
print(x)

#CONSUMO VOZ ENTRE USUARIOS SAAM SA
entelvozonnet <-
  subset(entelvoz,
         entelvoz["NET"] == 1)

y <- (sum(entelvozonnet["Duracion"]) / 60) / n
rm(entelvozonnet)
print(y)

#CONSUMO VOZ A TODO DESTINO
z <- x - y
print(z)

#SMARTPHONES GAMA ALTA
#SMARTPHONES GAMA MEDIA
#---
#BAM O SERVICIOS DE TELEMETRÍA
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

print(sapply(entelSMS["Precio"], median))
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

print(sapply(entelMMS["Precio"], median))
rm (entelMMS)

#USUARIOS ROAMING ON DEMAND
#ROAMING VOZ
eroam <-
  subset(cdr_entel,
         cdr_entel["Geografia"] == "Roaming saliente" |
           cdr_entel["Geografia"] == "Roaming entrante")
eroamvoz <-
  subset(eroam,
         eroam["Tipo de llamada"] == "Voz" &
           eroam["Duracion"] > 0)

print(sum(eroamvoz["Duracion"]) / 60 / n)
rm(eroamvoz)
#ROAMING DATOS
eroamdat <- subset(eroam,
                   eroam["Tipo de llamada"] == "Datos" &
                     eroam["Volumen"] > 0)

print(sum(eroamdat["Volumen"]) / 1024 / n)
rm(eroamdat)
#ROAMING MENSAJES
eroamsms <- subset(eroam,
                   eroam["Tipo de llamada"] == "SMS" &
                     eroam["Precio"] > 0)

print(sapply(eroamsms["Precio"], median))
rm(eroamsms)

#---
#$/minuto actual
entelvoz <-
  subset(
    entelvoz,
    entelvoz["Precio"] > 0  &
      #entelvoz["Mes"] == max(entelvoz["Mes"]) &
      (
        entelvoz$Geografia == "Nacional desconocido" |
          entelvoz$Geografia == "Local"
      )
  )

print(sum(entelvoz["Precio"]) / (sum(entelvoz["Duracion"]) / 60))

#$/Mb Actual

#Remoción tablas y variables
rm(cdr_entel, eroam, entelvoz, x, y, z)
