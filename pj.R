#ajustando la base de datos
cdr_accesses <-
  merge(cdr, ACCESSES, by.x = "Numero de llamada fix", by.y = "Acceso fix")
mes1 <- substr(cdr_accesses$`Fecha de llamada`, 6, 7)
mes <- as.numeric(mes1)
cdr_accesses <- data.frame(cdr_accesses, mes)
cdr_accesses <-
  subset(cdr_accesses, cdr_accesses$mes != min(cdr_accesses$mes))
n <-length(unique(cdr_accesses$mes))

#BAM o Servicios de Telemetria ->plan datos por dispositivos

#Precio por SMS

MensajeriaSMS <-
  subset(
    cdr_accesses$Precio,
    (
      cdr_accesses$Geografia == "Local" |
        cdr_accesses$Geografia == "Nacional desconocido"
    )
    &
      cdr_accesses$Precio > 0
    &
      cdr_accesses$Precio < 100
    &
      cdr_accesses$Proveedor == "Movistar CL"
    &
      cdr_accesses$Tipo.de.llamada == "SMS"
  )
a <- mean(MensajeriaSMS)



#Precio por MMS -> no aplica para  SAAM
MensajeriaMMS <-
  subset(
    cdr_accesses$Precio,
    (
      cdr_accesses$Geografia == "Local" |
        cdr_accesses$Geografia == "Nacional desconocido"
    )
    &
      cdr_accesses$Precio > 0
    &
      cdr_accesses$Proveedor == "Movistar CL"
    &
      cdr_accesses$Tipo.de.llamada == "MMS"
  )
b <- mean(MensajeriaMMS)
#Usuarios Roaming On Demand

#Roaming Voz
RoamingVoz3 <-
  subset(
    cdr_accesses$Duracion,
    (
      cdr_accesses$Geografia == "Roaming entrante" |
        cdr_accesses$Geografia == "Roaming saliente"
    )
    & cdr_accesses$Proveedor == "Movistar CL"
    & cdr_accesses$Tipo.de.llamada == "Voz"
    & cdr_accesses$Duracion <= 60
    & cdr_accesses$Duracion > 0
  )

i <- 01
prommesroamingVoz <- c()
while (i <= 12) {
  RoamingVoz2 <-
    subset(
      cdr_accesses$Duracion,
      (
        cdr_accesses$Geografia == "Roaming entrante" |
          cdr_accesses$Geografia == "Roaming saliente"
      )
      & cdr_accesses$Proveedor == "Movistar CL"
      & cdr_accesses$Tipo.de.llamada == "Voz"
      & cdr_accesses$Duracion > 60
      & cdr_accesses$mes == i
    )
  prommesroamingVoz[i] <- sum(RoamingVoz2)
  i <- i + 1
  print(i - 1)
}
print(prommesroamingVoz)
VRmin <- mean(subset(prommesroamingVoz / 60, prommesroamingVoz > 0))
#USAR VRminajustado para presentar RoamingVoz
VRminajustado <-
  (VRmin + (length(RoamingVoz3) / length(
    subset(prommesroamingVoz, prommesroamingVoz > 0)
  )))
f <- sum(prommesroamingVoz[c(2, 3, 4, 5)]) / 4 / 60
#Roaming Datos
RoamingDatos <-
  subset(
    cdr_accesses$Volumen,
    (
      #cdr_accesses$Geografia == "Roaming entrante" |
        cdr_accesses$Geografia == "Roaming saliente"
    )
    & cdr_accesses$Precio > 0
    & cdr_accesses$Proveedor == "Movistar CL"
    & cdr_accesses$Tipo.de.llamada == "Datos"
  )

d <- sum(RoamingDatos) / 1024
summary(RoamingDatos/1024)

#Roaming Mensajes
RoamingSMS <-
  subset(
    cdr_accesses$Precio,
    (
      cdr_accesses$Geografia == "Roaming entrante" |
        cdr_accesses$Geografia == "Roaming saliente"
    )
    & cdr_accesses$Precio > 0
    & cdr_accesses$Proveedor == "Movistar CL"
    & cdr_accesses$Tipo.de.llamada == "SMS"
  )
e <- mean(RoamingSMS)

print(c(a, b, c, d, e))
