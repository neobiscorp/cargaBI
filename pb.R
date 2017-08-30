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

#MOVISTAR
cdr_movistar <-
  subset(cdr_accesses, cdr_accesses$Proveedor.x == "Movistar CL")

#CONSUMO TOTAL VOZ
movistarvoz <-
  subset(cdr_movistar,
         cdr_movistar["Tipo de llamada"] == "Voz" &
           cdr_movistar["Duracion"] > 0)

x <- (sum(movistarvoz["Duracion"]) / 60) / n
#print(x)

#CONSUMO VOZ ENTRE USUARIOS SAAM SA
movistarvozonnet <-
  subset(movistarvoz,
         movistarvoz["NET"] == 1)

y <- (sum(movistarvozonnet["Duracion"]) / 60) / n
#print(y)

#CONSUMO VOZ A TODO DESTINO
z <- x - y
#print(z)
print(c(x, y, z))

#SMARTPHONES GAMA ALTA
#SMARTPHONES GAMA MEDIA
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

print(sum(movistarvoz["Precio"]) / (sum(movistarvoz["Duracion"]) / 60))

#Remoción tablas y variables
rm(cdr_movistar, movistarvoz, movistarvozonnet, x, y, z)
