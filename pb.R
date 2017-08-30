#MOVISTAR
cdr_accesses <-
  merge (cdr, ACCESSES, by.x = "Numero de llamada fix", by.y = "Acceso fix")

cdr_accesses <-
  merge (cdr_accesses, ACCESSES, by.x = "Numero llamado", by.y = "Acceso fix", all.x=TRUE)

cdr_accesses[, "NET"] <-
  ifelse (cdr_accesses["Proveedor Nivel 2.x"] == cdr_accesses["Proveedor Nivel 2.y"] &
            cdr_accesses["Proveedor Nivel 3.x"] == cdr_accesses["Proveedor Nivel 3.y"],1,0)

cdr_movistar <-
  subset(cdr_accesses, cdr_accesses$Proveedor.x == "Movistar CL")

#CONSUMO TOTAL VOZ
movistarvoz <-
  subset(cdr_movistar,
         cdr_movistar$"Tipo de llamada" == "Voz" &
           cdr_movistar$Duracion > 0)

print((sum(movistarvoz$Duracion) / 60))

#CONSUMO VOZ ENTRE USUARIOS SAAM
movistarvozonnet <-
  subset(movistarvoz,
         movistarvoz$"NET" == 1)

print((sum(movistarvozonnet$Duracion) / 60/4))

#CONSUMO VOZ A TODO DESTINO

#SMARTPHONES GAMA ALTA

#SMARTPHONES GAMA MEDIA
