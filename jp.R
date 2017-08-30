#MOVISTAR
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

cdr_accesses[,"NET"] <-
  ifelse (cdr_accesses["Proveedor Nivel 2.x"] == cdr_accesses["Proveedor Nivel 2.y"] &
            cdr_accesses["Proveedor Nivel 3.x"] == cdr_accesses["Proveedor Nivel 3.y"],
          1,
          0)

mes1 <- sapply(cdr_accesses["Fecha de llamada"],substr, 6, 7)
mes <- as.numeric(mes1)
cdr_accesses["Mes"] <- mes
rm(c(mes1,mes))

cdr_accesses <-
  subset(cdr_accesses, cdr_accesses["Mes"] != min(cdr_accesses["Mes"]))