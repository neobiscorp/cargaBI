{
  #Se busca rellenar los campos vacios de los centros de facturacion con informacion relevante
  for(i in 1:length(uso[["Acceso"]])){
    if (is.na(uso[["Acceso"]][i])==TRUE){ #Forma de identificar a los centros de facturacion en uso
      uso[["Acceso"]][i]<-uso[["Nombre"]][i] #se le pone el nombre del centro a la columna Acceso (puede quedar mal pero sirve que no quede vacia)
      uso[["Centro de facturacion"]][i]<-uso[["Nombre"]][i] #se le pone el nombre del centro a la columna Centro de facturacion 
      uso[["Acceso fix"]][i]<-uso[["Nombre"]][i] #se le pone el nombre del centro a la columna Acceso fix
      
    }
  }
  
  uso[["Nombre"]]<-NULL #Eliminamos la columna Nombre que es innecesaria
  #Dejamos los proveedores relevantes para conseguir la informacion necesaria de ACCESSES
    ACCESSES2<-subset(ACCESSES,
                       ACCESSES[["Proveedor"]]== "Movistar CL" |
                       ACCESSES[["Proveedor"]] == "Entel PCS (CL)"|
                        ACCESSES[["Proveedor"]]== "Claro CL"
                     )
  ASSOCIATIONS["Acceso fix"]<-substr(ASSOCIATIONS[["Acceso"]],3,1000000L) #Se crea Acceso fix como parametro para unir
  #Se borran las columnas innecesarias que causaran distorcion sobre los nombres finales de las columnas
ACCESSES2[["Acceso"]]<-NULL
ACCESSES2[["Proveedor"]]<-NULL
ACCESSES2[["Tipo"]]<-NULL
# se une uso con ACCESSES 
  U_accesses <- merge(uso,
                      ACCESSES2,
                      by.x = "Acceso fix",
                      by.y = "Acceso fix",
                      all.x = TRUE)
  ASSOCIATIONS2 <- ASSOCIATIONS
  ASSOCIATIONS2[["Acceso"]]<-NULL
  #SE une Uso y ACCESSES con ASSOCIATIONS
  UAAD_users<- merge(U_accesses,
                          ASSOCIATIONS2,
                          by.x = "Acceso fix",
                          by.y = "Acceso fix",
                          all.x = TRUE
                          )
  rm(ASSOCIATIONS2,U_accesses) #Se borran las tablas que no aportan
  

#Anomalias pueden ser detectadas si se compara el importe de las opciones descontadas con el plano tarifario rebajado
#se modifica plan eliminando las columnas innecesarias
  PLAN2 <- PLAN
  PLAN2[["Acceso"]] <- NULL
  PLAN2[, 'Proveedor2'] <- PLAN2[["Proveedor"]]
  PLAN2[["Proveedor"]] <- NULL
  PLAN2[["Centro de facturacion"]] <- NULL
  #Se dejan solo los que son plano tarifario para el analisis
  PLAN2 <- subset(PLAN2,
                  PLAN2[["Tipo de producto"]] == "Plano tarifario")
  #Se une plan teniendo el cuidado de que con los cambios de proveedor no haya una duplicacion de datos
  UAADP_usos <- merge(
    UAAD_users,
    PLAN2,
    #by.x = "Acceso fix",
    #by.y = "Acceso fix",
    by.x = c("Acceso fix", "Proveedor"),
    by.y = c("Acceso fix", "Proveedor2"),
    all.x = TRUE
  )
  UAADP_usos[["Proveedor2"]]<-NULL #Se elimina la columna sobrante
  #Se dejan los proveedores relevantes para analizar
  UAADP_usos <- subset(
    UAADP_usos,
    UAADP_usos[["Proveedor"]] == "Movistar CL" |
      UAADP_usos[["Proveedor"]] == "Entel PCS (CL)" |
      UAADP_usos[["Proveedor"]] == "Claro CL"
  )
  rm(ACCESSES2, UAAD_users, PLAN2) #Se borran las tablas que no aportan
}

