cdr_accesses <- merge(cdr,ACCESSES, by.x = "Numero de llamada fix", by.y = "Acceso fix")
#BAM o Servicios de Telemetria ->plan datos por dispositivos

#Precio por SMS

MensajeriaSMS <- subset(cdr_accesses$Precio, (cdr_accesses$Geografia == "Local" | cdr_accesses$Geografia == "Nacional desconocido")
                                              &cdr_accesses$Precio>0 
                                              &cdr_accesses$Precio<100
                                              &cdr_accesses$Proveedor == "Movistar CL"
                                              &cdr_accesses$`Tipo de llamada` == "SMS")
a <- mean(MensajeriaSMS)



#Precio por MMS -> no aplica para  SAAM
MensajeriaMMS <- subset(cdr_accesses$Precio, (cdr_accesses$Geografia == "Local" | cdr_accesses$Geografia == "Nacional desconocido")
                                              &cdr_accesses$Precio>0 
                                              &cdr_accesses$Proveedor == "Movistar CL"
                                              &cdr_accesses$`Tipo de llamada` == "MMS")
b <-mean(MensajeriaMMS)
#Usuarios Roaming On Demand

#Roaming Voz
RoamingVoz <- subset(cdr_accesses$Duracion, ( cdr_accesses$Geografia == "Roaming entrante" | cdr_accesses$Geografia == "Roaming saliente")
                     &cdr_accesses$Precio>0 
                     &cdr_accesses$Proveedor == "Movistar CL"
                     &cdr_accesses$`Tipo de llamada` == "Voz"
                     &cdr_accesses$Duracion>60)
mes <- month(cdr_accesses$`Fecha de llamada`)
summary(mes)
c <- sum(RoamingVoz)/60


#Roaming Datos
RoamingDatos <- subset(cdr_accesses$Volumen, ( cdr_accesses$Geografia == "Roaming entrante" | cdr_accesses$Geografia == "Roaming saliente")
                     &cdr_accesses$Precio>0 
                     &cdr_accesses$Proveedor == "Movistar CL"
                     &cdr_accesses$`Tipo de llamada` == "Datos")
d <-sum(RoamingDatos)/1024

#Roaming Mensajes
RoamingSMS <- subset(cdr_accesses$Precio, ( cdr_accesses$Geografia == "Roaming entrante" | cdr_accesses$Geografia == "Roaming saliente")
                     &cdr_accesses$Precio>0
                     &cdr_accesses$Proveedor == "Movistar CL"
                     &cdr_accesses$`Tipo de llamada` == "SMS")
 e <-mean(RoamingSMS)

print(c(a,b,c,d,e))
