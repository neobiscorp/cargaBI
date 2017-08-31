##################SETUP##############################
#Load Libraries
library(shiny)      #Dashboard
library(RMySQL)     #Secundary MySQL connection, Kill connections
library(DBI)        #Primary MySQL connection
library(openxlsx)   #Read xlsx files
library(data.table) #For merge, rbind and dataframe works

#Function to kill MySQL connections
killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for (con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}

#Increase the maxium size of an uploaded file to 250mb
options(shiny.maxRequestSize = 250 * 1024 ^ 2)

#Function to convert ITEM months to number
y <-
  c(
    "ene. " = "01",
    "feb. " = "02",
    "mar. " = "03",
    "abr. " = "04",
    "may. " = "05",
    "jun. " = "06",
    "jul. " = "07",
    "ago. " = "08",
    "sept. " = "09",
    "oct. " = "10",
    "nov. " = "11",
    "dic. " = "12"
  )

#############Start Shiny Server With session############
shinyServer(function(input, output, session) {
  #Run function after pressing execute button
  observeEvent(input$execute, {
    updateButton(session, "execute", style = "warning")
    #name input variables
    client <- input$Client
    export <- input$Export
    planes <- input$planes
    tipos <- input$tipos
    presupuesto <- input$presupuesto
    cuentas <- input$cuentas
    
    #Change name of client input to the same name of the database
    if (client == "Parque Arauco") {
      client <- "pa"
    }
    if (client == "Hogar De Cristo") {
      client <- "hdc"
    }
    if (client == "Aguas Andinas") {
      client <- "AguasAndinas"
    }
    if (client == "Licitacion Movil (Entel y Movistar)") {
      client <- "lmovil"
    }
    
    #Create DB connection variable
    DB <- dbConnect(
      MySQL(),
      user = "root",
      password = "",
      dbname = paste0(client)
    )
    
    #Run this function in case theres a file in the Factura upload
    if (!is.null(input$factura)) {
      #Read the csv of the factura
      facturas <<- read.csv2(input$factura[['datapath']])
      
      #Delete the Estado Column
      facturas[, 'Estado'] <<- NULL
      
      #Rename the columns of the file
      names(facturas)[names(facturas) == 'ï..NÃºmero.de.factura'] <<-
        'Factura'
      names(facturas)[names(facturas) == 'Proveedor'] <<-
        'Proveedor'
      names(facturas)[names(facturas) == 'Mes.de.facturaciÃ³n'] <<-
        'Mes'
      
      #Convert the ITEM months to YYYY/MM/01
      for (i in 1:12) {
        facturas['Mes'] <<-
          lapply(facturas['Mes'], function(x)
            gsub(names(y[i]), y[[i]], x))
      }
      facturas['Fecha'] <<-
        lapply(facturas['Mes'], function(x)
          paste(substr(x , 3 , 6),
                substr(x , 1 , 2),
                "01",
                sep = "/"))
      #Delete the Column MES that its not needed now
      facturas[, 'Mes'] <<- NULL
      
      #If the client is Parque Arauco, run the following
      if (client == "pa") {
        #Rename the columns
        names(facturas)[names(facturas) == 'Total.sin.impuestos..UF.'] <<-
          'Total Sin Impuestos (UF)'
        names(facturas)[names(facturas) == 'Total.sin.impuestos..COP.'] <<-
          'Total Sin Impuestos (COP)'
        names(facturas)[names(facturas) == 'Total.sin.impuestos..PEN.'] <<-
          'Total Sin Impuestos (PEN)'
        
        #Upload to the database the file factura with the following data types
        dbWriteTable(
          DB,
          "facturas",
          facturas,
          field.types = list(
            `Factura` = "varchar(255)",
            `Proveedor` = "varchar(255)",
            `Total Sin Impuestos (UF)` = "double(15,2)",
            `Total Sin Impuestos (COP)` = "double(15,2)",
            `Total Sin Impuestos (PEN)` = "double(15,2)",
            `Fecha` = "date"
          ),
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
        
      }
      
      else{
        names(facturas)[names(facturas) == 'Total.sin.impuestos'] <-
          'Total Sin Impuestos'
        
        dbWriteTable(
          DB,
          "facturas",
          facturas,
          field.types = list(
            `Factura` = "varchar(255)",
            `Proveedor` = "varchar(255)",
            `Total Sin Impuestos` = "double(15,2)",
            `Fecha` = "date"
          ),
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
    }
    #Run the following if the user uploads a file to the usos file input
    if (!is.null(input$usos)) {
      #Clear and create variable and read CSV file
      dataFilesUF <<- NULL
      dataFilesUF <<- lapply(input$usos[['datapath']], read.csv2)
      #Append all usos files to the same dataframe
      uso <<- rbindlist(dataFilesUF)
      
      #For Parque arauco clear and create variables with PEN and COP currency, and append files
      if (!is.null(input$usosPEN)) {
        dataFilesPEN <<- NULL
        dataFilesPEN <<-
          lapply(input$usosPEN[['datapath']], read.csv2)
        usoPEN <<- rbindlist(dataFilesPEN)
      }
      if (!is.null(input$usosCOP)) {
        dataFilesCOP <<- NULL
        dataFilesCOP <<-
          lapply(input$usosCOP[['datapath']], read.csv2)
        usoCOP <<- rbindlist(dataFilesCOP)
      }
      
      #If client is Parque Arauco run the following
      if (client == "pa") {
        ###UF###
        
        #Rename the columns of the csv file
        names(uso)[names(uso) == 'ï..Acceso'] <<-
          'Acceso'
        names(uso)[names(uso) == 'Proveedor'] <<-
          'Proveedor'
        names(uso)[names(uso) == 'PerÃ.odo.de'] <<-
          'Periodo de'
        names(uso)[names(uso) == 'Total..UF.'] <<-
          'Total (UF)'
        names(uso)[names(uso) == 'Plano.tarifario..UF.'] <<-
          'Plano tarifario (UF)'
        names(uso)[names(uso) == 'Uso..UF.'] <<-
          'Uso (UF)'
        names(uso)[names(uso) == 'Servicios..UF.'] <<-
          'Servicios (UF)'
        names(uso)[names(uso) == 'Descuentos..UF.'] <<-
          'Descuentos (UF)'
        names(uso)[names(uso) == 'Voz..UF.'] <<-
          'Voz (UF)'
        names(uso)[names(uso) == 'Voz.nacional..UF.'] <<-
          'Voz Nacional (UF)'
        names(uso)[names(uso) == 'Voz.inter...UF.'] <<-
          'Voz Inter. (UF)'
        names(uso)[names(uso) == 'Datos..UF.'] <<-
          'Datos (UF)'
        names(uso)[names(uso) == 'Datos.nac...UF.'] <<-
          'Datos Nacional (UF)'
        names(uso)[names(uso) == 'Datos.inter...UF.'] <<-
          'Datos Inter. (UF)'
        names(uso)[names(uso) == 'SMS.MMS..UF.'] <<-
          'SMS/MMS (UF)'
        names(uso)[names(uso) == 'N.Â..Copias'] <<-
          'N Copias'
        names(uso)[names(uso) == 'N.Â..Copias.B.N'] <<-
          'N Copias B/N'
        names(uso)[names(uso) == 'N.Â..Copias.Color'] <<-
          'N Copias Color'
        
        ###PEN##
        
        names(usoPEN)[names(usoPEN) == 'ï..Acceso'] <<-
          'Acceso'
        names(usoPEN)[names(usoPEN) == 'PerÃ.odo.de'] <<-
          'Periodo de'
        names(usoPEN)[names(usoPEN) == 'Total..PEN.'] <<-
          'Total (PEN)'
        names(usoPEN)[names(usoPEN) == 'Plano.tarifario..PEN.'] <<-
          'Plano tarifario (PEN)'
        names(usoPEN)[names(usoPEN) == 'Uso..PEN.'] <<-
          'Uso (PEN)'
        names(usoPEN)[names(usoPEN) == 'Servicios..PEN.'] <<-
          'Servicios (PEN)'
        names(usoPEN)[names(usoPEN) == 'Descuentos..PEN.'] <<-
          'Descuentos (PEN)'
        names(usoPEN)[names(usoPEN) == 'Voz..PEN.'] <<-
          'Voz (PEN)'
        names(usoPEN)[names(usoPEN) == 'Voz.nacional..PEN.'] <<-
          'Voz Nacional (PEN)'
        names(usoPEN)[names(usoPEN) == 'Voz.inter...PEN.'] <<-
          'Voz Inter. (PEN)'
        names(usoPEN)[names(usoPEN) == 'Datos..PEN.'] <<-
          'Datos (PEN)'
        names(usoPEN)[names(usoPEN) == 'Datos.nac...PEN.'] <<-
          'Datos Nacional (PEN)'
        names(usoPEN)[names(usoPEN) == 'Datos.inter...PEN.'] <<-
          'Datos Inter. (PEN)'
        names(usoPEN)[names(usoPEN) == 'SMS.MMS..PEN.'] <<-
          'SMS/MMS (PEN)'
        names(usoPEN)[names(usoPEN) == 'N.Â..Copias'] <<-
          'N Copias'
        names(usoPEN)[names(usoPEN) == 'N.Â..Copias.B.N'] <<-
          'N Copias B/N'
        names(usoPEN)[names(usoPEN) == 'N.Â..Copias.Color'] <<-
          'N Copias Color'
        
        
        ###COP###
        
        names(usoCOP)[names(usoCOP) == 'ï..Acceso'] <<-
          'Acceso'
        names(usoCOP)[names(usoCOP) == 'PerÃ.odo.de'] <<-
          'Periodo de'
        names(usoCOP)[names(usoCOP) == 'Total..COP.'] <<-
          'Total (COP)'
        names(usoCOP)[names(usoCOP) == 'Plano.tarifario..COP.'] <<-
          'Plano tarifario (COP)'
        names(usoCOP)[names(usoCOP) == 'Uso..COP.'] <<-
          'Uso (COP)'
        names(usoCOP)[names(usoCOP) == 'Servicios..COP.'] <<-
          'Servicios (COP)'
        names(usoCOP)[names(usoCOP) == 'Descuentos..COP.'] <<-
          'Descuentos (COP)'
        names(usoCOP)[names(usoCOP) == 'Voz..COP.'] <<-
          'Voz (COP)'
        names(usoCOP)[names(usoCOP) == 'Voz.nacional..COP.'] <<-
          'Voz Nacional (COP)'
        names(usoCOP)[names(usoCOP) == 'Voz.inter...COP.'] <<-
          'Voz Inter. (COP)'
        names(usoCOP)[names(usoCOP) == 'Datos..COP.'] <<-
          'Datos (COP)'
        names(usoCOP)[names(usoCOP) == 'Datos.nac...COP.'] <<-
          'Datos Nacional (COP)'
        names(usoCOP)[names(usoCOP) == 'Datos.inter...COP.'] <<-
          'Datos Inter. (COP)'
        names(usoCOP)[names(usoCOP) == 'SMS.MMS..COP.'] <<-
          'SMS/MMS (COP)'
        names(usoCOP)[names(usoCOP) == 'N.Â..Copias'] <<-
          'N Copias'
        names(usoCOP)[names(usoCOP) == 'N.Â..Copias.B.N'] <<-
          'N Copias B/N'
        names(usoCOP)[names(usoCOP) == 'N.Â..Copias.Color'] <<-
          'N Copias Color'
        
        
        #Delete columns that are going to be duplicated after the merge of the 3 currencies
        usoPEN[, 'Acceso'] <<- NULL
        usoPEN[, 'Periodo de'] <<- NULL
        usoPEN[, 'Proveedor'] <<- NULL
        usoPEN[, 'N Copias'] <<- NULL
        usoPEN[, 'N Copias B/N'] <<- NULL
        usoPEN[, 'N Copias Color'] <<- NULL
        
        usoCOP[, 'Acceso'] <<- NULL
        usoCOP[, 'Periodo de'] <<- NULL
        usoCOP[, 'Proveedor'] <<- NULL
        usoCOP[, 'N Copias'] <<- NULL
        usoCOP[, 'N Copias B/N'] <<- NULL
        usoCOP[, 'N Copias Color'] <<- NULL
        
        #Create an ID to join the usos file
        id <<- rownames(uso)
        uso <<- cbind(id = id, uso)
        
        id <<- rownames(usoCOP)
        usoCOP <<- cbind(id = id, usoCOP)
        
        id <<- rownames(usoPEN)
        usoPEN <<- cbind(id = id, usoPEN)
        
        #Merge dataframes by ID in a single dataframe
        uso <<- merge(uso, usoPEN, by = "id")
        uso <<- merge(uso, usoCOP, by = "id")
        
      }
      #If the client its not Parque Arauco and licitacion run the following
      else if (client != "lmovil") {
        #Change the name of the columns
        names(uso)[names(uso) == 'ï..Acceso'] <<- 'Acceso'
        names(uso)[names(uso) == 'Proveedor'] <<- 'Proveedor'
        names(uso)[names(uso) == 'PerÃ.odo.de'] <<-
          'Periodo de'
        names(uso)[names(uso) == 'Total..UF.'] <<- 'Total (UF)'
        names(uso)[names(uso) == 'Plano.tarifario..UF.'] <<-
          'Plano tarifario (UF)'
        names(uso)[names(uso) == 'Uso..UF.'] <<- 'Uso (UF)'
        names(uso)[names(uso) == 'Servicios..UF.'] <<-
          'Servicios (UF)'
        names(uso)[names(uso) == 'Descuentos..UF.'] <<-
          'Descuentos (UF)'
        names(uso)[names(uso) == 'Voz..UF.'] <<- 'Voz (UF)'
        names(uso)[names(uso) == 'Voz.nacional..UF.'] <<-
          'Voz Nacional (UF)'
        names(uso)[names(uso) == 'Voz.inter...UF.'] <<-
          'Voz Inter. (UF)'
        names(uso)[names(uso) == 'Datos..UF.'] <<- 'Datos (UF)'
        names(uso)[names(uso) == 'Datos.nac...UF.'] <<-
          'Datos Nacional (UF)'
        names(uso)[names(uso) == 'Datos.inter...UF.'] <<-
          'Datos Inter. (UF)'
        names(uso)[names(uso) == 'SMS.MMS..UF.'] <<-
          'SMS/MMS (UF)'
        
        #Change the name of the columns for the clients that got Printing services
        if (client != "hdc" &
            client != "AguasAndinas" & client != "LMovil") {
          names(uso)[names(uso) == 'N.Â..Copias'] <<-
            'N Copias'
          names(uso)[names(uso) == 'N.Â..Copias.B.N'] <<-
            'N Copias B/N'
          names(uso)[names(uso) == 'N.Â..Copias.Color'] <<-
            'N Copias Color'
        }
      }
      else {
        #Change the name of the columns for the Licitacion movil
        names(uso)[names(uso) == 'ï..Acceso'] <<- 'Acceso'
        names(uso)[names(uso) == 'Proveedor'] <<- 'Proveedor'
        names(uso)[names(uso) == 'PerÃ.odo.de'] <<-  'Periodo de'
        names(uso)[names(uso) == 'Total..CLP.'] <<- 'Total (CLP)'
        names(uso)[names(uso) == 'Plano.tarifario..CLP.'] <<-
          'Plano tarifario (CLP)'
        names(uso)[names(uso) == 'Uso..CLP.'] <<- 'Uso (CLP)'
        names(uso)[names(uso) == 'Servicios...Otros..CLP.'] <<-
          'Servicios (CLP)'
        names(uso)[names(uso) == 'Descuentos..CLP.'] <<-
          'Descuentos (CLP)'
        names(uso)[names(uso) == 'Voz..CLP.'] <<- 'Voz (CLP)'
        names(uso)[names(uso) == 'Voz.nacional..CLP.'] <<-
          'Voz Nacional (CLP)'
        names(uso)[names(uso) == 'Voz.inter...CLP.'] <<-
          'Voz Inter. (CLP)'
        names(uso)[names(uso) == 'Datos..CLP.'] <<- 'Datos (CLP)'
        names(uso)[names(uso) == 'Datos.nac...CLP.'] <<-
          'Datos Nacional (CLP)'
        names(uso)[names(uso) == 'Datos.inter...CLP.'] <<-
          'Datos Inter. (CLP)'
        names(uso)[names(uso) == 'Voz.hacia.Int..CLP.'] <<-
          'Voz hacia Int (CLP)'
        names(uso)[names(uso) == 'N.Â..Datos.nac.'] <<-
          'N. Datos nac.'
        names(uso)[names(uso) == 'Datos.espec...CLP.'] <<-
          'Datos espec. (CLP)'
        names(uso)[names(uso) == 'SMS..CLP.'] <<- 'SMS (CLP)'
        names(uso)[names(uso) == 'SMS.nac...CLP.'] <<-
          'SMS nac. (CLP)'
        names(uso)[names(uso) == 'SMS.inter...CLP.'] <<-
          'SMS inter. (CLP)'
        names(uso)[names(uso) == 'SMS.espec...CLP.'] <<-
          'SMS espec. (CLP)'
        names(uso)[names(uso) == 'N.Â..Voz.nacional'] <<-
          'N. Voz nacional'
        names(uso)[names(uso) == 'N.Â..Voz.inter.'] <<-
          'N. Voz inter.'
        names(uso)[names(uso) == 'N.Â..Voz.hacia.Int'] <<-
          'N. Voz hacia Int'
        names(uso)[names(uso) == 'N.Â..Voz.roaming'] <<-
          'N. Voz roaming'
        names(uso)[names(uso) == 'N.Â..Datos.inter.'] <<-
          'N. Datos inter.'
        names(uso)[names(uso) == 'N.Â..Voz.Roaming.In'] <<-
          'N. Voz Roaming In'
        names(uso)[names(uso) == 'N.Â..Voz.Roaming.Out'] <<-
          'N. Voz Roaming Out'
        names(uso)[names(uso) == 'N.Â..SMS.nac.'] <<-
          'N. SMS nac.'
        names(uso)[names(uso) == 'N.Â..SMS.inter.'] <<-
          'N. SMS inter.'
        names(uso)[names(uso) == 'Voz.roaming..CLP.'] <<-
          'Voz roaming (CLP)'
        names(uso)[names(uso) == 'Servicios...Opciones..CLP.'] <<-
          'Opciones (CLP)'
        
        #Create a column with the phone number without the 56 (Chile)
        uso[, 'Acceso fix'] <<-
          lapply(uso[, 'Acceso'], function(x)
            substring(x, 3))
      }
      
      #If the client got printer access eliminate the thousand dot separator
      if (client != "hdc" &
          client != "AguasAndinas" & client != "lmovil") {
        uso[, c('N Copias', 'N Copias B/N', 'N Copias Color')] <<-
          lapply(uso[, c('N Copias', 'N Copias B/N', 'N Copias Color')], function(x)
            as.character(gsub("\\.", "", x)))
      }
      
      #Convert the ITEM months to YYYY/MM/01
      for (k in 1:12) {
        uso[, 'Periodo de'] <<-
          lapply(uso[, 'Periodo de'], function(x)
            gsub(names(y[k]), y[[k]], x))
      }
      
      uso[, 'Fecha'] <<-
        lapply(uso[, 'Periodo de'], function(x)
          paste(substr(x , 3 , 6),
                substr(x , 1 , 2),
                "01",
                sep = "/"))
      #Delete the Column MES that its not needed now
      uso[, 'Periodo de'] <<- NULL
      
      if (client == "pa") {
        #Upload to the Parque Arauco DB the data with those data type
        dbWriteTable(
          DB,
          "usos",
          uso,
          field.types = list(
            `Id` = "INT(10)",
            `Acceso` = "varchar(255)",
            `Proveedor` = "varchar(255)",
            `Total (UF)` = "double(15,2)",
            `Plano tarifario (UF)` = "double(15,2)",
            `Uso (UF)` = "double(15,2)",
            `Servicios (UF)` = "double(15,2)",
            `Descuentos (UF)` = "double(15,2)",
            `Voz (UF)` = "double(15,2)",
            `Voz Nacional (UF)` = "double(15,2)",
            `Voz Inter. (UF)` = "double(15,2)",
            `Datos (UF)` = "double(15,2)",
            `Datos Nacional (UF)` = "double(15,2)",
            `Datos Inter. (UF)` = "double(15,2)",
            `SMS/MMS (UF)` = "double(15,2)",
            `N Copias` = "INT(10)",
            `N Copias B/N` = "INT(10)",
            `N Copias Color` = "INT(10)",
            `Fecha` = "date",
            `Total (PEN)` = "double(15,2)",
            `Plano tarifario (PEN)` = "double(15,2)",
            `Uso (PEN)` = "double(15,2)",
            `Servicios (PEN)` = "double(15,2)",
            `Descuentos (PEN)` = "double(15,2)",
            `Voz (PEN)` = "double(15,2)",
            `Voz Nacional (PEN)` = "double(15,2)",
            `Voz Inter. (PEN)` = "double(15,2)",
            `Datos (PEN)` = "double(15,2)",
            `Datos Nacional (PEN)` = "double(15,2)",
            `Datos Inter. (PEN)` = "double(15,2)",
            `SMS/MMS (PEN)` = "double(15,2)",
            `Total (COP)` = "double(15,2)",
            `Plano tarifario (COP)` = "double(15,2)",
            `Uso (COP)` = "double(15,2)",
            `Servicios (COP)` = "double(15,2)",
            `Descuentos (COP)` = "double(15,2)",
            `Voz (COP)` = "double(15,2)",
            `Voz Nacional (COP)` = "double(15,2)",
            `Voz Inter. (COP)` = "double(15,2)",
            `Datos (COP)` = "double(15,2)",
            `Datos Nacional (COP)` = "double(15,2)",
            `Datos Inter. (COP)` = "double(15,2)",
            `SMS/MMS (COP)` = "double(15,2)"
          ),
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
        
      }
      else if (client != "hdc" &
               client != "AguasAndinas" & client != "lmovil") {
        dbWriteTable(
          DB,
          "usos",
          uso,
          field.types = list(
            `Acceso` = "varchar(255)",
            `Proveedor` = "varchar(255)",
            `Total (UF)` = "double(15,2)",
            `Plano tarifario (UF)` = "double(15,2)",
            `Uso (UF)` = "double(15,2)",
            `Servicios (UF)` = "double(15,2)",
            `Descuentos (UF)` = "double(15,2)",
            `Voz (UF)` = "double(15,2)",
            `Voz Nacional (UF)` = "double(15,2)",
            `Voz Inter. (UF)` = "double(15,2)",
            `Datos (UF)` = "double(15,2)",
            `Datos Nacional (UF)` = "double(15,2)",
            `Datos Inter. (UF)` = "double(15,2)",
            `SMS/MMS (UF)` = "double(15,2)",
            `N Copias` = "INT(10)",
            `N Copias B/N` = "INT(10)",
            `N Copias Color` = "INT(10)",
            `Fecha` = "date"
          ),
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      else if (client == "lmovil") {
        #Only select the following columns, if there are more, do not use them
        uso <<-
          subset(
            uso,
            select = c(
              "Acceso",
              "Proveedor",
              "Total (CLP)",
              "Plano tarifario (CLP)",
              "Uso (CLP)",
              "Servicios (CLP)",
              "Descuentos (CLP)",
              "Voz (CLP)",
              "Voz Nacional (CLP)",
              "Voz Inter. (CLP)",
              "Datos (CLP)",
              "Datos espec. (CLP)",
              "Datos Nacional (CLP)",
              "Voz hacia Int (CLP)",
              "Voz roaming (CLP)",
              "SMS (CLP)",
              "SMS nac. (CLP)",
              "SMS inter. (CLP)",
              "Datos Inter. (CLP)",
              "SMS espec. (CLP)",
              "Opciones (CLP)",
              "N. Voz nacional",
              "N. Voz inter.",
              "N. Voz hacia Int",
              "N. Voz roaming",
              "N. Datos nac.",
              "N. Datos inter.",
              "N. Voz Roaming In",
              "N. Voz Roaming Out",
              "N. SMS nac.",
              "N. SMS inter.",
              "Fecha",
              "Acceso fix"
            )
          )
        
        dbWriteTable(
          DB,
          "usos",
          uso,
          field.types = list(
            `Acceso` = "varchar(255)",
            `Proveedor` = "varchar(255)",
            `Total (CLP)` = "double(15,2)",
            `Plano tarifario (CLP)` = "double(15,2)",
            `Uso (CLP)` = "double(15,2)",
            `Servicios (CLP)` = "double(15,2)",
            `Descuentos (CLP)` = "double(15,2)",
            `Voz (CLP)` = "double(15,2)",
            `Voz Nacional (CLP)` = "double(15,2)",
            `Voz Inter. (CLP)` = "double(15,2)",
            `Datos (CLP)` = "double(15,2)",
            `Datos espec. (CLP)` = "double(15,2)",
            `Datos Nacional (CLP)` = "double(15,2)",
            `Voz hacia Int (CLP)` = "double(15,2)",
            `Voz roaming (CLP)` = "double(15,2)",
            `SMS (CLP)` = "double(15,2)",
            `SMS nac. (CLP)` = "double(15,2)",
            `SMS inter. (CLP)` = "double(15,2)",
            `Datos Inter. (CLP)` = "double(15,2)",
            `SMS espec. (CLP)` = "double(15,2)",
            `Opciones (CLP)` = "double(15,2)",
            `N. Voz nacional` = "INT(10)",
            `N. Voz inter.` = "INT(10)",
            `N. Voz hacia Int` = "INT(10)",
            `N. Voz roaming` = "INT(10)",
            `N. Datos nac.` = "INT(10)",
            `N. Datos inter.` = "INT(10)",
            `N. Voz Roaming In` = "INT(10)",
            `N. Voz Roaming Out` = "INT(10)",
            `N. SMS nac.` = "INT(10)",
            `N. SMS inter.` = "INT(10)",
            `Fecha` = "date",
            `Acceso fix` = "varchar(255)"
          ),
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      #Send an MySQL Query that delete spaces inside Accesos
      dbSendQuery(DB, "update `usos` set ACCESO = replace(ACCESO, ' ', '')")
    }
    #Run the following code if theres a file in the export file input
    if (!is.null(export)) {
      #Copy the file uploaded and add the .xlsx file type to the temp file
      file.copy(export$datapath,
                paste(export$datapath, ".xlsx", sep = ""))
      
      #######################################USERS############
      
      if (client != "lmovil") {
        #Read the xlsx file at the sheet USERS
        USERS <- read.xlsx(export$datapath,
                           sheet = "USERS",
                           startRow = 1)
        #Only select the following columns of the file
        USERS <- USERS[c(1, 5, 9)]
        #Rename the columns to the following
        names(USERS) <-
          c("UUI",
            "Nombre",
            "Estado")
        #In case the following file exist, delete it
        file.remove("USERS.txt")
        #Create an txt file from the previus columns, this is needed for getting the right encoding UTF-8
        write.table(USERS, file = "USERS.txt", fileEncoding = "UTF-8")
        #Read the txt File
        USERS <- read.table(file = "USERS.txt", encoding = "UTF-8")
        #Upload to the DB the data
        dbWriteTable(
          DB,
          "users",
          USERS,
          field.types = NULL ,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      #######################################ACCESSES############
      
      ACCESSES <- read.xlsx(export$datapath,
                            sheet = "ACCESSES",
                            startRow = 1)
      file.remove("ACCESSES.txt")
      if (client == "Subsole") {
        ACCESSES <- ACCESSES[c(1, 2, 10, 13, 45, 46, 47, 48, 49)]
        
        write.table(ACCESSES, file = "ACCESSES.txt", fileEncoding = "UTF-8")
        ACCESSES <-
          read.table(file = "ACCESSES.txt", encoding = "UTF-8")
        names(ACCESSES) <-
          c(
            "Acceso",
            "Tipo",
            "Estado",
            "Fecha Expiracion",
            "MANAGEMENT_ORG_1",
            "MANAGEMENT_ORG_2",
            "MANAGEMENT_ORG_3",
            "MANAGEMENT_ORG_4",
            "Proveedor"
          )
        ACCESSES$`Fecha Expiracion` <-
          as.Date(ACCESSES$`Fecha Expiracion`, origin = "1899-12-30")
        dbWriteTable(
          DB,
          "accesses",
          ACCESSES,
          field.types = list(
            Acceso = "varchar(255)",
            Tipo = "varchar(255)",
            Estado = "varchar(255)",
            `Fecha Expiracion` = "date",
            MANAGEMENT_ORG_1 = "varchar(255)",
            MANAGEMENT_ORG_2 = "varchar(255)",
            MANAGEMENT_ORG_3 = "varchar(255)",
            MANAGEMENT_ORG_4 = "varchar(255)",
            Proveedor = "varchar(255)"
          ) ,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      else if (client == "Walmart") {
        ACCESSES <- ACCESSES[c(1, 2, 10, 13, 45, 46, 47, 48, 49, 50, 51)]
        write.table(ACCESSES, file = "ACCESSES.txt", fileEncoding = "UTF-8")
        ACCESSES <-
          read.table(file = "ACCESSES.txt", encoding = "UTF-8")
        names(ACCESSES) <-
          c(
            "Acceso",
            "Tipo",
            "Estado",
            "Fecha Expiracion",
            "MANAGEMENT_ORG_1",
            "MANAGEMENT_ORG_2",
            "MANAGEMENT_ORG_3",
            "MANAGEMENT_ORG_4",
            "MANAGEMENT_ORG_5",
            "MANAGEMENT_ORG_6",
            "Proveedor"
          )
        ACCESSES$`Fecha Expiracion` <-
          as.Date(ACCESSES$`Fecha Expiracion`, origin = "1899-12-30")
        dbWriteTable(
          DB,
          "accesses",
          ACCESSES,
          field.types = list(
            Acceso = "varchar(255)",
            Tipo = "varchar(255)",
            Estado = "varchar(255)",
            `Fecha Expiracion` = "date",
            MANAGEMENT_ORG_1 = "varchar(255)",
            MANAGEMENT_ORG_2 = "varchar(255)",
            MANAGEMENT_ORG_3 = "varchar(255)",
            MANAGEMENT_ORG_4 = "varchar(255)",
            MANAGEMENT_ORG_5 = "varchar(255)",
            MANAGEMENT_ORG_6 = "varchar(255)",
            Proveedor = "varchar(255)"
          ) ,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      else if (client == "hdc") {
        ACCESSES <- ACCESSES[c(1, 2, 10, 13, 43, 44, 45, 46, 47)]
        write.table(ACCESSES, file = "ACCESSES.txt", fileEncoding = "UTF-8")
        ACCESSES <-
          read.table(file = "ACCESSES.txt", encoding = "UTF-8")
        names(ACCESSES) <-
          c(
            "Acceso",
            "Tipo",
            "Estado",
            "Fecha Expiracion",
            "MANAGEMENT_ORG_1",
            "MANAGEMENT_ORG_2",
            "MANAGEMENT_ORG_3",
            "MANAGEMENT_ORG_4",
            "Proveedor"
          )
        ACCESSES$`Fecha Expiracion` <-
          as.Date(ACCESSES$`Fecha Expiracion`, origin = "1899-12-30")
        dbWriteTable(
          DB,
          "accesses",
          ACCESSES,
          field.types = list(
            Acceso = "varchar(255)",
            Tipo = "varchar(255)",
            Estado = "varchar(255)",
            `Fecha Expiracion` = "date",
            MANAGEMENT_ORG_1 = "varchar(255)",
            MANAGEMENT_ORG_2 = "varchar(255)",
            MANAGEMENT_ORG_3 = "varchar(255)",
            MANAGEMENT_ORG_4 = "varchar(255)",
            Proveedor = "varchar(255)"
          ) ,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      else if (client == "Falabella") {
        ACCESSES <- ACCESSES[c(1, 2, 10, 13, 47, 48, 49, 50, 51, 52)]
        write.table(ACCESSES, file = "ACCESSES.txt", fileEncoding = "UTF-8")
        ACCESSES <-
          read.table(file = "ACCESSES.txt", encoding = "UTF-8")
        names(ACCESSES) <-
          c(
            "Acceso",
            "Tipo",
            "Estado",
            "Fecha Expiracion",
            "MANAGEMENT_ORG_1",
            "MANAGEMENT_ORG_2",
            "MANAGEMENT_ORG_3",
            "MANAGEMENT_ORG_4",
            "MANAGEMENT_ORG_5",
            "Proveedor"
          )
        ACCESSES$`Fecha Expiracion` <-
          as.Date(ACCESSES$`Fecha Expiracion`, origin = "1899-12-30")
        dbWriteTable(
          DB,
          "accesses",
          ACCESSES,
          field.types = list(
            Acceso = "varchar(255)",
            Tipo = "varchar(255)",
            Estado = "varchar(255)",
            `Fecha Expiracion` = "date",
            MANAGEMENT_ORG_1 = "varchar(255)",
            MANAGEMENT_ORG_2 = "varchar(255)",
            MANAGEMENT_ORG_3 = "varchar(255)",
            MANAGEMENT_ORG_4 = "varchar(255)",
            MANAGEMENT_ORG_5 = "varchar(255)",
            Proveedor = "varchar(255)"
          ) ,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      else if (client == "pa") {
        ACCESSES <- ACCESSES[c(1, 2, 10, 13, 44, 45, 46, 47, 48, 54)]
        write.table(ACCESSES, file = "ACCESSES.txt", fileEncoding = "UTF-8")
        ACCESSES <-
          read.table(file = "ACCESSES.txt", encoding = "UTF-8")
        names(ACCESSES) <-
          c(
            "Acceso",
            "Tipo",
            "Estado",
            "Fecha Expiracion",
            "MANAGEMENT_ORG_1",
            "MANAGEMENT_ORG_2",
            "MANAGEMENT_ORG_3",
            "MANAGEMENT_ORG_4",
            "Proveedor",
            "Tipo Servicio"
          )
        ACCESSES$`Fecha Expiracion` <-
          as.Date(ACCESSES$`Fecha Expiracion`, origin = "1899-12-30")
        dbWriteTable(
          DB,
          "accesses",
          ACCESSES,
          field.types = list(
            Acceso = "varchar(255)",
            Tipo = "varchar(255)",
            Estado = "varchar(255)",
            `Fecha Expiracion` = "date",
            MANAGEMENT_ORG_1 = "varchar(255)",
            MANAGEMENT_ORG_2 = "varchar(255)",
            MANAGEMENT_ORG_3 = "varchar(255)",
            MANAGEMENT_ORG_4 = "varchar(255)",
            Proveedor = "varchar(255)",
            `Tipo Servicio` = "varchar(255)"
          ) ,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      else if (client == "Copec") {
        ACCESSES <-
          ACCESSES[c(1, 2, 10, 13, 44, 45, 46, 47, 48, 49, 53, 54, 55, 56, 57)]
        write.table(ACCESSES, file = "ACCESSES.txt", fileEncoding = "UTF-8")
        ACCESSES <-
          read.table(file = "ACCESSES.txt", encoding = "UTF-8")
        names(ACCESSES) <-
          c(
            "Acceso",
            "Tipo",
            "Estado",
            "Fecha Expiracion",
            "MANAGEMENT_ORG_1",
            "MANAGEMENT_ORG_2",
            "MANAGEMENT_ORG_3",
            "MANAGEMENT_ORG_4",
            "MANAGEMENT_ORG_5",
            "Proveedor",
            "SITE_ORG_1_1",
            "SITE_ORG_1_2",
            "SITE_ORG_1_3",
            "SITE_ORG_1_4",
            "SITE_ORG_1_5"
          )
        ACCESSES$`Fecha Expiracion` <-
          as.Date(ACCESSES$`Fecha Expiracion`, origin = "1899-12-30")
        dbWriteTable(
          DB,
          "accesses",
          ACCESSES,
          field.types = list(
            Acceso = "varchar(255)",
            Tipo = "varchar(255)",
            Estado = "varchar(255)",
            `Fecha Expiracion` = "date",
            MANAGEMENT_ORG_1 = "varchar(255)",
            MANAGEMENT_ORG_2 = "varchar(255)",
            MANAGEMENT_ORG_3 = "varchar(255)",
            MANAGEMENT_ORG_4 = "varchar(255)",
            MANAGEMENT_ORG_5 = "varchar(255)",
            MANAGEMENT_ORG_6 = "varchar(255)",
            Proveedor = "varchar(255)",
            SITE_ORG_1_1 = "varchar(255)",
            SITE_ORG_1_2 = "varchar(255)",
            SITE_ORG_1_3 = "varchar(255)",
            SITE_ORG_1_4 = "varchar(255)",
            SITE_ORG_1_5 = "varchar(255)"
          ) ,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      else if (client == "AguasAndinas") {
        ACCESSES <-
          subset(
            ACCESSES,
            select = c(
              "ACCESS NUMBER",
              "TYPE",
              "CUSTOM_FIELD:Tipo línea",
              "MANAGEMENT_ORG:1",
              "MANAGEMENT_ORG:2",
              "MANAGEMENT_ORG:3",
              "MANAGEMENT_ORG:4",
              "MANAGEMENT_ORG:5",
              "MANAGEMENT_ORG:6",
              "MANAGEMENT_ORG:7",
              "CARRIER_ORG:1",
              "CARRIER_ORG:2",
              "CARRIER_ORG:3"
            )
          )
        write.table(ACCESSES, file = "ACCESSES.txt", fileEncoding = "UTF-8")
        ACCESSES <-
          read.table(file = "ACCESSES.txt", encoding = "UTF-8")
        names(ACCESSES) <-
          c(
            "Acceso",
            "Tipo",
            "CUSTOM_FIELD_Tipo linea",
            "MANAGEMENT_ORG_1",
            "Empresa",
            "Gerencia Corp.",
            "Gerencia",
            "Subgerencia",
            "Jefatura",
            "CECO",
            "Proveedor Nv1",
            "Proveedor Nv2",
            "Proveedor Nv3"
          )
        dbWriteTable(
          DB,
          "accesses",
          ACCESSES,
          field.types = list(
            Acceso = "varchar(255)",
            Tipo = "varchar(255)",
            `CUSTOM_FIELD_Tipo linea` = "varchar(255)",
            MANAGEMENT_ORG_1 = "varchar(255)",
            Empresa = "varchar(255)",
            `Gerencia Corp.` = "varchar(255)",
            Gerencia = "varchar(255)",
            Subgerencia = "varchar(255)",
            Jefatura = "varchar(255)",
            CECO = "varchar(255)",
            `Proveedor Nv1` = "varchar(255)",
            `Proveedor Nv2` = "varchar(255)",
            `Proveedor Nv3` = "varchar(255)"
          ) ,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      else if (client == "lmovil") {
        #Only select the columns with the following titles
        
        ACCESSES <-
          subset(
            ACCESSES,
            select = c(
              "ACCESS.NUMBER",
              "CARRIER_ORG:1",
              "CARRIER_ORG:2",
              "CARRIER_ORG:3"
            )
          )
        write.table(ACCESSES, file = "ACCESSES.txt", fileEncoding = "UTF-8")
        ACCESSES <-
          read.table(file = "ACCESSES.txt", encoding = "UTF-8")
        names(ACCESSES) <-
          c("Acceso",
            "Proveedor",
            "Proveedor Nivel 2",
            "Proveedor Nivel 3")
        #Create a column with the access number without the country code
        ACCESSES[, 'Acceso fix'] <-
          lapply(ACCESSES['Acceso'], function(x)
            substring(x, 3))
        
        dbWriteTable(
          DB,
          "accesses",
          ACCESSES,
          field.types = list(
            Acceso = "varchar(255)",
            Proveedor = "varchar(255)",
            `Proveedor Nivel 2` = "varchar(255)",
            `Proveedor Nivel 3` = "varchar(255)",
            `Acceso fix` = "varchar(255)"
          ) ,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
        ACCESSES <<- ACCESSES
        
      }
      file.remove("ACCESSES.txt")
      #######################################DEVICES############
      if (client != "lmovil") {
        DEVICES <- read.xlsx(export$datapath,
                             sheet = "DEVICES",
                             startRow = 1)
        file.remove("DEVICES.txt")
        if (client == "Subsole") {
          DEVICES <- DEVICES[c(1, 2, 3, 4, 9)]
          write.table(DEVICES, file = "DEVICES.txt", fileEncoding = "UTF-8")
          DEVICES <-
            read.table(file = "DEVICES.txt", encoding = "UTF-8")
          names(DEVICES) <-
            c("Tipo",
              "Modelo",
              "REFNUM",
              "IMEI",
              "Estado")
        } else if (client == "Walmart") {
          DEVICES <- DEVICES[c(1, 2, 3, 4, 9)]
          write.table(DEVICES, file = "DEVICES.txt", fileEncoding = "UTF-8")
          DEVICES <-
            read.table(file = "DEVICES.txt", encoding = "UTF-8")
          names(DEVICES) <-
            c("Tipo",
              "Modelo",
              "REFNUM",
              "IMEI",
              "Estado")
        } else if (client == "hdc") {
          DEVICES <- DEVICES[c(1, 2, 3, 4, 9, 27)]
          write.table(DEVICES, file = "DEVICES.txt", fileEncoding = "UTF-8")
          DEVICES <-
            read.table(file = "DEVICES.txt", encoding = "UTF-8")
          names(DEVICES) <-
            c("Tipo",
              "Modelo",
              "REFNUM",
              "IMEI",
              "Estado",
              "Ancho de Banda (MB)")
        } else if (client == "Falabella") {
          DEVICES <- DEVICES[c(1, 2, 3, 4, 9, 62, 64)]
          write.table(DEVICES, file = "DEVICES.txt", fileEncoding = "UTF-8")
          DEVICES <-
            read.table(file = "DEVICES.txt", encoding = "UTF-8")
          names(DEVICES) <-
            c("Tipo",
              "Modelo",
              "REFNUM",
              "IMEI",
              "Estado",
              "Propiedad",
              "Categoria")
        } else if (client == "pa") {
          DEVICES <- DEVICES[c(1, 2, 3, 4, 9)]
          write.table(DEVICES, file = "DEVICES.txt", fileEncoding = "UTF-8")
          DEVICES <-
            read.table(file = "DEVICES.txt", encoding = "UTF-8")
          names(DEVICES) <-
            c("Tipo",
              "Modelo",
              "REFNUM",
              "IMEI",
              "Estado")
        } else if (client == "Copec") {
          DEVICES <- DEVICES[c(1, 2, 3, 4, 9, 27)]
          write.table(DEVICES, file = "DEVICES.txt", fileEncoding = "UTF-8")
          DEVICES <-
            read.table(file = "DEVICES.txt", encoding = "UTF-8")
          names(DEVICES) <-
            c("Tipo",
              "Modelo",
              "REFNUM",
              "IMEI",
              "Estado",
              "Ancho de Banda")
        }
        else if (client == "AguasAndinas") {
          DEVICES <- DEVICES[c(1, 2, 3, 4, 9)]
          write.table(DEVICES, file = "DEVICES.txt", fileEncoding = "UTF-8")
          DEVICES <-
            read.table(file = "DEVICES.txt", encoding = "UTF-8")
          names(DEVICES) <-
            c("Tipo",
              "Modelo",
              "REFNUM",
              "IMEI",
              "Estado")
        }
        dbWriteTable(
          DB,
          "devices",
          DEVICES,
          field.types = NULL,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      #######################################ASSOCIATIONS############
      if (client != "lmovil") {
        ASSOCIATIONS <- read.xlsx(export$datapath,
                                  sheet = "ASSOCIATIONS",
                                  startRow = 1)
        ASSOCIATIONS <- ASSOCIATIONS[c(1:4)]
        file.remove("ASSOCIATIONS.txt")
        write.table(ASSOCIATIONS,
                    file = "ASSOCIATIONS.txt",
                    fileEncoding = "UTF-8")
        ASSOCIATIONS <-
          read.table(file = "ASSOCIATIONS.txt", encoding = "UTF-8")
        names(ASSOCIATIONS) <- c("Acceso", "UUI", "IMEI", "REFNUM")
        dbWriteTable(
          DB,
          "associations",
          ASSOCIATIONS,
          field.types = NULL ,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
      #######################################PRODUCT_ASSOCIATIONS############
      if (client != "lmovil") {
        PRODUCT_ASSOCIATIONS <- read.xlsx(export$datapath,
                                          sheet = "PRODUCT ASSOCIATIONS",
                                          startRow = 1)
        PRODUCT_ASSOCIATIONS <- PRODUCT_ASSOCIATIONS[c(1:2)]
        file.remove("PRODUCT_ASSOCIATIONS.txt")
        write.table(PRODUCT_ASSOCIATIONS,
                    file = "PRODUCT_ASSOCIATIONS.txt",
                    fileEncoding = "UTF-8")
        PRODUCT_ASSOCIATIONS <-
          read.table(file = "PRODUCT_ASSOCIATIONS.txt", encoding = "UTF-8")
        names(PRODUCT_ASSOCIATIONS) <- c("Acceso", "Plan")
        dbWriteTable(
          DB,
          "product_associations",
          PRODUCT_ASSOCIATIONS,
          field.types = NULL ,
          row.names = FALSE,
          overwrite = TRUE,
          append = FALSE,
          allow.keywords = FALSE
        )
      }
    }
    #Run the following code if theres a file in the presupuesto file input
    if (!is.null(presupuesto)) {
      file.copy(presupuesto$datapath,
                paste(presupuesto$datapath, ".xlsx", sep = ""))
      
      presupuesto <- read.xlsx(presupuesto$datapath,
                               sheet = "PTO",
                               startRow = 1)
      names(presupuesto) <-
        c(
          "Gestion",
          "Sociedad",
          "CECO",
          "Tipo de Servicio",
          "Cuenta Contable",
          "Item Servicio",
          "Proveedor",
          "Month",
          "Presupuesto Mensual",
          "Presupuesto Anual"
        )
      file.remove("presupuesto.txt")
      write.table(presupuesto, file = "presupuesto.txt", fileEncoding = "UTF-8")
      presupuesto <-
        read.table(file = "presupuesto.txt", encoding = "UTF-8")
      names(presupuesto) <-
        c(
          "Gestion",
          "Sociedad",
          "CECO",
          "Tipo de Servicio",
          "Cuenta Contable",
          "Item Servicio",
          "Proveedor",
          "Month",
          "Presupuesto Mensual",
          "Presupuesto Anual"
        )
      
      dbWriteTable(
        DB,
        "presupuesto",
        presupuesto,
        field.types = NULL ,
        row.names = FALSE,
        overwrite = TRUE,
        append = FALSE,
        allow.keywords = FALSE
      )
      
    }
    #Run the following code if theres a file in the planes file input
    if (!is.null(planes)) {
      file.copy(planes$datapath,
                paste(planes$datapath, ".xlsx", sep = ""))
      
      PLAN <- read.xlsx(planes$datapath,
                        sheet = "Uso por accesoproducto",
                        startRow = 1)
      
      PLAN <-
        subset(
          PLAN,
          select = c(
            "Acceso",
            "Producto",
            "Tipo.de.producto",
            "Proveedor",
            "Centro.de.facturación",
            "Importe.de.las.opciones.descontadas.(CLP)"
          )
        )
      
      file.remove("Planes.txt")
      write.table(PLAN,
                  file = "Planes.txt",
                  fileEncoding = "UTF-8")
      PLAN <- read.table(file = "Planes.txt", encoding = "UTF-8")
      
      names(PLAN) <- c(
        "Acceso",
        "Producto",
        "Tipo de producto",
        "Proveedor",
        "Centro de facturacion",
        "Importe de las opciones descontadas (CLP)"
      )
      
      PLAN[, 'Acceso fix'] <-
        lapply(PLAN['Acceso'], function(x)
          substring(x, 3))
      
      dbWriteTable(
        DB,
        "plan",
        PLAN,
        field.types = list(
          `Acceso` = "varchar(255)",
          `Producto` = "varchar(255)",
          `Tipo de Producto` = "varchar(255)",
          `Proveedor` = "varchar(255)",
          `Centro de Facturacion` = "varchar(255)",
          `Importe de las opciones descontadas (CLP)` = "double(15,2)",
          `Acceso fix` = "varchar(255)"
        ),
        row.names = FALSE,
        overwrite = TRUE,
        append = FALSE,
        allow.keywords = FALSE
      )
      
      PLAN <<- PLAN
      
      file.remove("Planes.txt")
    }
    #Run the following code if theres a file in the tipos file input
    if (!is.null(tipos)) {
      file.copy(tipos$datapath, paste(tipos$datapath, ".xlsx", sep = ""))
      
      TIPO <- read.xlsx(tipos$datapath,
                        sheet = 1,
                        startRow = 1)
      
      TIPO <- TIPO[c(1:4)]
      
      file.remove("TIPO.txt")
      write.table(TIPO,
                  file = "TIPO.txt",
                  fileEncoding = "UTF-8")
      TIPO <- read.table(file = "TIPO.txt", encoding = "UTF-8")
      
      names(TIPO) <- c("Producto",
                       "Descripcion Plan",
                       "Tipo",
                       "Proveedor")
      
      dbWriteTable(
        DB,
        "tipo_plan",
        TIPO,
        field.types = list(
          `Producto` = "varchar(255)",
          `Descripcion Plan` = "varchar(255)",
          `Tipo` = "varchar(255)",
          `Proveedor` = "varchar(255)"
        ),
        row.names = FALSE,
        overwrite = TRUE,
        append = FALSE,
        allow.keywords = FALSE
      )
      
      TIPO <<- TIPO
      
      file.remove("TIPO.txt")
    }
    #Run the following code if theres a file in the nombre and link text input
    if (!is.null(input$link) && !is.null(input$nombre)) {
      #Set variable names
      link <<- input$link
      nombre <<- input$nombre
      
      #Create table logo_cliente if doesnt exist
      dbSendQuery(
        DB,
        "CREATE TABLE IF NOT EXISTS `logo_cliente` (
        `id` int(11) NOT NULL,
        `Nombre Cliente` varchar(255) NOT NULL,
        `Link` text NOT NULL);"
  )
      
      #Delete all data in logo_cliente
      dbSendQuery(DB, "TRUNCATE `logo_cliente`;")
      
      #Insert data in logo_cliente (if exist update)
      dbSendQuery(
        DB,
        paste(
          "INSERT INTO `logo_cliente`(`id`, `Nombre Cliente`, `Link`) VALUES (1,",
          input$nombre,
          ",",
          input$link,
          ")
          on duplicate key update
          `Nombre Cliente` = values(`Nombre Cliente`), `Link` = values(`Link`);",
          sep = '\''
        )
      )
    }
    #Run the following code if theres a file in the cuentas file input
    if (!is.null(cuentas)) {
      #Copy the file uploaded and add the .xlsx file type to the temp file
      file.copy(cuentas$datapath,
                paste(cuentas$datapath, ".xlsx", sep = ""))
      
      CUENTAS <- read.xlsx(cuentas$datapath,
                           sheet = "CUENTAS",
                           startRow = 1)
      
      CUENTAS <- CUENTAS[c(1:4)]
      
      file.remove("CUENTAS.txt")
      write.table(CUENTAS,
                  file = "CUENTAS.txt",
                  fileEncoding = "UTF-8")
      CUENTAS <- read.table(file = "CUENTAS.txt", encoding = "UTF-8")
      
      names(CUENTAS) <- c("Empresa",
                          "RUT",
                          "Cuenta Cliente",
                          "Proveedor")
      
      dbWriteTable(
        DB,
        "cuentas",
        CUENTAS,
        field.types = list(
          `Empresa` = "varchar(255)",
          `RUT` = "varchar(255)",
          `Cuenta Cliente` = "varchar(255)",
          `Proveedor` = "varchar(255)"
        ),
        row.names = FALSE,
        overwrite = TRUE,
        append = FALSE,
        allow.keywords = FALSE
      )
      
      CUENTAS <<- CUENTAS
      
      file.remove("CUENTAS.txt")
      
    }
    #Run the following code if theres a file in the cdr file input
    if (!is.null(input$cdr)) {
      CDRFile <<- NULL
      #Read CDR file with correct fileencoding
      CDRFile <<-
        lapply(input$cdr[['datapath']], function(x)
          read.csv2(x, fileEncoding = "UTF-8-BOM"))
      
      #join all CDR months
      cdr <<- rbindlist(CDRFile)
      
      #Change column names of the CDR
      names(cdr)[names(cdr) == 'Número.de.llamada'] <<-
        'Numero de llamada'
      names(cdr)[names(cdr) == 'Número.llamado'] <<-
        'Numero llamado'
      names(cdr)[names(cdr) == 'Tipo de llamada'] <<-
        'Tipo de llamada'
      names(cdr)[names(cdr) == 'Fecha de llamada'] <<-
        'Fecha de llamada'
      names(cdr)[names(cdr) == 'Geografía'] <<- 'Geografia'
      names(cdr)[names(cdr) == 'País.emisor'] <<- 'Pais emisor'
      names(cdr)[names(cdr) == 'País.destinatario'] <<-
        'Pais destinatario'
      names(cdr)[names(cdr) == 'Duración'] <<- 'Duracion'
      names(cdr)[names(cdr) == 'Volumen'] <<- 'Volumen'
      names(cdr)[names(cdr) == 'Precio'] <<- 'Precio'
      names(cdr)[names(cdr) == 'Organización.Proveedor'] <<-
        'Organizacion Proveedor'
      names(cdr)[names(cdr) == 'Tarificación'] <<- 'Tarificacion'
      names(cdr)[names(cdr) == 'ï..Usuario'] <<- 'Usuario'
      names(cdr)[names(cdr) == 'Tecnología'] <<- 'Tecnologia'
      names(cdr)[names(cdr) == 'Red.recurrente'] <<-
        'Red recurrente'
      names(cdr)[names(cdr) == 'Red.destinada'] <<- 'Red destinada'
      names(cdr)[names(cdr) == 'Organización.de.gestiòn'] <<-
        'Organización de gestion'
      names(cdr)[names(cdr) == 'VPN'] <<- 'VPN'
      names(cdr)[names(cdr) == 'Llamadas.internas'] <<-
        'Llamadas internas'
      names(cdr)[names(cdr) == 'Servicio.llamado'] <<-
        'Servicio llamado'
      
      #delete not used columns
      cdr[, 'Usuario'] <<- NULL
      cdr[, 'Tecnologia'] <<- NULL
      cdr[, 'Red recurrente'] <<- NULL
      cdr[, 'Red destinada'] <<- NULL
      cdr[, 'Organización de gestion'] <<- NULL
      cdr[, 'VPN'] <<- NULL
      cdr[, 'Llamadas internas'] <<- NULL
      cdr[, 'Servicio llamado'] <<- NULL
      
      cdr[, 'Numero de llamada fix'] <<-
        lapply(cdr[, 'Numero de llamada'], function(x)
          substring(x, 3))
      
      file.remove("cdr.txt")
      write.table(cdr, file = "cdr.txt", fileEncoding = "UTF8")
      cdr <<- read.table(file = "cdr.txt", encoding = "UTF8")
      names(cdr) <<-
        c(
          "Numero de llamada",
          "Numero llamado",
          "Tipo de llamada",
          "Fecha de llamada",
          "Geografia",
          "Pais emisor",
          "Pais destinatario",
          "Duracion",
          "Volumen",
          "Precio",
          "Organizacion Proveedor",
          "Tarificacion",
          "Numero de llamada fix"
        )
      
      
      dbWriteTable(
        DB,
        "cdr",
        cdr,
        field.types = list(
          `Numero de llamada` = "char(11)",
          `Numero de llamada fix` = "int(9)",
          `Numero llamado` = "varchar(20)",
          `Tipo de llamada` = "ENUM('Datos','MMS','SMS','Voz','E-mail','Desconocidos') NOT NULL",
          `Fecha de llamada` = "DATE",
          `Geografia` = "ENUM('A internacional','Local','Regional','Nacional desconocido','Roaming entrante','Roaming saliente','Roaming desconocido','Internacional desconocido','Desconocidos') NOT NULL",
          `Pais emisor` = "varchar(40)",
          `Pais destinatario` = "varchar(40)",
          `Duracion` = "SMALLINT(8) UNSIGNED NOT NULL",
          `Volumen` = "MEDIUMINT(10) UNSIGNED NOT NULL",
          `Precio` = "FLOAT(10,2) NOT NULL",
          `Tarificacion` = "ENUM('En el plan','Más alla del plan','Desconocidos','Fuera de plan') NOT NULL",
          `Organizacion Proveedor` = "varchar(255)"
        ),
        row.names = FALSE,
        overwrite = TRUE,
        append = FALSE,
        allow.keywords = FALSE
      )
      
      dbSendQuery(DB, "DROP TABLE IF EXISTS `cdr_join_accesses_all`;")
      dbSendQuery(DB, "ALTER TABLE `accesses` ADD INDEX(`Acceso fix`);")
      dbSendQuery(DB,
                  "ALTER TABLE `cdr` ADD id int NOT NULL AUTO_INCREMENT primary key;")
      dbSendQuery(DB,
                  "ALTER TABLE `cdr` ADD INDEX `Numero llamado` (`Numero llamado`);")
      dbSendQuery(
        DB,
        "CREATE TABLE cdr_join_accesses_all SELECT * FROM `cdr` LEFT JOIN `accesses` ON `cdr`.`Numero llamado`=`accesses`.`Acceso fix`;"
      )
      dbSendQuery(
        DB,
        "ALTER TABLE `cdr_join_accesses_all` CHANGE COLUMN `Acceso` `Acceso llamado` VARCHAR(255);"
      )
      dbSendQuery(
        DB,
        "ALTER TABLE `cdr_join_accesses_all` CHANGE COLUMN `Proveedor` `Proveedor llamado` VARCHAR(255);"
      )
      dbSendQuery(
        DB,
        "ALTER TABLE `cdr_join_accesses_all` CHANGE COLUMN `Proveedor Nivel 2` `Proveedor Nivel 2 llamado` VARCHAR(255);"
      )
      dbSendQuery(
        DB,
        "ALTER TABLE `cdr_join_accesses_all` CHANGE COLUMN `Proveedor Nivel 3` `Proveedor Nivel 3 llamado` VARCHAR(255);"
      )
      dbSendQuery(DB, "DROP TABLE IF EXISTS `cdr`;")
      
      file.remove("cdr.txt")
      
      #CREATE TABLE cdr_accesses
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
      rm(mes1)
      cdr_accesses["Mes"] <- mes
      
      cdr_accesses <-
        subset(cdr_accesses, cdr_accesses["Mes"] != min(cdr_accesses["Mes"]))
      
      dbWriteTable(
        DB,
        "cdr_accesses",
        cdr_accesses,
        field.types = list(
          `Numero de llamada` = "char(11)",
          `Numero de llamada fix` = "int(9)",
          `Numero llamado` = "varchar(20)",
          `Tipo de llamada` = "ENUM('Datos','MMS','SMS','Voz','E-mail','Desconocidos') NOT NULL",
          `Fecha de llamada` = "DATE",
          `Geografia` = "ENUM('A internacional','Local','Regional','Nacional desconocido','Roaming entrante','Roaming saliente','Roaming desconocido','Internacional desconocido','Desconocidos') NOT NULL",
          `Pais emisor` = "varchar(40)",
          `Pais destinatario` = "varchar(40)",
          `Duracion` = "SMALLINT(8) UNSIGNED NOT NULL",
          `Volumen` = "MEDIUMINT(10) UNSIGNED NOT NULL",
          `Precio` = "FLOAT(10,2) NOT NULL",
          `Tarificacion` = "ENUM('En el plan','Más alla del plan','Desconocidos','Fuera de plan') NOT NULL",
          `Organizacion Proveedor` = "varchar(255)",
          `Acceso.x` = "char(11)",
          `Proveedor.x` = "varchar(255)",
          `Proveedor Nivel 2.x` = "varchar(255)",
          `Proveedor Nivel 3.x` = "varchar(255)",
          `Acceso.y` = "char(11)",
          `Proveedor.y` = "varchar(255)",
          `Proveedor Nivel 2.y` = "varchar(255)",
          `Proveedor Nivel 3.y` = "varchar(255)",
          `NET` = "int(2)",
          `Mes` = "int(2)"
        ),
        row.names = FALSE,
        overwrite = TRUE,
        append = FALSE,
        allow.keywords = FALSE
      )
      
      cdr_accesses<<-cdr_accesses
      
    }
    #Run the following code if theres a ticket in the RFP excel checkbox
    if (input$excel == TRUE) {
      #open RFP Workbook
      wb <-
        loadWorkbook("Z:\\AUT Informes\\Licitacion SAAM\\RFP TELEFONIA MOVIL.xlsx")
      
      #Insert Nombre licitacion to Workbook
      licitacion <-
        paste0("SOLUCIÓN DE TELECOMUNICACIONES MÓVILES PARA ",
               input$nombre)
      
      writeData(
        wb,
        sheet = "RFP MOVISTAR",
        licitacion,
        startCol = 2,
        startRow = 6
      )
      writeData(
        wb,
        sheet = "RFP ENTEL",
        licitacion,
        startCol = 2,
        startRow = 6
      )
      writeData(
        wb,
        sheet = "RFP PLANES",
        licitacion,
        startCol = 2,
        startRow = 6
      )
      writeData(
        wb,
        sheet = "RFP CATEGORIAS PLANES",
        licitacion,
        startCol = 2,
        startRow = 6
      )
      writeData(
        wb,
        sheet = "RFP EQUIPOS",
        licitacion,
        startCol = 2,
        startRow = 6
      )
      
      #Insert Nombre cliente to Workbook
      cliente <- input$nombre
      
      writeData(
        wb,
        sheet = "RFP MOVISTAR",
        cliente,
        startCol = 2,
        startRow = 7
      )
      writeData(
        wb,
        sheet = "RFP ENTEL",
        cliente,
        startCol = 2,
        startRow = 7
      )
      writeData(
        wb,
        sheet = "RFP PLANES",
        cliente,
        startCol = 2,
        startRow = 7
      )
      writeData(
        wb,
        sheet = "RFP CATEGORIAS PLANES",
        cliente,
        startCol = 2,
        startRow = 7
      )
      writeData(
        wb,
        sheet = "RFP EQUIPOS",
        cliente,
        startCol = 2,
        startRow = 7
      )
      
      #Insert Date to Workbook
      fecha <- input$fecha
      
      writeData(
        wb,
        sheet = "RFP MOVISTAR",
        fecha,
        startCol = 2,
        startRow = 9
      )
      writeData(
        wb,
        sheet = "RFP ENTEL",
        fecha,
        startCol = 2,
        startRow = 9
      )
      writeData(
        wb,
        sheet = "RFP PLANES",
        fecha,
        startCol = 2,
        startRow = 9
      )
      writeData(
        wb,
        sheet = "RFP CATEGORIAS PLANES",
        fecha,
        startCol = 2,
        startRow = 9
      )
      writeData(
        wb,
        sheet = "RFP EQUIPOS",
        fecha,
        startCol = 2,
        startRow = 9
      )
      
      #Insert client image to Workbook
      link <- input$link
      z <- tempfile()
      download.file(link, z, mode = "wb")
      
      insertImage(
        wb,
        sheet = "RFP MOVISTAR",
        z,
        startCol = 3,
        startRow = 1
      )
      insertImage(
        wb,
        sheet = "RFP ENTEL",
        z,
        startCol = 3,
        startRow = 1
      )
      insertImage(
        wb,
        sheet = "RFP PLANES",
        z,
        startCol = 3,
        startRow = 1
      )
      insertImage(
        wb,
        sheet = "RFP CATEGORIAS PLANES",
        z,
        startCol = 3,
        startRow = 1
      )
      insertImage(
        wb,
        sheet = "RFP EQUIPOS",
        z,
        startCol = 3,
        startRow = 1,
      )
      
      #Functions in other files
      #source("pj.r", local = TRUE)
      source("pb.r", local = TRUE)
      #source("jp.r", local = TRUE)
      
      
      # Consumo total Voz MOVISTAR
       writeData(wb, sheet = "RFP MOVISTAR", MovTotMin, startCol = 4, startRow = 14)
      #
      # Consumo voz entre usuarios MOVISTAR
       writeData(wb, sheet = "RFP MOVISTAR", MovVozOnNet, startCol = 4, startRow = 16)
      #
      # Consumo voz a todo destino MOVISTAR
       writeData(wb, sheet = "RFP MOVISTAR", MovATodDes, startCol = 4, startRow = 18)
      #
      # BAM o Servicios de Telemetria MOVISTAR
      # writeData(wb, sheet = "RFP MOVISTAR", X, startCol = 4, startRow = 26)
      #
      # Mensajeria SMS MOVISTAR
       writeData(wb, sheet = "RFP MOVISTAR", MovSms, startCol = 4, startRow = 28)
      #
      # Mensajeria MMS MOVISTAR
       writeData(wb, sheet = "RFP MOVISTAR", MovMms, startCol = 4, startRow = 30)
      #
      # Usuarios Roaming On Demand MOVISTAR
      # writeData(wb, sheet = "RFP MOVISTAR", X, startCol = 4, startRow = 32)
      #
      # Roaming Voz MOVISTAR
       writeData(wb, sheet = "RFP MOVISTAR", MovRoaVoz, startCol = 4, startRow = 34)
      #
      # Roaming Datos MOVISTAR
       writeData(wb, sheet = "RFP MOVISTAR", MovRoaDat, startCol = 4, startRow = 36)
      #
      # Roaming Mensajes MOVISTAR
       writeData(wb, sheet = "RFP MOVISTAR", MovRoaSms, startCol = 4, startRow = 38)
      #
      # $/Minuto Actual MOVISTAR
       writeData(wb, sheet = "RFP MOVISTAR", MovMinAct, startCol = 8, startRow = 15)
      #
      # $/Mb Actual MOVISTAR
      # writeData(wb, sheet = "RFP MOVISTAR", X, startCol = 8, startRow = 20)
      
      
      # Consumo total Voz ENTEL
       writeData(wb, sheet = "RFP ENTEL", EntTotMin, startCol = 4, startRow = 14)
      #
      # Consumo voz entre usuarios ENTEL
      writeData(wb, sheet = "RFP ENTEL", EntVozOnNet, startCol = 4, startRow = 16)
      #
      # Consumo voz a todo destino ENTEL
      writeData(wb, sheet = "RFP ENTEL", EntATodDes, startCol = 4, startRow = 18)
      #
      # BAM o Servicios de Telemetria ENTEL
      # writeData(wb, sheet = "RFP ENTEL", X, startCol = 4, startRow = 26)
      #
      # Mensajeria SMS ENTEL
       writeData(wb, sheet = "RFP ENTEL", EntSms, startCol = 4, startRow = 28)
      #
      # Mensajeria MMS ENTEL
       writeData(wb, sheet = "RFP ENTEL", EntMms, startCol = 4, startRow = 30)
      #
      # Usuarios Roaming On Demand ENTEL
      # writeData(wb, sheet = "RFP ENTEL", X, startCol = 4, startRow = 32)
      #
      # Roaming Voz ENTEL
       writeData(wb, sheet = "RFP ENTEL", EntRoaVoz, startCol = 4, startRow = 34)
      #
      # Roaming Datos ENTEL
       writeData(wb, sheet = "RFP ENTEL", EntRoaDat, startCol = 4, startRow = 36)
      #
      # Roaming Mensajes ENTEL
       writeData(wb, sheet = "RFP ENTEL", EntRoaSms, startCol = 4, startRow = 38)
      #
      # Internacional Voz ENTEL
       writeData(wb, sheet = "RFP ENTEL", EntIntVoz, startCol = 4, startRow = 40)
      #
      # $/Minuto promedio ENTEL
       writeData(wb, sheet = "RFP ENTEL", EntMinAct, startCol = 8, startRow = 16)
      #
      # $/Mb Actual ENTEL
       writeData(wb, sheet = "RFP ENTEL", EntMbAct, startCol = 8, startRow = 21)
      
      #Save Workbook
      saveWorkbook(
        wb,
        paste0(
          "Z:\\AUT Informes\\Licitacion SAAM\\RFP TELEFONIA MOVIL ",
          cliente,
          ".xlsx"
        ),
        overwrite = T
      )
    }
    
    #Kill open connections
    killDbConnections()
    
    #Update the excecute button to "finished" status
    updateButton(session,
                 "execute",
                 style = "success",
                 icon = icon("check"))
  })
  })