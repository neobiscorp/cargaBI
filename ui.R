#Load Libraries
library(shiny) #Dashboard
library(RMySQL) #Secundary MySQL connection 
library(DBI) #Primary MySQL connection
library(shinythemes) #Load Themes for shiny
library(shinyBS) #Load Javascript plugin for shiny

#Start the UI file from Shiny
shinyUI(fluidPage(
  #Name the title from title panel
  titlePanel("Carga de Datos a BD"),
  #Create the following tipe of layout of Shiny
  sidebarLayout(
    #Put the following at the sideBar Panel
    sidebarPanel(
      #Create a Selection for the type of data to upload
      selectInput(
      "Client",
      label = h4(
        "Cliente"),
        choices = c("Walmart","Copec","Hogar De Cristo","Subsole","Parque Arauco","Falabella","Aguas Andinas","Licitacion Movil (Entel y Movistar)"),
      selected = ""
    ),
    conditionalPanel(
      condition = "input.Client == 'Licitacion Movil (Entel y Movistar)'",
      textInput('nombre',
                'Nombre de la Empresa a Licitar')
    ),
    conditionalPanel(
      condition = "input.Client == 'Licitacion Movil (Entel y Movistar)'",
      textInput('link',
                'Link de Imagen de la Empresa a Licitar')
    ),
      fileInput('usos', 'Elegir los Archivos CSV que contenga los Usos del cliente en UF (Licitacion en CLP)', multiple = T,
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
    conditionalPanel(
      condition = "input.Client == 'Parque Arauco'",
      fileInput('usosPEN', 'Elegir los Archivos CSV que contenga los Usos del cliente en PEN', multiple = T,
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))),
    conditionalPanel(
      condition = "input.Client == 'Parque Arauco'",
      fileInput('usosCOP', 'Elegir los Archivos CSV que contenga los Usos del cliente en COP', multiple = T,
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))),
      fileInput('Export', 'Elegir el Archivo xlsx que contenga el Export de iTem',
                accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')),
    conditionalPanel(
      condition = "input.Client == 'Parque Arauco'",
      fileInput('factura', 'Elegir el Archivo CSV que contenga las facturas del cliente',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv'))),
    conditionalPanel(
      condition = "input.Client == 'Parque Arauco'",
      fileInput('presupuesto', 'Elegir el Archivo xlsx que contiene el presupuesto del cliente',
                accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'))),
    conditionalPanel(
      condition = "input.Client == 'Licitacion Movil (Entel y Movistar)'",
      fileInput('cdr', 'Elegir el Archivo CSV que contenga el CDR del cliente',multiple = T,
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))),
    conditionalPanel(
      condition = "input.Client == 'Licitacion Movil (Entel y Movistar)'",
      fileInput('planes', 'Elegir el Archivo xlsx que contiene el informe de los planes de los proveedores',
                accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'))),
    conditionalPanel(
      condition = "input.Client == 'Licitacion Movil (Entel y Movistar)'",
      fileInput('tipos', 'Elegir el Archivo xlsx que contiene tipo planes de los proveedores',
                accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'))),
    conditionalPanel(
      condition = "input.Client == 'Licitacion Movil (Entel y Movistar)'",
      checkboxInput('excel', '¿Crear RFP en Excel?', value = FALSE)),
    conditionalPanel(
      condition = "input.excel == true",
      dateInput('fecha', 'Fecha del RFP', format = "dd-mm-yyyy", weekstart = 1, language = "es")),
      tags$hr(),
    bsButton("execute", " Importar!")
    ),
    mainPanel(
      h5("Campos Uso: #Acceso
 #
        #Período de
         #Proveedor
         #Total
         #Plano tarifario
         #Uso
         #Servicios
         #Descuentos
         #Voz
         #Voz nacional
         #Voz inter.
         #Datos
         #Datos nac.
         #Datos inter.
         #SMS/MMS
         #N.° Copias
         #N.° Copias B/N
         #N.° Copias Color"),
      br(),
      h5("Campos Facturas: #Número de factura
#Proveedor
         #Mes de facturación
         #Total sin impuestos"),
      br()
    )
  )
))