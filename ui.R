##################SETUP##############################
#Load Libraries
#comentario vacío
library(shiny) #Dashboard
library(shinythemes) #Load Themes for shiny
library(shinyBS) #Load Javascript plugin for shiny
#############Start the UI file from Shiny############
shinyUI(fluidPage(
  #Name the title from title panel
  titlePanel("Carga de Datos a BD"),
  #Create the following tipe of layout of Shiny
  sidebarLayout(
    sidebarPanel(
      ##################SideBar Panel##################
      #Create a Selection for the type of data to upload
      selectInput(
        "Client",
        label = h4("Tipo de Informe"),
        choices = c(
          "Walmart",
          "Copec",
          "Hogar De Cristo",
          "Subsole",
          "Parque Arauco",
          "Falabella",
          "Aguas Andinas",
          "Licitacion Movil (Entel y Movistar)",
          "Informe Gestion Movil",
          "Anomalias de Gestion Movil"
        ),
        selected = ""
      ),
      #Create conditional select input with text input
      selectInput(
        "nombre",
        label = h4("Nombre de la Empresa"),
        choices = c(
          "Aguas Andinas",
          "Carabineros de Chile",
          "Claro",
          "Copec",
          "Enap",
          "Hogar de Cristo",
          "Nueva Pudahuel",
          "Parque Arauco",
          "Rhona",
          "SAAM",
          "Subsole",
          "Walmart"
        ),
        selected = ""
        ),
      #Create file upload accepting multiple csv files
      fileInput(
        'usos',
        'Elegir los Archivos CSV que contenga los Usos del cliente en UF (en CLP)',
        multiple = T,
        accept = c('text/csv',
                   'text/comma-separated-values,text/plain',
                   '.csv')
      ),
      #Create conditional Panel accepting multiple csv files
      conditionalPanel(
        condition = "input.Client == 'Parque Arauco'",
        fileInput(
          'usosPEN',
          'Elegir los Archivos CSV que contenga los Usos del cliente en PEN',
          multiple = T,
          accept = c('text/csv',
                     'text/comma-separated-values,text/plain',
                     '.csv')
        )
      ),
      #Create conditional Panel accepting multiple csv files
      conditionalPanel(
        condition = "input.Client == 'Parque Arauco'",
        fileInput(
          'usosCOP',
          'Elegir los Archivos CSV que contenga los Usos del cliente en COP',
          multiple = T,
          accept = c('text/csv',
                     'text/comma-separated-values,text/plain',
                     '.csv')
        )
      ),
      #Create conditional Panel accepting one xlsx file
      fileInput(
        'Export',
        'Elegir el Archivo xlsx que contenga el Export de iTem',
        accept = c(
          'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
        )
      ),
      #Create conditional Panel accepting multiple csv files
      conditionalPanel(
        condition = "input.Client == 'Parque Arauco'",
        fileInput(
          'factura',
          'Elegir el Archivo CSV que contenga las facturas del cliente',
          accept = c('text/csv',
                     'text/comma-separated-values,text/plain',
                     '.csv')
        )
      ),
      #Create conditional Panel accepting one xlsx file
      conditionalPanel(
        condition = "input.Client == 'Parque Arauco'",
        fileInput(
          'presupuesto',
          'Elegir el Archivo xlsx que contiene el presupuesto del cliente',
          accept = c(
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
          )
        )
      ),
      #Create conditional Panel accepting multiple csv files
      conditionalPanel(
        condition = "input.Client == 'Licitacion Movil (Entel y Movistar)'||input.Client =='Anomalias de Gestion Movil'",
        fileInput(
          'cdr',
          'Elegir el Archivo CSV que contenga el CDR del cliente',
          multiple = T,
          accept = c('text/csv',
                     'text/comma-separated-values,text/plain',
                     '.csv')
        )
      ),
      #Create conditional Panel accepting one xlsx file
      conditionalPanel(
        condition = "input.Client == 'Licitacion Movil (Entel y Movistar)' ||input.Client =='Informe Gestion Movil'||input.Client =='Anomalias de Gestion Movil'",
        fileInput(
          'planes',
          'Elegir el Archivo xlsx que contiene el informe de los planes de los proveedores O los servicios facturados para Anomalias',
          accept = c(
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
          )
        )
      ),
      #Create conditional Panel accepting one xlsx file
      conditionalPanel(
        condition = "input.Client == 'Licitacion Movil (Entel y Movistar)'",
        fileInput(
          'tipos',
          'Elegir el Archivo xlsx que contiene tipo planes de los proveedores',
          accept = c(
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
          )
        )
      ),
      #Create conditional Panel accepting one xlsx file
      conditionalPanel(
        condition = "input.Client == 'Licitacion Movil (Entel y Movistar)'",
        fileInput(
          'cuentas',
          'Elegir el Archivo xlsx que contiene RUT de la empresa a licitar y sus cuentas clientes',
          accept = c(
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
          )
        )
      ),
      #Create conditional Panel accepting one xlsx file
      conditionalPanel(
        condition = "input.Client =='Anomalias de Gestion Movil'",
        fileInput(
          'contrato',
          'Elegir el Archivo xlsx que contiene el informe del Contrato del cliente con su(s) provedor(es)',
          multiple = T,
          accept = c(
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
          )
        )
      ),
      #Create conditional panel with a checkbox
      conditionalPanel(
        condition = "input.Client == 'Licitacion Movil (Entel y Movistar)'",
        checkboxInput('excel', '¿Crear RFP en Excel?', value = FALSE)
      ),
      #Create conditional panel with a dateinput
      conditionalPanel(
        condition = "input.excel == true",
        dateInput(
          'fecha',
          'Fecha del RFP',
          format = "dd-mm-yyyy",
          weekstart = 1,
          language = "es"
        )
      ),
      tags$hr(),
      #Create action button
      bsButton("execute", " Importar!")
    ),
    #Call HTML file with use documentation
    mainPanel(##################Main Panel##################
              htmlTemplate("www/help.html"))
  )
))