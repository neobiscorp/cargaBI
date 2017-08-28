library(openxlsx)

x <- sum(cdr$Duracion)/60

wb <- loadWorkbook("Z:\\AUT Informes\\Licitacion SAAM\\RFP TELEFONIA MOVIL.xlsx")
writeData(wb, sheet = "RFP MOVISTAR", x, startCol = 4, startRow = 14)
saveWorkbook(wb,"Z:\\AUT Informes\\Licitacion SAAM\\RFP TELEFONIA MOVIL.xlsx",overwrite = T)
