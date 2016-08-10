shinyServer(function(input, output) {
    
    library(xlsx)
    meldown<-read.csv('http://www.pronosticos.gob.mx/Documentos/Historicos/Melate.csv')
    file.copy(from="MelateCalc-Rv1.xlsx",to="MelateCalc-R.xlsx")
    linea<-meldown[1,]
    linea[,1:11]<-c(0,0,0,0,0,0,0,0,0,NA,NA)
    injerto<-rbind(linea,meldown[1:350,])
    wb = loadWorkbook("MelateCalc-R.xlsx")
    sheets = getSheets(loadWorkbook("MelateCalc-R.xlsx"))
    removeSheet(wb, sheetName = names(sheets[6]))
    saveWorkbook(wb, "MelateCalc-R.xlsx")
    write.xlsx(injerto,file="MelateCalc-R.xlsx",sheetName = "CalcMelate", append = TRUE, row.names = FALSE, col.names = TRUE)
    
    
    output$downloadData <- downloadHandler(
    filename = function() { 'MelateCalc.xlsx'},
    content = function(file) {
    file.copy("MelateCalc-R.xlsx", file)
    }
  )
})