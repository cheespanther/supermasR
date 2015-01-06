mensual <- function( año, mes, tienda, summarize = FALSE) 
{
library("lubridate")

  setwd("C:/R/TICKETS2014")

         if (año == "2014" ){
        setwd("C:/R/TICKETS2014")
     } else if (year == "2015"){
        setwd("C:/R/TICKETS2015")
     } 

     if (tienda == "SM1" ){
        setwd("C:/R/TICKETS2014/SM12014")
     } else if (tienda == "SM2"){
        setwd("C:/R/TICKETS2014/SM22014")
     } else if (tienda == "SM3"){
        setwd("C:/R/TICKETS2014/SM32014")
    }

  dataselect <- lapply(paste(mes, ".csv",sep=""), read.csv, header=TRUE)
  combined <- do.call(rbind, dataselect)
  datatickets <- c(combined$amount_total, na.rm= TRUE)
  numerodatos <- lapply (combined, nrow)
  soloventas <- subset (datatickets, datatickets > 0)
  ventastotales <- sum (soloventas)
  combinedmean <- mean(soloventas)

    c1 <- as.matrix(subset (soloventas, soloventas <= 20))
    c2 <- as.matrix(subset (soloventas, soloventas > 20 & soloventas <= 50))
    c3 <- as.matrix(subset (soloventas, soloventas > 50 & soloventas <= 100))
    c4 <- as.matrix(subset (soloventas, soloventas > 100 & soloventas <= 200))
    c5 <- as.matrix(subset (soloventas, soloventas > 200 & soloventas <= 400))
    c6 <- as.matrix(subset (soloventas, soloventas > 400))
    
    cuentas1 <- nrow(c1)
    cuentas2 <- nrow(c2)
    cuentas3 <- nrow(c3)
    cuentas4 <- nrow(c4)
    cuentas5 <- nrow(c5)
    cuentas6 <- nrow(c6)
    
    Frecuencia <- c(cuentas1, cuentas2, cuentas3, cuentas4, cuentas5, cuentas6)
    Clases <- c("Menor a $20.00", "$21.00 a $50.00", "$51.00 a $100.00",
                  "$101.00 a $200.00", "$201.00 a $400.00", "Mayor a $400.00")
                  
    tabla <- data.frame(Clases, Frecuencia)
    tablabonita <- format (tabla, width = 20, justify = "right")

  ##tablaresumen <- c(numerodatos, combinedmean, ventastotales)

  write.csv(combined, file = "C:/Users/Iskar/Desktop/ventastotales.csv")
  write.csv(tablabonita, file = "C:/Users/Iskar/Desktop/ventasporrango.csv")
  write.csv(datatickets, file = "C:/Users/Iskar/Desktop/datatickets.csv")
  ##write.csv(tablaresumen, file = "C:/Users/Iskar/Desktop/resumen.csv")

return (combinedmean)

}
