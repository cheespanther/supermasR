promediomensual <- function (mes,tienda) {

  setwd("C:/R/TICKETS2014")

     if (tienda == "SM1" ){
        setwd("C:/R/TICKETS2014/SM12014")
     } else if (tienda == "SM2"){
        setwd("C:/R/TICKETS2014/SM22014")
     } else if (tienda == "SM3"){
        setwd("C:/R/TICKETS2014/SM32014")
    }
    

    dataselect <- lapply(paste(mes, ".csv", sep="" ), read.csv, header = TRUE)
    tickets <- data.frame(dataselect, header = TRUE)
    datatickets <- c(tickets$amount_total, na.rm= TRUE)
    numerodatos <- lapply (dataselect, nrow)
    soloventas <- subset (datatickets, datatickets > 0)
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
    
    resumen <- summary (soloventas)
    promedio <- mean (soloventas)
    tablaresumen <- data.frame(numerodatos, promedio)
    write.csv(tablaresumen, file = "C:/Users/Iskar/Desktop/resumen.csv")
    
    Frecuencia <- c(cuentas1, cuentas2, cuentas3, cuentas4, cuentas5, cuentas6)
    Clases <- c("Menor a $20.00", "$21.00 a $50.00", "$51.00 a $100.00",
                  "$101.00 a $200.00", "$201.00 a $400.00", "Mayor a $400.00")
    tabla <- data.frame(Clases, Frecuencia)
    tablabonita <- format (tabla, width = 20, justify = "right")
    write.csv(tabla, file = "C:/Users/Iskar/Desktop/promediomensual.csv")
    write.csv(tickets, file = "C:/Users/Iskar/Desktop/ticketsmensual.csv")
    ## hist(Frecuencia)


    message ("Promedio Global y Tickets:")
    print (tablaresumen)
    message ("Resumen:")
    print(resumen)
    message ("Frecuencia por Clase:")
    return(tablabonita)
    
}
