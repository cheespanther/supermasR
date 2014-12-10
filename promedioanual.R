promedioanual <- function (year, tienda) {

  setwd("C:/R/TICKETS2014")

         if (year == "2014" ){
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

    files <- list.files (pattern = ".csv")
    message("Archivos Encontrados:")
    print (files)
    data <- lapply(files, read.csv, header = TRUE)
    tickets <- do.call(rbind, data)
    datatickets <- c(tickets$amount_total, na.rm= TRUE)
    numerodatos <- lapply (data, nrow)
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
    Frecuencia <- c(cuentas1, cuentas2, cuentas3, cuentas4, cuentas5, cuentas6)
    Clases <- c("Menor a $20.00", "$21.00 a $50.00", "$51.00 a $100.00",
                  "$101.00 a $200.00", "$201.00 a $400.00", "Mayor a $400.00")
    tabla <- data.frame(Clases, Frecuencia)
    tablabonita <- format (tabla, width = 20, justify = "right")
    write.csv(tabla, file = "C:/Users/Iskar/Desktop/promedioanual.csv")
    write.csv(tickets, file = "C:/Users/Iskar/Desktop/total.csv")
    ##write.csv(data, file = "C:/Users/Iskar/Desktop/datos.csv")
    ## hist(Frecuencia)
    resumen <- summary (soloventas)
    message ("Resumen:")
    print(resumen)
    promedio <- mean (soloventas)
    message ("Promedio Global:")
    print (promedio)
    message ("Frecuencia por Clase:")
    return(tablabonita)
    
    
}
