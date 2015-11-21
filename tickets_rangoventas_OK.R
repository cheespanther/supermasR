promedioanual <- function (year, tienda,meses) {

  setwd(paste("D:/KEEP_ISKAR/R/datos/TICKETS/TICKETS",year,"/datos/",tienda, sep=""))
  
    ## Archivo tiene:  "date_order" "session_id..id" "user_id.display_name" "amount_total" "shop_id.name"  
  
    files <- list.files (pattern = ".csv")
    message("Archivos Encontrados:")
    print (files)
    data <- lapply(paste(meses, ".csv",sep=""), read.csv, header=TRUE)

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
    
    setwd(paste("D:/KEEP_ISKAR/R/datos/TICKETS/TICKETS",year,"/reportes", sep=""))
    
    write.csv(tabla, file = paste("promedio_anual",year,".csv",sep=""))

    resumen <- summary (soloventas)
    message ("Resumen:")
    print(resumen)
    promedio <- mean (soloventas)
    message ("Promedio Global:")
    print (promedio)
    message ("Frecuencia por Clase:")
    return(tablabonita)
    
}
