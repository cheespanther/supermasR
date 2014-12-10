ticketsanalisis <- function (year, month, tienda) {

library("lubridate")

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
    numerodatos <- lapply (data, nrow)
    soloventas <- subset (tickets, tickets$amount_total > 0)
    
    fechas <- soloventas[,1]
    fechas <- sapply (fechas, as.character)
    fechas <- strsplit(fechas, " ")
    fechas <- do.call(rbind,(lapply(fechas, rbind)))
    fechas <- as.data.frame(fechas)
    colnames(fechas) <- c("fecha", "hora")
    dia <- wday(dmy(fechas[,1]))
    nuevo <- cbind(fechas,dia, soloventas[,-1])


    c1 <- subset (nuevo, nuevo["dia"] == 1)
    c2 <- subset (nuevo, nuevo["dia"] == 2)
    c3 <- subset (nuevo, nuevo["dia"] == 3)
    c4 <- subset (nuevo, nuevo["dia"] == 4)
    c5 <- subset (nuevo, nuevo["dia"] == 5)
    c6 <- subset (nuevo, nuevo["dia"] == 6)
    c7 <- subset (nuevo, nuevo["dia"] == 7)

    cuentas1 <- nrow(c1)
    cuentas2 <- nrow(c2)
    cuentas3 <- nrow(c3)
    cuentas4 <- nrow(c4)
    cuentas5 <- nrow(c5)
    cuentas6 <- nrow(c6)
    cuentas7 <- nrow(c7)

    p1 <- mean(as.matrix(c1["amount_total"]))
    p2 <- mean(as.matrix(c2["amount_total"]))
    p3 <- mean(as.matrix(c3["amount_total"]))
    p4 <- mean(as.matrix(c4["amount_total"]))
    p5 <- mean(as.matrix(c5["amount_total"]))
    p6 <- mean(as.matrix(c6["amount_total"]))
    p7 <- mean(as.matrix(c7["amount_total"]))


    numerodetickets <- c(cuentas1, cuentas2, cuentas3, cuentas4, cuentas5, cuentas6, cuentas7)
    dias <- c("Domingo", "Lunes", "Martes","Miercoles", "Jueves", "Viernes", "Sabado")
    promedio <- c(p1, p2, p3, p4, p5, p6, p7)
    estimadoventas <- numerodetickets * promedio

    setwd("C:/R/TICKETS2014/2014hora")

    tabla <- rbind(dias, numerodetickets, promedio, estimadoventas)
    tablabonita <- format (tabla, width = 20, justify = "right")

  if(tienda == "SM1"){
    write.csv(tabla, file = "C:/Users/Iskar/Desktop/ventasdiariassm1.csv")
    write.csv(nuevo, file = "C:/Users/Iskar/Desktop/SM1pordia.csv")
    hist(numerodetickets)
  }
  if(tienda == "SM2"){
    write.csv(tabla, file = "C:/Users/Iskar/Desktop/ventasdiariassm2.csv")
    write.csv(nuevo, file = "C:/Users/Iskar/Desktop/SM2pordia.csv")
    hist(numerodetickets)
  }
  if(tienda == "SM3"){
    write.csv(tabla, file = "C:/Users/Iskar/Desktop/ventasdiariassm3.csv")
    write.csv(nuevo, file = "C:/Users/Iskar/Desktop/SM3pordia.csv")
    hist(numerodetickets)
  }

   resumen <- summary(nuevo)
    message ("Resumen:")
    print(resumen)
    return(tablabonita)
}
