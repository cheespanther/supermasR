trilateral_admin <- function(trimestre, summarize = FALSE)
    
{
    
    library(plyr)
    
    setwd("D:/KEEP_ISKAR/R/datos/desarrollotrilateral/datos")  
    
    trilateral_datos2 <- as.data.frame(lapply(paste("TrilateralAdmin", trimestre, ".csv",sep=""), read.csv, header=TRUE))
    
    trilateral_datos_columnas2 <- as.data.frame(t(trilateral_datos2))
    
    number_col <- ncol(trilateral_datos_columnas2)
    names_col <- c(trilateral_datos_columnas2[1,])
    nombres <- t(trilateral_datos_columnas2[5,])
    
    trilateral_caracter2 <- trilateral_datos_columnas2[6:18,]
    colnames(trilateral_caracter2) <- nombres
    
    trilateral_trabajo2 <- trilateral_datos_columnas2[19:21,]
    colnames(trilateral_trabajo2) <- nombres
    
    trilateral_relaciones2 <- trilateral_datos_columnas2[22:27,]
    colnames(trilateral_relaciones2) <- nombres
    
    caracter_admin <- lapply(trilateral_caracter2, as.matrix, header = TRUE)
    caracter_admin_numeric <- lapply(caracter_admin, as.numeric, header = TRUE)
    mean_caracter_admin <- colMeans(t(do.call(rbind, caracter_admin_numeric)))
    
    trabajo_admin <- lapply(trilateral_trabajo2, as.matrix, header = TRUE)
    trabajo_admin_numeric <- lapply(trabajo_admin, as.numeric, header = TRUE)
    mean_trabajo_admin <- colMeans(t(do.call(rbind, trabajo_admin_numeric)))
    
    relaciones_admin <- lapply(trilateral_relaciones2, as.matrix, header = TRUE)
    relaciones_admin_numeric <- lapply(relaciones_admin, as.numeric, header = TRUE)
    mean_relaciones_admin <- colMeans(t(do.call(rbind, relaciones_admin_numeric)))
    
    resultados_tabla <- cbind(nombres, mean_caracter_admin, mean_trabajo_admin, mean_relaciones_admin)
    nombre_columnas <- c("Trabajador", "Caracter", "Trabajo", "Relaciones")
    colnames(resultados_tabla) <- as.factor(nombre_columnas)
    as.factor(resultados_tabla["Trabajador"])
    
    votacion <- trilateral_datos2[,28]
    votacion_cuentas <- summary(votacion)
    
    setwd("D:/KEEP_ISKAR/R/datos/desarrollotrilateral/reportes")
    
    write.csv(trilateral_caracter2, file = paste("caracter_admin" ,trimestre,".csv", sep= ""))
    write.csv(trilateral_trabajo2, file = paste("trabajo_admin" ,trimestre,".csv"))
    write.csv(trilateral_relaciones2, file = paste("relaciones_admin" ,trimestre, ".csv", sep= ""))
    write.csv(resultados_tabla, file = paste("trilateral_admin", trimestre,".csv", sep= ""))
    
    
    ##usersplit_trilateral_admin <<- split(resultados_tabla, resultados_tabla$Trabajador)
    
    x <<- read.csv(paste("trilateral_admin", trimestre, ".csv", sep=""))
    
    trilateral_admin_usuario <<- ddply(x, "Trabajador", summarise, 
                                       Caracter = 2*mean(as.numeric(Caracter)), 
                                       Trabajo = 2*mean(as.numeric(Trabajo)),
                                       Relaciones = 2*mean(as.numeric(Relaciones)))
    
    write.csv(votacion_cuentas, file = paste("trilateral_votacion_admin" ,trimestre,".csv", sep= ""))
    write.csv(trilateral_admin_usuario, file = paste("trilateral_admin_usuario" ,trimestre,".csv",sep=""))
    
    trilateral_admin_usuario
    
    setwd("D:/KEEP_ISKAR/R/datos/desarrollotrilateral/datos")  
    
    
}
