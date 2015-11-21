calidad_junior <- function(tienda, trimestre, summarize = FALSE)
    
{
    
    setwd("D:/KEEP_ISKAR/R/datos/calidadjr/datos")
    
    ## Read and Transpose Survey Data
    library(plyr)
    
    survey_junior_datos <- as.data.frame(lapply(paste("CalidadJunior", tienda, trimestre, ".csv",sep=""), read.csv, header=TRUE))
    
    survey_junior_columnas <- as.data.frame(t(survey_junior_datos))
    
    number_col_junior <- ncol(survey_junior_columnas)
    names_col_junior <- c(survey_junior_columnas[5,])
    nombres_junior <- c(t(survey_junior_columnas[5,]))
    
    survey_junior_lider <- survey_junior_columnas[6:10,]
    colnames(survey_junior_lider) <- nombres_junior
    
    survey_junior_tareas <- survey_junior_columnas[11:13,]
    colnames(survey_junior_tareas) <- nombres_junior
    
    survey_junior_trabajo <- survey_junior_columnas[14:15,]
    colnames(survey_junior_trabajo) <- nombres_junior
    
    lider_junior <- lapply(survey_junior_lider, as.matrix, header = TRUE)
    lider_junior_numeric <- lapply(lider_junior, as.numeric, header = TRUE)
    mean_lider_junior <- colMeans(t(do.call(rbind, lider_junior_numeric)))
    
    tareas_junior <- lapply(survey_junior_tareas, as.matrix, header = TRUE)
    tareas_junior_numeric <- lapply(tareas_junior, as.numeric, header = TRUE)
    mean_tareas_junior <- colMeans(t(do.call(rbind, tareas_junior_numeric)))
    
    trabajo_junior <- lapply(survey_junior_trabajo, as.matrix, header = TRUE)
    trabajo_junior_numeric <- lapply(trabajo_junior, as.numeric, header = TRUE)
    mean_trabajo_junior <- colMeans(t(do.call(rbind, trabajo_junior_numeric)))
    
    resultados_junior_tabla <<- cbind(nombres_junior, mean_lider_junior, mean_tareas_junior, mean_trabajo_junior)
    nombre_columnas_junior <<- c("Trabajador", "Liderazgo", "Tareas","Trabajo")
    colnames(resultados_junior_tabla) <<- nombre_columnas_junior
    
    setwd("D:/KEEP_ISKAR/R/datos/calidadjr/reportes")
    
    write.csv(survey_junior_lider, file = paste("lidersr_",tienda,trimestre,".csv", sep=""))
    write.csv(survey_junior_tareas, file = paste("tareasr_",tienda,trimestre,".csv", sep=""))
    write.csv(survey_junior_trabajo, file = paste("trabajosr_",tienda,trimestre,".csv", sep=""))
    
    write.csv(resultados_junior_tabla, file = paste("calidad_junior",tienda,trimestre,".csv", sep=""))
    
    usersplit <<- split(resultados_junior_tabla, resultados_junior_tabla["Trabajador"])
    
    y <<- read.csv(paste("calidad_junior", tienda, trimestre, ".csv", sep=""))
    
    
    calidad_junior_usuario <<- ddply(y, "Trabajador", summarise, 
                                     liderazgo = 2*mean(as.numeric(Liderazgo)), 
                                     tareas = 2*mean(as.numeric(Tareas)),
                                     calidad = 2*mean(as.numeric(Trabajo)))
    
    
    write.csv(calidad_junior_usuario, file = paste("calidad_junior_usuario",tienda, trimestre,".csv", sep=""))
    
    calidad_junior_usuario
    
}
