calidad_admin <- function(trimestre, summarize = FALSE)
    
{
 
    setwd("D:/KEEP_ISKAR/R/datos/calidad_admin/datos")    
    ## Read and Transpose Survey Data
    library(plyr)
    
    survey_admin_datos <<- as.data.frame(lapply(paste("CalidadAdmin", trimestre, ".csv",sep=""), read.csv, header=TRUE))
    
    survey_admin_columnas <<- as.data.frame(t(survey_admin_datos))
    
    number_col_admin <<- ncol(survey_admin_columnas)
    names_col_admin <<- c(survey_admin_columnas[5,])
    nombres_admin <<- c(t(survey_admin_columnas[5,]))
    
    
    survey_admin_lider <<- survey_admin_columnas[6:10,]
    colnames(survey_admin_lider) <<- nombres_admin
    
    survey_admin_tareas <<- survey_admin_columnas[11:13,]
    colnames(survey_admin_tareas) <<- nombres_admin
    
    survey_admin_trabajo <<- survey_admin_columnas[14:15,]
    colnames(survey_admin_trabajo) <<- nombres_admin
    
    lider_admin <- lapply(survey_admin_lider, as.matrix, header = TRUE)
    lider_admin_numeric <- lapply(lider_admin, as.numeric, header = TRUE)
    mean_lider_admin <- colMeans(t(do.call(rbind, lider_admin_numeric)))
    
    tareas_admin <- lapply(survey_admin_tareas, as.matrix, header = TRUE)
    tareas_admin_numeric <<- lapply(tareas_admin, as.numeric, header = TRUE)
    mean_tareas_admin <- colMeans(t(do.call(rbind, tareas_admin_numeric)))
    
    trabajo_admin <- lapply(survey_admin_trabajo, as.matrix, header = TRUE)
    trabajo_admin_numeric <- lapply(trabajo_admin, as.numeric, header = TRUE)
    mean_trabajo_admin <- colMeans(t(do.call(rbind, trabajo_admin_numeric)))
    
    
    resultados_admin_tabla <- as.data.frame(cbind(nombres_admin, mean_lider_admin, mean_tareas_admin, mean_trabajo_admin))
    nombre_columnas_admin <- c("Trabajador", "Liderazgo", "Tareas","Calidad")
    colnames(resultados_admin_tabla) <- as.factor(nombre_columnas_admin)
    as.factor(resultados_admin_tabla$Trabajador)

    setwd("D:/KEEP_ISKAR/R/datos/calidad_admin/reportes")
    
    write.csv(survey_admin_lider, file = paste("lidersr_",trimestre,".csv", sep=""))
    write.csv(survey_admin_tareas, file = paste("tareasr_",trimestre,".csv", sep=""))
    write.csv(survey_admin_trabajo, file = paste("trabajosr_",trimestre,".csv", sep=""))
    
    write.csv(resultados_admin_tabla, file = paste("calidad_admin",trimestre,".csv", sep=""))
    
    usersplit <<- split(resultados_admin_tabla, resultados_admin_tabla["Trabajador"])
    
    y <<- read.csv(paste("calidad_admin", trimestre, ".csv", sep=""))
    
    
    calidad_admin_usuario <<- ddply(y, "Trabajador", summarise, 
                                    liderazgo = 2*mean(as.numeric(Liderazgo)), 
                                    tareas = 2*mean(as.numeric(Tareas)),
                                    calidad = 2*mean(as.numeric(Calidad)))
    
    
    write.csv(calidad_admin_usuario, file = paste("calidad_admin_usuario", trimestre,".csv", sep=""))
    
    calidad_admin_usuario
    
    
}
