calidad_senior <- function(tienda, trimestre, summarize = FALSE)

{
 
    setwd("C:/R/calidadsr")
    
    ## Read and Transpose Survey Data
    library(plyr)

    survey_senior_datos <<- as.data.frame(lapply(paste("CalidadSenior", tienda, trimestre, ".csv",sep=""), read.csv, header=TRUE))
    
    survey_senior_columnas <<- as.data.frame(t(survey_senior_datos))
    
    number_col_senior <<- ncol(survey_senior_columnas)
    names_col_senior <<- c(survey_senior_columnas[5,])
    nombres_senior <<- c(t(survey_senior_columnas[5,]))
    
    survey_senior_lider <- survey_senior_columnas[6:10,]
    colnames(survey_senior_lider) <- nombres_senior
    
    survey_senior_tareas <- survey_senior_columnas[11:13,]
    colnames(survey_senior_tareas) <- nombres_senior
    
    survey_senior_trabajo <- survey_senior_columnas[14:15,]
    colnames(survey_senior_trabajo) <- nombres_senior
    
    lider_senior <<- lapply(survey_senior_lider, as.matrix, header = TRUE)
    lider_senior_numeric <<- lapply(lider_senior, as.numeric, header = TRUE)
    mean_lider_senior <- colMeans(t(do.call(rbind, lider_senior_numeric)))
    
    tareas_senior <<- lapply(survey_senior_tareas, as.matrix, header = TRUE)
    tareas_senior_numeric <<- lapply(tareas_senior, as.numeric, header = TRUE)
    mean_tareas_senior <- colMeans(t(do.call(rbind, tareas_senior_numeric)))
    
    trabajo_senior <<- lapply(survey_senior_trabajo, as.matrix, header = TRUE)
    trabajo_senior_numeric <<- lapply(trabajo_senior, as.numeric, header = TRUE)
    mean_trabajo_senior <- colMeans(t(do.call(rbind, trabajo_senior_numeric)))
    
    resultados_senior_tabla <<- cbind(nombres_senior, mean_lider_senior, mean_tareas_senior, mean_trabajo_senior)
    nombre_columnas_senior <<- c("Trabajador", "Liderazgo", "Tareas","Trabajo")
    colnames(resultados_senior_tabla) <<- nombre_columnas_senior
    
    y <<- as.data.frame(resultados_senior_tabla, header = TRUE, row.names = c(1:number_col_senior))
    
    write.csv(survey_senior_lider, file = paste("lidersr_",tienda,trimestre,".csv", sep=""))
    write.csv(survey_senior_tareas, file = paste("tareasr_",tienda,trimestre,".csv", sep=""))
    write.csv(survey_senior_trabajo, file = paste("trabajosr_",tienda,trimestre,".csv", sep=""))
    
    write.csv(resultados_senior_tabla, file = paste("calidad_senior",tienda,trimestre,".csv", sep=""))
    
    usersplit <<- split(resultados_senior_tabla, resultados_senior_tabla["Trabajador"])
    
    y <<- read.csv(paste("calidad_senior", tienda, trimestre, ".csv", sep=""))
    
    
    calidad_senior_usuario <<- ddply(y, "Trabajador", summarise, 
                                     liderazgo = 2*mean(as.numeric(Liderazgo)), 
                                     tareas = 2*mean(as.numeric(Tareas)),
                                     calidad = 2*mean(as.numeric(Trabajo)))
    
    
    write.csv(calidad_senior_usuario, file = paste("calidad_senior_usuario",tienda, trimestre,".csv", sep=""))
    
    calidad_senior_usuario
    
}
