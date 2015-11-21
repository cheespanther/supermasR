revision_movimientos <- function (fechas, inventario_nombre, tienda, inventario)
  ## fechas = fecha del inventario
  ## inventario = nombre del inventario en Open ERP que va a filtrar los datos
  ## Nombre de archivo movimientos_DDMMAA.csv
  
{
    xy <<- paste(inventario_nombre, inventario)
    csv_nombre <<- paste(tienda,inventario_nombre, sep = "")
    setwd("D:/KEEP_ISKAR/R/datos/movimientos/datos")
    datos <<- read.csv(paste("movimientos_", tienda, "_", fechas,  ".csv", sep=""))
    nombres <<- unique(datos$name)
    inventario <<- subset(datos, datos$name == inventario)
    unicos_inventario <<- unique(inventario["product_id.name"])
    x <<- inventario_nombre
    duplicados <<- duplicated(datos["product_id.name"])
    recuperacion <<- subset(inventario, inventario["location_dest_id.name"] != "Inventory loss")
    perdidas <<- subset(inventario, inventario["location_dest_id.name"] == "Inventory loss")
    total <<- rbind(recuperacion, perdidas)
    
    setwd("D:/KEEP_ISKAR/R/datos/movimientos/reportes")
    write.csv(perdidas, file = paste("perdidas_", inventario_nombre, ".csv", sep=""))
    write.csv(recuperacion, file = paste("recuperacion_", inventario_nombre, ".csv", sep="")) 
    write.csv(total, file = paste("movimientostotales", inventario_nombre, ".csv", sep=""))
    
}

datos_unicos <- subset(datos, duplicados!=FALSE)
