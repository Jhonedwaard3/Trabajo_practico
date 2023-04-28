library(ggplot2)
library(dplyr)
library(readxl)
###################

SalesData <- read_excel("H:/Mi unidad/Maestría/08. Visualización de Datos/Trabajo práctico/SalesData.xlsx")
Tiendas <- read_excel("H:/Mi unidad/Maestría/08. Visualización de Datos/Trabajo práctico/SalesData.xlsx", sheet = "Store Locations Sheet")
Productos <- read_excel("H:/Mi unidad/Maestría/08. Visualización de Datos/Trabajo práctico/SalesData.xlsx",sheet = "Products Sheet")
Regiones <- read_excel("H:/Mi unidad/Maestría/08. Visualización de Datos/Trabajo práctico/SalesData.xlsx",sheet = "Regions Sheet")
######################


TablaChannel <- aggregate(SalesData$`Order Quantity` ~ SalesData$`Sales Channel`, SalesData, sum) 
TablaChannel <- transform(TablaChannel, Porcentaje = TablaChannel$`SalesData$\`Order Quantity\``/sum(TablaChannel$`SalesData$\`Order Quantity\``) * 100)

colnames(TablaChannel)[1] <- "Canal"
colnames(TablaChannel)[2] <- "Unidades"

TablaChannel <- TablaChannel[order(TablaChannel$Unidades, decreasing = TRUE), ]

grafico1 <- ggplot(TablaChannel, aes(x = Canal, y=Unidades) ) + 
  geom_bar(stat="identity",fill = "#e8550f") + 
  facet_grid(~"Unidades vendidas por canal de ventas") +  
  coord_flip()+ theme_bw()


#######################################################################
library(tidyverse)

Tiendas <- inner_join(Tiendas, Regiones, by = "StateCode")
Ventas <- inner_join(SalesData, Tiendas, by = "_StoreID")
Ventas <- inner_join(Ventas, Productos, by = "_ProductID")

TablaProductos <- aggregate(Ventas$`Order Quantity` ~ Ventas$`Product Name`, Ventas, sum) 
TablaProductos <- transform(TablaProductos, Porcentaje = TablaProductos$`Ventas$\`Order Quantity\``/sum(TablaProductos$`Ventas$\`Order Quantity\``) * 100)

colnames(TablaProductos)[1] <- "Producto"
colnames(TablaProductos)[2] <- "Unidades"

TablaProductos <- TablaProductos[order(TablaProductos$Unidades, decreasing = TRUE), ]
top_10_Prod <- head(TablaProductos$Producto, 10)
datos_filtrados <- TablaProductos[TablaProductos$Producto %in% top_10_Prod, ]

grafico2 <- ggplot(datos_filtrados, aes(x = Producto, y=Unidades) ) + 
  geom_bar(stat="identity",fill = "#e8550f") + 
  facet_grid(~"Unidades vendidas por Productos (Top 10)") +  
  coord_flip()+ theme_bw()


TablaReg<- aggregate(Ventas$`Order Quantity` ~ Ventas$Region, Ventas, sum) 
TablaReg <- transform(TablaReg, Porcentaje = TablaReg$`Ventas$\`Order Quantity\``/sum(TablaReg$`Ventas$\`Order Quantity\``) * 100)

colnames(TablaReg)[1] <- "Región"
colnames(TablaReg)[2] <- "Unidades"

TablaReg <- TablaReg[order(TablaReg$Unidades, decreasing = TRUE), ]
top_10_Reg <- head(TablaReg$Región, 10)
datos_filtrados2 <- TablaReg[TablaReg$Región %in% top_10_Reg, ]

grafico3 <- ggplot(datos_filtrados2, aes(x = Región, y=Unidades) ) + 
  geom_bar(stat="identity",fill = "#e8550f") + 
  facet_grid(~"Unidades vendidas por Región") +  
  coord_flip()+ theme_bw()

par(mfrow = c(1, 3))
grafico1
grafico2
grafico3
