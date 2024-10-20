# importar bases de datos Excel

file.choose()
ventas = read_excel("C:\Users\wjcor\OneDrive - PUJ Cali\Escritorio\Maestria en Ciencia de Datos\Serie de tiempo\AnalisisSeriesTiempo-Grupo4Ventas_t.xlsx")
View(ventas)

head(ventas)

# Asegurarse de que la columna 'semana' tiene formato fecha
ventas$Fecha <- as.Date(ventas$Fecha, format = "%Y-%m-%d")  # Ajusta el formato según sea necesario

# Verificar el formato de la columna
str(ventas$Fecha)

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

# Convertir todos los valores negativos a ceros
ventas[ventas < 0] = 0


# Debido a que nuestra nuestra base de datos contiene 80 items, decidimos hacer una agrupacion por clase de item, 
# obteniendo las siguientes categorias =
# - AVR (Auto regulador de Voltaje).
# - Data_Center (Accesorio para equipos).
# - Line UPS (Baja capacidad, uso hogar).
# - Online UPS (Alta capacidad, uso empresarial).
# - Solar (Inversores Solares). 
# - Storage_Battery (Almacenadores de Energia).
# - Surge (Multitomas). 
# - Accesorios. 


# Agrupar por categorias 
AVR = ventas%>% select(`Fecha`, `Item 10`, `Item 26`, `Item 30`, `Item 39`, `Item 45`, `Item 49`,
                       `Item 55`, `Item 62`, `Item 66`, `Item 79`)


Data_Center = ventas%>% select(`Fecha`, `Item 53`)


Line_UPS = ventas%>% select(`Fecha`, `Item 1`, `item 2`, `Item 3`, `Item 6`, `Item 9`, 
                            `Item 15`, `Item 17`, `Item 22`, `Item 34`, `Item 48`, 
                            `Item 51`, `Item 59`, `Item 63`, `Item 68`, `Item 70`,
                            `Item 71`, `Item 75`)


Online_UPS = ventas%>% select(`Fecha`, `Item 4`, `Item 5`, `Item 7`, `Item 8`, `Item 12`, 
                            `Item 13`, `Item 14`, `Item 16`, `Item 19`, `Item 20`, 
                            `Item 21`, `Item 23`, `Item 25`, `Item 27`, `Item 28`,
                            `Item 29`, `Item 31`, `Item 32`, `Item 33`, `Item 38`, `Item 41`, 
                            `Item 43`, `Item 44`, `Item 46`, `Item 50`, `Item 56`, `Item 57`, 
                           `Item 58`, `Item 60`, `Item 65`, `Item 72`, `Item 76`, `Item 77`, 
                           `Item 80`)


Solar = ventas%>% select(`Fecha`, `Item 36`, `Item 52`, `Item 69`, `Item 74`)


Storage_Battery = ventas%>% select(`Fecha`, `Item 11`, `Item 18`, `Item 24`, `Item 35`, `Item 37`, 
                            `Item 40`, `Item 64`, `Item 67`, `Item 78`)



Surge = ventas%>% select(`Fecha`, `Item 47`, `Item 54`, `Item 61`)


Accesories = ventas%>% select(`Fecha`, `Item 42`, `Item 73`)


# Crear una columna del total de items por catgoria

AVR = AVR%>% mutate(Total = rowSums(select(.,`Item 10`, `Item 26`, `Item 30`, `Item 39`, `Item 45`, `Item 49`,
                                           `Item 55`, `Item 62`, `Item 66`, `Item 79`), na.rm = TRUE))

Data_Center = Data_Center%>% mutate(Total = rowSums(select(.,`Item 53`), na.rm = TRUE))


Line_UPS = Line_UPS%>% mutate(Total = rowSums(select(.,`Item 1`, `item 2`, `Item 3`, `Item 6`, `Item 9`, 
                                                     `Item 15`, `Item 17`, `Item 22`, `Item 34`, `Item 48`, 
                                                     `Item 51`, `Item 59`, `Item 63`, `Item 68`, `Item 70`,
                                                     `Item 71`, `Item 75`), na.rm = TRUE))


Online_UPS = Online_UPS%>% mutate(Total = rowSums(select(.,`Item 4`, `Item 5`, `Item 7`, `Item 8`, `Item 12`, 
                                                         `Item 13`, `Item 14`, `Item 16`, `Item 19`, `Item 20`, 
                                                         `Item 21`, `Item 23`, `Item 25`, `Item 27`, `Item 28`,
                                                         `Item 29`, `Item 31`, `Item 32`, `Item 33`, `Item 38`, `Item 41`, 
                                                         `Item 43`, `Item 44`, `Item 46`, `Item 50`, `Item 56`, `Item 57`, 
                                                         `Item 58`, `Item 60`, `Item 65`, `Item 72`, `Item 76`, `Item 77`, 
                                                         `Item 80`), na.rm = TRUE))


Solar = Solar%>% mutate(Total = rowSums(select(.,`Item 36`, `Item 52`, `Item 69`, `Item 74`), na.rm = TRUE))


Storage_Battery = Storage_Battery%>% mutate(Total = rowSums(select(.,`Item 11`, `Item 18`, `Item 24`, `Item 35`, `Item 37`, 
                                                                   `Item 40`, `Item 64`, `Item 67`, `Item 78`), na.rm = TRUE))


Surge = Surge%>% mutate(Total = rowSums(select(., `Item 47`, `Item 54`, `Item 61`), na.rm = TRUE))


Accesories = Accesories%>% mutate(Total = rowSums(select(., `Item 42`, `Item 73`), na.rm = TRUE))


# Después de un análisis de los datos, decidimos enfocarnos en cuatro categorías: 
# AVR, Line UPS, Online UPS y Battery Storage, que en total suman 70 ítems. 
# No se han considerado las categorías de Data Center, Solar, Surge y Accesorios, 
# ya que hemos observado una gran cantidad de ceros en estas, lo que sugiere que 
# podemos incluir estos ítems para venta solo por pedido especial y no es necesario 
# mantenerlos en inventario.



# A continuacion haremos un analisis de cada ua de las categoria seleccionadas: 

# AVR (Auto regulador de Voltaje)

# Convertir a serie de tiempo
AVR_ts <- ts(AVR$Total, start = c(2021, 2), frequency = 52)

AVR_ts

# Verificar la clase del objeto
class(AVR_ts) 

# plot
plot(AVR_ts, main= "AVR ", ylab= "Ventas", xlab= "Semana")


# Calcular la media móvil ( de 4períodos)
media_movil_AVR = stats::filter(AVR_ts, rep(1/4, 4), sides = 2)
print(media_movil_AVR)

plot(media_movil_AVR, main= "AVR Media Movil ", ylab= "Ventas", xlab= "Semana")


# lag plot grafica de resagos 
lag.plot(AVR_ts, 20, main = "AVR", do.lines = FALSE)


______


# Line UPS (Baja capacidad, uso hogar)

# Convertir a serie de tiempo
Line_UPS_ts <- ts(Line_UPS$Total, start = c(2021, 2), frequency = 52)

Line_UPS_ts

# Verificar la clase del objeto
class(Line_UPS_ts) 

# plot
plot(Line_UPS_ts, main= "Line UPS ", ylab= "Ventas", xlab= "Semana")


# Calcular la media móvil ( de 4períodos)
media_movil_Line_UPS = stats::filter(Line_UPS_ts, rep(1/4, 4), sides = 2)
print(media_movil_Line_UPS)

plot(media_movil_Line_UPS, main= "Line UPS Media Movil ", ylab= "Ventas", xlab= "Semana")


# lag plot grafica de resagos 
lag.plot(Line_UPS_ts, 20, main = "Line UPS", do.lines = FALSE)

_________


# Online UPS (Alta capacidad, uso empresarial)

# Convertir a serie de tiempo
Online_UPS_ts <- ts(Online_UPS$Total, start = c(2021, 2), frequency = 52)

Online_UPS_ts

# Verificar la clase del objeto
class(Online_UPS_ts) 

# plot
plot(Online_UPS_ts, main= "Online UPS ", ylab= "Ventas", xlab= "Semana")


# Calcular la media móvil ( de 4períodos)
media_movil_Online_UPS = stats::filter(Online_UPS_ts, rep(1/4, 4), sides = 2)
print(media_movil_Online_UPS)

plot(media_movil_Online_UPS, main= "Online UPS Media Movil ", ylab= "Ventas", xlab= "Semana")


# lag plot grafica de resagos 
lag.plot(Online_UPS_ts, 20, main = "Online UPS", do.lines = FALSE)


_______


# Storage_Battery (Almacenadores de Energia)

# Convertir a serie de tiempo
Storage_Battery_ts <- ts(Storage_Battery$Total, start = c(2021, 2), frequency = 52)

Storage_Battery_ts

# Verificar la clase del objeto
class(Storage_Battery_ts) 

# plot
plot(Storage_Battery_ts, main= "Storage Battery ", ylab= "Ventas", xlab= "Semana")


# Calcular la media móvil ( de 4períodos)
media_movil_Storage_Battery = stats::filter(Storage_Battery_ts, rep(1/4, 4), sides = 2)
print(media_movil_Storage_Battery)

plot(media_movil_Storage_Battery, main= "Online UPS Media Movil ", ylab= "Ventas", xlab= "Semana")


# lag plot grafica de resagos 
lag.plot(Storage_Battery_ts, 20, main = "Storage Battery",  do.lines = FALSE)


