install.packages(c("dplyr", "ggplot2", "visdat", "skimr", "insuranceData"))
library(dplyr)
library(ggplot2)
library(visdat)
library(skimr)
library(insuranceData)
#Base de Datos a usar dentro de insuranceData
data(dataCar)
#Ver la estructura de la base 
str(dataCar)
summary(dataCar)
#resumen mas detallado de la base
skim(dataCar)
#encabezado de la base de datos
head(dataCar,10)
#Dimension de la base de datos (filas, columnas)
dim(dataCar)
#Caracteristicas mas especificas
glimpse(dataCar)
#conocer el nombre de las columnas(2 funciones)
colnames(dataCar)
names(dataCar)
#verificar si hay datos faltantes 
miss <- any(is.na(dataCar))
#visualizar datos 
vis_dat(dataCar)
#visualizar datos faltantes 
vis_miss(dataCar)

#PREGUNTAS ESPECIFICAS PARA CONTESTAR EN LA BASE DE DATOS 
#conocer la magnitud de la columna del nbumero de reclamaciones
pol <- length(dataCar$numclaims)
#conocer que polizas tienen una o mas reclamaciones 
claims <- sum(dataCar$numclaims>=1)
#para conocer el dato anterior porcentualmente
porcentaje <- (claims/pol)*100
#Verifique el top 5 de vehiculos con mayor numero de reclamaciones 
claims_tipo <- dataCar %>%
  group_by(veh_body)%>%
  summarise(totclaims = sum(numclaims)) %>%
  arrange(desc(totclaims))
#--muestrame solo los primeros 5 registros 
head(claims_tipo, 5)
#numero de polizas por tipo de vehiculo 
num_pol_tipo <- dataCar%>%
  group_by(veh_body)%>%
  summarise(totveh = n())%>%
  arrange(desc(totveh))
head(num_pol_tipo)
#verifica el top 10 de vehiculos con mayor monto de reclamaciones 
monto_veh <- dataCar%>%
  group_by(veh_body)%>%
  summarise(monto = sum(claimcst0))%>%
  arrange(desc(monto))
head(monto_veh,10)

#-------------------------------------------------------------------------------
#  t  a  r  e  a
#-------------------------------------------------------------------------------
# Quien hace un no. mayor de reclamaciones: Hombres o Mujeres
# Número total de reclamaciones por género
claims_by_gender <- dataCar %>%
  group_by(gender, veh_body, numclaims) %>%
  summarise(total_claims = sum(claimcst0)) %>%
  arrange(desc(total_claims))

# Mostrar los resultados
print(claims_by_gender)

# Gráfico de barras para visualizar el número de reclamaciones por género
ggplot(claims_by_gender, aes(x = gender, y = total_claims, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Número de Reclamaciones por Género", x = "Género", y = "Total de Reclamaciones") +
  theme_minimal()
#-------------------------------------------------------------------------------

#quienes son los mas siniestrados entre hombres y mujeres y grupo de edad
claims_gender_age <- dataCar%>%
  group_by(gender, agecat, veh_body)%>%
  summarise(total_claims_gender = sum(numclaims)) %>%
  arrange(desc(total_claims_gender))
print(claims_gender_age)

#Graficos con la funcion ggplot
ggplot(dataCar,aes(x=exposure,y=veh_value))+
  geom_point()+
  labs(title = "Grafico dispersion Exposición vs Valor de Vehiculo", x="Exposicion", y="Valor Vehiculo")+
  theme_minimal()
#maximo valor del vehiculo
max_value_veh <- dataCar%>%
  group_by(veh_body, veh_value, claimcst0, exposure)%>%
  summarise(max_veh = max(veh_value))%>%
  arrange(desc(max_veh))
print(max_value_veh)

#grafico contemplando el genero
ggplot(claims_by_gender, aes(x = reorder(veh_body, -total_claims), y = total_claims, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tipos de Vehiculos más reclamados por género", 
       x = "Tipo de vehículo", 
       y = "Número de Reclamaciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

          