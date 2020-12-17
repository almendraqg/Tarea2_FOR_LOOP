
library("dplyr");
library("ggplot2");

# RN1
# Cargamos las bases de datos

# Micro
micro_chile <- read.csv2("micro_chile.csv")
micro_colombia <- read.csv2("micro_colombia.csv")
micro_peru <- read.csv2("micro_peru.csv")

# Pequeña
pequena_chile <- read.csv2("pequena_chile.csv")
pequena_colombia <- read.csv2("pequena_colombia.csv")
pequena_peru <- read.csv2("pequena_peru.csv")

# Mediana 
medianas_chile <- read.csv2("medianas_chile.csv")
medianas_colombia <- read.csv2("medianas_colombia.csv")
medianas_peru <- read.csv2("medianas_peru.csv")

# Grande
grandes_chile <- read.csv2("grandes_chile.csv")
grandes_colombia <- read.csv2("grandes_colombia.csv")
grandes_peru <- read.csv2("grandes_peru.csv")


# Se crean los vectores de la variable Tamanio
micro_chile$Tamanio <- "micro"
micro_colombia$Tamanio <- "micro"
micro_peru$Tamanio <- "micro"

pequena_chile$Tamanio <- "pequena"
pequena_colombia$Tamanio <- "pequena"
pequena_peru$Tamanio <- "pequena"

medianas_chile$Tamanio <- "medianas"
medianas_colombia$Tamanio <- "medianas"
medianas_peru$Tamanio <- "medianas"


grandes_chile$Tamanio <- "grandes"
grandes_colombia$Tamanio <- "grandes"
grandes_peru$Tamanio <- "grandes"

# RN2
# Unir bases de datos, antes verificamos que los nombres de las variables sean los mismos
names(micro_chile)
names(micro_colombia)
names(micro_peru)
names(grandes_chile)

# En las bases de datos de CHILE hay una variable que contiene un caracter diferente.
# Se cambia el nombre de una de las variables:
names (micro_chile)[5] = "porcentaje_mujeres"
names (medianas_chile)[5] = "porcentaje_mujeres"
names (pequena_chile)[5] = "porcentaje_mujeres"
names (grandes_chile)[5] = "porcentaje_mujeres"
names (pequena_peru)[5] = "porcentaje_mujeres"
names (medianas_peru)[5] = "porcentaje_mujeres"
names (grandes_colombia)[5] = "porcentaje_mujeres"
names (grandes_peru)[5] = "porcentaje_mujeres"

# Unir bases de datos
data <- rbind(micro_chile, pequena_chile, medianas_chile, micro_colombia, grandes_chile,
             micro_peru, pequena_peru, medianas_peru, grandes_peru,
             pequena_colombia, medianas_colombia, grandes_colombia)


peru <- filter(data, pais == "peru")      
chile <- filter(data, pais == "chile")

# RN3
# Se cuentan las obervaciones totales

for (i in 1:length(data$pais)) {            
  print(i)
}

# Se cuentan las obervaciones de Peru
for (i in 1:length(peru$pais)){            
  print(i)
}

p <- nrow(peru)
c <- nrow(chile)

# Se cuentan las obervaciones de Colombia
for (i in 1:length(chile$pais)){            
  
  if(p == c) {
    "Peru y Chile tienen el mismo número de obs"
  } else if(p < c) {
    "Chile contiene más obervaciones que Peru"
  } else {
    "Peru contiene más obervaciones que Chile"
  } 
  
  print("si")
}


if(p == c) {
  "Peru y Chile tienen el mismo número de obs"
} else if(p < c) {
  "Chile contiene más obervaciones que Peru"
} else {
  "Peru contiene más obervaciones que Chile"
} 


# País con los mayores ingresos de explotación:

peru <- filter(data, pais == "peru")      
chile <- filter(data, pais == "chile")
colombia <- filter(data, pais == "colombia")

peru_ingresos <- sum(peru$ingresos)
chile_ingresos <- sum(chile$ingresos)
colombia_ingresos <-sum(colombia$ingresos)

# Comparamos los dos primeros

if(peru_ingresos >= chile_ingresos && peru_ingresos >= colombia_ingresos) {
  PERU <- peru_ingresos
  print("El ingreso de mayor de explotación es de PERU:")
  PERU
} else if(chile_ingresos >= peru_ingresos && chile_ingresos >= colombia_ingresos) {
  CHILE <- chile_ingresos
  print("El ingreso de mayor de explotación es de CHILE:")
  CHILE
} else if (colombia_ingresos >= peru_ingresos && colombia_ingresos >= chile_ingresos) {
  COLOMBIA <- colombia_ingresos
  print("El ingreso de mayor de explotación es de COLOMBIA:")
  COLOMBIA
} 

# Se agrega la variable Colombia y se divide entre 10

nueva_variable_CL <- chile$tasa_interes*0.1
nueva_variable_CL <- as.data.frame(nueva_variable_CL)
names(nueva_variable_CL)[1] = "nueva_variable"

nueva_variable_PE <- peru$tasa_interes+0.3
nueva_variable_PE <- as.data.frame(nueva_variable_PE)
names(nueva_variable_PE)[1] = "nueva_variable"

nueva_variable_CO <- colombia$tasa_interes/10
nueva_variable_CO <- as.data.frame(nueva_variable_CO)
names(nueva_variable_CO)[1] = "nueva_variable"
  
nueva_variable <- rbind(nueva_variable_CL,
              nueva_variable_PE,
              nueva_variable_CO)
              
data <- cbind(data, nueva_variable)           
     

# Gráfico del porcentaje de participación de mujeres que trabajan en la empresa

ggplot(data, aes(x=fecha, y=porcentaje_mujeres, group = pais, colour =pais )) + 
  ggtitle("Porcentaje de participación de mujeres que trabajan en la empresa") +
  geom_line()  + 
  xlab("FECHA")+
  ylab("% de participación de mujeres que trabajan en la empresa")
  geom_point( size=2, shape=21, fill="white") + 
  theme_minimal()         
        
# Monto en millones de dólares de cuanto importan estas empresas


ggplot(data, aes(x=fecha, y=importaciones, group = pais, colour =pais )) + 
  ggtitle("Monto en millones de dólares de cuanto importan estas empresas") +
  geom_line()  + 
  xlab("FECHA")+
  ylab("Millones de dólares")
  geom_point( size=2, shape=21, fill="white") + 
  theme_minimal()


# Gráfico de ingresos para cada país durante los periodos de muestra


ggplot(data, aes(x=fecha, y=ingresos, group = pais, colour =pais )) + 
  ggtitle("Ingresos de explotación de las empresas") +
  geom_line()  + 
  xlab("FECHA")+
  ylab("INGRESOS")
  geom_point( size=2, shape=21, fill="white") + 
  theme_minimal()
    
    
# Gráfico de reservas para cada país durante los periodos de muestra  
    
ggplot(data, aes(x=fecha, y=reservas, group = pais, colour =pais )) + 
  ggtitle("Activos que mantienen sin movimiento en caso de dificultades") +
  geom_line()  + 
  xlab("FECHA")+
  ylab("RESERVAS")
  geom_point( size=2, shape=21, fill="white") + 
  theme_minimal()
