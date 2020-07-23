library(tidyverse)
library(lubridate)

dat <- read_csv("data/Covid19Casos.csv")
dat<- dat %>% filter(clasificacion_resumen == "Confirmado",
                     residencia_provincia_nombre != "SIN ESPECIFICAR") %>% 
  select(-c(residencia_departamento_id, residencia_pais_nombre,
            residencia_provincia_id, fecha_apertura, fecha_inicio_sintomas,
            sepi_apertura,carga_provincia_id,origen_financiamiento,
            CLASIFICACION,ultima_actualizacion, edad_años_meses)) %>%
  mutate(
    edad = str_replace_na(edad,0),
    fecha_internacion =  ymd(str_replace_na(fecha_internacion, "2020-01-01")),
    fecha_cui_intensivo = ymd(str_replace_na(fecha_cui_intensivo, "2020-01-01")),
    fecha_fallecimiento = ymd(str_replace_na(fecha_fallecimiento, "2020-01-01")),
    residencia_provincia_nombre = as.factor(residencia_provincia_nombre),
    Casos = 1)

lat <- c(-34.58591, -32.00000, -36.15722, -40.80000, -27.45138, -33.30000, -32.04777,
         -54.36194, -33.72277, -27.00000, -23.75000, -24.78333, -48.82380, -27.78333, -38.95166,
         -28.66660, -32.89027, -36.61666, -26.91666, -29.10000, -30.86666, -43.30000,
         -26.18330, -28.46666)
lng <- c(-58.38171, -64.00000, -60.56972, -63.00000, -58.98666, -66.35000, -60.28110, 
         -67.63805, -62.24611, -65.50000, -65.50000, -65.41666, -69.81500, -64.26666, -68.07444,
         -57.63330, -68.84722, -64.28333, -54.51666, -66.85000, -68.98333, -65.10000,
         -58.17500, -65.78333)

provincias <-c("CABA","Córdoba","Buenos Aires","Río Negro","Chaco","San Luis",
               "Entre Ríos","Tierra del Fuego","Santa Fe","Tucumán","Jujuy","Salta",
               "Santa Cruz","Santiago del Estero","Neuquén","Corrientes","Mendoza",
               "La Pampa","Misiones","La Rioja","San Juan","Chubut","Formosa","Catamarca")

coordenadas_provincias<- tibble(residencia_provincia_nombre = provincias,lat =lat,lng = lng)

dat<-inner_join(dat,coordenadas_provincias)
#write_csv(dat, "covid-Ar/datCovid.csv")


#para capas: casos confirmados, hospitalizados, fallecidos

total_casos_dia<-dat %>%
  select(fecha_diagnostico, Casos, id_evento_caso, 
         residencia_provincia_nombre, fecha_internacion, lat,lng)

data_casos <-dat%>% group_by(fecha_diagnostico, residencia_provincia_nombre,lat,lng) %>%
  filter(fecha_diagnostico != "2020-01-01") %>%
  summarize(Casos = sum(Casos))
#write_csv(data_casos, "covid-Ar/data_casos.csv")

data_internados <-dat %>% group_by(fecha_internacion, residencia_provincia_nombre,lat,lng) %>%
  filter(fecha_internacion != "2020-01-01") %>%
  summarize(Casos = sum(Casos))
#write_csv(data_internados, "covid-Ar/data_internados.csv")

data_fallecidos <- dat%>% group_by(fecha_fallecimiento, residencia_provincia_nombre,lat,lng) %>%
  filter(fecha_fallecimiento != "2020-01-01") %>%
  summarize(Casos = sum(Casos))
#write_csv(data_fallecidos, "covid-Ar/data_fallecidos.csv")


      

