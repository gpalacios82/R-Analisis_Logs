library(tidyverse)
library(lubridate)
library(jsonlite)

setwd("/Users/Root/R-Analisis_Logs/")

errores_impresion <- c(
  'Impresora apagada o desconectada',
  'Impresora OK',
  'Impresora ocupada',
  'Se ha superado el maximo numero de campos',
  'No se ha cargado el XML antes',
  'Error cargando la librería DOM',
  'Tag no reconocido.',
  'DOM devuelve un error de XML incorrecto',
  'Error desconocido',
  'Todavía no se ha definido ninguna impresora',
  'La impresora no contesta',
  'Respuesta de la impresora desconocida',
  'Impresora sin papel',
  'Impresora atascada',
  'Impresora necesita reset',
  'No se ha cargado hwinterface.sys',
  'Poco papel',
  'Memoria flash llena',
  'Impresora en modo menu',
  'Impresora en modo espera',
  'Cabezal abierto',
  'Error en comando',
  'Impresora operando')


files <- as.character(list.files(path=".", pattern = "*.log"))

log <- as_tibble()
for(f in files){
  print(paste("Abriendo fichero: ", f, sep=""))
  log <- rbind(log, as_tibble(paste(readLines(f),sep = "\n")))
}
print(paste("Procesadas ", nrow(log), " filas", sep=""))

Encoding(log$value) <- 'latin1' 

log <- log %>% 
  filter(!value == "") %>%
  mutate(
    fecha = ymd_hms(substring(value,2,20)),
    version = gsub("].*", "", substring(value,31)),
    log = trimws(gsub("[0-9]*.]", "", substring(value,31)))
  ) %>%
  filter(!log == "",
         !substring(log,1,9) == 'Contenido',
         !substring(log,1,7) == 'destino',
         !substring(log,1,8) == 'Lanzando',
         !substring(log, nchar(log)-6,nchar(log)) == '.zipped',
         !substring(log, nchar(log)-13,nchar(log)) == 'GetHomeTicket>',
         !substring(log,1,8) == '--------',
         !substring(log,1,4) == 'URL:',
         !substring(log,1,8) == 'DESTINO:',
         !substring(log,1,21) == 'Error en la impresion',
         !substring(log,1,21) == 'Numero de operaciones',
         !substring(log,1,9) == 'Operacion',
         !substring(log,1,40) == 'Error en la impresion, no se ha definido',
         !substring(log,1,23) == 'IMPRESION --> ErrorCode',
         !log == 'Ticket Impreso ERROR') %>%
  mutate(
    log = ifelse(substring(log,1,9) == 'IMPRESION', trimws(gsub("\\[\\ |\\]","",gsub(".*-->","",value))), log),
    tipo = ifelse(substring(log,1,3) %in% "htt" | substring(log,1,14) %in% "Tipo Impresion", "llamada_servicio",
                  ifelse(
                      substring(log,1,5) %in% "Nivel" | substring(log,1,7) %in% "Usuario" | 
                      substring(log,2,8) %in% "Empresa" | substring(log,1,3) %in% "xml" |
                      substring(log,2,10) %in% "Operacion" |
                      substring(log,1,8) %in% "Ticket :" | substring(log,1,8) %in% "IdCookie",
                    "llamada_peticion", "llamada_respuesta")
    ),
    tipo2 = ifelse(substring(log,1,3) %in% "htt" | substring(log,1,14) %in% "Tipo Impresion", "fecha_servicio",
                   ifelse(
                      substring(log,1,5) %in% "Nivel" | substring(log,1,7) %in% "Usuario" | 
                      substring(log,2,8) %in% "Empresa" | substring(log,1,3) %in% "xml" |
                      substring(log,2,10) %in% "Operacion" |
                      substring(log,1,8) %in% "Ticket :" | substring(log,1,8) %in% "IdCookie",
                     "fecha_peticion", "fecha_respuesta")
    )
  ) %>%
  filter(!substring(log,1,16) == "Podemos imprimir",
         !substring(log,1,9) == "CerrarLog") %>%
  mutate(id = row_number(),
         id_grupo = trunc((row_number()-1)/3)+1) %>%
  select(id, id_grupo, fecha, version, tipo, tipo2, log)




aux <- log %>%
  spread(tipo, log, convert = T, drop = T) %>%
  spread(tipo2, fecha, convert = T, drop = T) %>%
  select(-id)

a <- filter(aux, !is.na(llamada_peticion)) %>% select(-llamada_respuesta, -llamada_servicio, -fecha_respuesta, -fecha_servicio)
b <- filter(aux, !is.na(llamada_respuesta)) %>% select(-llamada_peticion, -llamada_servicio, -fecha_peticion, -fecha_servicio)
c <- filter(aux, !is.na(llamada_servicio)) %>% select(-llamada_respuesta, -llamada_peticion, -fecha_respuesta, -fecha_peticion)
final <- inner_join(a,b)
log <- inner_join(final, c) %>% select(id = id_grupo, version, fecha_peticion, llamada_servicio, llamada_peticion, fecha_respuesta, llamada_respuesta)

final <- NULL
aux <- NULL
a <- NULL
b <- NULL
c <- NULL

log <- log %>%
  mutate(
    metodo = ifelse(substring(llamada_servicio,1,5) == "http:", "http", 
                    ifelse(substring(llamada_servicio,1,5) == "Tipo ", "LOCAL", "https")),
    puerto = ifelse(metodo == "http", 8080, ifelse(metodo == "LOCAL", 0, 443)),
    host = ifelse(metodo == "http", "93.90.21.40", ifelse(metodo == "https", "www.janto.es", "LOCAL")),
    servicio = ifelse(host == "LOCAL", "Impresion", gsub("*.*/", "", llamada_servicio)),
    war = ifelse(servicio == "Impresion", "LOCAL", gsub("/.*", "", gsub(".*Janto_", "", llamada_servicio))),
    fecha_peticion = ymd_hms(fecha_peticion),
    fecha_respuesta = ymd_hms(fecha_respuesta),
    tiempo_respuesta = abs(fecha_peticion - fecha_respuesta)
  ) %>%
  arrange(fecha_peticion) %>%
  mutate(
    peticion = trimws(gsub("xmlDoc*.","",gsub("Ticket :*.","",llamada_peticion))),
    error = grepl("error", gsub("error=\"\"","", gsub("errorcode=\"\"","", gsub("error\":\"\"","", gsub("errorcode\":\"\"","", tolower(llamada_respuesta)))))),
    error_code = ifelse(
      error,
      ifelse(
        servicio == "Impresion",
        gsub("ErrorTxt ","",llamada_respuesta),
        'X'
      ),
      ''), 
    error_txt = ifelse(
      error,
      ifelse(
        servicio == "Impresion",
        errores_impresion[as.integer(gsub("ErrorTxt ","",llamada_respuesta))+1],
        'Otro error'
        ),
      ''
    )
    
  ) %>%
  select(metodo, host, puerto, war, servicio, version, fecha_peticion, 
         tiempo_respuesta, fecha_respuesta, peticion, 
         respuesta = llamada_respuesta, error, error_code, error_txt)

print(paste("Hay un total de: ",sum(log$error), " errores", sep = ""))
log %>%
  filter(error) %>%
  select(servicio, fecha_peticion, tiempo_respuesta, error_txt) %>%
  arrange(fecha_peticion) %>% View()


log %>%
  group_by(servicio, tiempo_respuesta) %>%
  mutate(rep = n(),
         Dia = substring(fecha_peticion,1,10)) %>%
  arrange(desc(servicio)) %>%
  ggplot(aes(tiempo_respuesta, factor(servicio), col=war)) +
  geom_text(aes(label = rep), size=4) +
  scale_x_time("Tiempos de respuesta") +
  scale_y_discrete("Servicio") +
  scale_size_continuous("Numero de \nPeticiones") +
  scale_color_discrete("Tipo de \nConexion") +
  facet_grid(.~Dia)





log %>%
  group_by(fecha_peticion, servicio, metodo) %>%
  summarise(TsecMed = mean(tiempo_respuesta)) %>%
  ggplot(aes(fecha_peticion, TsecMed, col= servicio)) +
  geom_jitter() +
  scale_x_datetime("Fecha y Hora") +
  scale_y_time("Tiempo medio de respuesta") +
  scale_color_discrete("Servicio") +
  facet_grid(metodo~.)



log %>%
  filter(metodo == "http") %>%
  group_by(fecha_peticion, metodo, servicio) %>%
  summarise(TsecMed = mean(tiempo_respuesta)) %>%
  ggplot(aes(fecha_peticion, TsecMed)) +
  geom_jitter(aes(col = servicio)) +
  geom_smooth(method = "loess") +
  scale_x_datetime("Hora de las peticiones") +
  scale_y_time("Tiempos de respuesta")

