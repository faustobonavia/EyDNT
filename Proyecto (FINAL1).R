library(academictwitteR)
library(twitteR)
library(writexl)
library(tidyverse)
library(tidytext)
library(rtweet)
library(jsonlite)
library(parallel)
library(udpipe)
library(forcats)
library(wordcloud2)
library(wordcloud)
library(syuzhet)
library(RColorBrewer)
library(tm)
library(scales)
library(zoo)
library(lubridate)
library(ggplot2)
library(ggcharts)
library(dplyr)

appname <- "mateafer"
API_key = "nMEiYvHDiBE7yFFF7OEDyQkL1" 
API_secret = "KM2Qxefyks43Szt2Dxqq2x8lkMScRgyj1pNfDspHysw3Jcc8DS"
Access_token = "1306053691-JHShOXQRX62QQf594BG0EUeN93wKONtZtaxmbGG"
Access_secret = "V1F3AKbCjLzchJrZc79GiE4yeDeBZxN2AZXAlIgnx07bf"

setup_twitter_oauth(API_key,API_secret,Access_token,Access_secret)
auth_as('create_token')

# obtengo tweets del oficialismo

af_tweets <- get_timeline("alferdez",n=4000, include_rts = FALSE)

ak_tweets <- get_timeline("Kicillofok",n=4000, include_rts = FALSE)

cfk_tweets <- get_timeline("CFKArgentina",n=4000, include_rts = FALSE)

sm_tweets <- get_timeline("SergioMassa",n=4000, include_rts = FALSE)

wdp_tweets <- get_timeline("wadodecorrido",n=4000, include_rts = FALSE)

fd_tweets <- get_timeline("FilmusDaniel",n=4000, include_rts = FALSE)

an_tweets <- get_timeline("FernandezAnibal",n=4000, include_rts = FALSE)

# obtengo tweets de la oposición

mm_tweets <- get_timeline("mauriciomacri",n=4000, include_rts = FALSE)

hl_tweets <- get_timeline("horaciorlarreta",n=4000, include_rts = FALSE)

pb_tweets <- get_timeline("PatoBullrich",n=4000, include_rts = FALSE)

ds_tweets <- get_timeline("diegosantilli",n=4000, include_rts = FALSE)

mv_tweets <- get_timeline("mariuvidal",n=4000, include_rts = FALSE)

lc_tweets <- get_timeline("elisacarrio",n=4000, include_rts = FALSE)

rf_tweets <- get_timeline("frigeriorogelio",n=4000, include_rts = FALSE)

#df <- do.call("rbind", lapply(af_test_2, as.data.frame))

#test<- search_tweets("auto", n=10, since ="2022-01-01", until = "2022-01-31")

# limpieza y transformación de los datos

af <- af_tweets %>% select(
  created_at, full_text
  ) %>% 
  mutate(usuario = "Alberto Fernandez") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

ak <- ak_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Axel Kicillof") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

cfk <- cfk_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Cristina Fernandez") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

sm <- sm_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Sergio Massa") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

wdp <- wdp_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Wado de Pedro") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

fd <- fd_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Daniel Filmus") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

anf <- af_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Anibal Fernandez") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

mm <- mm_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Mauricio Macri") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

hl <- hl_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Horacio Rodriguez Larreta") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

pb <- pb_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Patricia Bullrich") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

ds <- ds_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Diego Santilli") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

mv <- mv_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Maria Eugenia Vidal") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

lc <- lc_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Lilita Carrio") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)

rf <- rf_tweets %>% select(
  created_at, full_text
) %>% 
  mutate(usuario = "Rogelio Frigerio") %>% 
  mutate(date = format.Date(created_at, format="%d-%m-%Y")) %>% 
  mutate(tweet_text = full_text) %>% 
  select(usuario, date, tweet_text)



# unifico los dataframes entre oficialistas y opositores

partido_oficialista <- bind_rows(af, ak, cfk, sm, wdp, fd, anf)

partido_opositor <- bind_rows(mm, hl, pb, mv, ds, lc, rf)

# guardo los csv para no tener que realizar la carga cada vez que se utiliza el codigo
## utilizaar un encoding en español

partido_oficialista %>% 
write.csv('partido_oficialista.csv')

partido_opositor %>% 
  write.csv('partido_opositor.csv')



# Cargo los dataframes ----------------------------------------------------

partido_oficialista <- read.csv('C:/Users/Fausto/OneDrive - Económicas - UBA/Facultad/4to/Economía y PDNT/Econ-708/Repositorios de práctica/Proyecto final/partido_oficialista.csv', header=TRUE)

partido_opositor <- read.csv('C:/Users/Fausto/OneDrive - Económicas - UBA/Facultad/4to/Economía y PDNT/Econ-708/Repositorios de práctica/Proyecto final/partido_opositor.csv', header=TRUE)

# General cleaning --------------------------------------------------------

## genero las bases de datos por palabras

by_word_partido_oficialista <- partido_oficialista %>%
  unnest_tokens(word, tweet_text)

by_word_partido_opositor <- partido_opositor %>%
  unnest_tokens(word, tweet_text)

## elimino las stop words y números

# Filtro por stop_words de https://countwordsfree.com/stopwords/spanish
stop_words <- fromJSON("C:/Users/Fausto/OneDrive - Económicas - UBA/Facultad/4to/Economía y PDNT/Econ-708/Repositorios de práctica/Proyecto final/stop_words_spanish.json")
stop_words <- as_tibble(stop_words) %>% 
  rename(word=value)

more_stop_words <- as_tibble(c('sinceramente', 'difícil','gracias', 'lxs','link','mis','s','alferdez','pandemia','lean','𝗺𝘂𝗻𝗱𝗼','𝗮𝗹','𝘀𝗮𝗹𝘃𝗮','𝗗𝗶𝗼𝘀','evangelio','policia_ciudad','unmejorlugardondevivir','entreríos','horaciorlarreta','casa','ciudad','vivo','mil','as','argentinaunida','podés','quiero','dios','frente','provincia','mayoríaargentina','nuevamayoría','defenderlaargentina','hayesperanza','hayalternativa','año','política','país','argentina', 'gobierno', 'vecinos', 'gente', 'presidente', 'nacional', 'argentinos', 'argentinas','años','diciembre', 'millones','día', 'abre', 'acá', 'agosto', 'abril', 'alguien', 'allá', 'alta', 'alto', 'atrás',
                               'arroyo', 'avellaneda', 'avión', 'azul', 'auto', 'b', 'bahía', 'baja',
                               'balvanera', 'belgrano', 'berard', 'bienvenida', 'blanca', 'boca', 'brasil',
                               'brazos', 'buenosaires2018', 'c', 'c40cities', 'cabeza', 'cabo', 'café',
                               'calle', 'calles', 'celulares', 'ceo', 'ceremonia', 'Chacabuco', 'chacarita',
                               'chaco', 'Chubut', 'ciclo', 'cientos', 'cincosenadores', 'clarinconm', 'club',
                               'clubes', 'colegio', 'columna', 'combinación', 'comentarios', 'comisariascercanas',
                               'comité', 'comuna', 'comunal', 'concejales', 'conectividad', 'contacto', 'contactos',
                               'contagio', 'contagios', 'coronavirus', 'corresponde', 'corrientes', 'cosa', 'costa',
                               'covid', 'crespo', 'cristianritondo', 'cruz', 'cual', 'cualquiera', 'cuanto', 'cuando',
                               'cuarentena', 'cuartel', 'cuarto', 'd', 'd.e', 'damianarabia', 'daniel', 'debajo', 'debatebuenosaires',
                               'debatecapital', 'decían', 'delatorrej', 'den', 'depende', 'detrás', 'devoto', 'diciendo', 'diego',
                               'diegosantilli', 'dievalen', 'doble', 'documento', 'domingo', 'donde', 'dueños', 'duro', 'eavogadro',
                               'edad', 'edición', 'edificio', 'edufeiok', 'edumacchiavelli', 'efectivos', 'eje', 'ejes', 'eldiariodelecuo',
                               'elisacarrio', 'emmaferrario', 'enero', 'enfermeros', 'entrar', 'enviar', 'envío', 'época', 'escribir',
                               'españa', 'esteban', 'estebanbullrich', 'estrechos', 'estuve', 'Eugenia', 'febrero', 'felipemiguelba',
                               'feria', 'feriglesias', 'Fernando', 'fernanquirosba', 'física', 'flores', 'Formosa', 'formosenos', 'francisco',
                               'francomoccia', 'frigeriorogelio', 'fuego', 'futbol', 'g20', 'g20argentina', 'Gabriel', 'gas', 'gastronomía',
                               'gastronómicos', 'gerardomorales', 'gmilman', 'gmontenegro_ok', 'Gonzales', 'gracielaocano', 'gugalusto',
                               'guillodietrich', 'gusta', 'gustavo', 'gustavovaldesok', 'habíamos', 'hijas', 'hijo', 'hijos', 'hilo', 'hora',
                               'horacio', 'horaciolarreta', 'horas', 'https', 'iba', 'iban', 'india', 'insfran', 'Instagram', 'invierno',
                               'islas', 'Israel', 'Japón', 'javierjiguacel', 'jonatanviale', 'Jorge', 'jorgemacri', 'jose', 'juan', 'jueves',
                               'Jujuy', 'julio', 'juliogarro', 'Junín', 'juntoscambioar', 'juntospodemosmas', 'kg', 'kicillofok', 'kilómetros',
                               'kilos', 'km', 'lacornisa', 'lanacion', 'lanacionmas', 'Lanús', 'legiscaba', 'libro', 'llamar', 'lugano', 'Luis',
                               'lujan', 'lunes', 'majulluis', 'Malvinas', 'mañana', 'Manuel manupassaglia', 'mar', 'macedaless', 'mariamigliore',
                               'mariuvidal', 'martintetaz', 'marzo', 'matanza', 'mauriciomacri', 'maxiferraro', 'mayo', 'mendoza', 'metrobus', 'metropolitana',
                               'mi', 'miércoles','t.co', 'tu', 'san', 'https', 'miguel', 'miguelpichetto', 'minutos', 'mitre', 'moreno', 'morón', 'nachotorresch', 'navidad', 'natural', 'nestorgrindetti', 'neuquen', 'nicolas', 'niños', 'noche', 'nombre', 'norte', 'noviembre', 'numero', 'obelisco', 'obtener', 'octubre', 'oficinas', 'olavarria', 'ordinarias', 'pablo', 'pacientes', 'padre', 'padres', 'Palermo', 'Paraguay', 'patobullrich', 'patricios', 'pau_oliveto', 'paula', 'pba', 'pedro', 'pergamino', 'pfaoficial', 'pzifer', 'podes', 'prefecturanaval', 'pro', 'proargentina', 'quema', 'Quilmes', 'quiso', 'r', 'radiomitre', 'rato', 'recoleta', 'reino', 'reuní', 'revés', 'rincón', 'rio', 'rionja', 'ríos', 'Rivadavia', 'rivadavia630', 'rlopezmurphy', 'roca', 'rosa', 'rosada', 'rosario', 'rueda', 'rusia', 'ruta', 'rutas', 'sábado', 'sacar', 'sala', 'salas', 'sale', 'salen', 'salguero', 'salta', 'same', 'san', 'sanchezfdo', 'santa', 'sarmiento', 'se', 'seamos', 'semana', 'sentir', 'sepan', 'septiembre', 'seres', 'sergio', 'serie', 'serio', 'siglo', 'significa', 'sol', 'soledad_acunia', 'sos', 'sputnik', 'subte', 'suprema', 'sur', 'supuesto', 't.co', 'tanta', 'tantas', 'tantos', 'tapaboca', 'tapabocas', 'tasa', 'tasas', 'te', 'teatro', 'Telmo', 'tenes', 'terapia', 'tercer', 'tiempos', 'tigre', 'timbreo', 'tipo', 'toca', 'todonoticias', 'totalmente', 'tova', 'tramo', 'transito', 'traspasoprovincia', 'tren', 'trenes', 'tu', 'tucuman', 'turno', 'turnos', 'tus', 'tuve', 'tuvieron', 'tuvimos', 'uba', 'ubaonline', 'ucrania', 'une', 'Uruguay', 'ustedes', 'v', 'vaca', 'vale', 've', 'vehículos', 'ven', 'vemos', 'Vicente', 'video', 'viendo', 'virtual', 'virtuales', 'visita', 'vista', 'vivianaconvos', 'vivicanosaok', 'xxi', 'yendo'))

more_stop_words <- more_stop_words %>% 
  rename(word=value)

stop_words <- bind_rows(stop_words, more_stop_words)

oficialismo <- by_word_partido_oficialista %>% 
  anti_join(stop_words, by='word') %>% 
  mutate(date = as.Date(date,format= "%d-%m-%Y")) %>%
  select('usuario', 'date', 'word') %>% 
  filter(!str_detect(word, '0'),
         !str_detect(word, '1'),
         !str_detect(word, '2'),
         !str_detect(word, '3'),
         !str_detect(word, '4'),
         !str_detect(word, '5'),
         !str_detect(word, '6'),
         !str_detect(word, '7'),
         !str_detect(word, '8'),
         !str_detect(word, '9'))

oposicion <- by_word_partido_opositor %>% 
  anti_join(stop_words, by='word') %>% 
  mutate(date = as.Date(date,format= "%d-%m-%Y")) %>% 
  select('usuario', 'date', 'word') %>% 
  filter(!str_detect(word, '0'),
         !str_detect(word, '1'),
         !str_detect(word, '2'),
         !str_detect(word, '3'),
         !str_detect(word, '4'),
         !str_detect(word, '5'),
         !str_detect(word, '6'),
         !str_detect(word, '7'),
         !str_detect(word, '8'),
         !str_detect(word, '9'))



# Genero los grupos en 4 periodos -----------------------------

## 2017 - 2019

oficialismo_pre <- oficialismo %>% 
  filter(date<"2020-01-01")

oposicion_pre <- oposicion %>% 
  filter(date<"2020-01-01")

## 2021 - 2022

oficialismo_post <- oficialismo %>% 
  filter(date>="2020-01-01")

oposicion_post <- oposicion %>% 
  filter(date>="2020-01-01")

# genero dataframe para toda la muestra, para el pre y el post

oposicion_merge <- oposicion %>% 
  mutate(Partido = "Juntos por el Cambio")
oficialismo_merge <- oficialismo %>% 
  mutate(Partido="Frente de Todos")
ambos_partidos <- bind_rows(oposicion_merge, oficialismo_merge)

oposicion_pre_merge <- oposicion_pre %>% 
  mutate(Partido = "Juntos por el Cambio")
oficialismo_pre_merge <- oficialismo_pre %>% 
  mutate(Partido="Frente de Todos")
ambos_partidos_pre <- bind_rows(oposicion_pre_merge, oficialismo_pre_merge)

oposicion_post_merge <- oposicion_post %>% 
  mutate(Partido = "Juntos por el Cambio")
oficialismo_post_merge <- oficialismo_post %>% 
  mutate(Partido="Frente de Todos")
ambos_partidos_post <- bind_rows(oposicion_post_merge, oficialismo_post_merge)

# Topic modeling ------------------------------------------------


# What words were used most often by oficialismo and oposicion?


## Top words

oficialismo_pre %>%
  group_by(word) %>%
  count(sort = TRUE) %>%
  head(10) %>%
  ggplot(aes(x = fct_reorder(word,n), y = n)) +
  geom_col(aes(fill = n)) +
  xlab(NULL) +
  coord_flip() +
  labs(y = 'Cantidad de veces que utilizan la palabra',
       x = "Palabras")+
  geom_text(aes(label = n), size = 4, hjust = 1.5, color = "white") +
  theme(axis.text=element_text(size=12, colour='black'),
        axis.title=element_text(size=14,face="bold"))

oficialismo_post %>%
  group_by(word) %>%
  count(sort = TRUE) %>%
  head(10) %>%
  ggplot(aes(x = fct_reorder(word,n), y = n)) +
  geom_col(aes(fill = n)) +
  xlab('Cantidad de veces que utilizan la palabra') +
  coord_flip() +
  labs(y = 'Cantidad de veces que utilizan la palabra',
       x = "Palabras")+
  geom_text(aes(label = n), size = 4, hjust = 1.5, color = "white") +
  theme(axis.text=element_text(size=12, colour='black'),
        axis.title=element_text(size=14,face="bold"))

oposicion_pre %>%
  group_by(word) %>%
  count(sort = TRUE) %>%
  head(10) %>%
  ggplot(aes(x = fct_reorder(word,n), y = n)) +
  geom_col(aes(fill = n)) +
  xlab(NULL) +
  coord_flip() +
  labs(y = 'Cantidad de veces que utilizan la palabra',
       x = "Palabras")+
  geom_text(aes(label = n), size = 4, hjust = 1.5, color = "white") +
  theme(axis.text=element_text(size=12, colour='black'),
        axis.title=element_text(size=14,face="bold"))

oposicion_post %>%
  group_by(word) %>%
  count(sort = TRUE) %>%
  head(10) %>%
  ggplot(aes(x = fct_reorder(word,n), y = n)) +
  geom_col(aes(fill = n)) +
  xlab(NULL) +
  coord_flip() +
  labs(y = 'Cantidad de veces que utilizan la palabra',
       x = "Palabras")+
  geom_text(aes(label = n), size = 4, hjust = 1.5, color = "white") +
  theme(axis.text=element_text(size=12, colour='black'),
        axis.title=element_text(size=14,face="bold"))


# Wordcloud

oficialismo_pre %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  filter(year>2016) %>% 
  group_by(word) %>%
  count(sort = TRUE) %>%
  head(200) %>% 
  wordcloud2()

oficialismo_post %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  filter(year!=2020) %>%
  group_by(word) %>%
  count(sort = TRUE) %>%
  head(200) %>% 
  wordcloud2()

oposicion_pre %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  filter(year>2016) %>%
  group_by(word) %>%
  count(sort = TRUE) %>%
  head(200) %>% 
  wordcloud2()

oposicion_post %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  filter(year!=2020) %>%
  group_by(word) %>%
  count(sort = TRUE) %>%
  head(200) %>% 
  wordcloud2()

# Análisis de sentimientos ------------------------------------------------

# Diccionarios ------------------------------------------------------------
lexico <- read.csv("https://bitsandbricks.github.io/data/sdal.csv", 
                   stringsAsFactors = FALSE)
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

summary(afinn)

# extracción de sentimientos ----------------------------------------------

oficialismo_afinn <- 
  oficialismo %>%
  unnest_tokens(input = "word", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = usuario)

oposicion_afinn <- 
  oposicion %>%
  unnest_tokens(input = "word", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = usuario)

ambos_partidos_afinn <- 
  ambos_partidos %>%
  unnest_tokens(input = "word", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = usuario) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))


# 2017 - 2019
oficialismo_pre_afinn <- oficialismo_pre %>% 
  unnest_tokens(input = "word", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = usuario) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))

oposicion_pre_afinn <- oposicion_pre %>% 
  unnest_tokens(input = "word", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = usuario) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))

ambos_partidos_pre_afinn <- ambos_partidos_pre %>% 
  unnest_tokens(input = "word", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = usuario) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))

# 2021 - 2022
oficialismo_post_afinn <- oficialismo_post %>% 
  unnest_tokens(input = "word", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = usuario) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))

oposicion_post_afinn <- oposicion_post %>% 
  unnest_tokens(input = "word", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = usuario) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))

ambos_partidos_post_afinn <- ambos_partidos_post %>% 
  unnest_tokens(input = "word", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = usuario) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))



# visualización -----------------------------------------------------------

tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

# Sentimientos positivos y negativos

# 2017 - 2019
ambos_partidos_pre_afinn %>%
  filter(year>2016) %>% 
  count(Partido, Tipo) %>%
  group_by(Partido) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Partido, Proporcion, fill = Tipo, cex.axis = 20) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = "top",
        axis.title = element_text(size=20),
        axis.text = element_text(size=20),
        legend.title=element_text(size=20),
        legend.text = element_text(size=20),
        plot.title=element_text(size=30),
        strip.text=element_text(size=20)) + 
  labs(title = "Período 2017-2019")

# 2021 - 2022
ambos_partidos_post_afinn %>%
  filter(year>2020) %>%
  count(Partido, Tipo) %>%
  group_by(Partido) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Partido, Proporcion, fill = Tipo, cex.axis = 20) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = "top",
        axis.title = element_text(size=20),
        axis.text = element_text(size=20),
        legend.title=element_text(size=20),
        legend.text = element_text(size=20),
        plot.title=element_text(size=30),
        strip.text=element_text(size=20)) +
  labs(title = "Período 2021-2022")

# Tendencias de sentimientos usando las funciones de densidad de las puntuaciones

ambos_partidos_afinn %>% 
  filter(year>2016,
         year<2020) %>% 
  ggplot() +
  aes(Puntuacion, color = Partido) +
  geom_density() +
  facet_wrap(~year) +
  labs( y = 'Densidad', x = 'Valoración Sentimental') +
  theme(axis.text=element_text(size=12, colour='black'),
        axis.title=element_text(size=14),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)) +
  ambos_partidos_afinn %>% 
  filter(year>2020) %>% 
  ggplot() +
  aes(Puntuacion, color = Partido) +
  geom_density() +
  facet_wrap(~year) +
  labs( y = 'Densidad', x = 'Valoración Sentimental') +
  theme(axis.text=element_text(size=12, colour='black'),
        axis.title=element_text(size=14),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        legend.position="none")

# 2017-2019

ambos_partidos_pre_afinn %>%
  filter(Candidato =='Mauricio Macri') %>% 
  filter(year>2016) %>%
  group_by(Tipo) %>%
  count(Palabra, sort = TRUE) %>%
  head(n = 10) %>%
  ggplot(aes(x=fct_reorder(Palabra,n), n, fill = Tipo)) +
  geom_col() +
  coord_flip() + labs(title = "Palabras positivas y negativas de Mauricio Macri") +
  labs(subtitle = "Período 2017-2019") +
  labs(y = 'n',
       x = "")+
  theme(axis.text=element_text(size=12, colour='black'),
        axis.title=element_text(size=14),
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14),
        legend.position="none")

ambos_partidos_pre_afinn %>%
  filter(Candidato =='Cristina Fernandez') %>% 
  filter(year>2016) %>%
  group_by(Tipo) %>%
  count(Palabra, sort = TRUE) %>%
  head(n = 10) %>%
  ggplot(aes(x=fct_reorder(Palabra,n), n, fill = Tipo)) +
  geom_col() +
  xaxp = c(0,75) +
  coord_flip() + labs(title = "Palabras positivas y negativas de Cristina Fernández") +
  labs(subtitle = "Período 2017-2019") +
  labs(y = 'n',
       x = "")+
  theme(axis.text=element_text(size=12, colour='black'),
        axis.title=element_text(size=14),
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14))

#2021-2022


ambos_partidos_post_afinn %>%
    filter(Candidato =='Mauricio Macri') %>% 
    filter(year>2020) %>%
    group_by(Tipo) %>%
    count(Palabra, sort = TRUE) %>%
    head(n = 10) %>%
    ggplot(aes(x=fct_reorder(Palabra,n), n, fill = Tipo)) +
    geom_col() +
    coord_flip() + labs(title = "Palabras positivas y negativas de Mauricio Macri") +
    labs(subtitle = "Período 2021-2022") +
  labs(y = 'n',
       x = "")+
  theme(axis.text=element_text(size=12, colour='black'),
        axis.title=element_text(size=14),
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14),
        legend.position="none")

ambos_partidos_post_afinn %>%
  filter(Candidato =='Cristina Fernandez') %>% 
  filter(year>2020) %>%
  group_by(Tipo) %>%
  count(Palabra, sort = TRUE) %>%
  head(n = 10) %>%
  ggplot(aes(x=fct_reorder(Palabra,n), n, fill = Tipo)) +
  geom_col() +
  coord_flip() + labs(title = "Palabras positivas y negativas de Cristina Fernandez") +
  labs(subtitle = "Período 2021-2022") +
  labs(y = 'n',
       x = "")+
  theme(axis.text=element_text(size=12, colour='black'),
        axis.title=element_text(size=14),
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14))

