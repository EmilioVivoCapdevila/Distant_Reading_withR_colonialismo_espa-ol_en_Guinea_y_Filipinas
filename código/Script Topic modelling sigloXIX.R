# Recuerda que solo las tienes que instalar la primera vez.
# install.packages("tm")
# install.packages("topicmodels")
# install.packages("scales")

library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(scales)

# Para reproducir los resultados sin recurrir al dataset tan solo es necesario descargar el corpus y sustituir las urls.

setwd("C:/Users/EVivoCapdevila/Desktop/Antwerpen-Valencia/Artículos/Topic modelling/Distant_reading_sigloXIX")

#Vacías original de Fradejas (2019)
#vacias <- read.csv("https://tinyurl.com/7PartidasVacias",                locale = default_locale())
#MisVacias incluye algunos verbos dicendi a las palabras palabras gramaticales de Fradejas.
# Para reproducir los resultados sin recurrir al dataset tan solo es necesario descargar el corpus y sustituir las urls.

vacias <-  read.csv("C:/Users/EVivoCapdevila/Desktop/Antwerpen-Valencia/Artículos/Topic modelling/Distant_reading_sigloXIX/datos/MisVacias.txt", encoding ="UTF-8")

# Después de una serie de experimentos, se ha tenido que adaptar la lista de vacías a la proveniencia de los textos.
# El corpus ha sido extraido de las colecciones de HathiTrust Filipinas colonial (https://babel.hathitrust.org/cgi/mb?a=listis&c=424008362); y  Decimonónico Guinea 1850-1899 15.4.21 (https://babel.hathitrust.org/cgi/mb?a=listis&c=1646924889). 
# Se trata de un corpus oportúnistico (Tello, 2019).
# Los textos se han descargado en ebook y transformado en txt usando calibre.
# Por ello, he incluido en la lista de vacías partes del encabezamiento/cierre de HT que un modelo de LDA (Experimento n1) coaparecían.
# Tambien he incluido números, para evitar la paginación y otras coapareciones no significativas en el LDA.


#Las siguientes líneas, adaptadas de S. Rebora (https://github.com/SimoneRebora/) permiten incorporar fácilmente grandes corpus de forma automatizada al código.
# Por  comodidad, incluyo dos líneas de código con referencia a las diferentes carpetas con el corpus. Gracias a este comando, sobra con modificar la url de un corpus u otro (igual que en este ejemplo).
# Para reproducir los resultados sin recurrir al dataset tan solo es necesario descargar el corpus y sustituir las urls.

titulos <- list.files("C:/Users/EVivoCapdevila/Desktop/Antwerpen-Valencia/Artículos/Topic modelling/Distant_reading_sigloXIX/Corpus XIX/Guinea")
#titulos <- list.files("C:/Users/EVivoCapdevila/Desktop/Antwerpen-Valencia/Artículos/Topic modelling/Distant_reading_sigloXIX/Corpus XIX/Filipinas")

titulos <- gsub(".txt", "", titulos)
titulos <- gsub("_", "", titulos)

titulos

# Para reproducir los resultados sin recurrir al dataset tan solo es necesario descargar el corpus y sustituir las urls.

ficheros <- list.files("C:/Users/EVivoCapdevila/Desktop/Antwerpen-Valencia/Artículos/Topic modelling/Distant_reading_sigloXIX/Corpus XIX/Guinea", full.names = T)
#ficheros <- list.files("C:/Users/EVivoCapdevila/Desktop/Antwerpen-Valencia/Artículos/Topic modelling/Distant_reading_sigloXIX/Corpus XIX/Filipinas", full.names = T)

ficheros

ensayos <- tibble(texto = character(),
                  titulo = character(),
                  pagina = numeric())

for (j in 1:length(ficheros)){
  texto.entrada <- read_lines(paste(ficheros[j],
                                    sep = ""),
                              locale = default_locale())
  texto.todo <- paste(texto.entrada, collapse = " ")
  por.palabras <- strsplit(texto.todo, " ")
  texto.palabras <- por.palabras[[1]]
  trozos <- split(texto.palabras,
                  ceiling(seq_along(texto.palabras)/375))
  for (i in 1:length(trozos)){
    fragmento <- trozos[i]
    fragmento.unido <- tibble(texto = paste(unlist(fragmento),
                                            collapse = " "),
                              titulo = titulos[j],
                              pagina = i)
    ensayos <- bind_rows(ensayos, fragmento.unido)
  }
}

#{38690/375; floor(38690/375), ceiling(38690/375),length(ficheros)}


#length(ficheros)


# rm(ficheros, titulos, trozos, fragmento,
#    fragmento.unido, ruta, texto.entrada,
#    texto.palabras, texto.todo, por.palabras, i, j)

por_pagina_palabras <- ensayos %>%
  unite(titulo_pagina, titulo, pagina) %>%
  unnest_tokens(palabra, texto)
por_pagina_palabras %>%
  count(palabra, sort = T)

palabra_conteo <- por_pagina_palabras %>%
  anti_join(vacias) %>% 
  count(titulo_pagina, palabra, sort = TRUE) %>%
  ungroup()

unique(palabra_conteo$palabra)
vacias$palabra[1] %in% palabra_conteo$palabra
which(vacias$palabra %in% palabra_conteo$palabra)

palabra_conteo
#warnings()

paginas_dtm <- palabra_conteo %>%
  cast_dtm(titulo_pagina, palabra, n)

paginas_dtm



# Modificando las siguientes líneas determinamos el número de tópicos. Mimmo et al (2011) señalan que "the more k and top the better model". Navarro-Colorado, B. (2018:5) usa 10, 25, 50, 100, y 250, pero luego esto ha de interpretarse (Tangherlini), no siempre de forma autoevidente.
# Hasta 15 tópicos se puede visualizar bien en papel, aunque con la versión simplificada de gamma estop no debería ser un problema.
# En general, he observado que cuanto más se acerca el número de k al número de obras, más cerca estamos de ver los tópicos (palabras que más co-ocurren) en una obra.
# Si k = nº de items en el corpus podemos hacer una "lectura distante" de una colección.
# Por otro lado, si k es mucho mayor muchos de los tópicos pueden ser redundantes. 
# En ese sentido, creo que es mejor optar por menos tópicos pero atender a más palabras por tópico (top_n)

paginas_lda <- LDA(paginas_dtm, k = 15, control = list(seed = 1234))
paginas_lda
paginas_lda_td <- tidy(paginas_lda, matrix = "beta")
paginas_lda_td
#top_n define la cantidad de palabras por tópico que nos muestra las diferentes formas de extrar los resultados.
#Todas las palabras forman parte de todos los tópicos, pero aquellas están ordenadas por frecuencia de coaparición.
#Aunque lo ideal es 5, para colecciones menores (con por ejemplo gran peso de personajes literarios) puede ser más "significativo" analizar hasta 20.
# top_n = 10 aparece bastante en la bibliografía, por ejemplo en Blei (2012), Newman (2010), Mimmo (2012), pero estos emplean corpus mucho más grandes.
#Fradejas (epistolar) recuerda el problema enorme del cherry-picking a la hora de emplear un LDA. Esto es especialmente fuerte en el caso de los diferentes valores de top_n. 
# Aunque seguimos el modelo de Navarro-Colorado (2018: https://github.com/bncolorado/OnPoeticTopicModeling_Data/blob/master/keys-topics100_filtrado.txt), una mirada a los resultados reales (no en articulo) muestran que de hecho emplea un numero de tp_n mucho mas alto que el que enseña.
# Un Top_n elevado para el análisis puede no corresponderse con la visualización.
#A partir de aquí hay que prbar la interpretabilidad de los resultados y la agresividad del cherry-picking.
options(scipen=999)
terminos_frecuentes <- paginas_lda_td %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
terminos_frecuentes

terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

##### ----------
#####

### Wordcloud adaptado de Rebora (https://github.com/SimoneRebora/)
# ggplot library
library(ggwordcloud)

# extrar la información del LDA
p1 <- terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(label = term, size = beta)) +
  geom_text_wordcloud_area() +
  # scale_size_area(max_size = 20) nos permite de nuevo elegir el valor de top_n para la visualización (que no para el dataset/txt en el qe se base la interpretación)
  scale_size_area(max_size = 20) +
  theme_minimal() +
  facet_wrap(~topic)

# mostrar la nube de palabras
p1

# guardarla
ggsave(p1, filename = "Topics_wordcloudGuineaXIX15.png", scale = 3.5)

##### ----------

paginas_lda_gamma <- tidy(paginas_lda, matrix = "gamma")
paginas_lda_gamma

paginas_lda_gamma <- paginas_lda_gamma %>%
  separate(document,
           c("titulo", "pagina"),
           sep = "_", convert = TRUE)
paginas_lda_gamma

##### ----------
##### heatmap visualization (asignación de tópicos pos libros/segmentos)

library(maditr)
paginas_lda_gamma_copy <- paginas_lda_gamma
paginas_lda_gamma_copy$titulo <- paste(paginas_lda_gamma_copy$titulo, paginas_lda_gamma_copy$pagina, sep = "_")
paginas_lda_gamma_copy$pagina <- NULL
paginas_lda_gamma_copy <- dcast(data = paginas_lda_gamma_copy,
                                formula = titulo~topic,
                                fun.aggregate = sum,
                                value.var = "gamma")

my_rownames <- paginas_lda_gamma_copy$titulo
paginas_lda_gamma_copy$titulo <- NULL
paginas_lda_gamma_copy <- as.matrix(paginas_lda_gamma_copy)
rownames(paginas_lda_gamma_copy) <- my_rownames

# visualizar y grabar la visualización
png(filename = "GuineaXIX15heatmap.png", width = 4000, height = 4000)
heatmap(paginas_lda_gamma_copy, margins = c(25,25), cexRow = 2, cexCol = 2)
dev.off()


# mapa simplificado (suma de gamma values per titulo/topic)
paginas_lda_gamma_copy <- paginas_lda_gamma

paginas_lda_gamma_copy <- paginas_lda_gamma_copy %>%
  group_by(titulo, topic) %>%
  summarise(gamma = sum(gamma))

paginas_lda_gamma_copy <- dcast(data = paginas_lda_gamma_copy,
                                formula = titulo~topic,
                                fun.aggregate = sum,
                                value.var = "gamma")

my_rownames <- paginas_lda_gamma_copy$titulo
paginas_lda_gamma_copy$titulo <- NULL
paginas_lda_gamma_copy <- as.matrix(paginas_lda_gamma_copy)
rownames(paginas_lda_gamma_copy) <- my_rownames

# visualiza el heatmap simplificado y guárdalo:
png(filename = "GuineaXIX15heatmap_2.png", width = 1000, height = 1000)
heatmap(paginas_lda_gamma_copy, margins = c(15,15), cexRow = 1, cexCol = 1)
dev.off()

##### ----------


#gráfico alternativo por Fradejas (2019)
ggplot(paginas_lda_gamma, aes(gamma, fill = factor(topic))) +
  geom_histogram() +
  facet_wrap(~ titulo, nrow = 2)



# Este gráfico de Fradejas (2019) permite revisar qque el programa reasigna correctamente los datos (margen de error) a su fuente, pero no es muy útil.
paginas_clasificaciones <- paginas_lda_gamma %>%
  group_by(titulo, pagina) %>%
  top_n(4, gamma) %>%
  ungroup() %>%
  arrange(gamma)

paginas_clasificaciones

topico_texto <- paginas_clasificaciones %>%
  count(titulo, topic) %>%
  group_by(titulo) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consenso = titulo, topic)

topico_texto

paginas_clasificaciones %>%
  inner_join(topico_texto, by = "topic") %>%
  filter(titulo != consenso)

asignaciones <- augment(paginas_lda, data = paginas_dtm)

asignaciones

asignaciones <- asignaciones %>%
  separate(document, c("titulo",
                       "pagina"),
           convert = TRUE) %>%
  inner_join(topico_texto,
             by = c(".topic" = "topic"))

asignaciones

asignaciones %>%
  count(titulo, consenso, wt = count) %>%
  group_by(titulo) %>%
  mutate(porcentaje = n / sum(n)) %>%
  ggplot(aes(consenso, titulo, fill = porcentaje)) +
  geom_tile() +
  scale_fill_gradient2(high = "blue", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Asignó las palabras a: ",
       y = "Las palabras procedían de: ",
       fill = "% de asignaciones")

palabras_equivocadas <- asignaciones %>%
  filter(titulo != consenso)

palabras_equivocadas

palabras_equivocadas %>%
  count(titulo, consenso, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

#Este añadido es de Fradejas, en comunicación personal.
# Con esta línea sacamos y guardamos la lista de tópicos en un documento txt, que fácilmente podemos reordenar y anotar.

resultado <- terminos_frecuentes %>%
  group_by(topic) %>%
  count(term) 
write_tsv(resultado,"ResultadosLDAGuineaXIX15.txt")

