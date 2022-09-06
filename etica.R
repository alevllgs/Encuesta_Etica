library(rvest)
library(robotstxt)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(purrr)
library(readr)
library(beepr)
library(pdftools) # trabajo con archivos en pdf.
library(tidytext) # tokenizador enfocado en el trabajo en dataframes
library(tokenizers) # tokenizador
library(SnowballC) # stemming (stem = raíz)
library(udpipe) # etiquetado gramatical y lematización
library(ggplot2)
library(wordcloud)
library(readtext)
library(syuzhet)
library(readxl)

etica <- read_excel("Código de Ética - Hospital Roberto del Río (Respuestas).xlsx")

texto_libre <- etica$`¿Podría dar un ejemplo de un problema ético (comportamiento responsable y comprometido de las personas que se ocupan de los asuntos públicos)  o de falta a la probidad (el ejercicio de la función pública se debe realizada de manera honesta, honrada, primando el interés general sobre el particular)  que haya visto en la institución?`
texto_libre[1:3]
sum(str_count(texto_libre, "concurso"))
write_lines(texto_libre, "encuesta_etica_2022.txt")

# 
unas_stopwords <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")


# Ahora, vamos a tokenizar nuestro texto
otras_stopwords <- tibble(palabra = c("nada", "visto", "ninguno", NA, "as", 
                                      "tengo", "2", "ser", "años", "2",
                                      "nk", "3", "7", "mucho", "ningún", "tb",
                                      "cae", "alguien", "veo", "vs", "44", "50", "doce",
                                      "2012", "muchos", "mismo", "bien", "varios",
                                      "algun", "da", "dar", "ejemplo", "encuesta"))

frecuencias_etica <- tibble(encuesta_etica = texto_libre) %>% 
  unnest_tokens(input = encuesta_etica, 
                output = palabra, 
                strip_numeric = TRUE) %>% 
  count(palabra, sort = TRUE) %>% 
  anti_join(unas_stopwords) %>% 
  anti_join(otras_stopwords)



wordcloud(words = frecuencias_etica$palabra,
          freq = frecuencias_etica$n,
          min.freq = 5,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(6, "Spectral"))

bigrama <- tibble(discurso = texto_libre) %>% 
  unnest_ngrams(input = discurso,
                output = bigrama,
                n = 2) %>% 
  count(bigrama, sort = TRUE) %>% 
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>% 
  filter(!palabra2 %in% unas_stopwords$palabra,
         !palabra1 %in% unas_stopwords$palabra,
         !palabra2 %in% otras_stopwords$palabra,
         !palabra1 %in% otras_stopwords$palabra) %>% 
  unite(col = "bigrama", c(palabra1, palabra2), sep = " ")


#####
tibble(discurso = texto_libre) %>% 
  unnest_tokens(input = discurso, 
                output = palabra, 
                strip_numeric = TRUE) %>% 
  count(palabra, sort = TRUE) %>% 
  filter(palabra == "horas")

tibble(discurso = texto_libre) %>% 
  unnest_tokens(input = discurso, 
                output = palabra, 
                strip_numeric = TRUE) %>% 
  count(palabra, sort = TRUE) %>% 
  filter(str_detect(palabra, regex("ndo$")))


#####
encuesta_etica_2022 <- scan("encuesta_etica_2022.txt", what = "char")

sentimientos_etica <- get_nrc_sentiment(encuesta_etica_2022, language = "spanish")

sentimientos_etica_larga <- sentimientos_etica %>% 
  pivot_longer(anger:positive, names_to = "sentimiento", values_to = "frecuencia")

sentimientos_etica_larga %>% 
  filter(!sentimiento %in% c("negative", "positive")) %>% 
  ggplot(aes(reorder(sentimiento, -frecuencia), frecuencia)) +
  geom_col(fill = "tomato") +
  theme_minimal()


ggsave("figuras/columnas_sentimientos_etica.jpeg", height = 7, width = 10)


polaridad_boric <- boric %>% 
  get_sentences() %>% 
  get_sentiment(method = "nrc", language = "spanish")

head(polaridad_boric, n  = 20)


tibble(indice = 1:length(polaridad_boric), polaridad = polaridad_boric) %>% 
  ggplot(aes(indice, polaridad)) +
  geom_line()


polaridad_boric <- get_dct_transform(polaridad_boric)

tibble(indice = 1:length(polaridad_boric), polaridad = polaridad_boric) %>% 
  ggplot(aes(indice, polaridad)) +
  geom_line()

ggsave("figuras/polaridad.jpeg", height = 7, width = 10)

#2

sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt") %>% 
  filter(lexicon == "nrc") %>% 
  select(-valor)

write_rds(sentimientos, "sentimientos.rds")

###### agrupacion 
bigrama <- bigrama %>% mutate("Categoría agrupada" = case_when(
  #00102 Medico Especialista
  bigrama == "horas extras"~"horas extras",
  bigrama == "horas extraordinarias"~"horas extras",
  bigrama == "cobrar horas"~"horas extras",
  
  bigrama == "concurso publico"~"concursos públicos",
  bigrama == "concurso públicos"~"concursos públicos",
  bigrama == "públicos arreglados"~"concursos públicos",
  bigrama == "cargos arreglados"~"concursos públicos",
  bigrama == "concursos abiertos"~"concursos públicos",
  bigrama == "concursos públicos"~"concursos públicos",
  bigrama == "concursos publicos"~"concursos públicos",
  bigrama == "van dirigidos"~"concursos públicos",
  bigrama == "subir grados"~"concursos públicos",
  bigrama == "cumplir requisitos"~"concursos públicos",
  bigrama == "cargos dirigidos"~"concursos públicos",
  
  
  bigrama == "acoso laboral"~"maltrato laboral",
  bigrama == "malos tratos"~"maltrato laboral",
  bigrama == "maltrato laboral"~"maltrato laboral",
  bigrama == "tratos inadecuados"~"maltrato laboral",
  bigrama == "buen trato"~"maltrato laboral",
  bigrama == "buena convivencia"~"maltrato laboral",
  bigrama == "buena cordialidad"~"maltrato laboral",
  bigrama == "buena onda"~"maltrato laboral",
  bigrama == "burlas frente"~"maltrato laboral",
  bigrama == "aceptar presiones"~"maltrato laboral",
  bigrama == "haciendo burlas"~"maltrato laboral",
  
  
  bigrama == "licencia medica"~"licencias médicas",
  bigrama == "licencia menor"~"licencias médicas",
  bigrama == "licencias anunciadas"~"licencias médicas",
  bigrama == "licencias falsas"~"licencias médicas",
  bigrama == "licencias medicas"~"licencias médicas",
  bigrama == "livencia medica"~"licencias médicas",
  bigrama == "licencias médicas"~"licencias médicas",
  
  bigrama == "recursos públicos"~"uso de recursos públicos",
  bigrama == "administrativos gasto"~"uso de recursos públicos",
  bigrama == "buen uso"~"uso de recursos públicos",
  bigrama == "mal uso"~"uso de recursos públicos",
  bigrama == "bienes innecesarios"~"uso de recursos públicos",
  
  
  bigrama == "horario laboral"~"horario laboral",
  bigrama == "jornada laboral"~"horario laboral",
  bigrama == "cumplen horario"~"horario laboral",
  bigrama == "cumplir horario"~"horario laboral",
  
  
  TRUE ~ ""))

##### Valores
valores <- etica$`De esta lista de valores, ¿ cuales cree usted que deberían representar al hospital? Marque 5`



valores_df <- strsplit(valores, ",")

lista_valores <- data.frame(Valores = "valor")

for (i in 1:300) {
  a <- data.frame(valores_df[i])
  colnames(a)[1] <- "Valores"
  lista_valores <- rbind(lista_valores, a)
}

lista_valores <- lista_valores %>% filter(Valores != "valor")



##### GRABA
openxlsx::write.xlsx(bigrama, file = "bigrama.xlsx", colNames = TRUE, overwrite = TRUE)
