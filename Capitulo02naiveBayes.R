library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(e1071)
library(caret)

#Recogida de datos
# Asignamos a la variable 'temp' el archivo en cuestión
archivo = "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/sms_spam.csv"
file <- download.file(archivo, destfile = "sms_spam.csv")
# Utilizamos la función 'unz' para extraer el archivo CSV y lo asignamos a la variable 'temp'
data <- read.csv("sms_spam.csv", stringsAsFactors = F)
data$type <- factor(data$type)

#Limpieza y normalizacion de los datos
sms_text <- VCorpus(VectorSource(data$text))

#Limpieza de texto
sms_text_clean_1 <- tm_map(sms_text, content_transformer(tolower))
sms_text_clean_2 <- tm_map(sms_text_clean_1, removeNumbers)
sms_text_clean_3 <- tm_map(sms_text_clean_2, removeWords, stopwords())

# Para evitar el problema de unir palabras, haremos una función que reemplace los signos de puntuación por espacios, en lugar de eliminarlos
sms_text_clean_4 <- tm_map(sms_text_clean_3, content_transformer(function(x) { gsub("[[:punct:]]+", " ", x) }))

sms_text_clean_5 <- tm_map(sms_text_clean_4, stemDocument)
sms_text_clean_6 <- tm_map(sms_text_clean_5, stripWhitespace)

sms_dtm <- DocumentTermMatrix(sms_text_clean_6)

#Creacion de datasets de entrenamiento y prueba
sms_dtm_train <- sms_dtm[1:4169, ] 
sms_dtm_test <- sms_dtm[4170:5574, ]

sms_train_labels <- data[1:4169, ]$type 
sms_test_labels <- data[4170:5574, ]$type

#dividir datos proporcionalmente
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#visualizar datos en nubes de palabras
wordcloud(sms_text_clean_6, min.freq = 100, random.order = FALSE)

#Crear dos subsets 
spam <- filter(data, data$type == "spam")
ham <- filter(data, data$type == "ham")

wordcloud(spam$text, min.freq = 50, random.order = FALSE)
wordcloud(ham$text, min.freq = 100, random.order = FALSE)

#Reduccion de caracteristicas
#Eliminar palabras que aparezcan en menos de 7 mensajes
sms_freq_words <- findFreqTerms(sms_dtm_train, 7)

#reducir el numero de palabras relativas 
sms_dtm_freq_test <- sms_dtm_test[, sms_freq_words]
sms_dtm_freq_train <- sms_dtm_train[, sms_freq_words]

#variable categorica si aparece o no 
convert_counts <- function(x){x <- ifelse(x > 0, "yes", "no")}

sms_dtm_freq_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)
sms_dtm_freq_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)

#Entrenamiento del modelo
sms_classifier <- naiveBayes(sms_dtm_freq_train, sms_train_labels)

#evaluacion del modelo
prediccion_test <- predict(sms_classifier, sms_dtm_freq_test)
confusionMatrix(data = prediccion_test, reference = sms_test_labels)

#mejorar el modelo con laplace = 1
sms_classifier2 <- naiveBayes(sms_dtm_freq_train, sms_train_labels, laplace = 1)
prediccion_test2 <- predict(sms_classifier2, sms_dtm_freq_test)
confusionMatrix(data = prediccion_test2, reference = sms_test_labels)

#Como resultado obtenemos que no ha variado el número de falsos negativos (6), que era el número que nos interesaba reducir: evitar que un mensaje sea clasificado como spam cuando en realidad es ham. Sin embargo, sí ha aumentado el número de falsos positivos.