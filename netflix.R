# Instalación y carga de librerías necesarias
install.packages("e1071")  # Paquete para Naive Bayes
install.packages("ggplot2")  # Para visualización de gráficos
install.packages("dplyr")  # Para manipulación de datos
install.packages("tm")  # Para preprocesamiento de texto (si es necesario)
library(e1071)
library(ggplot2)
library(dplyr)

# Paso 1: Cargar el conjunto de datos (asegúrate de usar la ruta correcta de tu archivo CSV)
netflix_data <- read.csv("path_to_netflix_data.csv")

# Ver las primeras filas del conjunto de datos para obtener una idea general
head(netflix_data)

# Paso 2: Preprocesamiento de los datos
# Convertir 'genre' a una variable binaria para el modelo (Comedia vs No-Comedia)
netflix_data$genre_comedy <- ifelse(grepl("Comedy", netflix_data$genre), "Comedy", "Non-Comedy")

# Convertir la variable 'genre_comedy' a factor para usarla en Naive Bayes
netflix_data$genre_comedy <- as.factor(netflix_data$genre_comedy)

# Verificar la distribución de los géneros
table(netflix_data$genre_comedy)

# Preprocesamiento: Convertir 'duration' y 'rating' a numéricos
netflix_data$duration <- as.numeric(as.character(netflix_data$duration))
netflix_data$rating <- as.numeric(as.character(netflix_data$rating))

# Eliminar filas con valores NA en las columnas críticas
netflix_data <- netflix_data %>%
  filter(!is.na(genre_comedy), !is.na(duration), !is.na(rating))

# Verificar los datos después de la transformación
head(netflix_data)

# Paso 3: Visualización de la distribución de géneros
ggplot(netflix_data, aes(x = genre_comedy)) +
  geom_bar(fill = c("lightblue", "salmon")) +
  labs(title = "Distribución de Películas/Programas por Género", x = "Género", y = "Frecuencia")

# Paso 4: División del conjunto de datos en entrenamiento y prueba (80% entrenamiento, 20% prueba)
set.seed(123)  # Para reproducibilidad
train_index <- sample(1:nrow(netflix_data), 0.8 * nrow(netflix_data))
train_data <- netflix_data[train_index, ]
test_data <- netflix_data[-train_index, ]

# Paso 5: Entrenamiento del modelo Naive Bayes
naive_bayes_model <- naiveBayes(genre_comedy ~ duration + rating, data = train_data)

# Ver los parámetros del modelo entrenado
naive_bayes_model

# Paso 6: Predicciones en el conjunto de prueba
predictions <- predict(naive_bayes_model, test_data)

# Paso 7: Evaluación del modelo con matriz de confusión
confusion_matrix <- table(Predicted = predictions, Actual = test_data$genre_comedy)
confusion_matrix

# Calcular la precisión del modelo
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

# Paso 8: Visualización de las predicciones vs. la realidad (comedia vs no comedia)
ggplot(data = test_data, aes(x = genre_comedy, fill = predictions)) +
  geom_bar(position = "dodge") +
  labs(title = "Predicciones vs. Género Real", x = "Género Real", y = "Frecuencia")

# Mostrar algunos resultados
cat("Precisión del modelo Naive Bayes:", accuracy, "\n")
