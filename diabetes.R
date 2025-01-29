# Instalar y cargar las librerías necesarias
install.packages("e1071")  # Paquete para Naive Bayes
install.packages("ggplot2")  # Paquete para gráficos
install.packages("dplyr")  # Para manipulación de datos

library(e1071)  # Cargar el paquete para Naive Bayes
library(ggplot2)  # Cargar el paquete para gráficos
library(dplyr)  # Para manipulación de datos

# Cargar el conjunto de datos Pima Indians Diabetes
url <- "https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.data.csv"
column_names <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", 
                  "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")

# Leer los datos
data <- read.csv(url, header = FALSE, col.names = column_names)

# Convertir la variable 'Outcome' a factor (etiquetas de clase)
data$Outcome <- factor(data$Outcome, levels = c(0, 1), labels = c("No Diabetes", "Diabetes"))

# Ver las primeras filas del conjunto de datos
head(data)

# Dividir los datos en entrenamiento (70%) y prueba (30%)
set.seed(1234)
train_indices <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Crear la matriz de entrenamiento y las etiquetas de clase
sms_dtm_freq_train <- train_data[, -9]  # Eliminar la columna de la etiqueta (Outcome)
sms_train_labels <- train_data$Outcome

# Entrenar el modelo Naive Bayes
model <- naiveBayes(sms_dtm_freq_train, sms_train_labels)

# Realizar predicciones sobre el conjunto de prueba
predictions <- predict(model, test_data[, -9])  # Predicción sin la columna 'Outcome'

# Ver las predicciones
head(predictions)

# Evaluar el rendimiento usando una matriz de confusión
confusion_matrix <- table(Predicted = predictions, Actual = test_data$Outcome)
print(confusion_matrix)

# Calcular la exactitud (accuracy) del modelo
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", round(accuracy, 2)))

# Gráficos para visualizar los grupos de datos

# Gráfico de dispersión de Glucose vs BMI, coloreado por Outcome
ggplot(data, aes(x = Glucose, y = BMI, color = Outcome)) +
  geom_point() +
  labs(title = "Diabetes: Glucose vs BMI", x = "Glucose", y = "BMI") +
  theme_minimal()

# Gráfico de dispersión de Age vs Glucose, coloreado por Outcome
ggplot(data, aes(x = Age, y = Glucose, color = Outcome)) +
  geom_point() +
  labs(title = "Diabetes: Age vs Glucose", x = "Age", y = "Glucose") +
  theme_minimal()

# Diagrama de caja para Glucose, separado por Outcome
ggplot(data, aes(x = Outcome, y = Glucose, fill = Outcome)) +
  geom_boxplot() +
  labs(title = "Diabetes: Glucose Distribution by Outcome", x = "Outcome", y = "Glucose") +
  theme_minimal()

# Diagrama de caja para BMI, separado por Outcome
ggplot(data, aes(x = Outcome, y = BMI, fill = Outcome)) +
  geom_boxplot() +
  labs(title = "Diabetes: BMI Distribution by Outcome", x = "Outcome", y = "BMI") +
  theme_minimal()
