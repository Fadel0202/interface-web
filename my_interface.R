# Installer et charger les packages nécessaires
#install.packages(c("shiny", "ggplot2", "caret", "randomForest", "e1071", "readxl"))

library(shiny)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)
library(readxl)

# Définir l'interface Shiny
ui <- fluidPage(
  titlePanel("Exploration et Modélisation de Données"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choisir un fichier",
                accept = c(".csv", ".tsv", ".xlsx", ".xls", ".txt")),
      checkboxInput("outliers", "Identifier les outliers"),
      checkboxInput("missing", "Traiter les valeurs manquantes"),
      checkboxInput("normalize", "Normaliser les variables"),
      checkboxInput("dummy", "Dummifier les variables catégorielles"),
      checkboxInput("balance", "Équilibrer les classes")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Analyse Exploratoire",
                 plotOutput("univariate_plot"),
                 plotOutput("bivariate_plot")),
        tabPanel("Entraînement des Modèles",
                 selectInput("model1", "Modèle 1", choices = c("Random Forest", "SVM")),
                 selectInput("model2", "Modèle 2", choices = c("Logistic Regression", "k-NN")),
                 plotOutput("model_comparison"),
                 verbatimTextOutput("model_results"),
                 plotOutput("important_features")
        )
      )
    )
  )
)

# Définir le serveur Shiny
server <- function(input, output) {
  
  # Charger les données
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    read_data <- switch(ext,
                        csv = read.csv,
                        tsv = read.table,
                        txt = read.table,
                        xls = readxl::read_excel,
                        xlsx = readxl::read_excel,
                        stop("Format de fichier non pris en charge."))
    read_data(input$file$datapath, header = TRUE)
  })
  
  # Analyse exploratoire
  output$univariate_plot <- renderPlot({
    # Exemple d'analyse unidimensionnelle (histogramme)
    ggplot(data(), aes(x = data()[, 1])) + geom_histogram()
  })
  
  output$bivariate_plot <- renderPlot({
    # Exemple d'analyse bidimensionnelle (scatter plot)
    ggplot(data(), aes(x = data()[, 1], y = data()[, 2])) + geom_point()
  })
  
  # Entraînement des modèles
  output$model_comparison <- renderPlot({
    # Exemple d'entraînement des modèles et de comparaison
    set.seed(123)
    
    if (input$model1 == "Random Forest") {
      model1 <- randomForest(data()[, 1] ~ ., data = data())
    } else {
      model1 <- svm(data()[, 1] ~ ., data = data())
    }
    
    if (input$model2 == "Random Forest") {
      model2 <- randomForest(data()[, 1] ~ ., data = data())
    } else {
      model2 <- svm(data()[, 1] ~ ., data = data())
    }
    
    # Comparaison des modèles
    par(mfrow = c(1, 2))
    plot(model1)
    plot(model2)
  })
  
  output$model_results <- renderPrint({
    # Exemple d'affichage des résultats
    predictions1 <- predict(input$model1, newdata = data())
    predictions2 <- predict(input$model2, newdata = data())
    
    cm1 <- confusionMatrix(predictions1, data()[, 1])
    cm2 <- confusionMatrix(predictions2, data()[, 1])
    
    paste("Modèle 1:", cm1$overall["Accuracy"],
          "\nModèle 2:", cm2$overall["Accuracy"])
  })
  
  output$important_features <- renderPlot({
    # Exemple d'identification et d'affichage des features les plus importants
    if (input$model1 == "Random Forest") {
      importance <- importance(input$model1)
      varImpPlot(importance)
    } else {
      # Fournissez une alternative ou adaptez selon le modèle utilisé
      plot(NULL)
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
