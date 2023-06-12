


################################################################################################################################################

# Carga las librerías necesarias

################################################################################################################################################
  
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(webshot)
library(htmlwidgets)
library(DT)
library(e1071)
library(corrplot)
library(skimr)
library(stats)
library(ggcorrplot)


webshot::install_phantomjs() # Necesitas ejecutar esta línea solo una vez.

################################################################################################################################################


################################################################################################################################################
  
# Define la interfaz de usuario

################################################################################################################################################

ui <- fluidPage(

# Define el título de la aplicación
  
  navbarPage("Análisis de datos",
             theme = shinytheme("flatly"),
             tabPanel("Carga de Datos",
                      box(status = "success", solidHeader = TRUE, width = 12,
                        h4(strong("Carga de Datos")),
                        p("En esta pestaña puedes cargar los datos desde un archivo CSV y realizar la configuración correspondiente."),
                        br(),
                        fileInput("file", "Seleccione un archivo CSV:",
                                  accept = c("text/csv", 
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                        actionButton("load", "Cargar datos"),
                        br(),
                        br(),
                        div(
                          style = "text-align: center;",
                          img(src = "https://www.dropbox.com/s/q4o8ipmad0hgzcu/JMA_nofondo.png?raw=1", height = "100px"),
                          p(strong("Desarrollado por José María Arroyo")),
                          a(href = "mailto:arrocar@gmail.com", "Correo electrónico: arrocar@gmail.com")
                        ))
              ),
             
             tabPanel("Resumen de Datos",
                      h4(strong("Resumen de Datos")),
                      p("En esta pestaña se muestra un resumen de los datos cargados, incluyendo información sobre las variables numéricas y no numéricas."),
                      br(),
                      h4(strong("Listado de variables numéricas:")),
                      verbatimTextOutput("numeric_vars"),
                      br(),
                      h4(strong("Listado de variables no numéricas:")),
                      verbatimTextOutput("non_numeric_vars"),
                      br(),
                      h4(strong("Tabla de datos:")),
                      dataTableOutput("table")
              ),
             
             tabPanel("Análisis de Datos",
                  selectInput("selected_variable", "Variable:", choices = NULL),
                  br(),
                  h4(strong("Estructura de la variable")),
                  verbatimTextOutput("variable_str"),
                  br(),
                  h4(strong("Resumen de la variable")),
                  verbatimTextOutput("variable_summary"),
                  br(),
                  h4(strong("Resumen detallado de la variable")),
                  verbatimTextOutput("variable_skim")
             ),
            
             tabPanel("Gráficos importantes",
                      h4(strong("Gráficos")),
                      p("En esta pestaña se generan gráficos para explorar las variables numéricas y no numéricas del conjunto de datos."),
                      br(),
                      tabsetPanel(
                        tabPanel("Variables numéricas",
                                 br(),
                                 selectInput("numeric_variable", "Seleccione una variable:", NULL),
                                 plotOutput("histogram"),
                                 plotOutput("boxplot")),
                        tabPanel("Variables no numéricas",
                                 br(),
                                 selectInput("non_numeric_variable", "Seleccione una variable:", NULL),
                                 plotOutput("barchart"),
                                 plotOutput("piechart")))
              ),
             
             tabPanel("Medidas Estadísticas",
                      h4(strong("Medidas Estadísticas")),
                      p("En esta pestaña se calculan y muestran diferentes medidas estadísticas para analizar las características del conjunto de datos."),
                      br(),
                      tabsetPanel(
                        tabPanel("Medidas de tendencia central",
                                 br(),
                                 h5(strong("Media")),
                                 p("La media es el promedio de todos los valores en una variable numérica. Representa el valor central de la distribución de los datos."),
                                 h5(strong("Mediana")),
                                 p("La mediana es el valor que se encuentra en el medio de una distribución de datos ordenados de menor a mayor. Es útil para describir la tendencia central en datos sesgados."),
                                 h5(strong("Moda")),
                                 p("La moda es el valor que aparece con mayor frecuencia en un conjunto de datos."),
                                 br(),
                                 selectInput("central_tendency", "Seleccione una variable:", NULL),
                                 verbatimTextOutput("central_tendency_output")),
                        tabPanel("Medidas de dispersión",
                                 br(),
                                 h5(strong("Varianza")),
                                 p("La varianza es la esperanza del cuadrado de la desviación estándar de una variable respecto a su media."),
                                 h5(strong("Desviación Estándar")),
                                 p("La desviación estándar muestra cuánto varían los datos con respecto a la media. Indica la cantidad de dispersión o propagación de los datos."),
                                 h5(strong("Rango")),
                                 p("El rango es la diferencia entre el valor máximo y el valor mínimo en una variable numérica. Proporciona una medida de la amplitud total de los datos."),
                                 h5(strong("IQR")),
                                 p("El IQR o rango intercuartílico es la diferencia entre el tercer y el primer cuartil de una distribución. Es una medida de la dispersión estadística."),
                                 br(),
                                 selectInput("dispersion", "Seleccione una variable:", NULL),
                                 verbatimTextOutput("dispersion_output")),
                        tabPanel("Medidas de asimetría",
                                 br(),
                                 h5(strong("Asimetría")),
                                 p("La asimetría es una medida de la simetría de la distribución de los datos. Puede ser positiva (sesgo a la derecha), negativa (sesgo a la izquierda) o cero (simetría)."),
                                 br(),
                                 selectInput("asymmetry", "Seleccione una variable:", NULL),
                                 verbatimTextOutput("asimmetry_output")))
              ),
             
             tabPanel("Matriz de Correlación",
                      h4(strong("Matriz de Correlación")),
                      p("En esta pestaña se calcula y muestra la matriz de correlación entre las variables numéricas."),
                      p("La correlación es una medida estadística que indica la relación entre dos variables. Puede variar entre -1 y 1. Un valor cercano a 1 indica una correlación positiva, un valor cercano a -1 indica una correlación negativa y un valor cercano a 0 indica una correlación débil o nula."),
                      br(),
                      selectInput("correlation_var1", "Seleccione la primera variable:", NULL),
                      selectInput("correlation_var2", "Seleccione la segunda variable:", NULL),
                      plotOutput("correlation_plot", height = "800px"),
                      tags$style(type="text/css", "#correlation_plot {height: 800px;}")
              ),
             
             tabPanel("Análisis de regresión",
                      h4(strong("Análisis de regresión")),
                      p("Seleccione las variables numéricas para el análisis de regresión."),
                      selectInput("independent_variables", "Variables independientes (X):", choices = "", multiple = TRUE),
                      selectInput("dependent_variable", "Variable dependiente (Y):", choices = ""),
                      h4("Resumen de la regresión"),
                      verbatimTextOutput("regression_summary")
             ),
                 
  )
)

################################################################################################################################################
  
# Define el servidor de la aplicación

################################################################################################################################################

server <- function(input, output, session) {
  
  # Define la función para cargar los datos
  load_data <- function(file) {
    if (is.null(file)) {
      return(NULL)
    }
    data <- read.csv(file$datapath, header = TRUE, sep = ",")
    return(data)
  }
  
  # Datos cargados
  data <- reactiveVal(NULL)
  
  # Cargar datos al subir el archivo
  observeEvent(input$file, {
    data(load_data(input$file))
    showNotification("Archivo CSV cargado correctamente.")
  })
  
  # Cargar datos al presionar el botón
  observeEvent(input$load, {
    data(load_data(input$file))
    showNotification("Archivo CSV cargado correctamente.")
  })
  
  # Actualizar las opciones de la variable de respuesta según los datos cargados
  observe({
    updateSelectInput(
      inputId = "y_variable",
      choices = names(data())
    )
  })
  
################################################################################################################################################
  
  # Actualiza las opciones de las variables numéricas y no numéricas al cargar los datos
  observeEvent(data(), {
    updateSelectInput(session, "numeric_variable", choices = names(data()[sapply(data(), is.numeric)]))
    updateSelectInput(session, "non_numeric_variable", choices = names(data()[!sapply(data(), is.numeric)]))
    updateSelectInput(session, "central_tendency", choices = names(data()[sapply(data(), is.numeric)]))
    updateSelectInput(session, "dispersion", choices = names(data()[sapply(data(), is.numeric)]))
    updateSelectInput(session, "asymmetry", choices = names(data()[sapply(data(), is.numeric)]))
  })
  
  # Muestra el listado de variables numéricas
  output$numeric_vars <- renderPrint({
    cat(paste(names(data())[sapply(data(), is.numeric)], collapse = "\n"))
  })
  
  # Muestra el listado de variables no numéricas
  output$non_numeric_vars <- renderPrint({
    cat(paste(names(data())[!sapply(data(), is.numeric)], collapse = "\n"))
  })
  
  # Muestra la tabla de datos
  output$table <- renderDataTable({
    datatable(data(), options = list(pageLength = 10))
  })
 
################################################################################################################################################ 
  
  # Actualizar opciones de selección de variable
  observe({
    updateSelectInput(session, "selected_variable", choices = colnames(data()))
  })
  
  # Muestra la estructura de la variable seleccionada
  output$variable_str <- renderPrint({
    req(input$selected_variable, data())
    str(data()[, input$selected_variable])
  })
  
  # Muestra el resumen de la variable seleccionada
  output$variable_summary <- renderPrint({
    req(input$selected_variable, data())
    summary(data()[, input$selected_variable])
  })
  
  # Genera un resumen detallado de la variable seleccionada
  output$variable_skim <- renderPrint({
    req(input$selected_variable, data())
    skim(data()[, input$selected_variable])
  })
  
################################################################################################################################################
    
  # Histograma
  output$histogram <- renderPlot({
    req(input$numeric_variable)
    ggplot(df(), aes_string(x = input$numeric_variable)) +
      geom_histogram(fill = "steelblue", color = "black") +
      theme_minimal()
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    req(input$numeric_variable)
    ggplot(df(), aes_string(x = input$numeric_variable)) +
      geom_boxplot(fill = "steelblue", color = "black") +
      theme_minimal()
  })
  
  # Gráfico de barras
  output$barchart <- renderPlot({
    req(input$non_numeric_variable)
    ggplot(df(), aes_string(x = input$non_numeric_variable)) +
      geom_bar(fill = "steelblue", color = "black") +
      theme_minimal()
  })
  
  # Gráfico de pastel para variables no numéricas
  output$piechart <- renderPlot({
    req(input$non_numeric_variable)  # Asegura que 'non_numeric_variable' no es NULL
    non_numeric_var <- input$non_numeric_variable
    if (non_numeric_var != "") {
      df() %>%
        dplyr::count(!!rlang::sym(non_numeric_var)) %>%
        ggplot(aes(x = "", y = n, fill = !!rlang::sym(non_numeric_var))) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_minimal() +
        theme(axis.text.x = element_blank())
    }
  })

################################################################################################################################################
    
  # Renderiza las medidas de tendencia central, dispersión y asimetría
  output$central_tendency_output <- renderPrint({
    req(input$central_tendency, data())
    list(
      mean = mean(data()[, input$central_tendency], na.rm = TRUE),
      median = median(data()[, input$central_tendency], na.rm = TRUE),
      mode = as.numeric(names(sort(-table(data()[, input$central_tendency])))[1])
    )
  })
  
  output$dispersion_output <- renderPrint({
    req(input$dispersion, data())
    list(
      variance = var(data()[, input$dispersion], na.rm = TRUE),
      standard_deviation = sd(data()[, input$dispersion], na.rm = TRUE),
      range = diff(range(data()[, input$dispersion], na.rm = TRUE)),
      IQR = IQR(data()[, input$dispersion], na.rm = TRUE)
    )
  })
  
  # Calcula las medidas de asimetría para la variable seleccionada
  output$asimmetry_output <- renderPrint({
    req(input$asymmetry, data())
    skewness(data()[, input$asymmetry], na.rm = TRUE)
  })

################################################################################################################################################
    
  # Actualiza las opciones de las variables para la correlación
  observeEvent(data(), {
    updateSelectInput(session, "correlation_var1", choices = names(data()[sapply(data(), is.numeric)]))
    updateSelectInput(session, "correlation_var2", choices = names(data()[sapply(data(), is.numeric)]))
  })
  
  # Calcula la correlación entre las dos variables seleccionadas
  output$correlation_output <- renderPrint({
    req(input$correlation_var1, input$correlation_var2, data())
    cor(data()[, input$correlation_var1], data()[, input$correlation_var2], use = "complete.obs")
  })
  
  # El dataframe reactivo
  df <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })
  
  # Calcula la matriz de correlación
  output$correlation_table <- renderTable({
    req(df())
    numeric_vars <- sapply(df(), is.numeric)
    cor_matrix <- cor(df()[, numeric_vars, drop = FALSE], use = "complete.obs")
    cor_matrix
  }, rownames = TRUE, colnames = TRUE, nrows = Inf, ncols = Inf)
  
  
  # Gráfico de correlación
  output$correlation_plot <- renderPlot({
    req(df())
    numeric_vars <- sapply(df(), is.numeric)
    cor_matrix <- cor(df()[, numeric_vars, drop = FALSE], use = "complete.obs")
    ggcorrplot(cor_matrix, type = "lower", lab = TRUE)
  })

################################################################################################################################################
    
  # Actualizar las opciones de las variables independientes (X) y dependiente (Y) en la pestaña "Análisis de regresión"
  observeEvent(data(), {
    updateSelectInput(session, "independent_variables", choices = names(data()[sapply(data(), is.numeric)]))
    updateSelectInput(session, "dependent_variable", choices = names(data()[sapply(data(), is.numeric)]))
  })
  
  # Calcular y renderizar el resumen de la regresión
  output$regression_summary <- renderPrint({
    if (is.null(data())) {
      return(NULL)
    }
    
    x_vars <- input$independent_variables
    y_var <- input$dependent_variable
    
    if (length(x_vars) == 0 || y_var == "") {
      return(NULL)
    }
    
    x_vars_formula <- paste(x_vars, collapse = " + ")
    lm_model <- lm(as.formula(paste(y_var, "~", x_vars_formula)), data = data())
    lm_summary <- summary(lm_model)
    
    lm_summary
  })

}

################################################################################################################################################



################################################################################################################################################
  
# Ejecuta la aplicación
shinyApp(ui = ui, server = server)

################################################################################################################################################
  
  
  
