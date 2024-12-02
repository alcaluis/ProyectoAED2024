# Setup
rm(list=ls())
librerias <- c("dplyr",       # Gramática de manipulación de datos
               "ggplot2",     # Visualización mediante gráficas elegantes
               "tidyr",       # Tidy Messy Data
               "shiny",       # Gráficos interactivos a través de HTML
               "DT",          # DataFrames interactivos
               "plotly")      # Mejores gráficos
pacman::p_load(char = librerias)
load("../data/delitos_shiny.Rdata")

choices <- unique( df_delitos_menores$id_clasi )
choices <- choices[c(-1,-2,-4)]
choices <- choices[ -(  (length(choices)-4):length(choices) )]

# Shiny
ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;"))
  ),
  titlePanel("Gráficos interactivos delitos en menores"),
  sidebarLayout(
    sidebarPanel(
      sliderInput('n', 'Número de delitos en el top:', min= 1, max = 10, value = 1), 
      numericInput('edad','Edad:', min = 14, max = 17, value = 14, step=1),
      hr(),
      selectInput('clase', 'Tipo de delito:', choices = choices)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Delitos frecuentes por edad", plotly::plotlyOutput('plot_del_edad')),
        tabPanel("Evolución delitos y su subclasificación por año", plotly::plotlyOutput('plot_clasi_infrac'))
      )
    )
  )
)

server <- function(input, output, session){
  plot_clasific <- function(){
    top_n_delitos_por_edad <- delitos_por_edad %>%
      filter(edad == input$edad) %>%
      slice_max(order_by = total, n = input$n) %>%
      mutate(label_clasi_subclasi = ifelse(nchar(as.character(clasi_subclasi)) > 20,
                                           paste(substring(as.character(clasi_subclasi),
                                                           1, 16),
                                                 "..."),
                                           as.character(clasi_subclasi)))
    
    delitos_por_edad_i_año <- df_delitos_menores %>%
      filter(edad == input$edad) %>%
      # Creamos nueva columna con todas las las subclasificaciones
      mutate(clasi_subclasi = ifelse(is.na(as.character(sub_clasi_infrac)), 
                                     as.character(clasi_infrac), 
                                     as.character(sub_clasi_infrac))) %>%
      mutate(clasi_subclasi = as.factor(clasi_subclasi)) %>%
      # Quitamos los NA's
      filter(!is.na(edad) & !is.na(total_delitos)) %>%
      # Agrupamos segun el tipo de infracción
      group_by(año, clasi_subclasi, id_clasi) %>%
      # Sumar el total de infracciones
      summarise(total = sum(total_delitos), .groups = 'drop')
    
    top_n_delitos_por_edad_i_año <- delitos_por_edad_i_año %>%
      semi_join(top_n_delitos_por_edad,
                by = c("clasi_subclasi")) %>%
      mutate(label_clasi_subclasi = ifelse(nchar(as.character(clasi_subclasi)) > 20,
                                           paste(substring(as.character(clasi_subclasi),
                                                           1, 16),
                                                 "..."),
                                           as.character(clasi_subclasi))) %>%
      mutate(label_clasi_subclasi =
               ifelse(startsWith(label_clasi_subclasi, "Faltas"),
                      id_clasi,
                      label_clasi_subclasi))
    
    
    ggplot(data = top_n_delitos_por_edad_i_año,
           aes(x = año, y = total, color = label_clasi_subclasi)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Evolución del Total de Delitos para Edad"),
           x = "Año",
           y = "Total de Delitos",
           color = "Tipo de Delito") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2013, 2023, by = 2),
                         limits = c(2013, 2023),
                         labels = as.character(seq(13, 23, by = 2)))
    
  }
  
  plot_delitos_anyo <- function(){
    clase <-  input$clase
    
    delitos_subclasi_n <- df_delitos_menores %>%
      # Quitamos los NA's
      filter(!is.na(año) & !is.na(total_delitos) & !is.na(sub_clasi_infrac)) %>%
      # Agrupamos por año y subclasificación
      group_by(año, id_clasi, sub_clasi_infrac) %>%
      # Sumar el total de infracciones
      summarise(total = sum(total_delitos), .groups = 'drop') %>%
      # Filtrar solo el tipo de delito deseado
      filter(id_clasi == clase) %>%
      mutate(label_clasi_subclasi = ifelse(nchar(as.character(sub_clasi_infrac)) > 20,
                                           paste(substring(as.character(sub_clasi_infrac),
                                                           1, 16),
                                                 "..."),
                                           as.character(sub_clasi_infrac)))
    
    ggplot(data = delitos_subclasi_n,
           aes(x = año, y = total, fill = label_clasi_subclasi)) + 
      geom_bar(stat = "identity") +
      labs(title = as.character(unique(df_delitos_menores$clasi_infrac))[ which( unique(df_delitos_menores$id_clasi) == clase)], 
           x = "Año", 
           y = "Total de Delitos",fill='Subclasificación') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(size = 6))                  
  }
  
  output$plot_del_edad <- plotly::renderPlotly({
    plot_clasific()
  })
  
  output$plot_clasi_infrac <- plotly::renderPlotly({
    plot_delitos_anyo()
  })
}

shinyApp(ui = ui, server = server)