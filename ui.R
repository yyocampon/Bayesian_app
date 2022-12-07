library(shiny)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(html5)
library(ggplot2)
library(invgamma)
library(htmltools)
library(markdown)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Modelos conjugados normales"),
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "modelo_conj",
                  "Escoge modelo conjugado",
                  selected = "",
                  choices = c("","Normal","Poisson","Binomial")
      ),
      
      conditionalPanel(condition = "input.modelo_conj !='Binomial'",
                       numericInput("numberObservations",
                                    "Ingrese la cantidad de observaciones",
                                    value = 1),
      ),
      
      conditionalPanel(condition = "input.modelo_conj=='Normal'",
                       selectInput("cono_parametros_med",
                                   "Media: escoge el escenario",
                                   selected = "",
                                   choices = c("","Conocida","Desconocida")
                       ),
                       
                       selectInput(inputId = "cono_parametros_var",
                                   label = "Varianza: escoge el escenario",                            
                                   selected = "",
                                   choices = c("","Conocida","Desconocida")
                       )
      ),
      
      conditionalPanel(condition = "input.cono_parametros_med=='Desconocida' & input.cono_parametros_var=='Desconocida'",
                       selectInput("conditioned_means",
                                   "Media condicional: escoge el escenario",
                                   selected = "Media condicionada",
                                   choices = c("","Media condicionada","Media no condicionada")
                       )
                       
      ),
      div(actionButton("buttonGraph", "Generar Gráfico"), align = "center"),
      h5("Integrantes:"),
      tags$ul(
        tags$li(tags$a(href="mailto:yyocampon@unal.edu.co", "Yeison Yovany Ocampo Naranjo")),
        tags$li(tags$a(href="mailto:xcastaneda@unal.edu.co", "	Ximena Castaneda Ochoa")),
        tags$li(tags$a(href="mailto:yalcaraz@unal.edu.co", "Yojan Andres Alcaraz Perez"))
      ),
      h5("Profesores:"),
      tags$ul(
        tags$li(tags$a(href="mailto:iscramirezgu@unal.edu.co", "Isabel Cristina Ramirez Guevara")),
        tags$li(tags$a(href="mailto:cmlopera@unal.edu.co", "Carlos Mario Lopera Gomez"))
      ),
      h5("Correspondencia:"),
      tags$ul(
        tags$li(tags$a(href="mailto:iscramirezgu@unal.edu.co", "iscramirezgu@unal.edu.co"))
      ),
      img(src="Imagenes/Logo_unal.png", height = 120, width = 160,HSPACE="20"),
      img(src="Imagenes/Logo_esc_estadistica.png", height = 130, width = 130)
      #HTML('<center><img src="logo.png"></center>')
    ),
    
    mainPanel(
      tabsetPanel(type = "pills",
                  tabPanel(title = "Gráfico",
                           fluidRow(
                             column(4,
                                    uiOutput("data_parameters_norm"),
                                    
                             ),
                             column(8,
                                    uiOutput("prior_parameters_norm")
                             )
                             ,
                             column(12,
                                    plotlyOutput('distPlot')
                             )
                             ,
                             column(6,
                                    conditionalPanel(
                                      condition = "input.buttonGraph > 0",
                                      style = "display: none;",
                                      withSpinner(plotlyOutput('distPlot2'))
                                    )
                             ),
                             column(6,
                                    conditionalPanel(
                                      condition = "input.buttonGraph > 0",
                                      style = "display: none;",
                                      withSpinner(plotlyOutput('distPlot3'))
                                    )
                             )
                           )
                  ), # scrolling = yes
                  #tabPanel(title = "Teoria", htmlOutput("teoria"))
                  #tabPanel("Teoria",includeHTML("include.html"))
                  tabPanel(title = "Teoría",
                           tags$iframe(style = " height: 400px; width: 100%;",
                                       src="Explicacion_modelos.pdf"
                           )
                  )
      )
    )
  )
)
)