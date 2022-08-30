library(shiny)
library(shinythemes)
library(plotly)
library(shinycssloaders)
#library(html5)
library(markdown)

shinyUI(fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Modelos conjugados normales"),
    sidebarLayout(
        
        sidebarPanel(
            
            numericInput("numberObservations",
                         "Ingrese la cantidad de observaciones",
                         value = 1),
            
            
            selectInput("cono_parametros_med",
                        "Media: escoge el escenario",
                        selected = "",
                        choices = c("","Conocida","Desconocida")
            ),
            selectInput(inputId = "cono_parametros_var",
                        label = "Varianza: escoge el escenario",                            
                        selected = "",
                        choices = c("","Conocida","Desconocida")
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
            img(src="logo_mede_2.png", height = 60, width = 120),
            img(src=NULL, height = 60, width = 120),
            img(src="Logo_ESC_ESTAD.png", height = 60, width = 120)
            #HTML('<center><img src="logo.png"></center>')
        ),
        
        mainPanel(
            tabsetPanel(type = "pills",
                        tabPanel(title = "Gráfico",
            fluidRow(
                column(4,
                       uiOutput("data_parameters_norm"),
                       uiOutput("GraphTitle")
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
        ),
        #tabPanel(title = "Teoria", htmlOutput("teoria"))
        #tabPanel("Teoria",includeHTML("include.html"))
        tabPanel(title = "Teoría",
                 tags$iframe(style = " height: 400px; width: 100%; scrolling = yes ",
                             src="Explanation_models_esp.pdf"
                 )
        )
        )
        )
    )
    
)
)