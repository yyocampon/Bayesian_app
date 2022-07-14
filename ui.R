library(shiny)
library(shinythemes)

shinyUI(fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Conjugate priors"),

    fluidRow(
        column(6,
               flowLayout(
                 verticalLayout(
                    h3("Number of observations"),
                    numericInput("numberObservations",
                        "Choose the number of observations",
                        value = 0)
                 )
               ),
               
               flowLayout(
                   verticalLayout(
                       h3("Data Likelihood"),
                       selectInput("distribution_type",
                                   "Likelihood",
                                   selected = "",
                                   choices = c("","Normal")
                       ), 
                       
                       conditionalPanel(
                           condition = "input.distribution_type == 'Normal'",
                           selectInput("cono_parametros_med",
                                       "Mean: choose the scenario",
                                       selected = "",
                                       choices = c("","Known","Unknown")
                           ),
                           selectInput("cono_parametros_var",
                                       "Variance: choose the scenario",
                                       selected = "",
                                       choices = c("","Known","Unknown")
                           ),
                           
                           selectInput("conditioned_means",
                                       "Conditional mean:choose the scenario",
                                       selected = "",
                                       choices = c("","mean is conditional","mean is not conditional")
                           ),
                           
                        uiOutput("data_parameters_norm")

                       )
                       
                       
    
                   )
               ),
               
               flowLayout(
                   verticalLayout(
                       uiOutput("prior_parameters_norm")
                   )
              )
        ),

        column(6,
               actionButton("buttonGraph", "Show graphic"),
               uiOutput("GraphTitle"),
               plotlyOutput('distPlot')
               
        )
    )
))

