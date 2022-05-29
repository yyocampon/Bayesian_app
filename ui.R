library(shiny)
library(shinythemes)

shinyUI(fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Conjugate priors"),

    fluidRow(
        column(6,
               flowLayout(
                 verticalLayout(
                    h4("Number of observations"),
                    numericInput("numberObservations",
                        "Choose the number of observations",
                        value = 0),
                 )
               ),
               
               flowLayout(
                   verticalLayout(
                       h4("Data Likelihood"),
                       selectInput("distribution_type",
                                   "Likelihood",
                                   selected = "",
                                   choices = c("","Normal","Binomial")
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
                           
                        uiOutput("data_parameters_norm")

                       ),
                       
                       
    
                   )
               ),
               
               flowLayout(
                   verticalLayout(
                       h4("Prior distribucion"),
                       uiOutput("prior_parameters_norm")
                   )
              )
        ),

        column(6,
               h4("Density Graphics"),
               plotOutput('distPlot')
        )
    )
))
