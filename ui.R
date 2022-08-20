library(shiny)
library(shinythemes)
library(shinydashboard)
library(invgamma)
library(ggplot2)
library(plotly)
library(fontawesome)
library(gridExtra)


ui <- dashboardPage(skin = "green",
    dashboardHeader(
        title = "Modelos conjugados.",
        titleWidth = 400
    ),
    
    dashboardSidebar(width = 400,
        sidebarMenu(
            menuItem("Inicio", tabName = "home",icon = icon("home",verify_fa = FALSE)),
            menuItemOutput("item_data"),
            menuItemOutput("item_prior"),
            menuItemOutput("item_graphic")
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem("home",
                fluidRow(
                    column(width = 6,
                        selectInput("distribution_type",
                        "Verosimilitud",
                        selected = "",
                        choices = c("","Normal")
                        )
                    )
                ),
                fluidRow(
                    column( width = 6,
                        uiOutput("mean_input"),
                        uiOutput("variance_input")
                    ),
                    column(width = 6,
                        uiOutput("conditional_input")
                    )
                )   
            ),
            tabItem("likelihood_item",
                fluidRow(
                  column(width = 6,
                         numericInput("numberObservations",
                              "Ingrese la cantidad de observaciones",
                              value = 0)
                  )
                ),
                uiOutput("data_parameters")
            ),
            
            tabItem("prior_item",
                uiOutput("prior_parameters_norm"),
            ),
            
            tabItem("show_item",
                h4("Modelo normal conjugado:",  align = "center"),
                tabsetPanel(
                    tabPanel("Gráfico",
                    plotlyOutput("distPlot")
                    ),
                    tabPanel("Teoría", htmlOutput("teoria"))
                )
            )
        )
    )    
)

 
