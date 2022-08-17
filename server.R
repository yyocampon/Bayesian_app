library(shiny)
library(invgamma)
library(ggplot2)
library(plotly)
library(fontawesome)

#Source where there are auxiliar funtions to plot densities
source("funciones_aux_graph.R")

server <- function(input, output) {
  
#Add "likelihood" item and form to choose the scenario.####################
  observeEvent(input$distribution_type, {
    if(input$distribution_type == "Normal"){
      output$item_data <- renderUI({
        menuItem("Likelihood distribution", tabName = "likelihood_item", icon = icon("bar-chart",verify_fa = FALSE))
      })
      output$mean_input <- renderUI({
        selectInput("cono_parametros_med",
                    "Mean: choose the scenario",
                    selected = "",
                    choices = c("","Known","Unknown")
        )
      })
      output$variance_input <- renderUI({
          selectInput("cono_parametros_var",
                      "Variance: choose the scenario",
                      selected = "",
                      choices = c("","Known","Unknown")
          )
      })
      output$conditional_input <- renderUI({
        selectInput("conditioned_means",
              "Conditional mean:choose the scenario",
              selected = "",
              choices = c("","mean is conditional","mean is not conditional")
        )    
      })
    }
  })
  
  
#Add the item "prior" and "graphic". ##################################
  observeEvent(input$conditioned_means,{
    if(input$conditioned_means != ""){
      output$item_prior <- renderUI({
          menuItem("Prior distribution", tabName = "prior_item",icon = icon("users",verify_fa = FALSE))
      })
      output$item_graphic <- renderUI({
          menuItem("Graphic", tabName = "graph_item",
                   menuSubItem("Show graphic", tabName = "show_item"),
                   menuSubItem("Clear", tabName = "clear_item"),
                  icon = icon("line-chart",verify_fa = FALSE)
          
          )
      })
    }
  })

#Adds the form for the likelihood distribution. ##############################
  output$data_parameters <- renderUI({
    if(input$cono_parametros_med == 'Known' & input$cono_parametros_var == 'Unknown' & input$conditioned_means == 'mean is not conditional'){
      fluidRow(
        column(width = 6,
               numericInput("mean","Enter the mean",
                            value = 0)
        ),
        column(width = 6,
               numericInput("variance_n",
                            "Enter the sample variance",
                            value = 0,
                            min=0)
          
        )
      )
    }else if(input$cono_parametros_med == 'Unknown' & input$cono_parametros_var == 'Known' & input$conditioned_means == 'mean is not conditional'){
      fluidRow(
        column(width = 6,
               numericInput("yn","Enter the sample mean (y_n)",
                            value = 0)
        ),
        column(width = 6,
               numericInput("variance","Enter the variance",
                            value = 0, 
                            min = 0)
        )
      )
    }else if(input$cono_parametros_med == 'Unknown' & input$cono_parametros_var == 'Unknown' & input$conditioned_means == "mean is conditional"){
      fluidRow(
        column(width = 6,
            numericInput("y_barn", "Enter the sample mean",
                  value = 0)     
        ),
        column(width = 6,
               numericInput("variance_y", "Enter the sample variance",
                            value = 0,
                            min = 0)
        )
      )
    }
      
  })
  
  
  
#Add form for "prior" distribution #############################
  output$prior_parameters_norm <- renderUI({
    if(input$cono_parametros_med == 'Known' & input$cono_parametros_var == 'Unknown' & input$conditioned_means == 'mean is not conditional'){
      verticalLayout(
        numericInput("Alpha","Enter the alpha",
                     value = 0,
                     min = 0),
        numericInput("Beta",
                     "Enter the beta",
                     value = 0,
                     min=0),
        sliderInput( 'v', label = h3("Enter the v"), min = 0.1, 
                     max = 2, value = 1)
      )
    }else if(input$cono_parametros_med == 'Unknown' & input$cono_parametros_var == 'Known' & input$conditioned_means == 'mean is not conditional'){
      verticalLayout(
        numericInput("T0","Enter the T0",
                     value = 0,
                     min = 0),
        sliderInput( 'k', label = h3("Number of standar deviations"), min = -3, 
                     max = 3, value = 1)
      )
      
    }else if(input$cono_parametros_med == 'Unknown' & input$cono_parametros_var == 'Unknown' & input$conditioned_means == "mean is conditional"){
      verticalLayout(
        h4("Prior distribution for theta given sigma_2"),
      
        numericInput("M0", "Enter the prior mean",
                     value = 0
          
        ),
        numericInput("c", "Enter the prior belief about theta",
                     value = 0,
                     min = 0
        ),
        h4("Prior distribution for sigma_2"),
        numericInput("alpha", "Enter the alpha",
                     value = 0,
                     min = 0
        ),
        numericInput("beta_", "Enter the beta",
                     value = 0,
                     min = 0
        )
      )
    }
  })


  
#Graph by scenario ####################### 
  output$distPlot<- renderPlotly({
    if(input$cono_parametros_med == 'Known' & input$cono_parametros_var == 'Unknown' & input$conditioned_means == 'mean is not conditional'){
      g1 = fy_ivgamma(a = input$Alpha, b = input$Beta,theta = input$mean, v= input$v ,n= input$numberObservations, variance_n = input$variance_n)
      ggplotly(g1)
    }else if(input$cono_parametros_med == 'Unknown' & input$cono_parametros_var == 'Known'& input$conditioned_means == 'mean is not conditional'){
      g2 = fx_norm_n(t0 = input$T0 ,d = input$k , variance = input$variance, yn = input$yn ,n = input$numberObservations)
      ggplotly(g2)
    }else if(input$cono_parametros_med == 'Unknown' & input$cono_parametros_var == 'Unknown' & input$conditioned_means == "mean is conditional"){
      g3 = f_norm_uni(y_barn = input$y_barn, sigma_y = input$variance_y, mu0 = input$M0, 
                      c = input$c, alpha_0 = input$alpha, beta_0 = input$beta_, n = input$numberObservations)
      ggplotly(g3)
    }
    
  })

  
}


