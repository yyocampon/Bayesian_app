library(shiny)
library(invgamma)
library(ggplot2)
library(plotly)

# source where there are auxiliar funtions to plot densities

source("funciones_aux_graph.R")


shinyServer(function(input, output) {
  
  output$data_parameters_norm <- renderUI({
    h3("Prior distribution")
    if(input$cono_parametros_med == 'Known' & input$cono_parametros_var == 'Unknown'){
      numericInput("mean","Choose the mean",
                   value = 0,
                   min = 0)
    }else if(input$cono_parametros_med == 'Unknown' & input$cono_parametros_var == 'Known'){
      numericInput("variance","Choose the variance",
                   value = 0,
                   min = 0)
    }
  
  })
  
  output$prior_parameters_norm <- renderUI({
    if(input$cono_parametros_med == 'Known' & input$cono_parametros_var == 'Unknown'){
      verticalLayout(
        numericInput("Alpha","Choose the alpha",
                     value = 0,
                     min = 0),
        numericInput("Beta",
                     "Choose the beta",
                     value = 0,
                     min=0),
        sliderInput( 'v', label = h3("Choose the v"), min = 0.1, 
                     max = 2, value = 1),
        numericInput("sigma_n",
                     "Choose the sample sigma",
                     value = 0,
                     min=0)
      )
    }else if(input$cono_parametros_med == 'Unknown' & input$cono_parametros_var == 'Known'){
      verticalLayout(
        numericInput("T0","Choose the T0",
                     value = 0,
                     min = 0),
        numericInput("yn","Choose the yn",
                     value = 0,
                     min = 0),
        sliderInput( 'k', label = h3("Number of standar deviations"), min = -3, 
                     max = 3, value = 1)
      )
      
    }
  })
  
  observeEvent(input$buttonGraph, {
    output$GraphTitle <- renderUI({
      h4("Normal conjugate model.",  align = "center")
    })
    
    output$distPlot<- renderPlotly({
      if(input$cono_parametros_med == 'Known' & input$cono_parametros_var == 'Unknown'){
        g1 = fy_ivgamma(a = input$Alpha, b = input$Beta,theta = input$mean, v= input$v ,n= input$numberObservations, sigma_n = input$sigma_n)
        ggplotly(g1)
      }else if(input$cono_parametros_med == 'Unknown' & input$cono_parametros_var == 'Known'){
        g2 = fx_norm_n(t0 = input$T0 ,d = input$k , variance = input$variance, yn = input$yn ,n = input$numberObservations)
        ggplotly(g2)
      }
      
      
    })
  })
  
  
  

})


