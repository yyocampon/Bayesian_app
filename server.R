library(shiny)
library(invgamma)
library(ggplot2)
library(plotly)
library(gridExtra)
#library(markdown)

# source where there are auxiliary functions to plot densities

source("funciones_aux_graph.R")


shinyServer(function(input, output) {
  
  # generating inputs to likelihood dist with data set information
  output$data_parameters_norm <- renderUI({
    if(input$modelo_conj == 'Normal'){
      if(input$cono_parametros_med == 'Conocida' & input$cono_parametros_var == 'Desconocida'){
        fluidRow(
          h5("Parámetro poblacional conocido"),
          numericInput("mean",HTML("Ingrese  &mu;"),
                       value = 0,
                       min = 0)
        )
      }else if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Conocida'){
        fluidRow(
          h5("Parámetro poblacional conocido"),
          numericInput("variance",HTML("Ingrese  &sigma;<sup>2</sup>"),
                       value = 1,
                       min = 0)
        )
      }else if (input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Desconocida'){
        fluidRow(
          h5("Información muestral"),
          numericInput("yn_3",HTML("Ingrese  y<sub>n</sub>"),
                       value = 0.1,
                       min = 0),
          numericInput("sn_2_3",HTML("Ingrese  &sigma;<sub>n</sub><sup>2</sup>"),
                       value = 0.1,
                       min = 0)
        )
      }
    }else if(input$modelo_conj == 'Poisson'){
      fluidRow(
        column(12,
          h5("Información muestral"),
          numericInput("theta",HTML("Ingrese θ"),
                       value = 10,
                       min = 0)
        )
      )
    }else if(input$modelo_conj == 'Binomial'){
      fluidRow(
        column(12,
          h5("Información muestral"),
          numericInput("num_ensayos",HTML("Ingrese número de ensayos"),
                       value = 10,
                       min = 0),
          numericInput("num_exitos",HTML("Ingrese número de éxitos"),
                       value = 4,
                       min = 0)
        )

      )
    }
 
  })
  
  # generating inputs to build aprior information
  output$prior_parameters_norm <- renderUI({
    if(input$modelo_conj == 'Normal'){
      if(input$cono_parametros_med == 'Conocida' & input$cono_parametros_var == 'Desconocida'){
        fluidRow(
          column(6,
                 h5("Parámetros de distribución apriori"),
                 numericInput("Alpha",HTML("Ingrese  &alpha;"),
                              value = 1,
                              min = 0),
                 numericInput("Beta",
                              HTML("Ingrese  &beta;"),
                              value = 0.1,
                              min=0)
          ),
          column(6,
                 h5("Información muestral"),
                 numericInput("sigma_n",
                              "Ingrese the sample sigma",
                              value = 1,
                              min=0)
          )
          
        )
      }else if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Conocida'){
        fluidRow(
          column(6,
                 h5("Parámetros de distribución apriori"),
                 numericInput("T0",HTML("Ingrese  &tau;<sub>0</sub>"),
                              value = 0.1,
                              min = 0),
                 sliderInput( 'k', label = HTML("Number of standar deviations (&sigma;)"), min = -3, 
                              max = 3, value = 1)
          ),
          column(6,
                 h5("Información muestral"),
                 numericInput("yn",HTML("Ingrese  y<sub>n</sub>"),
                              value = 0,
                              min = 0)
          )
        )
        
      }else if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Desconocida'){
        fluidRow(
          column(6,
                 h5("Apriori parameters"),
                 numericInput("Alpha_3",HTML("Ingrese  &alpha;"),
                              value = 1,
                              min = 0),
                 numericInput("Beta_3",
                              HTML("Ingrese  &beta;"),
                              value = 2,
                              min=0)
          ),
          column(6,
                 h5("Apriori parameters"),
                 numericInput("kappa_3",HTML("Ingrese  &kappa;"),
                              value = 50,
                              min = 0),
                 numericInput("mu0_3",HTML("Ingrese  &mu;<sub>0</sub>"),
                              value = 0.1,
                              min = 0)
                 
          )
        )
      }
    }else if(input$modelo_conj == 'Poisson'){
      fluidRow(
        column(6,
          h5("Parámetros de distribución apriori"),
          numericInput("Alpha_pois",HTML("Ingrese  &alpha;"),
                       value = 1,
                       min = 0),
          numericInput("Beta_pois",
                       HTML("Ingrese  &beta;"),
                       value = 1,
                       min=0)
        )
      )
    }else if(input$modelo_conj == 'Binomial'){
      fluidRow(
        column(6,
          h5("Parámetros de distribución apriori"),
          numericInput("Alpha_bin",HTML("Ingrese  &alpha;"),
                       value = 1,
                       min = 0),
          numericInput("Beta_bin",
                       HTML("Ingrese  &beta;"),
                       value = 1,
                       min=0)
        )
      )
    }
  })
  
  
  observeEvent(input$buttonGraph, {
    
    output$distPlot<- renderPlotly({
      
      if(input$modelo_conj == 'Normal'){
        if(input$cono_parametros_med == 'Conocida' & input$cono_parametros_var == 'Desconocida'){
          g1 = fy_ivgamma(a = input$Alpha, b = input$Beta,
                          theta = input$mean ,n= input$numberObservations,s_2 =  input$sigma_n)
          ggplotly(g1[[1]])
        }else if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Conocida'){
          g2 = fx_norm_n(t0 = input$T0 ,d = input$k , variance = input$variance, yn = input$yn ,n = input$numberObservations)
          ggplotly(g2)
        }else if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Desconocida' & input$conditioned_means == 'Media condicionada'){
          g3 = f_norm_uni(y_barn = input$yn_3,S_y = input$sn_2_3,mu0 = input$mu0_3,kappa_0 = input$kappa_3,
                          alpha_0 =input$Alpha_3 ,beta_0 = input$Beta_3,n = input$numberObservations)
          ggplotly(g3[[1]],autorange="TRUE")
        }
      }else if(input$modelo_conj == 'Poisson'){
        pois_vero = fx_pois(nobs=input$numberObservations, theta_m=input$theta, alpha_0=input$Alpha_pois,
                    beta_0=input$Beta_pois)
        ggplotly(pois_vero[[1]])
      }else if(input$modelo_conj == 'Binomial'){
        bin_vero = fy_bin(n_ensayos=input$num_ensayos, a=input$Alpha_bin, b=input$Beta_bin, y=input$num_exitos)
        ggplotly(bin_vero[[1]])
      }
    })
    
    output$distPlot2 <- renderPlotly({
      if(input$modelo_conj == 'Normal'){
        if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Desconocida' & input$conditioned_means == 'Media condicionada'){
          g3 = f_norm_uni(y_barn = input$yn_3,S_y = input$sn_2_3,mu0 =input$mu0_3 ,kappa_0 =input$kappa_3 ,
                          alpha_0 = input$Alpha_3,beta_0 =input$Beta_3 ,n = input$numberObservations)
          ggplotly(g3[[2]],autorange="TRUE")
        }else if(input$cono_parametros_med == 'Conocida' & input$cono_parametros_var == 'Desconocida'){
          g3 = fy_ivgamma(a = input$Alpha, b = input$Beta,
                          theta = input$mean ,n= input$numberObservations,s_2 =  input$sigma_n)
          ggplotly(g3[[2]])
        }
      }else if(input$modelo_conj == 'Poisson'){
        pois_ap = fx_pois(nobs=input$numberObservations, theta_m=input$theta, alpha_0=input$Alpha_pois,
                            beta_0=input$Beta_pois)
        ggplotly(pois_ap[[2]])
      }else if(input$modelo_conj == 'Binomial'){
        bin_ap = fy_bin(n_ensayos=input$num_ensayos, a=input$Alpha_bin, b=input$Beta_bin, y=input$num_exitos)
        ggplotly(bin_ap[[2]])
      }
    })
    
    output$distPlot3 <- renderPlotly({
      if(input$modelo_conj == 'Normal'){
        if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Desconocida' & input$conditioned_means == 'Media condicionada'){
          g3 = f_norm_uni(y_barn = input$yn_3,S_y = input$sn_2_3,mu0 =input$mu0_3 ,kappa_0 =input$kappa_3 ,
                          alpha_0 = input$Alpha_3,beta_0 =input$Beta_3 ,n = input$numberObservations)
          ggplotly(g3[[3]],autorange="TRUE")
        }else if(input$cono_parametros_med == 'Conocida' & input$cono_parametros_var == 'Desconocida'){
          g3 = fy_ivgamma(a = input$Alpha, b = input$Beta,
                          theta = input$mean ,n= input$numberObservations,s_2 =  input$sigma_n)
          ggplotly(g3[[3]])
        }
      }else if(input$modelo_conj == 'Poisson'){
        pois_pos = fx_pois(nobs=input$numberObservations, theta_m=input$theta, alpha_0=input$Alpha_pois,
                            beta_0=input$Beta_pois)
        ggplotly(pois_pos[[3]])
      }else if(input$modelo_conj == 'Binomial'){
        bin_pos = fy_bin(n_ensayos=input$num_ensayos, a=input$Alpha_bin, b=input$Beta_bin, y=input$num_exitos)
        ggplotly(bin_pos[[3]])
      }
    })
    
    output$distPlot4 <- renderPlotly({
      if(input$modelo_conj == 'Poisson'){
        pois_pos = fx_pois(nobs=input$numberObservations, theta_m=input$theta, alpha_0=input$Alpha_pois,
                           beta_0=input$Beta_pois)
        ggplotly(pois_pos[[4]])}
    })
    
  })
  
})




