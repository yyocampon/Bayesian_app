
source("funciones_aux_graph.R")

server <- function(input, output) {
  

  observeEvent(input$distribution_type, {
    if(input$distribution_type == "Normal"){
      output$item_data <- renderUI({
        menuItem("Distribución de verosimilitud", tabName = "likelihood_item", icon = icon("bar-chart",verify_fa = FALSE))
      })
      output$mean_input <- renderUI({
        selectInput("cono_parametros_med",
                    "Media: escoge el escenario",
                    selected = "",
                    choices = c("","Conocida","Desconocida")
        )
      })
      output$variance_input <- renderUI({
          selectInput("cono_parametros_var",
                      "Varianza: escoge el escenario",
                      selected = "",
                      choices = c("","Conocida","Desconocida")
          )
      })
      output$conditional_input <- renderUI({
        selectInput("conditioned_means",
              "Media condicional: escoge el escenario",
              selected = "",
              choices = c("","Media condicionada","Media no condicionada")
        )    
      })
    }
  })
  
  

  observeEvent(input$conditioned_means,{
    if(input$conditioned_means != ""){
      output$item_prior <- renderUI({
          menuItem("Distribución a priori", tabName = "prior_item",icon = icon("users",verify_fa = FALSE))
      })
      output$item_graphic <- renderUI({
          menuItem("Gráfico", tabName = "graph_item",
                   menuSubItem("Mostrar gráfico", tabName = "show_item"),
                  icon = icon("line-chart",verify_fa = FALSE)
          
          )
      })
    }
  })

  output$data_parameters <- renderUI({
    if(input$cono_parametros_med == 'Conocida' & input$cono_parametros_var == 'Desconocida' & input$conditioned_means == "Media no condicionada"){
      fluidRow(
        column(width = 6,
               numericInput("mean","Ingrese media",
                            value = 0)
        ),
        column(width = 6,
               numericInput("s_2",
                            "Ingrese varianza muestral",
                            value = 0,
                            min=0)
          
        )
      )
    }else if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Conocida' & input$conditioned_means == "Media no condicionada"){
      fluidRow(
        column(width = 6,
               numericInput("yn", "Ingrese la media muestral(y_n)",
                            value = 0)
        ),
        column(width = 6,
               numericInput("variance","Ingrese la varianza",
                            value = 0, 
                            min = 0)
        )
      )
    }else if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Desconocida' & input$conditioned_means == "Media condicionada"){
      fluidRow(
        column(width = 6,
            numericInput("y_barn", "Ingrese la media muestral",
                  value = 0)     
        ),
        column(width = 6,
               numericInput("S_y", "Ingrese la varianza muestral",
                            value = 0,
                            min = 0)
        )
      )
    }
      
  })
  
  
  
  output$prior_parameters_norm <- renderUI({
    if(input$cono_parametros_med == 'Conocida' & input$cono_parametros_var == 'Desconocida' & input$conditioned_means == 'Media no condicionada'){
      verticalLayout(
        numericInput("Alpha","Ingrese parámetro de escala (alpha)",
                     value = 0,
                     min = 0),
        numericInput("Beta",
                     "Ingrese parámetro de forma (beta)",
                     value = 0,
                     min=0),
        sliderInput( 'v', label = h3("Ingrese v"), min = 0.1, 
                     max = 5, value = 1)
      )
    }else if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Conocida' & input$conditioned_means == 'Media no condicionada'){
      verticalLayout(
        numericInput("T0","Ingrese T_0",
                     value = 0,
                     min = 0),
        sliderInput( 'k', label = h3("Número de desviaciones estándar"), min = -3, 
                     max = 3, value = 1)
      )
      
    }else if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Desconocida' & input$conditioned_means == "Media condicionada"){
      verticalLayout(
        h4("Distribución a priori para theta dada sigma_2"),
      
        numericInput("M0", "Ingrese la media a priori",
                     value = 0
          
        ),
        numericInput("kappa_0", "Ingrese la creencia a priori sobre theta",
                     value = 0,
                     min = 0
        ),
        h4("Distribución a priori para sigma_2"),
        numericInput("alpha", "Ingrese parámetro de forma (alpha)",
                     value = 0,
                     min = 0
        ),
        numericInput("beta_", "Ingrese parámetro de escala (beta)",
                     value = 0,
                     min = 0
        )
      )
    }
  })
  
  getPage<-function() {
    return(includeHTML("Explanation_models.html"))
  }
  output$teoria<-renderUI({getPage()})

  
  output$distPlot<- renderPlotly({
    if(input$cono_parametros_med == 'Conocida' & input$cono_parametros_var == 'Desconocida' & input$conditioned_means == 'Media no condicionada'){
      g1 = fy_ivgamma(a = input$Alpha, b = input$Beta,theta = input$mean, v= input$v ,n= input$numberObservations, s_2 = input$s_2)
      ggplotly(g1)
    }else if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Conocida'& input$conditioned_means == 'Media no condicionada'){
      g2 = fx_norm_n(t0 = input$T0 ,d = input$k , variance = input$variance, yn = input$yn ,n = input$numberObservations)
      ggplotly(g2)
    }else if(input$cono_parametros_med == 'Desconocida' & input$cono_parametros_var == 'Desconocida' & input$conditioned_means == "Media condicionada"){
      g3 = f_norm_uni(y_barn = input$y_barn, S_y = input$S_y, mu0 = input$M0, kappa_0 = input$kappa_0, alpha_0 = input$alpha, beta_0 = input$beta_, n = input$numberObservations)
      ggplotly(g3)
    }
  })

  
}


