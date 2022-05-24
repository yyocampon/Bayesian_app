library(shiny)
library(invgamma)
library(ggplot2)

fy_ivgamma <- function(a,b,theta,v,n){

  v0 = 2*a 
  sigma0_2 = (2*b)/v0 
  v= v
  v0_n = (v0+n)/2
  v_n = (n*v+v0*sigma0_2)/2
  
  ggplot(data.frame(x = c(1, 500)), aes(x)) + 
    scale_color_manual(name='Regression Model',
                       breaks=c('Linear', 'Quadratic', 'Cubic'),
                       values=c('Cubic'='pink', 'Quadratic'='blue', 'Linear'='purple'))+
    # apriori distribution
    stat_function(fun = dinvgamma, args = list(shape = a,scale = 1/b),
                  size = 2,aes(colour = "Apriori")) +
    # posterior distribution
    stat_function(fun = dinvgamma, args = list(shape = v0_n,scale = 1/v_n),
                  size = 2,aes(colour = "Posterior")) +
    scale_colour_manual("Type of distribution", values = c("red", "blue"))+
    theme_bw()
}

shinyServer(function(input, output) {
  
  output$data_parameters_norm <- renderUI({
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
        numericInput("v",
                     "Choose the v",
                     value = 0,
                     min=0)
      )
    }else if(input$cono_parametros_med == 'Unknown' & input$cono_parametros_var == 'Known'){
      verticalLayout(
        numericInput("m0","Choose the mean",
                     value = 0,
                     min = 0),
        numericInput("T0","Choose the T0",
                     value = 0,
                     min = 0),
        numericInput("yn","Choose the yn",
                     value = 0,
                     min = 0)
      )
      
    }
  })
  
  
  
  output$distPlot<- renderPlot({
      fy_ivgamma(a = input$Alpha, b = input$Beta,theta = input$mean, v= 393.83,n=1)
        
  })
  

})


