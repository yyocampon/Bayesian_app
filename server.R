library(shiny)
library(invgamma)
library(ggplot2)
library(plotly)

#Funcion media conocida y varianza desconocida.
fy_ivgamma <- function(a,b,theta,v,n,sigma_n){

  v0 = 2*a 
  sigma0_2 = (2*b)/v0 
  v= v
  v0_n = (v0+n)/2
  v_n = (n*v+v0*sigma0_2)/2
  
  ggplot(data.frame(x = c(1, 500)), aes(x)) + 
    # scale_color_manual(name='Regression Model',
    #                    breaks=c('Linear', 'Quadratic', 'Cubic'),
    #                    values=c('Cubic'='pink', 'Quadratic'='blue', 'Linear'='purple'))+
    # apriori distribution
    stat_function(fun = dinvgamma, args = list(shape = a,scale = 1/b),
                  size = 1.5,aes(colour = "Apriori")) +
    # posterior distribution
    stat_function(fun = dinvgamma, args = list(shape = v0_n,scale = 1/v_n),
                  size = 1.3,aes(colour = "Posterior")) +
    #Verosimilitud
    stat_function(fun = dnorm, args = list(mean =theta, sd =sigma_n),
                  size = 1.2,aes(colour = "Verosimilitud"))+
    
    theme_bw()+
    labs(color = "Type of distribution.")
}

#Funcion varianza conocida y media desconocida
fx_norm_n = function(t0,d,variance,yn,n){
  
  sigma = sqrt(variance)
  mu0 = mean(yn)+(d*sigma)
  ymin = min(c((yn - 4*sigma),(mu0-4*t0),(mu0-4*sigma)))
  ymax = max(c((yn + 4*sigma),(mu0+4*t0),(mu0+4*sigma)))
  
  tn_2 = t0^2+(sigma^2)/(n)
  mn = (((yn*n)/sigma^2)+(mu0/(t0^2)))/((n/(sigma^2))+(1/(t0^2)))
  
  ggplot(data.frame(x = c(ymin, ymax)), aes(x)) + 
    # apriori distribution
    stat_function(fun = dnorm, args = list(mean = mu0,
                                           sd = sigma),
                  size = 1.5,aes(color = "A priori")) +
    # posterior distribution.
    stat_function(fun = dnorm, args = list(mean = mn, sd = sqrt(tn_2)),
                  size = 1.3, aes(color = "Posterior")) +
    stat_function(fun = dnorm, args = list(mean = yn, sd = sigma/sqrt(n)),
                  size = 1.3,aes(color = "Verosimilitud"))+
    theme_bw()+
    labs(color = "Type of distribution.") + 
    ggtitle("Known mean and unknown variance.")
}

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
                     max = 5, value = 1, step = 0.1),
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
    
    output$distPlot<- renderPlot({
      if(input$cono_parametros_med == 'Known' & input$cono_parametros_var == 'Unknown'){
        fy_ivgamma(a = input$Alpha, b = input$Beta,theta = input$mean, v= input$v ,n= input$numberObservations, sigma_n = input$sigma_n)
        #ggplotly(g1)
      }else if(input$cono_parametros_med == 'Unknown' & input$cono_parametros_var == 'Known'){
        fx_norm_n(t0 = input$T0 ,d = input$k , variance = input$variance, yn = input$yn ,n = input$numberObservations)
        #ggplotly(g2)
      }
      
      
    })
  })
  
  
  

})


