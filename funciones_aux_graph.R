library(ggplot2)

fx_norm = function(x_1,x_2){
  ggplot(data.frame(x = c(-10, 10)), aes(x)) + 
    stat_function(fun = dnorm, args = list(mean = x_1,
                                           sd = sqrt(x_2)),
                  size = 1.5, col='red') +
    stat_function(fun = dnorm, args = list(mean = 1, sd = .5),
                  size = 1.3, col='blue')+
    stat_function(fun = dchisq, args = list(df=1),
                  size = 2, col='orange')+theme_bw()+
    scale_colour_manual(values=c("red","green","blue"))
}