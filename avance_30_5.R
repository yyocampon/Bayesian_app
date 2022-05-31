library(ggplot2)
library(invgamma)


# function for normal model for one observation (mean) variance known ---------------------------
fx_norm_1 = function(t0,d,sigma,y){
  
  mu0 = mean(y)+(d*sigma)
  ymin = min(c((y-4*sigma),(mu0-4*t0),(mu0-4*sigma)))
  ymax = max(c((y+4*sigma),(mu0+4*t0),(mu0+4*sigma)))
  
  t1_2 = t0^2+sigma^2
  m1 = (y*(t0^2)+mu0*(sigma^2))/((sigma^2)+(t0^2))
  
  ggplot(data.frame(x = c(ymin, ymax)), aes(x)) + 
    # apriori distribution
    stat_function(fun = dnorm, args = list(mean = mu0,
                                           sd = sigma),
                  size = 1.5,aes(color = "A priori")) +
    # posterior distribution
    stat_function(fun = dnorm, args = list(mean = m1, sd = sqrt(t1_2)),
                  size = 1.3, aes(color = "Posterior"))+
    theme_bw()+
    labs(color= "Type of distribution.")
}


fx_norm_1(t0 =40 ,d = -2,sigma =20 ,y = 150)

fx_norm_1(t0 =20 ,mu0 = 180,sigma =40 ,y = 150)

fx_norm_1(t0 =110 ,mu0 = 170,sigma =40 ,y = 150)


# function for normal distribution for n observations (mean) variance known ---------------------


fx_norm_n = function(t0,d,sigma,yn,n){
  
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
    # posterior distribution
    stat_function(fun = dnorm, args = list(mean = mn, sd = sqrt(tn_2)),
                  size = 1.3, aes(color = "Posterior"))+
    theme_bw()+
    labs(color = "Type of distribution.")
}

fx_norm_n(t0 =5 ,d = -3,sigma =20 ,yn = 150,n =5)
fx_norm_n(t0 =5 ,mu0 = 100,sigma =20 ,yn = 150,n =50)
fx_norm_n(t0 =5 ,mu0 = 100,sigma =20 ,yn = 150,n =10000)


# function for normal ditribution one observation with (mean known) -------

fy_ivgamma <- function(a,b,theta,v,n){
  
  #ymin = min(c((y-4*sigma),(mu0-4*t0),(mu0-4*sigma)))
  #ymax = max(c((y+4*sigma),(mu0+4*t0),(mu0+4*sigma)))
  
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
                  size = 2,aes(color = "A priori")) +
    # posterior distribution
    stat_function(fun = dinvgamma, args = list(shape = v0_n,scale = 1/v_n),
                  size = 2,aes(color = "Posterior")) +
    labs(color = "Type of distribution.")+
    theme_bw()
}

fy_ivgamma(a = 1100, b = 250000,theta = 180, v= 393.83,n=12)
fy_ivgamma(a = 120, b = 25000,theta = 180, v= 393.83,n=30)
fy_ivgamma(a = 12, b = 2500,theta = 180, v= 193.83,n=30)