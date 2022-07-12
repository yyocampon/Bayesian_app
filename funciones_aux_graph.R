library(ggplot2)
library(invgamma)


# Funciton to plot conjugate model with mean unkown -----------------------

fx_norm_n = function(t0,d,variance,yn,n){
  
  sigma = sqrt(variance)
  mu0 = mean(yn)+(d*sigma)
  ymin = min(c((yn - 4*sigma),(mu0-4*t0),(mu0-4*sigma)))
  ymax = max(c((yn + 4*sigma),(mu0+4*t0),(mu0+4*sigma)))
  
  tn_2 = t0^2+(sigma^2)/(n)
  mn = (((yn*n)/sigma^2)+(mu0/(t0^2)))/((n/(sigma^2))+(1/(t0^2)))
  
  
  xx = seq(ymin, ymax,length.out = 10000)
  fy1 = dnorm(x = xx,mean = mu0,sd = sigma)
  fy2 = dnorm(x = xx,mean = mn,sd = sqrt(tn_2))
  fy3 = dnorm(x = xx,mean = yn,sd = sigma/sqrt(n))
  
  df1 = data.frame(xx,fy1)
  df2 = data.frame(xx,fy2)
  df3 = data.frame(xx,fy3)
  
  line_types = c("A priori"=1,"Posterior"=3,"Likelihood" = 2)
  p1 = ggplot(df1, aes(x = xx,y = fy1, colour="A priori")) + 
    geom_line(size = 1.4)+ # apriori
    geom_line(data=df2, aes(x=xx,y=fy2, colour="Posterior"),size = 1.5)+ # posterior
    geom_line(data=df3, aes(x=xx,y=fy3, colour="Likelihood"),size = 1.3)+ # verosimilitud
    theme_bw()+
    labs(color = "Distribution.") + 
    ggtitle("Unknown mean and known variance.")+
    scale_linetype_manual(values=line_types)
  p1
}

# Funciton to plot conjugate model with variance unkown -----------------------

fy_ivgamma <- function(a,b,theta,v,n,sigma_n){
  
  v0 = 2*a 
  sigma0_2 = (2*b)/v0 
  v= v
  v0_n = (v0+n)/2
  v_n = (n*v+v0*sigma0_2)/2
  
  xx = seq(100,500,length.out = 1000)
  fy1 = dinvgamma(x = xx,shape = a,scale = 1/b) # apriori
  fy2 = dinvgamma(x = xx,shape = v0_n,scale = 1/v_n) # posterior
  fy3 = dnorm(x = xx,mean = theta,sd = sigma_n)
  
  df1 = data.frame(xx,fy1)
  df2 = data.frame(xx,fy2)
  df3 = data.frame(xx,fy3)
  
  line_types = c("A priori"=1,"Posterior"=3,"Likelihood" = 2)
  p1 = ggplot(df1, aes(x = xx,y = fy1, colour="A priori")) + 
    geom_line(size = 1.4)+ # apriori
    geom_line(data=df2, aes(x=xx,y=fy2, colour="Posterior"),size = 1.5)+ # posterior
    geom_line(data=df3, aes(x=xx,y=fy3, colour="Likelihood"),size = 1.3)+ # verosimilitud
    theme_bw()+
    labs(color = "Distribution.") + 
    ggtitle("Known mean and unknown variance.")+
    scale_linetype_manual(values=line_types)
  p1
}


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

