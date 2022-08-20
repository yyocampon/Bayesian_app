library(ggplot2)
library(invgamma)
library(plotly)
library(gridExtra)

# Función para graficar modelo conjugado con media desconocida ----
fx_norm_n = function(t0,d,variance,yn,n){

#Parámetros de entrada -----  
  #t0: Desviación estándar de la distribución a priori
  #d: Indica a cuántas desviaciones estándar queremos que la media a priori está de la media muestral
  #variance: Varianza de la distribución de verosimilitud
  #yn: Media muestral
  #n: Tamaño  de la muestra
  
  
#Parámetros calculados -------
  sigma = sqrt(variance) #Desviación estándar muestral
  mu0 = mean(yn)+(d*sigma) # Media de la distribución a priori. 
  #Estos valores permiten obtener a cuatro desviaciones estándar el rango de la muestra a graficar
  ymin = min(c((yn - 4*sigma),(mu0-4*t0),(mu0-4*sigma))) # Valor mínimo de X
  ymax = max(c((yn + 4*sigma),(mu0+4*t0),(mu0+4*sigma))) # Valor máximo de X
  
  tn_2 = t0^2+(sigma^2)/(n) #Desviación estándar de la distribución posterior
  mn = (((yn*n)/sigma^2)+(mu0/(t0^2)))/((n/(sigma^2))+(1/(t0^2))) #Media de la distribución posterior
  
  xx = seq(ymin, ymax,length.out = 100000) #Secuencia de valores para graficar

  fy1 = dnorm(x = xx,mean = mu0,sd = t0) #Densidades para la distribución a priori
  fy2 = dnorm(x = xx,mean = mn,sd = sqrt(tn_2)) #Densidades para la distribución posterior
  fy3 = dnorm(x = xx,mean = yn,sd =  sigma) #Densidades para la distribución de verosimilitud
  
  df1 = data.frame(xx,fy1) #Base de datos para distribución a priori: valores en X y densidades
  df2 = data.frame(xx,fy2)  #Base de datos para distribucón a priori: valores en X y densidades
  df3 = data.frame(xx,fy3) #Base de datos para distribución a priori: valores en X y densidades
  
#Gráfico de las distribuciones -----
  p_1 = ggplot(data=df1, aes(x=xx, y=fy1, colour="A priori")) + 
    geom_line(size=0.8)+
    geom_line(size=0.8) +
    geom_line(data=df2, aes(x=xx, y=fy2, colour="Posterior"), size=0.8) +
    geom_line(data=df3, aes(x=xx, y=fy3, colour="Verosimilitud"), size=0.8) +
    theme_bw() +
    labs(color="Distribución", y= "Densidad", x="Valores") +
    ggtitle("Media desconocida y varianza conocida.") 

  p_1 #Gráfico
}

# fx_norm_n(t0=40 ,d = 3,variance = 1600 ,yn = 150,n = 100)
# fx_norm_n(t0=15 ,d = 2,variance = 1600 ,yn = 150,n = 50)


# Función para graficar el modelo conjugado con varianza desconocida ----
fy_ivgamma <- function(a,b,theta,v,n,s_2){
  
  # Parámetros calculados  -------
  sigma_n = sqrt(s_2) # Desviación estándar muestral
  v0 = 2*a # usado para calcular los parámetros alpha y beta de la distribuiones a priori y posterior de sigma^2
  sigma0_2 = (2*b)/v0 # usado para calcular los parámetros alpha y beta de la distribuiones a priori y posterior de sigma^2
  v= v # usado para calcular los parámetros alpha y beta de la distribuiones a priori y posterior de sigma^2
  v0_n = (v0+n)/2 # Cálculo del parámetro alpha de la distribución gamma inversa a priori de sigma^2
  v_n = (n*v+v0*sigma0_2)/2 # usado para calcular el parámetro beta de la distribución posterior de sigma2
  

  x = seq(0.1,0.99,length.out = 100000)
  #Parámetros para a priori de sigma ----
  cuant_apr_sigma = qinvgamma(p = x, shape = a, scale = 1/b) #Valores de la distribución a priori
  den_apri_sigma = dinvgamma(cuant_apr_sigma, shape = a, scale = 1/b) #Densidades muestrales
  P = ecdf(cuant_apr_sigma)
  df4 = data.frame(cuant_apr_sigma,den_apri_sigma)
  df4_red = df4[df4$cuant_apr_sigma<cuant_apr_sigma[min(which(P(cuant_apr_sigma) >0.99))],]
  
  dposterior <-  rinvgamma(100000,shape = v0_n,scale = 1/v_n)
  veros <- rnorm(100000,mean = theta,sd = sigma_n)
  dens_pos <- density(dposterior)
  dens_vero <- density(veros)
  
  df2 = data.frame(dens_pos$x,dens_pos$y)
  df3 = data.frame(dens_vero$x,dens_vero$y)
  
  p_21 = ggplot(data=df4_red, aes(x=cuant_apr_sigma, y=den_apri_sigma)) + 
    geom_line(size = 0.8, col = 3) +
    labs(color = "Dsitribución", y= "Densidad", x="Valores")+
    ggtitle("varianza desconocida - A priori") +
    theme_bw()
  
  p_22 = ggplot() + 
    geom_line(data = df2, aes(x=dens_pos.x, y=dens_pos.y), size=0.8, col = 4)+
    labs(color = "Distribución.", y= "Densidad", x="Valores")+
    ggtitle("Varianza desconocida - Posterior") +
    theme_bw()
  
  p_23 = ggplot()+
    geom_line(data = df3, aes(x=dens_vero.x, y=dens_vero.y), size=0.8, col = 2)+
    labs(color = "Dsitribución", y= "Densidad", x="Valores")+
    ggtitle("Varianza desconocida - Verosimilitud") +
    theme_bw()
  p_23
 # grid.arrange(p_21, p_22, p_23, ncol = 2, nrow = 2 )

}

# fy_ivgamma(a = 1100, b = 250000,theta = 220, v= 395.83,n=12, s_2 = 396.78)
# fy_ivgamma(a = 30, b = 30,theta = 12, v= 0.5,n=100, s_2 = 25)
# fy_ivgamma(a = 1, b = 1,theta = 180, v= 193.83,n=30, s_2 = 400)
# fy_ivgamma(a = 1, b = 0.1,theta =5, v= 4,n=5, s_2 = 4)
#-------------------------------------------------------------

# Modelos paramétricos.


# Función para graficar el modelo conjugado con media y varianza desconocida: distribución a priori de la media depende de la varianza ----

f_norm_uni = function(y_barn, S_y, mu0, kappa_0, alpha_0, beta_0, n){
#Parámetros de entrada -----   
  # n: Tamaño de la muestra
  # mu0: Representa la media de la distribución a prior condicionada en la varianza.
  # y_barn: Media muestral
  # S_y: Varianza muestral 
  # kappa_0: Creencia a priori que se tiene sobre la varianza (sigma_2)
  # alpha_0: Parámetro de forma para la distribución a priori
  # beta_0: Parámetro de escala para la distribución posterior
  
#Parámetros calculados -------
  nu_0 = 2*alpha_0 
  sigma0_2 = (2*beta_0)/nu_0

  #Distribución de verosimilitud ----
  val_vero = rnorm(100000,mean = y_barn, sd = sqrt(S_y)) #Muestra para verosimilitud
  densfy1 = density(val_vero)
  fy1 = densfy1$y #Densidades de los valores muestrales
  df1 = data.frame(densfy1$x, densfy1$y) #Base de datos para verosimilitud: valores en X y densidades

  # Parámetros para la apiori de theta ----
  sigma_2 = rinvgamma(1, shape = alpha_0, scale = 1/beta_0) 
  while (sigma_2 == Inf) {
    sigma_2 = rinvgamma(1,shape =  alpha_0,scale = 1/beta_0)
  }

  val_apriori_theta = rnorm(100000, mean=mu0, sd=sqrt(sigma_2/kappa_0)) #Muestra para a priori
  densfy2 = density(val_apriori_theta) 
  fy2 = densfy2$y #Densidades muestrales
  df2 = data.frame(densfy2$x, densfy2$y) #Base de datos para distribución a priori: valores en X y densidades

  x = seq(0.01,0.99,length.out = 100000) #Secuencia de cuantiles desde 0.01 hasta 0.99
  
  #Parámetros para a priori de sigma ----
  cuant_apr_sigma = qinvgamma(p = x, shape = alpha_0, scale = 1/beta_0) #Valores de la distribución a priori
  den_apri_sigma = dinvgamma(cuant_apr_sigma, shape = alpha_0, scale = 1/beta_0) #Densidades muestrales
  P = ecdf(cuant_apr_sigma)
  df4 = data.frame(cuant_apr_sigma,den_apri_sigma)
  df4_red = df4[df4$cuant_apr_sigma<cuant_apr_sigma[min(which(P(cuant_apr_sigma) > 0.99))],] #Base de datos filtrada con valores hasta el cuantil 99 para distribuci?n a priori
  
  #Parámetros para posterior de sigma ----
  kappa_n = kappa_0 + n 
  nu_n = nu_0 + n
  val_vero_m = sample(val_vero, n) #Muestra tamaño n de los valores de verosimilitud para calcular el parámetro nu, necesario en el cálculo de la varianza posterior de sigma
  nu = sum((val_vero_m-mu0)^2)/n
  alpha_n = nu_n/2  #Parámetro de forma para la distribución posterior
  beta_n = (n*nu+nu_0*sigma0_2)/2 #Parámetro de escala para distribución psoterior
  sigma_n2 = (nu_0*sigma0_2 + (n-1)*S_y + ((n*kappa_0*(y_barn-mu0)^2)/(kappa_n)))/(nu_n) #Varianza psoterior
  cuant_post_sigma = rinvgamma(100000, shape= alpha_n, scale = 1/beta_n) #Valores de la distribución posterior
  den_post_sigma = density(cuant_post_sigma) #Densidades de la distribución
  df5 = data.frame(den_post_sigma$x, den_post_sigma$y) #Base de datos para distribución posterior de sigma
  
  
  #Parámetros para posterior de theta ------
  gl = n + nu_0 # Grados de libertad
  mu_n = ((mu0*kappa_0)+(n*y_barn))/(kappa_n) #Media de la distribución psoterior
  val_theta = rt(1000,gl) *sqrt(sigma_n2/(n+kappa_0))+ mu_n #Muestra para distribución posterior de theta
  densidades = density(val_theta) # Densidades muestrales
  df3 = data.frame(densidades$x, densidades$y) # Base de datos para distribución posterior de theta
  
#Gráfico para cada parámetro -----
  
  # Gráfico para theta ----
  p_31 = ggplot(data=df1, aes(x=densfy1.x, y=densfy1.y,colour="Likelihood")) +
    geom_line(size = 0.8) +
    geom_line(data = df2, aes(x=densfy2.x, y=densfy2.y,colour="A priori"), size=0.8)+
    geom_line(data = df3, aes(x=densidades.x, y=densidades.y, colour="Posterior"), size=0.8)+
    labs(color = "Distribución", y= "Densidad", x="Valores")+
    ggtitle("Media y varianza desconocidos: distribución a priori de la media dependiendo de la varianza", subtitle = "Theta") +
    theme_bw()
  
  # Gráfico para apriori de sigma ----
  p_32 = ggplot(data = df4_red, aes(cuant_apr_sigma,den_apri_sigma))+
    geom_line(size = 0.8, color = 3) +
    labs(y= "Densidad", x="Valores")+
    ggtitle("Distribución a priori", subtitle = "Varianza") +
    theme_bw()
  
  # Gráfico para posterior de sigma ----
  p_33 = ggplot(data = df5, aes(den_post_sigma.x,den_post_sigma.y))+
    geom_line(size = 0.8, color = 4) +
    labs(y= "Densidad", x="Valores")+
    ggtitle("Distribución posterior", subtitle = "Varianza") +
    theme_bw()
  p_33
  #grid.arrange(p_31, p_32, p_33, ncol = 2, nrow = 2 )

}
# g2 <- f_norm_uni(5.5, 2,6, 50, 1,2,5)
# g2 <- f_norm_uni(26.21,115.35,25,5,1,1,66)
# ggplotly(g2)