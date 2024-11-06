library(numDeriv)
library(tidyverse)
library(sf)
library(mapview)
library(ggmap)
library(maptools)
library(maps)
library(ggplot2)
library(gridExtra)
library(parallel)
library(CPAT)
library(rgdal)                                                                                                      
library(raster)
library(ggeasy)

prob <- function(t,theta) {
  n  <- length(t)
  nu <- t%%52
  mu <- theta[1]+theta[2]*(cos(2*pi*(nu-theta[3])/52))
  p  <- 1/(1+exp(-(mu+theta[4]*t)))
  return(p)
}

para_MapTo <- function(theta){
  
  theta[2] <- exp(theta[2])
  theta[6] <- exp(theta[6])
  
  theta[3] <- 52/pi*atan(theta[3]) + 26
  theta[7] <- 52/pi*atan(theta[7]) + 26
  
  theta
}

para_MapTo_inv <- function(theta){
  
  theta[2] <- log(theta[2])
  theta[6] <- log(theta[6])
  
  theta[3] <- tan((theta[3]-26)*pi/52)
  theta[7] <- tan((theta[7]-26)*pi/52)
  
  theta
}

likelihood <- function(theta, observation=series, H = FALSE, seed.num = 1234) {
  
  set.seed(seed.num)
  
  if (H == FALSE){
    theta <- para_MapTo(theta)
  }
  
  n <- length(observation) 
  
  p01 <- prob(t=1:n, theta[1:4])
  p10 <- prob(t=1:n, theta[5:8])
  
  P_array <- array(rep(0, 4*n), dim=c(2,2,n))
  P_array[1,1,] <- 1-p01
  P_array[1,2,] <- p01
  P_array[2,1,] <- p10
  P_array[2,2,] <- 1-p10
  
  log.likelihood = 0
  
  for (t in 1:(n-1)){
    
    log.likelihood = log.likelihood + log(P_array[observation[t]+1, observation[t+1]+1,t])
    
  }

  return(log.likelihood)
}

se <- function(theta, series) {
  
  t    <- length(series)
  
  year <- t %/% 52
 
  pi1  <- rep(0, n)
  
  if (series[1]==0) {
    pi1[1] <- 0
  } else {
    pi1[1] <- 1
  }
  
  p01   <- prob(1:t, theta[1:4])
  p10   <- prob(1:t, theta[5:8])
  
  tem_P <- matrix(rep(NaN, (52*year)^2), nrow = year*52)
  
  for (v in (52*year):2){
    i       <- v - 1
    product <- diag(1,2)
    for (u in (v-1):1){
      product = matrix(c(1-p01[i], p10[i],p01[i],1-p10[i]),nrow=2,ncol=2) %*% product
      tem_P[u, v] <- product[2,2]
      i <- i - 1
    }
  }
  
  for (i in 1:(t-1)) {
    pi1[i+1] <- (1-pi1[i])*p01[i] + pi1[i]*(1-p10[i])
  }
  
  sn = rep(0, year) ##Question marks
  
  for (j in 1:year){
    sn[j] = sum(obs[((j*52)-51): (j*52)])
  }
  
  e.sn <- apply(matrix(pi1[1:(52*year)],
                       ncol=52,
                       byrow = T),
                       MARGIN = 1,
                       FUN = sum)
  
  Sigma <- matrix(0,nrow=year,ncol=year)
  
  for (n in 1:(year-1)) {
    for (h in 1:(year-n)) {
      mat <- matrix(NaN,nrow=52,ncol=52) #long version
      for (u in 1:52) {
        for (v in 1:52) {
          mat[u,v] <- pi1[(n-1)*52+u] * tem_P[(n-1)*52+u,(n+h-1)*52+v]
        }
      }
      a <- sum(mat)
      b <- e.sn[n]
      c <- e.sn[(n+h)]
      Sigma[n,(n+h)] <- a-b*c
    }
  }
  
  Sigma <- t(Sigma) + Sigma
  
  for (n in 1:year) {
    mat2 <- matrix(0,nrow=52,ncol=52)
    for (v in 2:52) {
      for (u in (v-1):1) {
        mat2[u,v] <- pi1[(n-1)*52+u] * tem_P[(n-1)*52+u,(n-1)*52+v]
      }
    }
    Sigma[n,n] <- e.sn[n] + 2*sum(mat2) - e.sn[n]^2
  }
  
  D    <- cbind(rep(1,year),1:year)
  variance.mat <- solve(t(D)%*%D)%*%t(D)%*%Sigma%*%D%*%solve(t(D)%*%D)
  variance <- variance.mat[2,2]
  sd <- sqrt(variance)
  beta_est <- lm(e.sn~D[,2])
  
  return(list(Sigma=Sigma, 
              se = sd, 
              beta_est = beta_est$coefficients[2],
              sn = sn,
              e.sn = e.sn))
}

Binary_Markov_TS <- function(obs){
  
  DF_tem = rep(NaN, 15)
  
  Total_Weeks_Snow = sum(obs)
  
  DF_tem[1:2] <- rep(NaN, 2)
  DF_tem[14] <- Total_Weeks_Snow
  
  if (Total_Weeks_Snow <= 10){
    return(DF_tem)
  }else{
    try({
      par_init <- list()
      fit <- list()
      w <- 1
      
      ##Try differnt initial points and select the best MLE
      par_init[[1]] <- c(-5, 2, 20, 0, 20, 1.67, 50, 0)
      par_init[[2]] <- c(-10, 5, 20, 0, 10, 1.67, 10, 0)
      par_init[[3]] <- c(-3, 2, 26, 0, 5, 3, 26, 0)
      par_init[[4]] <- c(-3, 1, 15, 0, 3, 1, 15, 0)
      par_init[[5]] <- c(-3, 4, 36, 0, 3, 5, 36, 0)
      
      for (j in 1:length(par_init)){
        
        par_init_tran <- para_MapTo_inv(par_init[[j]])
        
        try({
          fit[[j]] = list()
          fit[[j]]<-optim(par=par_init_tran, 
                          observation=obs,
                          fn=likelihood,  
                          method="BFGS",
                          control = list(factr = 1e1, fnscale = -1))
        }, silent = TRUE)
        
        if (is.null(fit[[j]]$value)){
          fit[[j]]$value <- -1e10
        }   
        
        if (fit[[j]]$value >= fit[[w]]$value){w = j}
      }
      
      fit[[w]]$MLE <- para_MapTo(fit[[w]]$par)

      beta_res <- se(fit[[w]]$MLE, obs)
      
      DF_tem[3:10] <- fit[[w]]$MLE
      DF_tem[11] <- beta_res$beta_est
      DF_tem[12] <- beta_res$se
      DF_tem[13] <- beta_res$beta_est/beta_res$se
      DF_tem[15] <- w
    }, silent = TRUE)
  }
  
  DF_tem <- matrix(DF_tem, nrow = 1)
  colnames(DF_tem) <- c("lon",
                        "lat",
                        "A_0",
                        "A_1",
                        "Kappa",
                        "Alpha",
                        "A_0.star",
                        "A_1.star",
                        "Kappa.star",
                        "Alpha.star",
                        "beta_hat",
                        "beta_se",
                        "Z_score",
                        "Total_Weeks",
                        "init_points")
  #readline(prompt="Press [enter] to continue")
  return(DF_tem)
}

ADD_coodinate <- function(coodinate, DF){
  
  if (dim(coodinate)[1] == dim(DF)[1]){
    DF[,1:2] <- coodinate
  }else{
    print("# of coodinate doesn't match # of pixels")
  }
  
}

ADD_Area <- function(Area, DF){
  
  if (length(Area) == dim(DF)[1]){
    DF$Area <- Area
  }else{
    print("# of Area doesn't match # of pixels")
  }
  
}

change_point_detection <- function(obs_df, index){
  
  test_num <- dim(obs_df)
  
  sn = rep(0, test_num)
  
  for (j in 1:test_num){
    sn[j] = sum(obs[((j*52)-51): (j*52)])
  }
  
  return(CUSUM.test(sn))
  
}

G_v <- function(obs_df, Area){
  
  n <- dim(obs_df)[2]
  G_v <- rep(n, 0)
  
  for (j in 1:n){
    
    ind = as.logical(as.numeric(obs_df[, j]) * as.numeric(DD_file$Group==4))
    G_v[j] <- sum(DD_file$Area[ind])
    
  }
  
  return(G_v/1e6) #divided 1e6 to obtain km^2
}

Regression_With_Design_Matrix(obs_df){
  
  year <- obs_df %% 52
  
  
  D1 = diag(1, 52)
  D2 = D1
  
  for (j in 2:year){
    D2 = rbind(D2, D1)
  }
  
  D3 = diag(1, 52)
  
  for (j in 2:year){
    D3 = rbind(D3, diag((52*j-51):(52*j), 52))
  }
  
  
  betaArea1 = cbind(D2, D3)
  
  result = lm(G_v~betaArea1-1)
  
  beta_change = result$coefficients[53:104]
  #plot(beta_change, type = 'l')
  
  #All breakpoints
  Jun_1977 = c(rep(0, 512), rep(1, 2808-512))
  Feb_1988 = c(rep(0, 1066), rep(1, 2808-1066))
  Jan_1989 = c(rep(0, 1114), rep(1, 2808-1114))
  Jun_1999 = c(rep(0, 1651), rep(1, 2808-1651))
  Jun_2008 = c(rep(0, 2123), rep(1, 2808-2123))
  
  #Backward elimination regression
  betaArea2 = cbind(D2, D3, Jun_1977, Feb_1988, Jan_1989, Jun_1999, Jun_2008)
  result2 = lm(G_v~betaArea2-1)
  result2$coefficients[104: length(result2$coefficients)]
  summary(result2)
  
  betaArea3 = cbind(D2, D3, Jun_1977, Feb_1988, Jan_1989, Jun_1999)
  result3 = lm(G_v~betaArea3-1)
  result3$coefficients[104: length(result3$coefficients)]
  summary(result3)
  
  betaArea4 = cbind(D2, D3, Feb_1988, Jan_1989, Jun_1999)
  result4 = lm(G_v~betaArea4-1)
  result4$coefficients[104: length(result4$coefficients)]
  summary(result4)
  
  beta_change = result4$coefficients[53:104]
  
  return(result5)
}
