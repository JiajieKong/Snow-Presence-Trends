mapWorld <- borders("world", colour="gray50", fill="white")
data("wrld_simpl", package = "maptools")                                                                            
wm <- crop(wrld_simpl, extent(-180, 180, 45, 90))   

snow <- read.csv(".data/snow.csv", header = T)
napoleon <- as.matrix(read.csv(file=".data/napoleon.csv", header = F))
DF <- readRDS(".data/github_DF")

#Function to reproduce the result in the paper
main(snow) <- function{
  
  pixel_num <- dim(snow)[1]
  
  DF <- data.frame(lon = rep(NaN, pixel_num),
                   lat = rep(NaN, pixel_num),
                   A_0 = rep(NaN, pixel_num),
                   A_1 = rep(NaN, pixel_num),
                   Kappa = rep(NaN, pixel_num),
                   Alpha = rep(NaN, pixel_num),
                   A_0.star = rep(NaN, pixel_num),
                   A_1.star = rep(NaN, pixel_num),
                   Kappa.star = rep(NaN, pixel_num),
                   Alpha.star = rep(NaN, pixel_num),
                   beta_hat = rep(NaN, pixel_num),
                   beta_se = rep(NaN, pixel_num),
                   Z_score = rep(NaN, pixel_num),
                   Total_Weeks = rep(NaN,pixel_num),
                   init_points = rep(NaN,pixel_num),
                   Note = rep(NaN, pixel_num))
  
  snow_obs_list <- list()
  obs_df <- matrix(rep(NaN, pixel_num*2808), ncol = 54)
  
  for (j in 1:pixel_num){
    
    snow_obs_list[[j]] <- as.numeric(snow[j, 4:2811])
    
  }
  
  number_core = detectCores() ##detect Cores
  mc <- getOption("mc.cores", number_core-1) ##use parallel computing
  result_list <- mclapply(mc.cores = mc, snow_obs_list, FUN = Binary_Markov_TS)
  
  for (j in 1:pixel_num){
    
    DF[j, 1:15] <- result_list[[j]]
    DF[j, 14] <- as.integer(DD_file[j, 14])
    
    Total_Weeks_Snow <- DD_file[j, 14]
    if (Total_Weeks_Snow <= 10){
      DF$Note[j] <- 'Less than 10'
    }else if(Total_Weeks_Snow <= 50){
      DF$Note[j] <- 'Less than 50'
    }else{
      DF$Note[j] <- 'Greater than 50'
    }
  }
  
  ADD_coodinate(snow[,1:2], DF)        ##return
  ADD_Area(snow$AREA, DF)
  
  DF$p_value <- change_point_detection(obs_df, 1951)
  DF$Group <- 1    #more for group selection
  
  DD_file$Z_score_sub = 'other'   #categorical Z score
  DD_file$Z_score_sub[DD_file$Z_score > 2] = 'Z > 2'
  DD_file$Z_score_sub[DD_file$Z_score < -2] = 'Z < -2'

  return(DF)
}



