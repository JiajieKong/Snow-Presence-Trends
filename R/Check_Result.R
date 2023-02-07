mapWorld <- borders("world", colour="gray50", fill="white")
data("wrld_simpl", package = "maptools")                                                                            
wm <- crop(wrld_simpl, extent(-180, 180, 45, 90))   

check <- function(i){
  
  obs <- as.numeric(snow[i, 3:2810])
  sn = rep(0, 54)
  for (j in 1:54){
    sn[j] = sum(obs[((j*52)-51): (j*52)])
    print(obs[((j*52)-51): (j*52)])
  }
  
  print(sn)
  print(DF[i,])
  
  WD = data.frame(Long = snow[i,1], Lat = snow[i,2], Model = 7, Total = 1)
  mp <- ggplot() +   mapWorld
  mp <- mp + geom_point(data = WD,
                        aes(x = Long, y = Lat, color = Model, size = Total),
                        alpha = 0.5)
  
  
  par(mfrow = c(1,3))
  
  p01_prob <- function(t){prob(t, as.numeric(DF[i,3:6]))}
  p10_prob <- function(t){prob(t, as.numeric(DF[i,7:10]))}
  
  data <- data.frame(week = 1:2808,
                     value = as.numeric(snow[i,4:2811]))
  # Most basic bubble plot
  p <- ggplot(data, aes(x=week, y=value)) +
    geom_line() +
    xlab("week") +  ylab('Snow (yes/no)')
  
  data2 <- data.frame(week = 1:54,
                      value = sn)
  # Most basic bubble plot
  p2 <- ggplot(data2, aes(x=week, y=value)) +
    geom_line() +
    xlab("year") + ylab('Sn')
  
  p01 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
    stat_function(fun = p01_prob) + xlim(0,52) + ylim(0, 1)+ ylab('p01')
  
  p10 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))+
    stat_function(fun = p10_prob) + xlim(0,52) + ylim(0, 1) + ylab('p10')
  
  
  grid.arrange(arrangeGrob(mp, ncol = 1),
               arrangeGrob(p,p2,p01,p10, ncol = 4),
               nrow = 2)
  
  
}

check_loop_all <- function(i){
  
  for (k in i:3011){
    
    check(k)
    readline(prompt="Press [enter] to continue")
    
  }
  
}

check_loop_2 <- function(i){
  
  for (k in i:3011){
    
    if (DF$Group[k] == '2'){
      check(k)
      readline(prompt="Press [enter] to continue")
    }
  }
}

check_loop_3 <- function(i){
  
  for (k in i:3011){
    
    if (DF$Group[k] == '3'){
      check(k)
      readline(prompt="Press [enter] to continue")
    }
  }
}

check_loop_4 <- function(i){
  
  for (k in i:3011){
    
    if (DF$Group[k] == '4'){
      check(k)
      readline(prompt="Press [enter] to continue")
    }
  }
}

plot_Group <- function(DF){
  
  pp <- ggplot() +
    geom_polygon(data = wm, aes(x = long, y = lat, group = group), fill = "grey", colour = "black", alpha = 0.8) +
    
    # Convert to polar coordinates
    coord_map("ortho", orientation = c(90, -90, 0)) +
    #coord_map("rectangular", 30) +
    scale_y_continuous(breaks = seq(0, 90, by = 30), labels = NULL) +
    
    # Removes Axes and labels
    scale_x_continuous(breaks = NULL) +
    xlab("") +
    ylab("") +
    
    geom_point(data=DF, aes(x = lon, y = lat, color= Group),size=3, shape=15) +
    geom_polygon(data = wm, aes(x = long, y = lat, group = group), fill=NA,
                 colour = "black", alpha = 0.1)+
    
    # Adds labels
    #geom_text(aes(x = 180, y = seq(55, 85, by = 10), hjust = -0.2, label = paste0(seq(55, 85, by = 10), "°N"))) +
    #geom_text(aes(x = x_lines, y = 39, label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W"))) +
    
    # Adds axes
    #geom_hline(aes(yintercept = 0), size = 1)  +
    #geom_segment(aes(y = 45, yend = 90, x = x_lines, xend = x_lines), linetype = "dashed") +
    
    # Change theme to remove axes and ticks
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "black"),
          axis.ticks=element_blank())
  
    return(pp)
}

plot_Trend <- function(DF){
  
  trendmap <- DF[DF$Group == '4',]
  trendmap$Z_score = trendmap$Z_score_sub
  trendmap$Trend = trendmap$beta_hat*10
  
  trendmap$'weeks/decade' = trendmap$Trend
  
  pp <- ggplot() + ggtitle("Snow cover trends 1967-2020") +
    theme(plot.title = element_text(hjust = 0.5))+
    geom_polygon(data = wm, aes(x = long, y = lat, group = group), fill = "grey", colour = "black", alpha = 0.8) +
    
    # Convert to polar coordinates
    coord_map("ortho", orientation = c(90, -90, 0)) +
    #coord_map("rectangular", 30) +
    scale_y_continuous(breaks = seq(0, 90, by = 30), labels = NULL) +
    
    # Removes Axes and labels
    scale_x_continuous(breaks = NULL) +
    xlab("") +
    ylab("") +
    
    geom_point(data=trendmap, aes(x = lon, y = lat, color=`weeks/century`),size=3, shape=15) +
    scale_color_gradient2(
      low = "#db0209",
      mid = "White",
      high = "#0800ff",
      midpoint = 0) +
    geom_polygon(data = wm, aes(x = long, y = lat, group = group), fill=NA,
                 colour = "black", alpha = 0.1)+
    
    # Adds labels
    #geom_text(aes(x = 180, y = seq(55, 85, by = 10), hjust = -0.2, label = paste0(seq(55, 85, by = 10), "°N"))) +
    #geom_text(aes(x = x_lines, y = 39, label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W"))) +
    
    # Adds axes
    #geom_hline(aes(yintercept = 0), size = 1)  +
    #geom_segment(aes(y = 45, yend = 90, x = x_lines, xend = x_lines), linetype = "dashed") +
    
    # Change theme to remove axes and ticks
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "black"),
          axis.ticks=element_blank())
  
    return(pp)
}

plot_Z_Score <- function(DF, my_colors = c("#81de43",  "#fc0032", "#0032fc")){
  
  trendmap <- DF[DF$Group == '4',]
  trendmap$Z_score = trendmap$Z_score_sub
  
  pp <- ggplot() + ggtitle("Snow trend Z-score")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_polygon(data = wm, aes(x = long, y = lat, group = group), fill = "grey", colour = "black", alpha = 0.8) +
    
    # Convert to polar coordinates
    coord_map("ortho", orientation = c(90, -90, 0)) +
    #coord_map("rectangular", 30) +
    scale_y_continuous(breaks = seq(0, 90, by = 30), labels = NULL) +
    
    # Removes Axes and labels
    scale_x_continuous(breaks = NULL) +
    xlab("") +
    ylab("") +
    
    geom_point(data=trendmap, aes(x = lon, y = lat, color=Z_score),size=3, shape=15) +
    scale_color_manual(values = my_colors) +
    labs(fill='NEW LEGEND TITLE') +
    geom_polygon(data = wm, aes(x = long, y = lat, group = group), fill=NA,
                 colour = "black", alpha = 0.1)+
    
    # Adds labels
    #geom_text(aes(x = 180, y = seq(55, 85, by = 10), hjust = -0.2, label = paste0(seq(55, 85, by = 10), "°N"))) +
    #geom_text(aes(x = x_lines, y = 39, label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W"))) +
    
    # Adds axes
    #geom_hline(aes(yintercept = 0), size = 1)  +
    #geom_segment(aes(y = 45, yend = 90, x = x_lines, xend = x_lines), linetype = "dashed") +
    
    # Change theme to remove axes and ticks
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "black"),
          axis.ticks=element_blank())
  
  return(pp)
  
}

plot_Hist <- function(DF){
  
  trendmap <- DF[DF$Group == '4',]
  
  p_h1 <- ggplot(trendmap, aes(x=beta_hat),) + 
    geom_histogram(color="black", fill="lightblue", bins = 30)
  p_h2 <- ggplot(trendmap, aes(x=Alpha),) + 
    geom_histogram(color="black", fill="lightblue", bins = 30)
  p_h3 <- ggplot(trendmap, aes(x=Alpha.star),) + 
    geom_histogram(color="black", fill="lightblue", bins = 30)

  pp <- grid.arrange(arrangeGrob(p_h1,p_h2,p_h3, ncol = 3), nrow = 1)

  return(pp)

}

plot_G_v <- function(G_v, snow){
  
  betterDates <- as.Date(colnames(snow)[4:2811],format = "X%m.%d.%Y")
  
  data <- data.frame(week = betterDates, value = G_v)
  # Most basic bubble plot
  pp <- ggplot(data, aes(x=week, y=value)) +
    geom_line() +
    ggtitle("Total area observing snow") +
    ggeasy::easy_center_title() +
    xlab("") + 
    ylab('Area in million km^2') +
    scale_x_date(date_labels = "%m-%Y",
                 limits = c(betterDates[1],betterDates[2808]))
  
  return(pp)

}

plot_beta_change <- function(result){
  
  rec = rep(0, 52)
  beta_change <- result$coefficients[53:104]
  
  #obtain a baseline
  for (j in 1:52){
    indd = seq(j, 2808, 52)
    D = 1:54
    res = lm(G_v[indd]~D)
    rec[j] = res$coefficients[2] * 100
  }
  
  data <- data.frame(Week = betterDates[1:52],
                     NoBreakPoint = rec,
                     WithBreakPoints = beta_change*52*100
  )

  prescriptionMelted <- reshape::melt(data, id.var="Week")
  prescriptionMelted$`a` = prescriptionMelted$variable
  
  pp <- ggplot(prescriptionMelted, aes(x=Week, y=value/10, col=variable)) + 
    ggtitle("Snow cover area lost/gained per decade by season") +
    ggeasy::easy_center_title()+
    geom_line()+
    scale_x_date(date_labels = "%B",
                 limits = c(betterDates[1],betterDates[52]))+
    ylab('Area in million km^2') +
    xlab('Week of year')+
    theme(legend.position="top",
          legend.title=element_blank())+
    scale_color_manual(labels = c("Without breakpoints", "With breakpoints")
                       ,values = c("red", "cyan3"))
  
  return(pp)
  
}