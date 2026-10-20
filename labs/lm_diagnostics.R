#-------------------------------------------------------------------------------
# lm_diagnostics()
#-------------------------------------------------------------------------------

library(ggplot2)
library(cowplot)

lm_diagnostics <- function(lm_obj, binwidth = NULL) {
  
  # ta fram residualer och anpassade värden
  res_vect <- residuals(lm_obj)
  fit_vect <- fitted(lm_obj)
  
  # histogram för residualer
  res_df <- data.frame(residuals = res_vect)
  
  p1 <- ggplot(res_df, aes(x = residuals)) +
    geom_histogram(
      aes(y = after_stat(density)),
      binwidth = binwidth
    ) +
    stat_function(
      fun = dnorm,
      args = list(
        mean = mean(res_df$residuals),
        sd = sd(res_df$residuals)
      ),
      linewidth = 1,
      col = "blue"
    )
  
  # fitted values vs residuals
  p2 <- ggplot(
    data.frame(fit_vect, res_vect),
    aes(x = fit_vect, y = res_vect)
  ) +
    geom_point() +
    geom_hline(yintercept = 0, col = "blue") +
    xlab("fitted values") +
    ylab("residuals")
  
  # observation order vs residuals
  index <- seq_along(res_vect)
  
  p3 <- ggplot(
    data.frame(index, res_vect),
    aes(x = index, y = res_vect)
  ) +
    geom_line() +
    geom_hline(yintercept = 0, col = "blue") +
    xlab("index") +
    ylab("residuals")
  
  # normal Q-Q plot
  scale_res_vect <- scale(res_vect)
  
  df <- data.frame(y = scale_res_vect)
  
  p4 <- ggplot(df, aes(sample = y)) +
    stat_qq() +
    stat_qq_line() +
    ylab("Scaled residuals")
  
  plot_final <- plot_grid(
    p4, p2,
    p1, p3,
    nrow = 2
  )
  
  return(plot_final)
  
}


#-------------------------------------------------------------------------------
# model_diagnostics()
#-------------------------------------------------------------------------------

model_diagnostics <- function(res_vect,
                              fit_vect,
                              binwidth = NULL) {
  
  # histogram för residualer
  res_df <- data.frame(residuals = na.omit(res_vect))
  
  p1 <- ggplot(res_df, aes(x = residuals)) +
    geom_histogram(
      aes(y = after_stat(density)),
      binwidth = binwidth
    ) +
    stat_function(
      fun = dnorm,
      args = list(
        mean = mean(res_df$residuals),
        sd = sd(res_df$residuals)
      ),
      linewidth = 1,
      col = "blue"
    )
  
  # fitted values vs residuals
  p2 <- ggplot(
    data.frame(fit_vect, res_vect),
    aes(x = fit_vect, y = res_vect)
  ) +
    geom_point() +
    geom_hline(yintercept = 0, col = "blue") +
    xlab("fitted values") +
    ylab("residuals")
  
  # observation order vs residuals
  index <- seq_along(res_vect)
  
  p3 <- ggplot(
    data.frame(index, res_vect),
    aes(x = index, y = res_vect)
  ) +
    geom_line() +
    geom_hline(yintercept = 0, col = "blue") +
    xlab("index") +
    ylab("residuals")
  
  # normal Q-Q plot
  scale_res_vect <- scale(res_vect)
  
  df <- data.frame(y = scale_res_vect)
  
  p4 <- ggplot(df, aes(sample = y)) +
    stat_qq() +
    stat_qq_line() +
    ylab("Scaled residuals")
  
  plot_final <- plot_grid(
    p4, p2,
    p1, p3,
    nrow = 2
  )
  
  return(plot_final)
  
}





# #-------------------------------------------------------------------------------
# # lm_diagnostics():
# #-------------------------------------------------------------------------------
# library(ggplot2)
# library(cowplot)
# 
# # uppdatera funktionen om du vill ändra layout på plottarna
# # lm_obj: ett objekt med klassen lm
# lm_diagnostics<-function(lm_obj,binwidth=NULL){
#   # ta fram residualer och anpassade värden:
#   res_vect<-residuals(lm_obj)
#   fit_vect<-fitted(lm_obj)
#   
#   # histogram för residualer
#   res_df<-data.frame(residuals=res_vect)
#   p1<-ggplot(res_df, aes(residuals)) +
#     geom_histogram(aes(y = stat(density)),binwidth = binwidth) +
#     stat_function(
#       fun = dnorm, 
#       args = list(mean = mean(res_df$residuals), sd = sd(res_df$residuals)), 
#       lwd = 1, 
#       col = 'blue'
#     )
#   
#   # Fitted values vs residuals
#   p2<-qplot(x = fit_vect,res_vect,ylab="residuals",xlab="fitted values")+geom_hline(yintercept=0,col="blue")
#   
#   # Obervation order vs residuals
#   index<-1:length(res_vect)
#   p3<-qplot(x = index,res_vect,xlab="index",ylab="residuals",geom="line")+geom_hline(yintercept=0,col="blue")
#   
#   
#   # Normal Q-Q plot
#   scale_res_vect<-scale(res_vect)
#   df <- data.frame(y = scale_res_vect)
#   p4 <- ggplot(df, aes(sample = y))+ stat_qq() + stat_qq_line()+
#     ylab("Scaled residuals")
#   
#   
#   plot_final<-plot_grid(p4,p2,p1,p3,nrow = 2)
#   
#   return(plot_final)
#   
# }



# uppdatera funktionen om du vill ändra layout på plottarna
# res_vect = vector with residuals 
# fit_vect = vector with fitted values
model_diagnostics<-function(res_vect,fit_vect,binwidth=NULL){

  # histogram för residualer
  res_df<-data.frame(residuals=na.omit(res_vect))
  p1<-ggplot(res_df, aes(residuals)) +
    geom_histogram(aes(y = stat(density)),binwidth = binwidth) +
    stat_function(
      fun = dnorm, 
      args = list(mean = mean(res_df$residuals), sd = sd(res_df$residuals)), 
      lwd = 1, 
      col = 'blue'
    )
  
  # Fitted values vs residuals
  p2<-qplot(x = fit_vect,res_vect,ylab="residuals",xlab="fitted values")+
    geom_hline(yintercept=0,col="blue")
  
  # Observation order vs residuals
  index<-1:length(res_vect)
  p3<-qplot(x = index,res_vect,xlab="index",ylab="residuals",geom="line")+
    geom_hline(yintercept=0,col="blue")
  
  
  # Normal Q-Q plot
  scale_res_vect<-scale(res_vect)
  df <- data.frame(y = scale_res_vect)
  p4 <- ggplot(df, aes(sample = y))+ stat_qq() + stat_qq_line()+
    ylab("Scaled residuals")
  
  
  plot_final<-plot_grid(p4,p2,p1,p3,nrow = 2)
  
  return(plot_final)
  
}
