###--- script para proyectar las variables seleccionadas en el modelo ---###



### proyecciones de cp y aff ###
serie_aff_im <- ts(daily_df_aff$daily_im_txn, frequency = 7, c(1,2))
serie_cp_im <- ts(daily_df_cp$daily_im_txn, frequency = 7, c(1,2))
serie_aff_to <- ts(daily_df_aff$daily_to_txn, frequency = 7, c(1,2))
serie_cp_to <- ts(daily_df_cp$daily_to_txn, frequency = 7, c(1,2))

best_proy <- function(serie){
  aditivo <- HoltWinters(serie, seasonal = "additive")
  multiplicativo <- HoltWinters(serie, seasonal = "multiplicative")
  #exp <- HoltWinters(serie, gamma = FALSE, beta = FALSE)
  #dexp <- HoltWinters(serie, gamma = FALSE)
  #SSEs <- c(aditivo$SSE, multiplicativo$SSE, exp$SSE, dexp$SSE)
  SSEs <- c(aditivo$SSE, multiplicativo$SSE) 
  #modelos <- list(aditivo, multiplicativo, exp, dexp)
  modelos <- list(aditivo, multiplicativo)
  return(modelos[[which.min(SSEs)]])
}
proyeccion <- predict(best_proy(serie_aff_to), h = 14)

best_aff_im <- best_proy(serie_aff_im)
best_aff_to <- best_proy(serie_aff_to)
best_cp_im <- best_proy(serie_cp_im)
best_cp_to <- best_proy(serie_cp_to)

df$pred_aff_im <- predict(best_aff_im, 14)[1:14]
