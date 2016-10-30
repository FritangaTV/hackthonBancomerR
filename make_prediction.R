###---script para hacer la prediccion---###

#asume la existencia del new_dataset
new_aff <- merge(daily_df_aff[!daily_df_aff$date %in% train, ],
                 daily_df_cp[!daily_df_aff$date %in% train, ],
                 by = "date")
new_exo <- df[!df$date %in% train, ]
#new_cp <- daily_df_cp[!daily_df_cp$date %in% train, ]

m_aff_m <- model_to_proyect$af_m
m_aff_c <- model_to_proyect$af_c
#### affiliados monto ####
new_dataset_m <- merge(new_aff, new_exo, by="date")[, vars_to_proyect[["af_m"]]]
new_dataset_c <- merge(new_aff, new_exo, by="date")[, vars_to_proyect[["af_c"]]]
p_m <- predict(m_aff_m, new_dataset_m, interval = "confidence", level = .9)
p_c <- predict(m_aff_c, new_dataset_c, interval = "confidence", level = .9)
 
plot(new_dataset_m$money_total.x, type = "b")
lines(p_m[,1], col = "red")
lines(p_m[,2], col ="blue")
lines(p_m[,3], col = "green")

plot(new_dataset_c$trans_count.x, type = "b")
lines(p_c[,1], col = "red")
lines(p_c[,2], col ="blue")
lines(p_c[,3], col = "green")
