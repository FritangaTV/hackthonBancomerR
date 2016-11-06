###---script para hacer la prediccion---###

m_aff_m <- model_to_proyect$af_m
m_aff_c <- model_to_proyect$af_c

p_m <- predict(m_aff_m, model, interval = "confidence", level = .9)
p_c <- predict(m_aff_c, model, interval = "confidence", level = .9)
 

png(paste0(output_path, "money_proyected.png"))
plot(p_m[,1], type = "b", main = "Monto proyectado")
lines(p_m[,2], col ="blue")
lines(p_m[,3], col = "green")
abline(h=0)
dev.off()

png("transactions_proyected.png")
plot(p_c[,1], type = "b", main = "Numero de transacciones",
     ylab = "Transacciones", xlab= "días proyectados",
     ylim=c(0,max(p_c[,3])))
lines(p_c[,2], col ="blue")
lines(p_c[,3], col = "green")
abline(h=0)
dev.off()
