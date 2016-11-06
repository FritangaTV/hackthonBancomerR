###--- ouput writing ---###

### tienes p_c y p_m
aff_im_txn_min <- p_m[ , 2]
aff_im_txn_min[aff_im_txn_min < 0] <- 0

aff_to_txn_min <- p_c[ , 2]
aff_to_txn_min[aff_to_txn_min < 0] <- 0

aff_im_txn_max <- p_m[ , 3]
aff_im_txn_max[aff_im_txn_max <0] <- 0

aff_to_txn_max <- p_c[ , 3]
aff_to_txn_max[aff_to_txn_max <0] <- 0


L7D <- train
ultimos7dias <- L7D[order(L7D$date, decreasing = TRUE)[1:7], "date"]
L7D <- L7D[L7D$date %in% ultimos7dias, ]


days <- model$date
n <- length(days)

output <- data.frame(date = days,
             aff_im_txn_min = aff_im_txn_min,
             aff_to_txn_min = aff_to_txn_min,
             aff_im_txn_max = aff_im_txn_max,
             aff_to_txn_max = aff_to_txn_max,
             aff_im_mL7D = rep(mean(L7D$money_total.x), n),#mean in the las 7 days
             aff_to_mL7D = rep(mean(L7D$trans_count.x), n) #mean in the las 7 days
             )
weather_vars <- names(input[[1]][[3]])
meta_vars <- names(input[[1]][[1]])
weather_output <- cbind(days, model[, weather_vars])
meta_output <- cbind(days, model[,meta_vars])

require(jsonlite)
cat(toJSON(output, pretty=TRUE), file = paste0(output_path, "output.json"))
cat(toJSON(weather_output, pretty = TRUE), file = paste0(output_path, "weather_output.json"))
cat(toJSON(meta_output, pretty= TRUE), file = paste0(output_path, "meta_output.json"))
