###--- ouput writing ---###

### tienes p_c y p_m
aff_im_txn_min <- p_m[ , 2]
aff_im_txn_min[aff_im_txn_min < 0] <- 0

aff_to_txn_min <- p_c[ , 2]
aff_to_txn_min[aff_to_txn_min < 0] <- 0

aff_im_txn_max <- p_m[ , 3]
aff_to_txn_max <- p_c[ , 3]
L7D <- parsed[["Affiliation"]]
ultimos7dias <- L7D[order(L7D$date, decreasing = TRUE)[1:7], "date"]
L7D <- L7D[L7D$date %in% ultimos7dias, ]

days <-seq(as.Date("2016-09-23"), as.Date("2016-09-30"), by = "day")
#dummy data.frame
output <- data.frame(date = days,
             aff_im_txn_min = aff_im_txn_min,
             aff_to_txn_min = aff_to_txn_min,
             aff_im_txn_max = aff_im_txn_max,
             aff_to_txn_max = aff_to_txn_max,
             aff_im_mL7D = rep(mean(L7D$money_total.x), 8),#mean in the las 7 days
             aff_to_mL7D = rep(mean(L7D$trans_count.x), 8) #mean in the las 7 days
             )
weather_vars <- names(input[[1]][[3]])
meta_vars <- names(input[[1]][[1]])
weather_output <- cbind(days, new_exo[,weather_vars])
meta_output <- cbind(days, new_exo[,meta_vars])

output <- list(output = output,
               weather= weather_output)
require(jsonlite)
cat(toJSON(output, pretty=TRUE), file = "output.json")
cat(toJSON(weather_output, pretty = TRUE), file = "weather_output.json")
cat(toJSON(meta_output, pretty= TRUE), file = "meta_output.json")
