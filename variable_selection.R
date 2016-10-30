###--- feature selection ---###
library(MASS)

### asume la existencia de la variable parsed ###

aff <- merge(parsed[["Affiliation"]], parsed[["exogenous"]])
aff <- aff[! is.na(aff$proy_m.x) |  !is.na(aff$proy_c.x),]

ex_names <- names(aff)[6:32]

aff_money_total <- aff[, c("money_total.x", ex_names)]
lm.fit <- lm(money_total.x ~. , data = aff_money_total[, -12])
step_money <- stepAIC(lm.fit, direction="both")
final_vars_money_total <- names(step_money$model)


aff_trans_count <- aff[, c("trans_count.x", ex_names)]
lm.fit <- lm(trans_count.x ~. , data = aff_trans_count[, -12])
step_count <- stepAIC(lm.fit, direction="both")
final_vars_count <- names(step_count$model)


vars_to_proyect <- list()
vars_to_proyect[["af_m"]] <- final_vars_money_total
vars_to_proyect[["af_c"]] <- final_vars_count

model_to_proyect <- list()
model_to_proyect [["af_m"]] <- step_money
model_to_proyect [["af_c"]] <- step_count
