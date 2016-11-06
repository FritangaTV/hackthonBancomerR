###--- feature selection ---###
library(MASS)

### asume la existencia de la variable parsed ###


lm.fit <- lm(money_total.x ~. , data = train)
step_money <- stepAIC(lm.fit, direction="both")
final_vars_money_total <- names(step_money$model)


lm.fit <- lm(trans_count.x ~. , data = train)
step_count <- stepAIC(lm.fit, direction="both")
final_vars_count <- names(step_count$model)


vars_to_proyect <- list()
vars_to_proyect[["af_m"]] <- final_vars_money_total
vars_to_proyect[["af_c"]] <- final_vars_count

model_to_proyect <- list()
model_to_proyect [["af_m"]] <- step_money
model_to_proyect [["af_c"]] <- step_count
