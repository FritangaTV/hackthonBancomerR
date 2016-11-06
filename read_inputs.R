### Script para leer la consulta realizada ###
## input data en formato Json ##
list.of.packages <- c("jsonlite", "dplyr", "forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(jsonlite)
library(dplyr)
library(forecast)
#### read Json ####
input <- fromJSON(input_path)
timeframe <- as.Date(names(input))
timeframe <- timeframe[order(timeframe)]
n <- length(timeframe)

##### daily data frame aff ####
daily_df_aff <- data.frame(date = timeframe,
                           money_total = rep(0, n),
                           trans_count = rep(0, n),
                           clients = rep(0, n),
                           cards = rep(0, n)
                           )

for(i in seq_along(timeframe)){
  if(!is.null(input[[i]][["transactions"]])){
    daily_df_aff[i ,"money_total"] <- input[[i]][["transactions"]]$money_total
    daily_df_aff[i ,"trans_count"] <- input[[i]][["transactions"]]$trans_count
    daily_df_aff[i ,"clients"] <- input[[i]][["transactions"]]$clients
    daily_df_aff[i ,"cards"] <- input[[i]][["transactions"]]$cards
  }
}
daily_df_aff <-  daily_df_aff[order(daily_df_aff$date),] #ordenado por fechas

#### daily data frame cp ####
daily_df_cp <- data.frame(date = timeframe,
                           money_total = rep(0, n),
                           trans_count = rep(0, n),
                           clients = rep(0, n),
                           cards = rep(0, n)
)

for(i in seq_along(timeframe)){
  if(!is.null(input[[i]][["area"]])){
  daily_df_cp[i ,"money_total"] <- input[[i]][["area"]]$money_total
  daily_df_cp[i ,"trans_count"] <- input[[i]][["area"]]$trans_count
  daily_df_cp[i ,"clients"] <- input[[i]][["area"]]$clients
  daily_df_cp[i ,"cards"] <- input[[i]][["area"]]$cards
  }
}
daily_df_cp <- daily_df_cp[order(daily_df_cp$date),]

#### proyeccions ####
add_proyection <- function(df, var){
  time_series_dates <- df[1:(n-7), "date"] #sin los ultimos 7 dias
  series <- ts(df[1:(n-7), var], 
               frequency = 7, 
               start=c(1,1))
  fitted_model <- HoltWinters(series)
  fitted_data <- as.numeric(fitted(fitted_model)[,1]) #fitted data for the timesereis period
  proy <- c(rep(as.numeric(NA),7), fitted_data, as.numeric(predict(fitted_model, 7)[,1]))
  df_proy <- data.frame(proy = proy, 
                        date = df[, "date"])
  names(df_proy) <- c(paste0("proy_", var), "date")
  return( merge(df_proy, df, by = "date", all.y = TRUE) )
}

daily_df_aff <- add_proyection(df = daily_df_aff, var = "money_total")
daily_df_aff <- add_proyection(df = daily_df_aff, var = "trans_count")

daily_df_cp <- add_proyection(df = daily_df_cp, var = "money_total")
daily_df_cp <- add_proyection(df = daily_df_cp, var = "trans_count")


#### daily data frame variables exogenas ####
df <- data.frame(date = timeframe)

#### weather df ####
w_list <- list()
for( d in seq_along(timeframe) ){ #dentro del dia d
  informacion <- input[[d]][["weather"]]
  info_unlist <- c(as.character(as.Date(names(input))[d]),unlist(informacion))
  names(info_unlist)[1] <- "date"
  w_list[[d]] <- as.data.frame(t(info_unlist))
}
df_w <- Reduce(rbind, w_list)

#### metadata df ####
m_list <- list()
for( d in seq_along(timeframe) ){ #dentro del dia d
  informacion <- input[[d]][["meta"]]
  info_unlist <- c(as.character(as.Date(names(input))[d]),unlist(informacion))
  names(info_unlist)[1] <- "date"
  m_list[[d]] <- as.data.frame(t(info_unlist))
}
df_m <- Reduce(rbind, m_list)

df <- merge(df_m, df_w)

#### corrigiendo tipos de valores en el df de exogenas ####
df$date <- as.Date(df$date)
variables_factores  <- c("weekday", "feriado", "event", "partido_seleccion")
for( i in seq_along(names(df))){
  if (! names(df)[i] %in% variables_factores){
    df[ , i] <- as.numeric(df[ ,i])
  }
}

rm(df_m, df_w, variables_factores, w_list, m_list)

##### create data frame ####

model <- data.frame()
train <- data.frame()
### subsetting the data ###
train_index <- timeframe[8:(n-7)] #para quitar los NA's de las predicciones
model_index <- timeframe[(n-6):n] #los ultimos 7 dias

train <- merge(daily_df_aff[daily_df_aff$date %in% train_index, ],
                                 daily_df_cp[daily_df_cp$date %in% train_index, ],
                                 by = "date")
train <- merge(train, df[df$date %in% train_index, ], by = "date")


model <- merge(daily_df_aff[daily_df_aff$date %in% model_index, ],
               daily_df_cp[daily_df_cp$date %in% model_index, ],
               by = "date")
model <- merge(model, df[df$date %in% model_index, ], by = "date")
