### Script para leer la consulta realizada ###
## input data en formato Json ##
list.of.packages <- c("jsonlite", "dplyr", "forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(jsonlite)
library(dplyr)
library(forecast)
#### read Json ####
input <- fromJSON("input.json")
timeframe <- as.Date(names(input))
n <- length(timeframe)

## daily data frame aff
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


af_ts_money_total <- ts(daily_df_aff[order(daily_df_aff$date), "money_total"], 
                        frequency = 7, 
                        start=c(1,1))
proy_m <- fitted(HoltWinters(af_ts_money_total))[,1]
proy_m <- cbind(af_ts_money_total, proy_m)
proy_m <- as.data.frame(proy_m)
daily_df_aff <- as.data.frame(cbind(daily_df_aff, proy_m))




af_ts_trans_count <- ts(daily_df_aff[order(daily_df_aff$date), "trans_count"], 
                        frequency = 7, 
                        start=c(1,1))
proy_c <- fitted(HoltWinters(af_ts_trans_count))[,1]
proy_c <- cbind(af_ts_trans_count, proy_c)
proy_c <- as.data.frame(proy_c)
daily_df_aff <- as.data.frame(cbind(daily_df_aff, proy_c))
daily_df_aff <- daily_df_aff[ , c(1,2,3,4,5,7,9)]


## daily data frame cp
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



cp_ts_money_total <- ts(daily_df_cp[order(daily_df_cp$date), "money_total"], 
                        frequency = 7, 
                        start=c(1,1))
proy_m <- fitted(HoltWinters(cp_ts_money_total))[,1]
proy_m <- cbind(cp_ts_money_total, proy_m)
proy_m <- as.data.frame(proy_m)
daily_df_cp <- as.data.frame(cbind(daily_df_cp, proy_m))




cp_ts_trans_count <- ts(daily_df_cp[order(daily_df_cp$date), "trans_count"], 
                        frequency = 7, 
                        start=c(1,1))
proy_c <- fitted(HoltWinters(cp_ts_trans_count))[,1]
proy_c <- cbind(cp_ts_trans_count, proy_c)
proy_c <- as.data.frame(proy_c)
daily_df_cp <- as.data.frame(cbind(daily_df_cp, proy_c))
daily_df_cp <- daily_df_cp[ , c(1,2,3,4,5,7,9)]



## daily data frame variables exogenas
df <- data.frame(date = timeframe)
categ_names <- names(input[[1]])
exogenous_categ <- categ_names[! categ_names %in% c("area", "transactions")]

w_list <- list()
for( d in seq_along(timeframe) ){ #dentro del dia d
  i <- 2
  informacion <- input[[d]][[exogenous_categ[i]]]
  info_unlist <- c(as.character(timeframe[d]),unlist(informacion))
  names(info_unlist)[1] <- "date"
  w_list[[d]] <- as.data.frame(t(info_unlist))
}
df_w <- Reduce(rbind, w_list)

m_list <- list()
for( d in seq_along(timeframe) ){ #dentro del dia d
  i <- 1
  informacion <- input[[d]][[exogenous_categ[i]]]
  info_unlist <- c(as.character(timeframe[d]),unlist(informacion))
  names(info_unlist)[1] <- "date"
  m_list[[d]] <- as.data.frame(t(info_unlist))
}
df_m <- Reduce(rbind, m_list)

df <- merge(df_m, df_w)

df$date <- as.Date(df$date)
for( i in c(2, 5:25)){
  df[ , i] <- as.numeric(df[ ,i])
}
# 
# #### Data frame aff dumy example ####
# df <- readRDS("list_by_id_afil.rds")
# df <- df[[1]] 
# 
# df %>%
#   group_by(id_afil, date_year, date_month, date_day) %>%
#     select(date_year, date_month, date_day, im_txn, to_txn) %>%
#     summarise(
#       daily_im_txn = sum(im_txn),
#       daily_to_txn = sum(to_txn)
#       ) %>%
#     ungroup() %>%
#     mutate(
#       date = paste(date_year, date_month, date_day, sep = "-")
#     )%>%
#     select(id_afil, date, daily_im_txn, daily_to_txn) -> daily_df_aff
# #### Data frame cp dumy example ####
# df <- readRDS("list_by_cd_postal_cmr.rds")
# df <- df[[1]] 
# 
# df %>%
#   group_by(cd_postal_cmr, date_year, date_month, date_day) %>%
#   select(date_year, date_month, date_day, im_txn, to_txn) %>%
#   summarise(
#     daily_im_txn = sum(im_txn),
#     daily_to_txn = sum(to_txn)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     date = paste(date_year, date_month, date_day, sep = "-")
#   )%>%
#   select(cd_postal_cmr, date, daily_im_txn, daily_to_txn)-> daily_df_cp
# #### Data frame variables exogenas dumy example ####
# 
# ini_date <- as.Date(min(c(min(daily_df_aff$date), min(daily_df_cp$date))))
# end_date <- as.Date(max(c(max(daily_df_aff$date), max(daily_df_cp$date))))
# df <- data.frame(date = seq(ini_date, end_date, by = "day"))
# df$X1 <- rnorm(n = nrow(df), mean = 100, sd = 10)
# df$X2 <- rnorm(n = nrow(df), mean = 6, sd = 10)
# df$X3 <- rnorm(n = nrow(df), mean = 62, sd = 11)
# df$X4 <- rnorm(n = nrow(df), mean = 103, sd = 13)
# df$X5 <- rnorm(n = nrow(df), mean = 113, sd = 6)
# df$X6 <- rnorm(n = nrow(df), mean = 10, sd = 12)
# df$X7 <- rnorm(n = nrow(df), mean = 118, sd = 8)
# df$X8 <- rnorm(n = nrow(df), mean = 143, sd = 12)
# df$X9 <- rnorm(n = nrow(df), mean = 20, sd = 10)
# #### Export to Json de dataframes example ####
# # cat(toJSON(daily_df_cp, pretty = TRUE), file = "cp_json.json")
# # cat(toJSON(daily_df_aff, pretty = TRUE), file = "aff_json.json")
# 
# #### create data frame ####

parsed <- list()
train <- timeframe[daily_df_aff$date < max(timeframe)-7]
parsed[["Affiliation"]] <- merge(daily_df_aff[daily_df_aff$date %in% train, ],
                                 daily_df_cp[daily_df_aff$date %in% train, ],
                                 by = "date")
parsed[["exogenous"]] <- df[daily_df_aff$date %in% train,]
parsed[["CodigoPostal"]] <- daily_df_cp[daily_df_aff$date %in% train, ]
#rm(daily_df_aff, daily_df_cp, df, df_w, categ_names, d, df_m, exogenous_categ, i, info_unlist,informacion, input, m_list, n, timeframe, w_list)
