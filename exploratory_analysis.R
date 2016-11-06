#---- Base de datos para el reto PyME Bancomer ----#
library(stats)
library(tidyr)
library(dplyr)
library(forecast)

df <- read.csv("pyme_hackaton_dummy.tsv", sep = "|")
df$id_afil <- as.factor(df$id_afil)
df$cd_postal_cmr <- as.factor(df$cd_postal_cmr)

# lista de series de tiempo por numero de afiliacion
# l_afil <- list()
# for (i in 1:length(levels(df$id_afil))) {
#   l_afil[[i]] <- df[df$id_afil == levels(df$id_afil)[i], ]
# }

#lista de series de tiempo por codigo postal
# l_cp <- list()
# for (i in 1:length(levels(df$cd_postal_cmr))) {
#   l_cp[[i]] <- df[df$cd_postal_cmr == levels(df$cd_postal_cmr)[i], ]
# }

#saveRDS(l_cp, file = "list_by_cd_postal_cmr.rds")
#saveRDS(l_afil, file = "list_by_id_afil.rds")
df <- readRDS("list_by_id_afil.rds")
df_cp <- readRDS("list_by_cd_postal_cmr")

### caso particular, restaurante 1 en lista
set.seed(123)
afiliado_random <- round(runif(n = 1, min=1, max=length(df)))
r_1 <- df[[afiliado_random]]
r_1 %>%
  group_by(date_year, date_month, date_day) %>%
  select(im_txn, to_txn) %>%
  summarise(
    daily_im_txn = sum(im_txn),
    daily_to_txn = sum(to_txn)
  ) %>%
  ungroup() %>%
  mutate(
    date = as.Date(paste(date_year, date_month, date_day, sep="-"))
    ) %>%
  select(date, daily_im_txn, daily_to_txn)-> daily_r_1
  
head(daily_r_1)
plot( daily_r_1$date, 1:163, main = paste0("n = ",nrow(daily_r_1)))
# hay que poner ceros en los dias que no se registró actividad.
dates <- seq.Date(from=min(daily_r_1$date), 
             to=max(daily_r_1$date), by = "day") 
df <- data.frame(date = dates, 
                 money=rep(0,length(dates)), 
                 count=rep(0,length(dates)))

for(i in seq_along(dates)){
  if( df[i, "date"] %in% daily_r_1$date) {
    df[i, "money"] <- daily_r_1[daily_r_1$date == df[i, "date"], "daily_im_txn"]
    df[i, "count"] <- daily_r_1[daily_r_1$date == df[i, "date"], "daily_to_txn"]
    }
}

ts <- ts(data = df$money, frequency = 7)
trend <- 5
seasonal <- 7
fit <- stl(ts, t.window=trend, s.window=seasonal, robust=TRUE)
plot(forecast(fit, h=7))


HWa <- HoltWinters(ts, seasonal = "additive")
(pred_HWa <- predict(HWa, 7 ))
plot(HWa, pred_HWa, xlim=c(30, 50))



# When the time series is long enough to take in more than a year, then it may be necessary to allow for annual seasonality as well as weekly seasonality. In that case, a multiple seasonal model such as TBATS is required.
# 
# y <- msts(x, seasonal.periods=c(7,365.25))
# fit <- tbats(y)
# fc <- forecast(fit)
# plot(fc)


