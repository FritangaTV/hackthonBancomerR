#---- Base de datos para el reto PyME Bancomer ----#
library(stats)
library(tidyr)
library(dplyr)


df <- read.csv("pyme_hackaton_dummy.tsv", sep = "|")
df$id_afil <- as.factor(df$id_afil)
df$cd_postal_cmr <- as.factor(df$cd_postal_cmr)

# lista de series de tiempo por numero de afiliacion
# l_afil <- list()
# for (i in 1:length(levels(df$id_afil))) {
#   l_afil[[i]] <- df[df$id_afil == levels(df$id_afil)[i], ]
# }

#lista de series de tiempo por codigo postal
l_cp <- list()
for (i in 1:length(levels(df$cd_postal_cmr))) {
  l_cp[[i]] <- df[df$cd_postal_cmr == levels(df$cd_postal_cmr)[i], ]
}

saveRDS(l_cp, file = "list_by_cd_postal_cmr.rds")
#saveRDS(l_afil, file = "list_by_id_afil.rds")
df <- readRDS("list_by_id_afil.rds")
df_cp <- readRDS("list_by_cd_postal_cmr")

n <- 4000
small_df <- df[sample(nrow(df), n), ]
plot(small_df$to_txn, small_df$im_txn,  xlim = c(0, 60), ylim = c(0,12000))

### caso particular, restaurante 1 en lista
r_1 <- l_afil[[1]]
r_1 %>%
  group_by(date_year, date_month, date_day) %>%
  select(im_txn) %>%
  summarise(
    daily_im_txn = sum(im_txn)
  ) -> daily_r_1
  
ts <- ts(data = daily_r_1$daily_im_txn, frequency = 7, 
         start = c(1, 2))
plot(ts)

HWa <- HoltWinters(ts, seasonal = "multiplicative")
pred_HWa <- predict(HWa, h = 7 ) 
plot(HWa, pred_HWa)

