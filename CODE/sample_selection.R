library(dtplyr)
library(tidyverse) 
library(sf)

rm(list = ls())

load("DATAlocal/occtable.RData")

res <- read_csv("DATAlocal/res_data.csv")


# kraje <- st_read("/media/Clouds/Owncloud/DATA/ArcCR500/AdministrativniCleneni_v13.gdb/", "KrajeBody") %>% 
#   st_drop_geometry() %>% 
#   as_tibble() %>% 
#   select(KOD_CZNUTS3,NAZ_CZNUTS3, POCET_OBYV)

kraje <- tribble(
~KOD_CZNUTS3, ~NAZ_CZNUTS3, ~POCET_OBYV,
"CZ010",       "Hlavní město Praha",     1301489,
"CZ020",       "Středočeský kraj",        1415277,
"CZ031",       "Jihočeský kraj",          631801,
"CZ032",       "Plzeňský kraj",            581445,
"CZ041",       "Karlovarský kraj",         279100,
"CZ042",       "Ústecký kraj",             789153,
"CZ051",       "Liberecký kraj",           435254,
"CZ052",       "Královéhradecký kraj",     538333,
"CZ053",       "Pardubický kraj",          510036,
"CZ063",       "Kraj Vysočina",            497517,
"CZ064",       "Jihomoravský kraj",       1197731,
"CZ071",       "Olomoucký kraj",           619807,
"CZ072",       "Zlínský kraj",             564374,
"CZ080",       "Moravskoslezský kraj",    1162850
)
kraje$POCET_OBYV %>% sum()

kraje <- kraje %>% 
  mutate(
    W = POCET_OBYV/sum(POCET_OBYV)
  ) %>% 
  select(-POCET_OBYV)

okresy <- st_read("/media/Clouds/Owncloud/DATA/ArcCR500/AdministrativniCleneni_v13.gdb/", "OkresyBody") %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(OKRESLAU = KOD_LAU1, KOD_CZNUTS3)


total_sample <- 15000

set.seed(4242)

res_sample <- res %>% 
  filter(is.na(ZPZAN)) %>% 
  filter(
    FORMA %in% c("111","112","113","114","121")
  ) %>% 
  filter(
    KATPO != "000"
  ) %>% 
  filter(
    KATPO != "110"
  ) %>% 
  mutate(
    NACE = str_sub(NACE,1,3)
  ) %>% 
  semi_join(
    .,occtable
  ) %>% 
  left_join(
    .,okresy
  ) %>% 
  left_join(
    .,kraje
  ) %>% 
  select(NACE,KOD_CZNUTS3,W,ICO) %>% 
  split(.$KOD_CZNUTS3) %>%
  map_dfr(
    function(x){
      s <- as.integer(ceiling(first(x$W)*total_sample))
      x %>% 
        slice_sample(n = s)
    }
  )
  
res_sample$NACE %>% table()
res_sample$KOD_CZNUTS3 %>% table() %>% sum()

save(res_sample, file = "DATAlocal/sample.RData")
write_csv(res_sample, file = "sample.csv")