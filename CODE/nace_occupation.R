library(dtplyr)
library(tidyverse)
library(lubridate)
library(countrycode)

rm(list = ls())

load("/media/Clouds/DATAoffline/PBS/pesest_md1_v2.RData")
load("DATAlocal/res2020.RData")

eu <- countrycode::codelist %>% 
  mutate(
    eu28 = ifelse(is.na(eu28),"X",eu28)
  ) %>% 
  filter(eu28 == "EU") %>% 
  drop_na(iso3c) %>% 
  pull(iso3c)


res_pbs <- res2020 %>% 
  distinct(ICO, .keep_all = TRUE)


up2020 <- pesest %>% 
  mutate(
    ZAM_DO = if_else(is.na(ZAM_DO),Sys.Date(),ZAM_DO)
  ) %>% 
  filter(POHL != "M") %>% 
  filter(
    year(ZAM_OD) <= 2020,
    year(ZAM_DO) >= 2020
  ) %>% 
  filter(!(OBC %in% eu)) %>% 
  select(ICO = FIR_ICO, ISCO) %>% 
  mutate(ICO = as.character(ICO)) %>% 
  left_join(.,res_pbs)

occtable <- up2020 %>% 
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
    NACE = str_sub(NACE,1,3),
    ISCO = str_sub(ISCO,1,4)
  ) %>% 
  filter(
    str_length(NACE) == 3,
    str_length(ISCO) == 4
  ) %>% 
  group_by(NACE,ISCO) %>% 
  filter(
    str_detect(ISCO,"^8") | str_detect(ISCO,"^9")
  ) %>% 
  filter(!str_detect(NACE,"^78")) %>% 
  summarise(
    obs = n(),
    .groups = "drop"
  ) %>% 
  arrange(desc(obs)) %>% 
  mutate(
    share = 100*obs/sum(obs),
    total = cumsum(share)
  ) %>% 
  filter(total <= 75) %>% 
  arrange(ISCO)

save(occtable, file = "DATAlocal/occtable.RData")
write_csv2(occtable, "occtable_words.csv")  
  
