library(dtplyr)
library(tidyverse)
library(sf)

rm(list = ls())

kraje <- st_read("DATA/ArcCR500/AdministrativniCleneni_v13.gdb/","ObceBody") %>% 
  select(KOD_OBEC,KOD_CZNUTS3) %>% 
  st_drop_geometry() %>% 
  as_tibble()
  
kraje2 <- st_read("DATA/ArcCR500/AdministrativniCleneni_v13.gdb/","MestskeObvodyAMestskeCastiBody") %>% 
  select(KOD_OBEC = KOD_MOaMC,KOD_CZNUTS3) %>% 
  st_drop_geometry() %>% 
  as_tibble()

kraje <- bind_rows(kraje,kraje2) %>% distinct()

up <- read_delim("DATA/Ukrajina/firmy_up.csv", delim = "|")
res <- read_csv("DATA/Ukrajina/res_data.csv")

res <- res %>% 
  filter(is.na(DDATZAN)) %>% 
  select(
    ico = ICO,
    KATPO,
    FORMA,
    NACE,
    KOD_OBEC = ICZUJ
  ) %>% 
  mutate(
    KOD_OBEC = as.character(KOD_OBEC)
  ) %>% 
  distinct(ico, .keep_all = TRUE) %>% 
  left_join(.,kraje)

set.seed(4326)
upall <- up %>% 
  slice_sample(.,n = nrow(.)) %>% 
  group_by(ico) %>% 
  summarise(
    nazev = first(nazev),
    email = first(email),
    jmeno = first(jmeno),
    prijmeni = first(prijmeni),
    cizinec = any(cizinec),
    ISCO = first(isco),
    .groups = "drop"
  ) %>% 
  left_join(.,res) %>% 
  # Kick out companies without a match in RES
  drop_na(KOD_OBEC) %>% 
  drop_na(KOD_CZNUTS3) %>% 
  #filter(FORMA %in% c(111,112,113,114,121,932)) %>% 
  slice_sample(.,n = nrow(.)) %>% 
  mutate(
    email = str_squish(email),
    email = str_extract(email, "[^,]+")
  ) %>% 
  filter(KATPO != "0") %>% 
  filter(KATPO != "000") %>% 
  filter(KATPO != "110") %>% 
  distinct(email, .keep_all = TRUE) %>% 
  mutate(
    employer = case_when(
      KATPO %in% c(120,130,210,220,230) ~ "small", #1-49
      KATPO %in% c(240,310,320) ~ "medium", # 50-249
      TRUE ~ "large", # 250+
    )
  )

# prijmeni5p <- bind_rows(
# read_csv("DATA/PrijmeniCR_osloveni_20150123/prijmeni_muzi_1.csv", col_names = c("N","prijmeni","p5p")),
# read_csv("DATA/PrijmeniCR_osloveni_20150123/prijmeni_muzi_2.csv", col_names = c("N","prijmeni","p5p")),
# read_csv("DATA/PrijmeniCR_osloveni_20150123/prijmeni_zeny_1.csv", col_names = c("N","prijmeni","p5p")),
# read_csv("DATA/PrijmeniCR_osloveni_20150123/prijmeni_zeny_1.csv", col_names = c("N","prijmeni","p5p"))
# ) %>% 
#   mutate(
#     prijmeni = str_to_lower(prijmeni) %>% str_squish()
#   ) %>% 
#   rename(
#     lprijmeni = prijmeni
#   ) %>% 
#   distinct(lprijmeni, .keep_all = TRUE)

set.seed(4326)
upall <- upall %>% 
  mutate(
    ISCO = str_sub(ISCO,1,1)
  ) %>% 
  group_by(employer,cizinec,ISCO,KOD_CZNUTS3) %>% 
  mutate(
    batch = rep(sample(1:6,6),3000)[1:n()]
  ) %>% 
  ungroup()


save(upall, file = "all_batches.RData")


aux <- upall %>%
  select(
    batch,
    RespondentID = ico,
    Email = email,
    FirstName = jmeno,
    LastName = prijmeni
  )

for(p in 1:6){
  aux %>% 
    filter(batch == p) %>% 
    select(-batch) %>% 
    mutate(
      across(
        where(is.character),
        replace_na,""
      )
    ) %>% 
    write_csv(
      file = str_c("rfg_w",p,".csv")
      )
}


aux %>% 
  filter(batch == 1) %>% 
  slice_sample(n = 5) %>% 
  mutate(
    Email = "stepan.mikula@gmail.com"
  ) %>% 
  write_csv("test.csv")
