library(dtplyr)
library(tidyverse) 
library(readxl)
library(lubridate)

rm(list = ls())

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


kraje <- kraje %>% 
  mutate(
    N = round(200*POCET_OBYV/sum(POCET_OBYV)) %>% as.integer()
  ) %>% 
  mutate(
    Ntotal = N*24
  )


# Load Orbis data
orbis <- read_excel("DATAlocal/orbis_sample.xlsx", sheet = 2)
orbis <- orbis %>% 
  select(
    mail = `E-mail address`,
    ICO = `National ID`
  ) %>% 
  # Kick out companies without email
  drop_na() %>% 
  distinct(.keep_all = TRUE)

# Load original sample
load("DATAlocal/sample.RData")

set.seed(7678)
companies <- res_sample %>% 
  left_join(.,orbis) %>% 
  drop_na() %>% 
  left_join(select(kraje,KOD_CZNUTS3,Ntotal,N),.) %>% 
  select(-W) %>% 
  split(.$KOD_CZNUTS3) %>% 
  map_dfr(
    function(x){
      tot <- first(x$Ntotal) 
      wee <- first(x$N)
      
      x %>% 
        slice_sample(n = tot, replace = FALSE) %>% 
        mutate(
          WEEK = rep(1:24,wee)
        )
    }
  )

Ntotal <- nrow(companies)

set.seed(7678)
companies <- companies %>% 
  slice_sample(n = Ntotal, replace = FALSE) %>% 
  split(.$WEEK) %>% 
  map_dfr(
    function(x){
      x %>%
        mutate(
          IDENTITY = rep(LETTERS[1:4],200/4)
        )
    }
  ) %>%
  slice_sample(n = Ntotal, replace = FALSE) %>% 
  split(.$WEEK) %>% 
  map_dfr(
    function(x){
      x %>%
        mutate(
          NAME_CZ = rep(LETTERS[1:5],200/5)
        )
    }
  ) %>%
  slice_sample(n = Ntotal, replace = FALSE) %>% 
  split(.$WEEK) %>% 
  map_dfr(
    function(x){
      x %>%
        mutate(
          NAME_FOR = rep(LETTERS[1:5],200/5)
        )
    }
  ) %>%
  slice_sample(n = Ntotal, replace = FALSE) %>% 
  split(.$WEEK) %>% 
  map_dfr(
    function(x){
      x %>%
        mutate(
          ORDER = rep(1:2,200/2)
        )
    }
  )
  

load("occtable.RData")

occtable <- occtable %>% 
  select(NACE,ISCO)

Sdate <- as_date("2022-03-21")

set.seed(7678)
schedule <- companies %>% 
  left_join(.,occtable) %>% 
  group_by(ICO) %>% 
  slice_sample(n = 1) %>% 
  ungroup() %>% 
  slice_sample(n = Ntotal, replace = FALSE) %>% 
  arrange(WEEK,ICO) %>% 
  group_by(WEEK) %>% 
  mutate(
    date = rep(1:5,200/5)
  ) %>% 
  ungroup() %>% 
  bind_rows(.,.) %>% 
  arrange(WEEK,date,ICO) %>% 
  group_by(ICO) %>% 
  mutate(
    case = 1:2
  ) %>% 
  ungroup() %>% 
  select(-Ntotal,-N) %>% 
  mutate(
    date = Sdate + (WEEK-1)*7 + (date-1)
  ) %>% 
  mutate(
    date = if_else(case == 1, date, date + 7)
  )

# IDENTITY (citizenship/ethnicity)
## A ... refugee (UA/UA)
## B ... refugee (UA/RU)
## C ... permanent residency (UA/UA)
## D ... permanent residency (RU/RU)


### Add names + emails

RU <- tribble(
  ~NAME_sender, ~mail_sender, ~NAME_FOR,
"Evgeniya Sergeeva", "xxx","A",
"Galina Goncharova", "xxx","B",
"Evgenia Guseva", "xxx","C",
"Irina Grigoreva", "xxx","D",
"Svetlana Gavrilova", "xxx","E"
) %>% 
  bind_rows(
    .,.
  ) %>% 
  mutate(
    IDENTITY = c(rep("B",5),rep("D",5))
  )

UA <- tribble(
  ~NAME_sender, ~mail_sender, ~NAME_FOR,
  "Yryna Hordiyenko", "xxx","A",
  "Olha Shevchenko", "xxx","B",
  "Zhanna Marchenko", "xxx","C",
  "Evhenyia Tkachenko", "xxx","D",
  "Anzheia Kharchenko", "xxx","E"
) %>% 
  bind_rows(
    .,.
  ) %>% 
  mutate(
    IDENTITY = c(rep("A",5),rep("C",5))
  )

NAMES <- bind_rows(RU,UA)

CZ <- tribble(
  ~NAME_sender, ~mail_sender, ~NAME_CZ,
  "Kateřina Nováková", "xxx","A",
  "Lenka Horáková", "xxx","B",
  "Lucie Dvořáková", "xxx","C",
  "Marie Veselá", "xxx","D",
  "Věra Svobodová", "xxx","E"
)

schedule <- bind_rows(
  schedule %>% 
    filter(ORDER == case) %>% 
    left_join(.,NAMES),
  schedule %>% 
    filter(ORDER != case) %>% 
    left_join(.,CZ)
) %>% 
  arrange(date,ICO)