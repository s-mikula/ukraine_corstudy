library(tidyverse)
library(lubridate)

rm(list=ls())

load("schedule.RData")

adresy <- read_tsv("adresy.csv") %>% 
  select(ethnicity,name,name_sender = name_full,email_sender = Gmail) %>% 
  mutate(
    email_sender = str_c(email_sender,"@gmail.com")
  )

fdate <- as_date("2022-4-10")
edate <- fdate + 3

set.seed(4328)

UKRlow <- schedule %>% 
  filter(ethnicity == "UKR", literacy == "low") %>% 
  group_by(child,letter) %>% 
  slice_sample(n = 100) %>% 
  mutate(
    refugee = rep(c(TRUE,FALSE),50)
  ) %>% 
  ungroup()

other <- schedule %>% 
  filter(ethnicity == "CZE" | (ethnicity == "UKR" & literacy == "high")) %>% 
  group_by(ethnicity,literacy,child,letter) %>% 
  slice_sample(n = 50) %>% 
  ungroup()


set.seed(4328)
schedule <- bind_rows(UKRlow,other) %>% 
  #slice_sample(n = 1000) %>% 
  select(-email_sender,-name_sender) %>%
  # Change names/letters
  mutate(
    name = ifelse(name == 1, 2, 1),
    letter = ifelse(letter == 1, 2, 1)
  ) %>% 
  left_join(.,adresy) %>% 
  group_by(ethnicity,child,literacy,refugee,email_sender) %>% 
  add_tally() %>% 
  mutate(
    email_batch = rep(sample(fdate:edate,4,replace = FALSE),100)[1:first(n)] %>% as_date()
  ) %>% 
  select(-n) %>%
  group_by(email_batch,email_sender) %>% 
  add_tally() %>% 
  mutate(
    daytime = rep(c("morning","afternoon"),300)[1:first(n)]
  ) %>% 
  select(-n) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    email_sender = ifelse(ethnicity == "UKR" & name == 1,
                          sample(c("anna.shevchenko541@gmail.com","anna.shevchenko145@gmail.com"),1),
                          email_sender
                          ),
    email_sender = ifelse(ethnicity == "UKR" & name == 2,
                          sample(c("yelyzaveta.tkachenko65@gmail.com","yelyzaveta.tkachenko56@gmail.com"),1),
                          email_sender)
  ) %>% 
  ungroup() %>% 
  mutate(
    refugee = as.character(refugee),
    refugee = ifelse(is.na(refugee),"na",refugee),
    refugee = ifelse(refugee == "TRUE","refugee",refugee),
    refugee = ifelse(refugee == "FALSE","settled",refugee)
  )

save(schedule, file = "schedule2022.RData")
  
# schedule %>% 
#   group_by(ethnicity,literacy,refugee,email_batch) %>% 
#   summarise(obs = n()) %>% print(n=300)
