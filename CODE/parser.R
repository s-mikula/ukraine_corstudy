library(tidyverse)
library(mboxr)
library(qdapRegex)
library(broom)

load("schedule.RData")

db <- "~/Clouds/Dropbox/Projects/UkraineCorstudy/RAW/temp/"
mbx <- list.files(db, full.names = TRUE) %>% 
  map(read_mbox)

dbm <- bind_rows(mbx)

received <- dbm %>% 
  rowwise() %>% 
  mutate(
    from_email = from %>% ex_email() %>% unlist() %>% str_c(collapse = "|")
  ) %>% 
  ungroup() %>% 
  drop_na(from_email) %>% 
  separate_rows(from_email, sep = "\\|") %>% 
  select(from_email) %>% 
  distinct(from_email) %>% 
  mutate(
    callback = TRUE
  ) %>% 
  rename(mail = from_email)

schedule %>% 
  filter(date < Sys.Date()) %>% 
  left_join(.,received) %>% 
  replace_na(list(callback = FALSE)) %>% 
  filter(NAME_sender != "Marie Veselá") %>% 
  split(.$IDENTITY) %>% 
  map_dfr(
    function(x) lm(callback ~ 1, data = x) %>% tidy(conf.int = TRUE, conf.level = 0.87),
    .id = "IDENTITY"
  ) %>% 
  mutate(
    IDENTITY = factor(IDENTITY)
  ) %>% 
  ggplot(
    aes(x = IDENTITY)
  ) +
  geom_col(
    aes(y = estimate)
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high)
  )
