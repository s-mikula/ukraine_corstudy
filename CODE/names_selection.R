# library(dtplyr)
library(tidyverse) 

graw <- read_tsv("DATAlocal/names_bc.tsv")

graw %>% 
  filter(`Je váš rodný jazyk čeština?` == "Ano") %>% 
  filter(str_detect(`Jste studentkou nebo studentem Masarykovy univerzity? (Pokud na MUNI zároveň studujete bakalářský i magisterský studijní program, vyberte možnost "magisterský".)`,"bak")) %>% 
  select(-2,-3) %>% 
  pivot_longer(-1) %>%
  mutate(
    name = str_remove_all(name,".*\\[") %>% str_remove("\\]") %>% str_trim()
  ) %>% 
  select(-1) %>% 
  group_by(name,value) %>% 
  summarise(
    obs = n(),
    .groups = "drop"
  ) %>% 
  complete(
    name, value, fill = list(obs = 0)
  ) %>% 
  group_by(name) %>% 
  mutate(
    obs = 100*obs/sum(obs)
  ) %>% 
  ungroup() %>% 
  pivot_wider(values_from = obs, names_from = value) %>% 
  mutate(
    diff =  ruská - ukrajinská
  ) %>% 
  arrange(desc(diff)) %>% 
  print(n=200)
