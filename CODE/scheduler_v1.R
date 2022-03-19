
library(methods)
library(tidyverse)
library(lubridate)
library(curl)

###################

set.seed(42)
starting_date <- as_date("2022-03-21") # Must be Monday
namesN <- 5


combinations <- crossing(
  name_native = LETTERS[1:namesN],
  name_refugee = c(str_c("R",LETTERS[1:namesN]),str_c("U",LETTERS[1:namesN])),
  order_refugee = 1:2
)

combinations_randomized <-  1:24 %>% 
  map_dfr(
    function(x){
      combinations %>% 
        slice_sample(n = nrow(combinations), replace = FALSE)
    }
  )


schedule <- tibble(
  ICO = letters,
  mail = str_c("@",letters)
) %>%
  slice_sample(n = 2400, replace = TRUE) %>%
  mutate(
    ICO = str_c(ICO,"_",row_number())
  ) %>% 
  # End of to-be-replaced section
  # Randomize order
  slice_sample(
    n = 2400, replace = FALSE
  ) %>% 
  # Create blocks -- one block contains (first) mails for one week
  mutate(
    block = rep(1:24,100)
  ) %>% 
  group_by(block) %>% 
  # Assign day of week + names of natives
  mutate(
    day = rep(1:5,100/5)
  ) %>% 
  ungroup() %>% 
  arrange(block,day) %>% 
  mutate(
    date = starting_date + (day-1) + 7*(block-1)
  ) %>% 
  mutate(
    ID = row_number()
  ) %>% 
  ungroup() %>% 
  bind_cols(.,combinations_randomized) %>% 
  select(-block,-day) %>% 
  bind_rows(.,.) %>% 
  group_by(ID) %>% 
  mutate(
    order = row_number(),
    letter = sample(letters[1:2],2,replace = FALSE)
  ) %>% 
  ungroup() %>% 
  mutate(
    name_native = ifelse(order == order_refugee, NA, name_native),
    name_refugee = ifelse(order == order_refugee, name_refugee, NA),
    # Send second email exactly after 7 days
    date = if_else(order == 2, date + 7, date)
  ) %>% 
  arrange(ID) %>% 
  mutate(
    ethnicity = str_sub(name_refugee,1,1),
    name_refugee = str_sub(name_refugee,2,2)
  )


### Tests

schedule %>%
  drop_na(name_refugee) %>% 
  group_by(date,name_refugee,ethnicity) %>% 
  add_tally() %>% 
  pull(n) %>% range()
