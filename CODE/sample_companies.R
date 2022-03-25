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

nace <- readxl::read_excel("DATAlocal/cz_nace.xlsx")
nace <- nace %>% 
  filter(UROVEN <= 2) %>% 
  mutate(
    sector = ifelse(UROVEN == 1,CHODNOTA,NA)
  ) %>% 
  fill(sector) %>% 
  filter(UROVEN == 2) %>% 
  select(
    NACE2 = CHODNOTA,
    sector
  )


exclude <- tibble()
lof <- list.files("sample/", pattern = "log_out", full.names = TRUE)
exclude <- lof %>% 
  map_dfr(
    function(x){
      load(x)
      exclude <- bind_rows(exclude,out)
    }
  ) %>% 
  distinct(ICO) %>% 
  filter(ICO != "Error") %>% 
  separate(
    ICO,c("ICO","foreigner"), sep = "_"
  ) %>% 
  pull(ICO)


# Load Orbis data
orbis <- read_excel("DATAlocal/orbis_sample.xlsx", sheet = 2)
orbis <- orbis %>% 
  select(
    mail = `E-mail address`,
    ICO = `National ID`
  ) %>% 
  # Kick out companies without email
  drop_na() %>% 
  distinct(.keep_all = TRUE) %>% 
  # Kick out companies used in the pilot
  filter(!(ICO %in% exclude))

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

# companies %>% 
#   mutate(
#     NACE2 = str_sub(NACE,1,2)
#   ) %>% 
#   left_join(.,nace) %>% 
#   pull(sector) %>% table()

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
          NAME_CZ = rep(LETTERS[1:3],200/2)[1:200]
        )
    }
  ) %>%
  slice_sample(n = Ntotal, replace = FALSE) %>% 
  split(.$WEEK) %>% 
  map_dfr(
    function(x){
      x %>%
        mutate(
          NAME_FOR = rep(LETTERS[1:3],200/2)[1:200]
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
  

load("DATAlocal/occtable.RData")

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
    case = 1:2,
    subject = sample(c("Hledám práci","Hledám zaměstnání"), 2, replace = FALSE),
    text = sample(1:2, 2, replace = FALSE),
    position = sample(c("v1","v2"), 2, replace = FALSE)
  ) %>% 
  ungroup() %>% 
  select(-Ntotal,-N) %>% 
  mutate(
    date = Sdate + (WEEK-1)*7 + (date-1)
  ) %>% 
  mutate(
    date = if_else(date < as_date("2022-3-22"), as_date("2022-3-24"), date),
    date = if_else(date == as_date("2022-3-22"), as_date("2022-3-25"), date),
    date = if_else(date == as_date("2022-3-23"), as_date("2022-3-25"), date)
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

# Evgeniya.Sergeeva729@outlook.com
# Galina.Goncharova216@outlook.com
# Evgenia.Guseva794@outlook.com
# Irina.Grigoreva908@outlook.com
# Svetlana.Gavrilova178@outlook.com

RU <- tribble(
  ~NAME_sender, ~mail_sender, ~NAME_FOR,
"Evgeniya Sergeeva", "Evgeniya.Sergeeva729@gmail.com","A",
"Galina Goncharova", "Galina.Goncharova216@gmail.com","B",
"Evgenia Guseva", "Evgenia.Guseva795@gmail.com","C"
# "Irina Grigoreva", "Irina.Grigoreva908@outlook.com","D",
# "Svetlana Gavrilova", "Svetlana.Gavrilova178@outlook.com","E"
) %>% 
  bind_rows(
    .,.
  ) %>% 
  mutate(
    IDENTITY = c(rep("B",3),rep("D",3))
  )

# olha.shevchenko831@outlook.com
# evhenyia.tkachenko308@outlook.com
# Zhanna.Marchenko671@outlook.com
# Anzheia.Kharchenko408@outlook.com

UA <- tribble(
  ~NAME_sender, ~mail_sender, ~NAME_FOR,
  #"Yryna Hordiyenko", "yryna.hordiyenko903@outlook.com","A",
  "Olha Shevchenko", "olha.shevchenko831@gmail.com","A",
  "Zhanna Marchenko", "Zhanna.Marchenko672@gmail.com","B",
  #"Evhenyia Tkachenko", "evhenyia.tkachenko308@outlook.com","D",
  "Anzheia Kharchenko", "Anzheia.Kharchenko408@gmail.com","C"
) %>% 
  bind_rows(
    .,.
  ) %>% 
  mutate(
    IDENTITY = c(rep("A",3),rep("C",3))
  )

NAMES <- bind_rows(RU,UA)

# katerina.novakova448@outlook.com
# lenka.horakova738@outlook.com
# marie.vesela472@outlook.com
# lucie.dvorakova565@outlook.com
# vera.svobodova817@outlook.com
# yryna.hordiyenko903@outlook.com

CZ <- tribble(
  ~NAME_sender, ~mail_sender, ~NAME_CZ,
  "Kateřina Nováková", "katerina.novakova448@outlook.com","A",
  "Lenka Horáková", "lenka.horakova738@outlook.com","B",
  #"Lucie Dvořáková", "lucie.dvorakova565@outlook.com","C",
  "Marie Veselá", "marie.vesela472@outlook.com","C"
  #"Věra Svobodová", "vera.svobodova817@outlook.com","E"
)

schedule <- bind_rows(
  schedule %>% 
    filter(ORDER == case) %>% 
    left_join(.,NAMES),
  schedule %>% 
    filter(ORDER != case) %>% 
    left_join(.,CZ)
) %>% 
  arrange(date,ICO) %>% 
  mutate(
    foreigner = ORDER == case
  )


mail_1_foreign_perm <- "Dobrý den,\n
Hledám práci a chtěla jsem se zeptat, jestli hledáte někoho, kdo by se k vám přidal. Můžu dělat operátorku strojů, ale třeba i ve skladu nebo různě pomáhat.\n\n
Jsem sice z YYY, ale mám tu trvalý pobyt, takže můžu začít hned pracovat.\n
Nemám děti, takže můžu pracovat na směny nebo o víkendech.\n\n
S pozdravem,\n
NNN"


mail_1_foreign <- "Dobré odpoledne,\n
Hledám práci a chtěla jsem se zeptat, jestli hledáte někoho, kdo by se k vám přidal. Můžu dělat operátorku strojů, ale třeba i ve skladu nebo různě pomáhat.\n\n
Právě jsem přijel do České republiky z Ukrajiny a mohu začít hned pracovat.\n
Nemám děti, takže můžu pracovat na směny nebo o víkendech.\n
Píši v YYY pomocí Překladače Google, takže doufám, že v textu nejsou žádné chyby.\n\n
S pozdravem,\n
NNN"


mail_1_cz <- "Dobrý den,\n
hledám práci a chtěla jsem se zeptat, jestli u Vás někoho nehledáte. Můžu dělat operátorku strojů, ale třeba i ve skladu nebo různě pomáhat.\n\n
Můžu hned nastoupit a nemám děti takže můžu pracovat i na směny nebo o víkendu.\n\n
S pozdravem,\n
NNN"

################################################################################

mail_2_foreign <- "Dobré odpoledne,\n
Mám zájem o místo a chtěla jsem se zeptat, jestli někoho nepotřebujete. Můžu obsluhovat stroje, ale klidně můžu dělat i ve skladu nebo pomáhat s jinou prací.\n\n
Právě jsem přijel do České republiky z Ukrajiny a mohu začít hned pracovat.\n
Nevadí mi práce na směny. Můžu si vzít i víkendy, protože nemám děti.\n
Píši v YYY pomocí Překladače Google, takže doufám, že v textu nejsou žádné chyby.\n\n
Zdraví,\n
NNN"


mail_2_foreign_perm <- "Dobrý den,\n
Mám zájem o místo a chtěla jsem se zeptat, jestli někoho nepotřebujete. Můžu obsluhovat stroje, ale klidně můžu dělat i ve skladu nebo pomáhat s jinou prací.\n\n
Jsem sice z YYY, ale mám tu trvalý pobyt, takže můžu začít hned pracovat.\n
Nevadí mi práce na směny. Můžu si vzít i víkendy, protože nemám děti.\n\n
Zdraví,\n
NNN"

mail_2_cz <- "Dobrý den,\n
Mám zájem o místo a chtěla jsem se zeptat, jestli někoho nepotřebujete. Můžu obsluhovat stroje, ale klidně můžu dělat i ve skladu nebo pomáhat s jinou prací.\n\n
Práce na směny mi nevadí. Můžu i víkendy protože nemám děti. Začít pracovat bych mohla hned.\n\n
Zdraví,\n
NNN"


isco <- read_tsv("DATAlocal/isco_names_utf8.tsv")
isco <- isco %>% 
  mutate(
    ISCO = as.character(ISCO)
  ) %>% 
  select(-v1,-v2) %>% 
  rename(v1 = v3, v2 = v4) %>% 
  pivot_longer(-ISCO, names_to = "position", values_to = "position_desc")


MAILS <- tribble(
  ~IDENTITY, ~text, ~mail_text,
  "A",1,mail_1_foreign,
  "A",2,mail_2_foreign,
  "B",1,mail_1_foreign,
  "B",2,mail_2_foreign,
  "C",1,mail_1_foreign_perm,
  "C",2,mail_2_foreign_perm,
  "D",1,mail_1_foreign_perm,
  "D",2,mail_2_foreign_perm,
  "CZ",1,mail_1_cz,
  "CZ",2,mail_2_cz
)

schedule <- schedule %>% 
  mutate(
    IDENTITY = ifelse(foreigner,IDENTITY,"CZ")
  ) %>% 
  left_join(MAILS) %>% 
  left_join(.,isco) %>% 
  mutate(
    language = case_when(
      IDENTITY == "A" ~ "ukrajinštině",
      IDENTITY == "B" ~ "ruštině",
      IDENTITY == "C" ~ "Ukrajiny",
      IDENTITY == "D" ~ "Ruska",
      IDENTITY == "CZ" ~ ""
    )
  ) %>% 
  mutate(
    mail_text = str_replace_all(mail_text,"YYY",language),
    mail_text = str_replace_all(mail_text,"NNN",NAME_sender),
    mail_text = str_replace_all(mail_text,"XXX",position_desc)
  )

save(schedule, file = "schedule.RData")