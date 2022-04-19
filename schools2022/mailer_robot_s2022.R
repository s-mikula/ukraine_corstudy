#!/usr/bin/env Rscript

library(methods)
library(tidyverse)
library(lubridate)
library(curl)

rm(list = ls())

# Schedule table
load("schedule2022.RData")
#load("DATAgit/schedule.RData")

# Remove previously used ic

lof <- list.files(pattern = "log_out") 

get_ic <- function(l){
  load(l)
  out$ic
}

ic_exclude <- lof %>% map(get_ic) %>% unlist() %>% unique()

schedule <- schedule %>% 
  filter(!(ic %in% ic_exclude))


#### Tests (delete before release)
schedule <- schedule %>% group_by(email_sender) %>% slice_sample(n = 1) %>% ungroup()
schedule$email_batch <- Sys.Date()
schedule$daytime <- "afternoon"
schedule$email <- "explab2021@gmail.com" #explab2021@gmail.com
####


### Prepare messages

lHl1gMcM <- "Vážený pane řediteli,\n\n
mám syna, který má v září nastoupit do první třídy a hledám pro něj školu. Chtěla jsem se zeptat, 
zda ještě budete pořádat nějakou informační schůzku pro rodiče nebo den otevřených dveří.\n\n
S pozdravem,\n\n"

lHl1gMcF <- "Vážený pane řediteli,\n\n
mám dceru, která má v září nastoupit do první třídy a hledám pro ni školu. Chtěla jsem se zeptat, 
zda ještě budete pořádat nějakou informační schůzku pro rodiče nebo den otevřených dveří.\n\n
S pozdravem,\n\n"

lHl1gFcM <- "Vážená paní ředitelko,\n\n
mám syna, který má v září nastoupit do první třídy a hledám pro něj školu. Chtěla jsem se zeptat, 
zda ještě budete pořádat nějakou informační schůzku pro rodiče nebo den otevřených dveří.\n\n
S pozdravem,\n\n"

lHl1gFcF <- "Vážená paní ředitelko,\n\n
mám dceru, která má v září nastoupit do první třídy a hledám pro ni školu. Chtěla jsem se zeptat, 
zda ještě budete pořádat nějakou informační schůzku pro rodiče nebo den otevřených dveří.\n\n
S pozdravem,\n\n"

lHl2gMcM <- "Dobrý den pane řediteli,\n\n
hledám školu pro svého syna. Zajímalo by mě, zda ještě budete pořádat informační schůzku pro 
rodiče nebo den otevřených dveří. Ráda bych se před zápisem do první třídy se školou blíže seznámila.\n\n
S pozdravem,\n\n"

lHl2gMcF <- "Dobrý den pane řediteli,\n\n
hledám školu pro svou dceru. Zajímalo by mě, zda ještě budete pořádat informační schůzku pro 
rodiče nebo den otevřených dveří. Ráda bych se před zápisem do první třídy se školou blíže seznámila.\n\n
S pozdravem,\n\n"

lHl2gFcM <- "Dobrý den paní ředitelko,\n\n
hledám školu pro svého syna. Zajímalo by mě, zda ještě budete pořádat informační schůzku pro 
rodiče nebo den otevřených dveří. Ráda bych se před zápisem do první třídy se školou blíže seznámila.\n\n
S pozdravem,\n\n"

lHl2gFcF <- "Dobrý den paní ředitelko,\n\n
hledám školu pro svou dceru. Zajímalo by mě, zda ještě budete pořádat informační schůzku pro 
rodiče nebo den otevřených dveří. Ráda bych se před zápisem do první třídy se školou blíže seznámila.\n\n
S pozdravem,\n\n"

lLl1gMcM <- "Dobrý den pan ředitel,\n\n
Mám syna co by mněl nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"

lLl1gMcF <- "Dobrý den pan ředitel,\n\n
Mám dceru co by mněla nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"

lLl1gFcM <- "Dobrý den paní ředitelka,\n\n
Mám syna co by mněl nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"

lLl1gFcF <- "Dobrý den paní ředitelka,\n\n
Mám dceru co by mněla nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"

lLl2gMcM <- "Vážený páne ředitel,\n\n
Hledám školu pri syna. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svojeho syna před zapsáním do první.\n\n
s pozdravem.\n\n"

lLl2gMcF <- "Vážený páne ředitel,\n\n
Hledám školu pri dceru. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svoju dceru před zapsáním do první.\n\n
s pozdravem.\n\n"

lLl2gFcM <- "Vážená páni ředitelka,\n\n
Hledám školu pri syna. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svojeho syna před zapsáním do první.\n\n
s pozdravem.\n\n"

lLl2gFcF <- "Vážená páni ředitelka,\n\n
Hledám školu pri dceru. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svoju dceru před zapsáním do první.\n\n
s pozdravem.\n\n"


# Low literacy, settled, letter 1

lLl1gMcF_settled <- "Dobrý den pan ředitel,\n\n
Po roku jsme se přistihovali z Česká třebova a mám dceru co by mněla nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"

lLl1gMcM_settled <- "Dobrý den pan ředitel,\n\n
Po roku jsme se přistihovali z Česká třebova a mám syna co by mněl nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"

lLl1gFcF_settled <- "Dobrý den paní ředitelka,\n\n
Po roku jsme se přistihovali z Česká třebova a mám dceru co by mněla nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"

lLl1gFcM_settled <- "Dobrý den paní ředitelka,\n\n
Po roku jsme se přistihovali z Česká třebova a mám syna co by mněl nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"


# Low literacy, settled, letter 2

lLl2gMcF_settled <- "Vážený páne ředitel,\n\n
Hledám školu pri dceru, pritože po roce jsem se přistěhovali z Třebíči. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svoju dceru před zapsáním do první.\n\n
s pozdravem.\n\n"

lLl2gMcM_settled <- "Vážený páne ředitel,\n\n
Hledám školu pri syna, pritože po roce jsem se přistěhovali z Třebíči. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svoju dceru před zapsáním do první.\n\n
s pozdravem.\n\n"

lLl2gFcF_settled <- "Vážený páni ředitelka,\n\n
Hledám školu pri dceru, pritože po roce jsem se přistěhovali z Třebíči. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svoju dceru před zapsáním do první.\n\n
s pozdravem.\n\n"

lLl2gFcM_settled <- "Vážený páni ředitelka,\n\n
Hledám školu pri syna, pritože po roce jsem se přistěhovali z Třebíči. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svoju dceru před zapsáním do první.\n\n
s pozdravem.\n\n"

# Low literacy, refugee, letter 1

lLl1gMcF_refugee <- "Dobrý den pan ředitel,\n\n
Přišli jseme z Ukrajiny a mám dceru co by mněla nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"

lLl1gMcM_refugee <- "Dobrý den pan ředitel,\n\n
Přišli jseme z Ukrajiny a mám syna co by mněl nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"

lLl1gFcF_refugee <- "Dobrý den paní ředitelka,\n\n
Přišli jseme z Ukrajiny a mám dceru co by mněla nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"

lLl1gFcM_refugee <- "Dobrý den paní ředitelka,\n\n
Přišli jseme z Ukrajiny a mám syna co by mněl nastoupit letos rok do první třídy. Hledám školu ale potřebovala bysem více 
informaci , abych mohla se rozhodnout  Chtěla jsem vědět , jestli ješte bude rodičovska infromačný 
porada nebo den otevřených dveři?\n\n
S pozdravem\n\n"

# Low literacy, refugee, letter 2

lLl2gMcF_refugee <- "Vážený páne ředitel,\n\n
Hledám školu pri dceru protoze jsem přišli z Ukrajiny. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svoju dceru před zapsáním do první.\n\n
s pozdravem.\n\n"

lLl2gFcM_refugee <- "Vážený páni ředitelka,\n\n
Hledám školu pri syna protoze jsem přišli z Ukrajiny. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svoju dceru před zapsáním do první.\n\n
s pozdravem.\n\n"

lLl2gFcF_refugee <- "Vážený páni ředitelka,\n\n
Hledám školu pri dceru protoze jsem přišli z Ukrajiny. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svoju dceru před zapsáním do první.\n\n
s pozdravem.\n\n"

lLl2gMcM_refugee <- "Vážený páne ředitel,\n\n
Hledám školu pri syna protoze jsem přišli z Ukrajiny. Přemýšlela sjem ,pokud budete organizovat nějaká den otevřených dveří 
ve škole nebo informační schůze? Bych ráda víc poznala školu pro svoju dceru před zapsáním do první.\n\n
s pozdravem.\n\n"






MAILS <- tribble(
  ~t_child,	~t_literacy,	~t_letter,	~t_gender, ~t_refugee, ~body,
  "female",	"high",	1,	"Male", "na",   lHl1gMcF,
  "male",	"low",	1,	"Male",   "na",  lLl1gMcM,
  "female",	"high",	2,	"Male",  "na", lHl2gMcF,
  "male",	"low",	1,	"Female",   "na", lLl1gFcM,
  "male",	"low",	2,	"Female",   "na", lLl2gFcM,
  "female",	"high",	1,	"Female", "na", lHl1gFcF,
  "male",	"high",	1,	"Female",   "na", lHl1gFcM,
  "female",	"low",	1,	"Female", "na", lLl1gFcF,
  "male",	"high",	1,	"Male",     "na", lHl1gMcM,
  "female",	"high",	2,	"Female", "na", lHl2gFcF,
  "male",	"high",	2,	"Female",   "na", lHl2gFcM,
  "female",	"low",	2,	"Female", "na", lLl2gFcF,
  "male",	"low",	2,	"Male",   "na",  lLl2gMcM,
  "female",	"low",	1,	"Male", "na",  lLl1gMcF,
  "female",	"low",	2,	"Male",  "na",  lLl2gMcF,
  "male",	"high",	2,	"Male",  "na",   lHl2gMcM,
  # Added in 2022
  "female",	"low",	1,	"Male", "settled", lLl1gMcF_settled,
  "male",	"low",	1,	"Female",   "settled",  lLl1gFcM_settled,
  "female",	"low",	1,	"Female", "settled", lLl1gFcF_settled,
  "male",	"low",	1,	"Male",   "settled",  lLl1gMcM_settled,
  "female",	"low",	2,	"Male", "settled", lLl2gMcF_settled,
  "male",	"low",	2,	"Female",   "settled",  lLl2gFcM_settled,
  "female",	"low",	2,	"Female", "settled", lLl2gFcF_settled,
  "male",	"low",	2,	"Male",   "settled",  lLl2gMcM_settled,
  "male",	"low",	1,	"Male", "refugee", lLl1gMcM_refugee,
  "female",	"low",	1,	"Female",   "refugee",  lLl1gFcF_refugee,
  "female",	"low",	1,	"Male", "refugee", lLl1gMcF_refugee,
  "male",	"low",	1,	"Female",   "refugee",  lLl1gFcM_refugee,
  "male",	"low",	2,	"Male", "refugee", lLl2gMcM_refugee,
  "female",	"low",	2,	"Female",   "refugee",  lLl2gFcF_refugee,
  "female",	"low",	2,	"Male", "refugee", lLl2gMcF_refugee,
  "male",	"low",	2,	"Female",   "refugee",  lLl2gFcM_refugee
)



#### Functions

send_mail <- function(ic,ethnicity,name_sender,email_sender,child,literacy,letter,gender,email,refugee, t_MAILS = MAILS){
  
  Sys.sleep(20)
  
  # Create email message (msg) in RFC2822 format:
  msg_text <- t_MAILS %>% 
    filter(
      t_child == child,
      t_literacy == literacy,
      t_letter == letter,
      t_gender == gender,
      t_refugee == refugee
    ) %>% 
    pull(body)
  
  msg_text <- msg_text[1] %>% str_c(.,name_sender)
  
  msg_header <- str_c(
    "From: \"",name_sender,"\" <",email_sender,">\n",
    "To: <",email,">\n",
    #"bcc: <stepan.mikula@gmail.com>\n",
    "Subject: Zápis do školy\n",
    "MIME-Version: 1.0\n",
    "Content-Transfer-Encoding: 8bit\n",
    "Content-Type: text/plain;charset=utf-8\n\n"
  )
  
  msg <- str_c(msg_header,msg_text)
  
  out <- curl::send_mail(email_sender, 
                         email, #"explab2021@gmail.com"
                         msg,
                         smtp_server = 'smtps://smtp.gmail.com:465',
                         password  = "MuniUK21", #"MuniUK21",
                         username = email_sender
  )
  
  # Create log
  tibble(
    ic = ic,
    sent_time = Sys.time(),
    curl_out = list(out)
  )
}

send_mail_possibly <- possibly(send_mail,
                               tibble(
                                 ic = "Error",
                                 sent_time = Sys.time(),
                                 curl_out = NA
                               )
)

current_daytime <- ifelse(hour(Sys.time()) < 12, "morning", "afternoon")

out <- schedule %>% 
  filter(email_batch == Sys.Date()) %>% 
  filter(daytime == current_daytime) %>% 
  group_by(email_sender) %>% 
  mutate(
    nord = row_number()
  ) %>% 
  ungroup() %>% 
  arrange(nord,psc) %>% 
  select(ic,ethnicity,name_sender,email_sender,child,literacy,letter,gender,refugee,email) %>% 
  pmap(send_mail_possibly) %>% 
  bind_rows()

dtime <- Sys.time() %>% as.character() %>% str_remove_all(":") %>% str_replace_all(" ","_")

save(out, file = str_c("log_out_",dtime,".RData"))
