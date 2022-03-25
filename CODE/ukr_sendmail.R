#!/usr/bin/env Rscript

library(methods)
library(tidyverse)
library(lubridate)
library(curl)

rm(list = ls())

# Schedule table
load("schedule.RData")
today <- Sys.Date()

# # To be removed
# schedule <- schedule %>% 
#   mutate(
#     mail = "stepan.mikula@gmail.com"
#   )
# today <- Sys.Date() + 1
# # ...END


        
exclude <- tribble(
  ~ID,
  "27765857_TRUE",
  "03502341_FALSE",
  "27656331_FALSE",
  "49900501_FALSE",
  "25171178_TRUE",
  "25372670_FALSE"
) %>% 
  separate(
    ID,c("ICO","foreigner"), sep = "_"
  ) %>% 
  mutate(
    foreigner = foreigner == "TRUE"
  )


schedule <- schedule %>% 
  filter(date == today) %>%
  anti_join(.,exclude) %>% 
  select(ICO,foreigner,mail,mail_sender,subject,NAME_sender,mail_text)




#### Functions

send_mail <- function(ICO,foreigner,mail_sender,NAME_sender,subject,mail_text,mail){
  
  Sys.sleep(20)
  
  # Create email message (msg) in RFC2822 format:
  msg_text <- mail_text
  
  msg_header <- str_c(
    "From: \"",NAME_sender,"\" <",mail_sender,">\n",
    "To: <",mail,">\n",
    #"bcc: <stepan.mikula@econ.muni.cz>\n",
    "Subject: ",subject,"\n",
    "MIME-Version: 1.0\n",
    "Content-Transfer-Encoding: 8bit\n",
    "Content-Type: text/plain;charset=utf-8\n\n"
  )
  
  msg <- str_c(msg_header,msg_text)
  
  out <- curl::send_mail(mail_sender, 
                         mail, #"explab2021@gmail.com"
                         msg,
                         smtp_server = 'smtp://smtp-mail.outlook.com:587',
                         password  = "UKRrefPR_22", #"MuniUK21",
                         username = mail_sender
  )
  
  # Create log
  tibble(
    ICO = str_c(ICO,"_",foreigner),
    sent_time = Sys.time(),
    curl_out = list(out)
  )
}

send_mail_possibly <- possibly(send_mail,
                               tibble(
                                 ICO = "Error",
                                 sent_time = Sys.time(),
                                 curl_out = NA
                               )
)

out <- schedule %>% 
  #filter(email_batch == Sys.Date()) %>% 
  select(ICO,foreigner,mail_sender,NAME_sender,subject,mail_text,mail) %>% 
  pmap(send_mail_possibly) %>% 
  bind_rows()

dtime <- Sys.time() %>% as.character() %>% str_remove_all(":") %>% str_replace_all(" ","_")

save(out, file = str_c("log_out_",dtime,".RData"))
