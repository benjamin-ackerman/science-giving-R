require(RCurl)
library(dplyr)
library(ggplot2)
library(stringr)
library(rebus)

link = "https://media.githubusercontent.com/media/fivethirtyeight/data/master/science-giving/science_federal_giving.csv"
data = read.csv(text = getURL(link),stringsAsFactors = FALSE)

data_cleaned = data %>% 
  mutate_all(funs(ifelse(. %in% c("","NULL"),NA,.)))


president_funding = data_cleaned %>% 
  mutate(transaction_amt = as.numeric(transaction_amt)) %>% 
  filter(cand_pty_affiliation %in% c("DEM","REP","LIB","IND"),
         cycle == "2016",
         cand_office == "P",
         cand_status == "C") %>% 
  group_by(state,cand_pty_affiliation,cand_name,classification) %>% 
  summarise(money = sum(transaction_amt,na.rm=TRUE))

president_funding %>% 
  group_by(cand_name,classification) %>% 
  summarise(mon = sum(money)) %>% 
  arrange(desc(mon)) %>% 
  ggplot()+geom_bar(aes(x = reorder(cand_name,mon),y=mon,fill=classification),stat='identity') + coord_flip()+  scale_y_continuous(expand=c(0,0)) + theme(legend.position = c(0.8, 0.15),legend.background = element_rect(fill="white"))



us <- map_data("state")

