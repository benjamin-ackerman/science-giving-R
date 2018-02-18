devtools::install_github("tidyverse/ggplot2")
require(RCurl)
library(reshape)
library(dplyr)
library(ggplot2)
library(stringr)
library(rebus)
library(maps)
library(USAboundaries)

### Download the data 
link = "https://media.githubusercontent.com/media/fivethirtyeight/data/master/science-giving/science_federal_giving.csv"
data = read.csv(text = getURL(link),stringsAsFactors = FALSE)

### Clean the data
data_cleaned = data %>% 
  mutate_all(funs(ifelse(. %in% c("","NULL"),NA,.)))

### Summarise donations to 2016 presidential candidates by state and science classification group
president_funding = data_cleaned %>% 
  mutate(transaction_amt = as.numeric(transaction_amt)) %>% 
  filter(cand_pty_affiliation %in% c("DEM","REP","LIB","IND"),
         cycle == "2016",
         cand_office == "P",
         cand_status == "C") %>% 
  group_by(state,cand_pty_affiliation,cand_name,classification) %>% 
  summarise(money = sum(transaction_amt,na.rm=TRUE))


### See the top 10 candidates who received funding from scientists by classification
top_10_funded = president_funding %>% 
  group_by(cand_name) %>% 
  summarise(total_donations = sum(money)) %>% 
  ungroup() %>% 
  top_n(10,wt = total_donations)

president_funding %>% 
  group_by(cand_name,classification) %>% 
  summarise(mon = sum(money)/1000000) %>% 
  right_join(top_10_funded,by="cand_name")%>%
  ggplot()+geom_bar(aes(x = reorder(cand_name,mon),y=mon,fill=classification),stat='identity') + 
  coord_flip() +  scale_y_continuous(expand=c(0,0)) + 
  theme(legend.position = c(0.8, 0.15),legend.background = element_rect(fill="white")) +
  labs(title="Campaign Donations by Scientists to 2016 Presidential Candidates", x = "Presidential Candidate",y = "Total Donations Received ($, millions)")


##### Funding for Congress Races in 2016
congress_map = us_congressional() %>% 
  mutate(cand_office_district = str_replace(cd115fp, START %R% "0",""),
         state_name = tolower(state_name)) %>% 
  dplyr::rename(cand_office_st = state_abbr) %>% 
  select(cand_office_st,cand_office_district,state_name,geometry)

congress_funding = data_cleaned %>% 
  mutate(transaction_amt = as.numeric(transaction_amt)) %>% 
  filter(cand_pty_affiliation %in% c("DEM","REP"),
         cycle == "2016",
         cand_office == "H",
         cand_status == "C")%>% 
  group_by(cand_office_st,cand_pty_affiliation,cand_office_district) %>% 
  summarise(money = sum(transaction_amt,na.rm=TRUE)) %>% 
  cast(cand_office_st + cand_office_district ~ cand_pty_affiliation, value="money") %>% 
  filter(!is.na(cand_office_district)) %>% 
  mutate_all(funs(ifelse(is.na(.),0,.))) %>% 
  mutate(party_diff = DEM - REP) %>% 
  right_join(congress_map,by=c("cand_office_st","cand_office_district"))


congress_funding %>% 
  group_by(state_name) %>% 
  summarise(sums = sum(DEM,REP,na.rm=TRUE))

congress_funding %>% 
  filter(!cand_office_st %in% c("HI","AK")) %>% 
  ggplot() + geom_sf(aes(fill=party_diff)) +
  scale_fill_gradient2("Donation Diffs ($)",low = "darkred", mid = "white", high = "darkblue")+
  theme_void()+theme(panel.grid.major = element_line(colour = "white"))


congress_donations <- function(state){
  if(state == "usa"){
    return(congress_funding %>% 
             filter(!cand_office_st %in% c("HI","AK")) %>% 
             ggplot() + geom_sf(aes(fill=party_diff)) +
             scale_fill_gradient2("Donation Diffs ($)",low = "darkred", mid = "white", high = "darkblue")+
             theme_void()+theme(panel.grid.major = element_line(colour = "white"))+
             labs(title = "Partisan Differences in Scientist Donations to 2016 Congressional Races"))
  }
  else{
  return(congress_funding %>% 
    filter(state_name %in% state) %>% 
    ggplot() + geom_sf(aes(fill=party_diff)) +
    scale_fill_gradient2("Donation Diffs ($)",low = "darkred", mid = "white", high = "darkblue")+
    theme_void()+theme(panel.grid.major = element_line(colour = "white"))+
      labs(title = paste0("Partisan Differences in Scientist Donations to 2016 Congressional Races in ",state)))}
}

congress_donations("usa")
