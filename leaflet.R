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
  mutate(party_diff = DEM - REP) %>% #,
         #cand_office_district = ifelse(cand_office_district == 0, 1, cand_office_district)) %>% 
  left_join(state.fips %>% 
              select(cand_office_st = abb, fips) %>% 
              filter(!duplicated(fips)),by="cand_office_st") %>% 
  mutate(fips = ifelse(nchar(fips)== 1, paste0("0",fips),fips),
         cand_office_district = ifelse(nchar(cand_office_district)== 1, paste0("0",cand_office_district),cand_office_district),
         ID = paste(fips,cand_office_district,sep="_"),
         cand_office_district = str_replace(cand_office_district,"00","01"),
         ID_name = paste0(cand_office_st,cand_office_district))

### Get info on all party funding to figure out how much of it is due to scientists
all_funding = function(state_district){
  all_donations_link = paste0("https://www.opensecrets.org/races/summary.csv?cycle=2016&id=",state_district)
  
  all_donations = read.csv(text = getURL(all_donations_link),stringsAsFactors=FALSE) %>% 
    mutate(party = str_match(FirstLastP, OPEN_PAREN %R% capture(UPPER))[,2]) %>% 
    group_by(party) %>% 
    summarise(party_earnings = sum(Rcpts,na.rm=TRUE)) %>% 
    cast(~party) %>% 
    mutate(cand_office_st = substr(state_district,1,2), cand_office_district = substr(state_district,3,4)) %>% 
    select(-value)
  
  return(all_donations)
}


all_funding_data = congress_funding$ID_name[1:10] %>% 
  map_df(~all_funding(.))


districts = congressional_districts(cb=TRUE,year = 2016)
districts$ID = paste(districts$STATEFP,districts$CD115FP,sep="_")

data = geo_join(districts, congress_funding, 'ID', 'ID', how = 'left')

test1 = colorRampPalette(c("blue","white"))
pal1 <- colorBin(test1(5), domain = c(0,max(data$party_diff,na.rm=TRUE)), bins = 5,na.color = NA)

test = colorRampPalette(c("red","white","blue"))
pal2 <- colorBin(test(10), domain = c(min(data$party_diff,na.rm=TRUE),90000), bins = 10,na.color=NA)

leaflet(data) %>%
  setView(lng = -98.5, lat =39.5,zoom=4) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal2(party_diff),
              opacity=0.5,
              weight = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = .9)




addLegend(position = "bottomleft",
            colors = c("blue", "red"), 
            labels = c("Democrat", "Republican"), 
            opacity = 1, 
            title = "Texas House of Representatives")
