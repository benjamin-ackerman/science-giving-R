### Get info on all party funding to figure out how much of it is due to scientists
all_funding = function(state_district){
  all_donations_link = paste0("https://www.opensecrets.org/races/summary.csv?cycle=2016&id=",state_district)
  if(getURL(all_donations_link) == "\n"){
    all_donations = data.frame(cand_office_st = substr(state_district,1,2), cand_office_district = substr(state_district,3,4),D = NA, R = NA, winner = NA)
    return(all_donations)
  }
  else{
    data = read.csv(text = getURL(all_donations_link),stringsAsFactors=FALSE)
    
    winner = data$FirstLastP[which(data$Result == "W")]
    
    all_donations = suppressMessages(
      read.csv(text = getURL(all_donations_link),stringsAsFactors=FALSE) %>% 
        mutate(party = str_match(FirstLastP, OPEN_PAREN %R% capture(UPPER))[,2]) %>% 
        filter(party %in% c("D","R")) %>% 
        group_by(party) %>% 
        summarise(party_earnings = sum(Rcpts,na.rm=TRUE)) %>% 
        cast(~party) %>% 
        mutate(D = ifelse("D" %in% names(.), D, NA),
               R = ifelse("R" %in% names(.), R, NA),
               cand_office_st = substr(state_district,1,2), 
               cand_office_district = substr(state_district,3,4),
               winner = winner) %>% 
        select(-value,cand_office_st, cand_office_district,D,R,winner))
    
    return(all_donations)}
}

all_funding_data = congress_funding$ID_name %>%
  map_df(~all_funding(.))