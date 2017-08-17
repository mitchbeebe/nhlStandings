library(rvest)
library(httr)
library(stringr)
library(tidyverse)
library(jsonlite)

# Function to determine finish for final standings
getFinish <- function(name) {
    name %>% str_extract(" \\w{1,2}$") %>% str_trim()
}

# Determine top 8 teams and join back to the conference tables
topEight <- function(data) {
    data %>% 
        arrange(desc(P),GP) %>% 
        filter(row_number(P) <= 8) %>% 
        select(Team) %>% 
        mutate(novPO = 1) %>% 
        right_join(data, by = c("Team" = "Team"))
}

getStandings <- function(season, asOf = c("Final","Nov1")){
    
    # Due to NHL lockout year 2004-2005, the function will throw and error if the season parameter
    # is set to 2005
    if(season == 2005) stop("2005 was a lockout year. Please select another year.")
    
    # Determine what link, season (if necessary), and table index (used in html_table()) to use
    # Note: If you input 2016 for the season parameter, that indicates you want the 2015-2016 season
    #       therefore your Nov1 standings will be in 2015
    if(asOf == "Final") {
        link <- "http://dropyourgloves.com/Games/AnyDateStandings.aspx?League=1&Season=%s"
        i <- 1
    }
    else {
        link <- "http://dropyourgloves.com/Games/AnyDateStandings.aspx?Date=%s-11-1&League=1"
        season <- season - 1
        i <- 2
    }
    
    if(season < 2005) cols <- "c(1:5,7:9)" %>% parse(text = .) %>% eval()
    else cols <- "c(1:8)" %>% parse(text = .) %>% eval()
    
    # Webscrape the link and desired season
    xmlTable <- sprintf(link, season) %>%
        GET(.,add_headers(Referer = sprintf(link, season))) %>% 
        content(., encoding = "UTF-8") %>% 
        html_nodes("table")
    
    # Extract the table from the appropriate table index
    table <- html_table(xmlTable[i],fill=T)[[1]][,cols]
    
    # Rename columns
    colnames(table) <- c("Team","GB","GP","WOT","LOT","GF","GA","P")
    
    # Remove lines containing "Team" or "NHL"
    table <- table[!str_detect(table[,1],"Team|NHL"),]
    
    # Convert Points to numeric and remove "Stanley Cup" from team name if needed
    table <- table %>% 
        mutate(P = as.numeric(P),
               Team = Team %>% str_replace("Stanley Cup","F"))
    
    # If the table is for Nov1 standings, figure out the top 8 teams for the East and West
    if(asOf == "Nov1") {
        # Determine what rows have the conference cutoffs
        confRows <- grep("Conference", table[,1])
        
        # Separate east and west
        east <- table[(confRows[1]+1):(confRows[2]-1),]
        west <- table[(confRows[2]+1):nrow(table),]
        
        # Determine top 8 teams and join back to the conference tables
        east <- topEight(east)
        
        west <- topEight(west)
        
        table <- rbind(east, west) %>% 
            mutate(Season = season + 1) %>% 
            select(Team, Season, novPO) %>% 
            replace_na(list(novPO = 0))
        
    }
    # If the table is for final standings, determine the finishing place of the teams
    else {
        table <- table %>% 
            mutate(Finish = getFinish(Team),
                   Team = str_extract(Team,".+[^ R1|QF|SF|F]"),
                   finalPO = ifelse(is.na(Finish),0,1),
                   Season = season) %>% 
            select(Team,Season,P,finalPO,Finish) %>% 
            replace_na(list(Finish = "E"))
    }
    
    return(table)
}

# Vector of seasons to pull standings
seasons <- c(1990:2004,2006:2016)

# Apply getStandings() to seasons of interest for November standings
novStandings <- seasons %>% 
    lapply(function(i) getStandings(i,"Nov1"))

# Apply getStandings() to seasons of interest for final standings
finalStandings <- seasons %>% 
    lapply(function(i) getStandings(i,"Final"))

# Function to join November standings to final standings
joinStandings <- function(nov,final) {
    standingsTbl <- final %>% 
        left_join(nov, by = c("Team" = "Team", 
                              "Season" = "Season")) %>% 
        select(Team, Season, P, novPO, finalPO, Finish)
}

# Apply join function to our standings lists
allStandings <- seasons %>% 
    length() %>% 
    seq() %>% 
    lapply(function(i) joinStandings(novStandings[[i]], finalStandings[[i]]))

save(allStandings,file = "./nhlStandings.Rdata")





