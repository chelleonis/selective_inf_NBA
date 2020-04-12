#data loading and cleaning
library(nbastatR)
library(dplyr)

library(tidyr)

plan(multiprocess)
'%notin%' <- Negate('%in%')

#test <- game_logs(seasons = 2018, league = "NBA")

# nba 2k player rating
twoK <- read.csv("C:/Users/typer321/Desktop/nba2k ratings.csv") %>% 
  rename(namePlayer = Player) %>% mutate(Player = gsub("’","'", namePlayer)) %>% 
  group_by(Pos.) %>% separate(Height, c('feet', 'inches'), "'", convert = TRUE) %>% 
  mutate(inches = gsub("\"","", inches)) %>% mutate(inches = as.integer(inches)) %>%
  mutate(cm_height = (12*feet + inches)*2.54/100) %>% select(-c(feet,inches))


players_careers(players = twoK$namePlayer, modes = c("Totals","PerGame"))

strat <- group_split(twoK) 

C <- strat[[1]]
PF <- strat[[2]] 
PG <- strat[[3]] 
SF <- strat[[4]] 
SG <- strat[[5]] 

#generate career data

gen_career <- function(position) {
  uwu <- players_careers(players = position$Player,
                                     modes = c("Totals", "PerGame"))
  fixer <- which(position$Player %notin% unique(uwu$namePlayer))
  position$Player[fixer]
  return(uwu)
}

#don't need this anymore fml
# C_career <- gen_career(C)
# PF_career <- gen_career(PF)
# PG_career <- gen_career(PG)
# SF_career <- gen_career(SF)

# SG_career <- gen_career(SG)

# draft combine (TBD), career numbers(?), season numbers (slugSeason: 2018-2019)
#DoF respectively: 

sift_data <- function(role_names,data_vec) {
  #Player totals for season 2018
  reg_2018 <- data_vec %>% filter(nameTable == "SeasonTotalsRegularSeason")
  
  merge <- reg_2018[[5]] %>% filter(slugSeason == "2018-19") %>%
    dplyr::select(-one_of("slugSeason","slugTeam","idTeam","urlNBAAPI","idPlayer","slugSeasonType", "isRookie"))
  #2018 year post season
  
  #career totals
  
  #career totals post season
  
  
}



#need to change these titels lol
set1 <- dataPlayerSeasonTotalsRegularSeason %>% filter(slugSeason == "2018-19") %>%
  dplyr::select(-one_of("slugSeason","slugTeam","idTeam","urlNBAAPI","idPlayer","slugSeasonType", "isRookie",
                        "numberPlayerSeason")) %>%
  group_by(namePlayer) %>%
  summarise_all(mean)

set2 <- dataPlayerSeasonTotalsPostSeason %>% filter(slugSeason == "2018-19") %>%
  dplyr::select(-one_of("slugSeason","slugTeam","idTeam","urlNBAAPI","idPlayer","slugSeasonType", "isRookie",
                        "numberPlayerSeason")) %>%
  group_by(namePlayer) %>%
  summarise_all(mean)

set3 <- dataPlayerCareerTotalsRegularSeason %>%
  dplyr::select(-one_of("urlNBAAPI","idPlayer","slugSeasonType")) %>%
  group_by(namePlayer) %>%
  summarise_all(mean)

set4 <- dataPlayerCareerTotalsPostSeason %>%
  dplyr::select(-one_of("urlNBAAPI","idPlayer","slugSeasonType")) %>%
  group_by(namePlayer) %>%
  summarise_all(mean)
 

#

C_test <- left_join(C,set1, by = "namePlayer") %>%
  left_join(.,set2, by = "namePlayer",suffix = c("_1","_2")) %>%
  left_join(.,set3, by = "namePlayer")

shipped_to_xqc <- function(position,set1,set2,set3,set4) {
  baka <- left_join(position, set1, by = "namePlayer") %>% 
    #left_join(.,set2, by = "namePlayer",suffix = c("","_post18")) %>% 
    left_join(.,set3, by = "namePlayer",suffix = c("","_regcar")) %>% 
    #left_join(.,set4, by = "namePlayer",suffix = c("","postcar")) %>% 
    rename(twoK_score = OVR) %>%
    dplyr::select(-one_of("Pos.","Rank","Team","Build","namePlayer","idTeam"))
  return(baka)
}

library(mice)

C_data <- shipped_to_xqc(C,set1,set2,set3,set4) %>% filter(gp > 5 & !is.na(gp)) %>%
  dplyr::select(-one_of("Player","gp_regcar","gs_regcar","gs"))
#C_data[is.na(C_data)] <- 0

impy = mice(C_data, m=10, printFlag=FALSE, maxit = 30, seed=2525, method = "cart")
impyfit = with(data=impy, exp = lm(twoK_score ~ ptsPerGame))
combFit = pool(impyfit)

C_data_full = complete(impy, 2) %>% mutate(cm_height = cm_height/100)%>%
  dplyr::select(-one_of("fg2mPerGame","fg2aPerGame","fg2mPerGame_regcar","fg2aPerGame_regcar"))

PF_data <- shipped_to_xqc(PF,set1,set2,set3,set4)
impy2 = mice(PF_data, m=10, printFlag=FALSE, maxit = 30, seed=2525, method = "cart")
impyfit2 = with(data=impy2, exp = lm(twoK_score ~ ptsPerGame))
combFit = pool(impyfit2)

PF_data_full = complete(impy, 2) %>% mutate(cm_height = cm_height/100) 

SF_data <- shipped_to_xqc(SF,set1,set2,set3,set4)
SF_data[is.na(SF_data)] <- 0

SG_data <- shipped_to_xqc(SG,set1,set2,set3,set4)
SG_data[is.na(SG_data)] <- 0

PG_data <- shipped_to_xqc(PG,set1,set2,set3,set4)
PG_data[is.na(PG_data)] <- 0



overall_data_test <- inner_join(twoK, set1, by = "namePlayer") %>% 
  rename(twoK_score = OVR) %>%
  dplyr::select(-one_of("Pos.","Rank","Team","Build","namePlayer","Height"))

#need to do stuff with lambda

#X = matrix(rnorm(n * p), n, p)  # Design Matrix

overall_data_test[is.na(overall_data_test)] <- 0





#filter players that have played more than a certain amount of minutes
#defunct code, use the function above
C_career <- players_careers(players = C$Player,
                            modes = c("Totals", "PerGame")) #career data and season data
fix_C <- which(C$Player %notin% unique(C_career$namePlayer))
C$Player[fix_C] #"Mohamed Bamba"  "Tony Bradley Jr." "Harry Giles"    

PF_career <- players_careers(players = PF$Player,
                             modes = c("Totals", "PerGame")) #career data and season data
fix_PF <- which(PF$Player %notin% unique(PF_career$namePlayer))
PF$Player[fix_PF]
#[1] "PJ Washington"    "T.J. Leaf"        "Robert Williams"  "Juan Hernangomez"
#[5] "Devontae Cacok"   "Kyle Alexander"   "B.J. Johnson"    

PG_career <- players_careers(players = PG$Player,
                             modes = c("Totals", "PerGame")) #career data and season data
fix_PG <- which(PG$Player %notin% unique(PG_career$namePlayer))
PG$Player[fix_PG] #rename steph xd

SF_career <- players_careers(players = SF$Player,
                             modes = c("Totals", "PerGame")) #career data and season data
fix_SF <- which(SF$Player %notin% unique(SF_career$namePlayer))
SF$Player[fix_SF]
#[1] "Marcus Morris Sr" "James Ennis"      "Kevin Knox"       "Danuel House"    
#[5] "Cameron Reddish"  "C.J. Miles"       "Wesley Iwundu"    "Juan Toscano"    
#[9] "Vince Edwards"  

SG_career <- players_careers(players = SG$Player,
                             modes = c("Totals", "PerGame")) #career data and season data
fix_SG <- which(SG$Player %notin% unique(SG_career$namePlayer))
SG$Player[fix_SG]
#[1] "C.J. McCollum"         "Donte Divincenzo"      "Jordan Mcrae"         
#[4] "Sviatoslav Mykhailiuk" "Melvin Frazier"        "P.J. Dozier"          
#[7] "Lugentz Dort"        
