#data loading and cleaning
library(nbastatR)
library(dplyr)
library(future)

library(tidyr)

plan(multiprocess)
'%notin%' <- Negate('%in%')

#test <- game_logs(seasons = 2018, league = "NBA")

# nba 2k player rating
twoK <- read.csv("C:/Users/typer321/Desktop/nba2k ratings.csv") %>% 
  rename(namePlayer = Player) %>% mutate(Player = gsub("’","'", namePlayer)) %>% 
  group_by(Pos.) %>% separate(Height, c('feet', 'inches'), "'", convert = TRUE) %>% 
  mutate(inches = gsub("\"","", inches)) %>% mutate(inches = as.integer(inches)) %>%
  mutate(m_height = (12*feet + inches)*2.54/100) %>% select(-c(feet,inches))


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
                        "numberPlayerSeason","fgaPerGame", "fg3aPerGame", "ftaPerGame")) %>%
  group_by(namePlayer) %>%
  summarise_all(mean)

set2 <- dataPlayerSeasonTotalsPostSeason %>% filter(slugSeason == "2018-19") %>%
  dplyr::select(-one_of("slugSeason","slugTeam","idTeam","urlNBAAPI","idPlayer","slugSeasonType", "isRookie",
                        "numberPlayerSeason")) %>%
  group_by(namePlayer) %>%
  summarise_all(mean)

set3 <- dataPlayerCareerTotalsRegularSeason %>%
  dplyr::select(-one_of("urlNBAAPI","idPlayer","slugSeasonType","idTeam", 
                        "fgaPerGame", "fg3aPerGame","ftaPerGame")) %>% 
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

any_imputers <- function(pos_data) {
  temp_imp <- mice(pos_data, m=10, printFlag=FALSE, maxit = 30, seed=2525, method = "cart")
  simpyfit = with(data=temp_imp, exp = lm(twoK_score ~ ptsPerGame))
  combFit = pool(simpyfit)
  pos_data_full = complete(temp_imp, 2)
  return(pos_data_full) 
}

C_data <- shipped_to_xqc(C,set1,set2,set3,set4) %>% filter(gp > 5 & !is.na(gp)) %>%
  dplyr::select(-one_of("Player","fg2mPerGame","fg2aPerGame","fg2mPerGame_regcar","fg2aPerGame_regcar")) %>%
  mutate(start_pct = gs/gp) %>%
  mutate(start_pct_regcar = gs_regcar/gp_regcar) %>%
  dplyr::select(-c("gp","gs","gs_regcar","gp_regcar")) %>%
C_data[is.na(C_data)] <- 0

impy = mice(C_data, m=10, printFlag=FALSE, maxit = 30, seed=2525, method = "cart")
impyfit = with(data=impy, exp = lm(twoK_score ~ ptsPerGame))
combFit = pool(impyfit)

C_data_full = complete(impy, 2) %>% mutate(cm_height = cm_height/100)%>%
  dplyr::select(-one_of("fg2mPerGame","fg2aPerGame","fg2mPerGame_regcar","fg2aPerGame_regcar"))

PF_data <- shipped_to_xqc(PF,set1,set2,set3,set4) %>%
  dplyr::select(-one_of("Player","fg2mPerGame","fg2aPerGame","fg2mPerGame_regcar","fg2aPerGame_regcar")) %>%
  mutate(start_pct = gs/gp) %>%
  mutate(start_pct_regcar = gs_regcar/gp_regcar) %>%
  dplyr::select(-c("gp","gs","gs_regcar","gp_regcar")) %>%
  filter(!is.na(fgmPerGame))
PF_data[is.na(PF_data)] <- 0

impy2 = mice(PF_data, m=10, printFlag=FALSE, maxit = 30, seed=2525, method = "norm")

impyfit2 = with(data=impy2, exp = lm(twoK_score ~ ptsPerGame + m_height))
combFit = pool(impyfit2)

PF_data_full = complete(impy2, 2) %>% dplyr::select(-c("Player"))

SF_data <- shipped_to_xqc(SF,set1,set2,set3,set4) %>%
  dplyr::select(-one_of("Player","fg2mPerGame","fg2aPerGame","fg2mPerGame_regcar","fg2aPerGame_regcar")) %>%
  mutate(start_pct = gs/gp) %>%
  mutate(start_pct_regcar = gs_regcar/gp_regcar) %>%
  dplyr::select(-c("gp","gs","gs_regcar","gp_regcar")) %>%
  filter(!is.na(fgmPerGame))
SF_data[is.na(SF_data)] <- 0


SF_data_full <- any_imputers(SF_data)

SG_data <- shipped_to_xqc(SG,set1,set2,set3,set4) %>%
  dplyr::select(-one_of("Player","fg2mPerGame","fg2aPerGame","fg2mPerGame_regcar","fg2aPerGame_regcar")) %>%
  mutate(start_pct = gs/gp) %>%
  mutate(start_pct_regcar = gs_regcar/gp_regcar) %>%
  dplyr::select(-c("gp","gs","gs_regcar","gp_regcar")) %>%
  filter(!is.na(fgmPerGame))


SG_data_full <- any_imputers(SG_data)

PG_data <- shipped_to_xqc(PG,set1,set2,set3,set4) %>%
  dplyr::select(-one_of("Player","fg2mPerGame","fg2aPerGame","fg2mPerGame_regcar","fg2aPerGame_regcar")) %>%
  mutate(start_pct = gs/gp) %>%
  mutate(start_pct_regcar = gs_regcar/gp_regcar) %>%
  dplyr::select(-c("gp","gs","gs_regcar","gp_regcar")) %>%
  filter(!is.na(fgmPerGame))

PG_data_full <- any_imputers(PG_data)

