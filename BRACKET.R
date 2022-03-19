###################################
####       Daily Updates       ####
###################################


#' W = East
#' X = West
#' Y = Midwest (16)
#' Z = South (16)

# gets seeding info
seeds <- read_csv('Data/MNCAATourneySeeds.csv') %>% # data from https://www.kaggle.com/c/mens-march-mania-2022/data
  filter(Season == 2022) %>% 
  select(TeamID, Season, Seed) %>%
  mutate(
    seed_n = str_sub(Seed, 2, -1),
    seed_playin = str_sub(Seed, 4),
    seed_n = as.numeric(str_replace_all(seed_n, "[a-z]", "")), 
    seed_region = str_sub(Seed, 1, 1),
    Seed = str_sub(Seed, 1, 3)
  )

# gets team info
teams <- read_csv('Data/MTeams.csv') %>% # data from https://www.kaggle.com/c/mens-march-mania-2022/data
  select(TeamID, TeamName) %>%
  inner_join(seeds, by = "TeamID") %>% 
  select(TeamID, TeamName, Seed) %>% 
  filter(!TeamID %in% c(1394, 1461, 1136, 1353)) 

#' 1394 = TAM C. Christi (Lost to Texas Southern 76-67)
#' 1461 = Wyoming        (Lost to Indiana        66-58)
#' 1136 = Bryant         (Lost to Wright St.     93-82)
#' 1353 Rutgers or 1323 Notre Dame


# gets the predicted results for previous project
MarchMadness <- read_csv('Data/MarchMadness2022.csv') # data from A.I. Sports and https://www.kaggle.com/c/mens-march-mania-2022/data


# First Round
get_win_prob <- function(
  team1seed = "W05", 
  team2seed = "W12", 
  region = "East", 
  bracket_round = "Round 1"
  ) {
  
  
  team1 <- teams %>% 
    filter(Seed == team1seed) %>% 
    pull(TeamID)
  team2 <- teams %>% 
    filter(Seed == team2seed) %>% 
    pull(TeamID)
  
  if (team1 > team2) {
    tmp   <- team1
    team1 <- team2
    team2 <- tmp
    
    tmp2      <- team1seed
    team1seed <- team2seed
    team2seed <- tmp2 
  }
  
  team1name <- teams %>% 
    filter(TeamID == team1) %>% 
    pull(TeamName)
  team2name <- teams %>% 
    filter(TeamID == team2) %>% 
    pull(TeamName)
  
  
  # Extract Probabilities for each team in the matchup
  win_prob <- MarchMadness %>% 
    filter(Team1Name == team1name,
           Team2Name == team2name) %>% 
    select(Team1Name, Team2Name, pred) %>% 
    mutate(
      Round = bracket_round,
      Seed1 = team1seed,
      Seed2 = team2seed
    ) %>% 
    select(
      Seed1,
      Team1Name,
      Seed2,
      Team2Name,
      pred
    )
  
  
  cat("\n")
  cat("----------------------------------------------\n")
  cat(glue::glue("{region}: {bracket_round}"))
  cat("\n----------------------------------------------\n\n")
  cat(glue::glue("{team1name} has a {round(win_prob$pred * 100, 1)}% chance to beat {team2name}"))
  cat("\n")
  cat(glue::glue("{team2name} has a {round((1 - win_prob$pred) * 100, 1)}% chance to beat {team1name}"))
  cat("\n\n\n")
  
  return(win_prob)
}

rm(team2seed)

##### East games Round 1
# Top Round of 64
get_win_prob("W01", "W16", region = "East", bracket_round = "Round 1")
get_win_prob("W08", "W09", region = "East", bracket_round = "Round 1")
get_win_prob("W05", "W12", region = "East", bracket_round = "Round 1") ### PLAY IN GAME.... 
get_win_prob("W04", "W13", region = "East", bracket_round = "Round 1")

# Bottom Round of 64
get_win_prob("W06", "W11", region = "East", bracket_round = "Round 1")
get_win_prob("W03", "W14", region = "East", bracket_round = "Round 1")
get_win_prob("W07", "W10", region = "East", bracket_round = "Round 1")
get_win_prob("W02", "W15", region = "East", bracket_round = "Round 1")

# Round of 32
get_win_prob("W01", "W08", region = "East", bracket_round = "Round 2")
get_win_prob("W04", "W05", region = "East", bracket_round = "Round 2")
get_win_prob("W06", "W03", region = "East", bracket_round = "Round 2")
get_win_prob("W07", "W15", region = "East", bracket_round = "Round 2")

# Sweet 16
get_win_prob("W01", "W04", region = "East", bracket_round = "Sweet 16")
get_win_prob("W07", "W06", region = "East", bracket_round = "Sweet 16")

# Elite 8
get_win_prob("W01", "W06", region = "East", bracket_round = "Elite 8")

##### X games WEST
# Round 1
# Top Round of 64
get_win_prob("X01", "X16", region = "West", bracket_round = "Round 1")
get_win_prob("X08", "X09", region = "West", bracket_round = "Round 1")
get_win_prob("X05", "X12", region = "West", bracket_round = "Round 1")
get_win_prob("X04", "X13", region = "West", bracket_round = "Round 1")

# Bottom Round of 64
get_win_prob("X06", "X11", region = "West", bracket_round = "Round 1")
get_win_prob("X03", "X14", region = "West", bracket_round = "Round 1")
get_win_prob("X07", "X10", region = "West", bracket_round = "Round 1")
get_win_prob("X02", "X15", region = "West", bracket_round = "Round 1")

# Round of 32
get_win_prob("X01", "X09", region = "West", bracket_round = "Round 2")
get_win_prob("X04", "X12", region = "West", bracket_round = "Round 2")
get_win_prob("X03", "X06", region = "West", bracket_round = "Round 2")
get_win_prob("X02", "X07", region = "West", bracket_round = "Round 2")

# Sxeet 16
get_win_prob("X01", "X04", region = "West", bracket_round = "Sweet 16")
get_win_prob("X02", "X03", region = "West", bracket_round = "Sweet 16")

# Elite 8
get_win_prob("X01", "X03", region = "West", bracket_round = "Elite 8")

##### Y games Round 1 MIDWEST
# Top Round of 64
get_win_prob("Y01", "Y16", region = "Midwest", bracket_round = "Round 1")
get_win_prob("Y08", "Y09", region = "Midwest", bracket_round = "Round 1")
get_win_prob("Y05", "Y12", region = "Midwest", bracket_round = "Round 1") 
get_win_prob("Y04", "Y13", region = "Midwest", bracket_round = "Round 1")

# Bottom Round of 64
get_win_prob("Y06", "Y11", region = "Midwest", bracket_round = "Round 1")
get_win_prob("Y03", "Y14", region = "Midwest", bracket_round = "Round 1")
get_win_prob("Y07", "Y10", region = "Midwest", bracket_round = "Round 1")
get_win_prob("Y02", "Y15", region = "Midwest", bracket_round = "Round 1")

# Round of 32
get_win_prob("Y01", "Y09", region = "Midwest", bracket_round = "Round 2")
get_win_prob("Y04", "Y12", region = "Midwest", bracket_round = "Round 2")
get_win_prob("Y06", "Y03", region = "Midwest", bracket_round = "Round 2")
get_win_prob("Y10", "Y02", region = "Midwest", bracket_round = "Round 2")

# Sweet 16
get_win_prob("Y01", "Y04", region = "Midwest", bracket_round = "Sweet 16")
get_win_prob("Y02", "Y03", region = "Midwest", bracket_round = "Sweet 16")

# Elite 8
get_win_prob("Y03", "Y01", region = "Midwest", bracket_round = "Elite 8")

##### Z games Round 1 SOUTH
# Top Round of 64
get_win_prob("Z01", "Z16", region = "South", bracket_round = "Round 1")
get_win_prob("Z08", "Z09", region = "South", bracket_round = "Round 1")
get_win_prob("Z05", "Z12", region = "South", bracket_round = "Round 1") 
get_win_prob("Z04", "Z13", region = "South", bracket_round = "Round 1")

# Bottom Round of 64
get_win_prob("Z06", "Z11", region = "South", bracket_round = "Round 1")
get_win_prob("Z03", "Z14", region = "South", bracket_round = "Round 1")
get_win_prob("Z07", "Z10", region = "South", bracket_round = "Round 1")
get_win_prob("Z02", "Z15", region = "South", bracket_round = "Round 1")

# Round of 32
get_win_prob("Z01", "Z08", region = "South", bracket_round = "Round 2")
get_win_prob("Z04", "Z05", region = "South", bracket_round = "Round 2")
get_win_prob("Z11", "Z03", region = "South", bracket_round = "Round 2")
get_win_prob("Z07", "Z02", region = "South", bracket_round = "Round 2")

# Sweet 16
get_win_prob("Z01", "Z04", region = "South", bracket_round = "Sweet 16")
get_win_prob("Z02", "Z03", region = "South", bracket_round = "Sweet 16")

# Elite 8
get_win_prob("Z04", "Z02", region = "South", bracket_round = "Elite 8")

## Final Four!!! Game Time Baby!!!
# Semi Finals
get_win_prob("W01", "X01", region = "National", bracket_round = "Final 4")
get_win_prob("Y05", "Z02", region = "National", bracket_round = "Final 4")

# Championship Game
get_win_prob("X01", "Z02", region = "National", bracket_round = "Elite 8")



