###################################
###   Monte Carlo Simulation   ####
###################################



# This script will generate probabilities for each team to advance to each round of the NCAA Tourney.



###########################
## Set working directory ##
###########################


##############################
## Load necessary libraries ## --------------------------------------------------------------------------
##############################
library("here")
library("PlayerRatings")
library("knitr")
library("tidyverse")
library("tictoc")

##############################
## Load necessary Datafiles ## --------------------------------------------------------------------------
##############################

#####################################
#####                           #####
##### Read and Process the data ##### ---------------------------------------------------------------
#####                           #####
#####################################


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
  select(TeamID, TeamName, Seed)

# gets the predicted results for previous project
MarchMadness <- read_csv('Data/MarchMadness2022.csv') # data from A.I. Sports and https://www.kaggle.com/c/mens-march-mania-2022/data




#############################
#####                   #####
##### SET EVERYTHING UP ##### ---------------------------------------------------------------------------
#####                   #####
#############################


set.seed(1234)
simulation.results <- c()

# Set number of simulations at 15,000
num_sims = 7000
i = 1



###################################
#####                         #####
##### ALL NECESSARY FUNCTIONS ##### ---------------------------------------------------------------------
#####                         #####
###################################


## simulate play in game
simulate.playin.game <- function(team1, team2){
  if(team1 > team2){
    tmp <- team1
    team1 <- team2
    team2 <- tmp
  }
  
  # Extract Probabilities for each team in the matchup
  p.1.2 <- MarchMadness %>% 
    filter(Team1 == team1,
           Team2 == team2) %>% 
    pull(pred)
  p.2.1 <- 1 - p.1.2
  
  # simulate Game
  game.result <- sample(c(team1, team2), size = 1, prob = c(p.1.2, p.2.1), replace=TRUE)
  
  if (game.result == team1){ 
    loser <- team2
  } else {
    loser <- team1
  }
  # return the winner
  loser
}

## simulate regular game
simulate.game <- function(team1seed, team2seed){
  
  team1 <- tourney.seeds %>% 
    filter(Seed == team1seed) %>% 
    pull(TeamID)
  team2 <- tourney.seeds %>% 
    filter(Seed == team2seed) %>% 
    pull(TeamID)
  
  if(team1 > team2){
    tmp   <- team1
    team1 <- team2
    team2 <- tmp
  }
  
  # Extract Probabilities for each team in the matchup
  p.1.2 <- MarchMadness %>% 
    filter(Team1 == team1,
           Team2 == team2) %>% 
    pull(pred)
  p.2.1 <- 1 - p.1.2
  
  
  # simulate Game
  game.result <- sample(c(team1, team2), size = 1, prob = c(p.1.2, p.2.1), replace=TRUE)
  
  if (game.result == team1){ 
    winner <- tourney.seeds %>% 
      filter(TeamID == team1) %>% 
      pull("Seed")
  } else {
    winner <- tourney.seeds %>% 
      filter(TeamID == team2) %>% 
      pull("Seed")
  }
  # return the winner
  winner
}


## chance.df
chance.df <- function(series){
  tbl <- table(sim.results.df[ , series])
  df <- data.frame(team = names(tbl), chance = as.numeric(tbl)/sum(tbl))
  df <- df[order(df$chance, decreasing=TRUE), ]
  df
}


###############################
#####                     #####
##### Run the Simulations ##### ------------------------------------------------------------------------
#####                     #####
###############################


tictoc::tic()
while (i <= num_sims) {
  tourney.seeds <- seeds
  play.teams <- seeds %>% filter(seed_playin == "a" | seed_playin == "b")
  play.seeds <- unique(play.teams$Seed)
  
  # play in games
  for(seeding in play.seeds) {
    play.1.2 <- play.teams %>% filter(Seed == seeding) %>% select(TeamID)
    team1 <- play.1.2$TeamID[1]
    team2 <- play.1.2$TeamID[2]
    loser <- simulate.playin.game(team1, team2)
    tourney.seeds <- tourney.seeds %>% 
      filter(TeamID != loser)
  }
  
  
  ##### West games Round 1
  # Top Round of 64
  R32.1.w <- simulate.game("W01", "W16")
  R32.2.w <- simulate.game("W08", "W09")
  R32.3.w <- simulate.game("W05", "W12")
  R32.4.w <- simulate.game("W04", "W13")
  
  # Bottom Round of 64
  R32.5.w <- simulate.game("W06", "W11")
  R32.6.w <- simulate.game("W03", "W14")
  R32.7.w <- simulate.game("W07", "W10")
  R32.8.w <- simulate.game("W02", "W15")
  
  # Round of 32
  S16.1.w <- simulate.game(R32.1.w, R32.2.w)
  S16.2.w <- simulate.game(R32.3.w, R32.4.w)
  S16.3.w <- simulate.game(R32.5.w, R32.6.w)
  S16.4.w <- simulate.game(R32.7.w, R32.8.w)
  
  # Sweet 16
  E8.1.w <- simulate.game(S16.1.w, S16.2.w)
  E8.2.w <- simulate.game(S16.3.w, S16.4.w)
  
  # Elite 8
  F4.w <- simulate.game(E8.1.w, E8.2.w)
  
  ##### X games Round 1
  # Top Round of 64
  R32.1.x <- simulate.game("X01", "X16")
  R32.2.x <- simulate.game("X08", "X09")
  R32.3.x <- simulate.game("X05", "X12")
  R32.4.x <- simulate.game("X04", "X13")
  
  # Bottom Round of 64
  R32.5.x <- simulate.game("X06", "X11")
  R32.6.x <- simulate.game("X03", "X14")
  R32.7.x <- simulate.game("X07", "X10")
  R32.8.x <- simulate.game("X02", "X15")
  
  # Round of 32
  S16.1.x <- simulate.game(R32.1.x, R32.2.x)
  S16.2.x <- simulate.game(R32.3.x, R32.4.x)
  S16.3.x <- simulate.game(R32.5.x, R32.6.x)
  S16.4.x <- simulate.game(R32.7.x, R32.8.x)
  
  # Sxeet 16
  E8.1.x <- simulate.game(S16.1.x, S16.2.x)
  E8.2.x <- simulate.game(S16.3.x, S16.4.x)
  
  # Elite 8
  F4.x <- simulate.game(E8.1.x, E8.2.x)
  
  ##### Y games Round 1
  # Top Round of 64
  R32.1.y <- simulate.game("Y01", "Y16")
  R32.2.y <- simulate.game("Y08", "Y09")
  R32.3.y <- simulate.game("Y05", "Y12")
  R32.4.y <- simulate.game("Y04", "Y13")
  
  # Bottom Round of 64
  R32.5.y <- simulate.game("Y06", "Y11")
  R32.6.y <- simulate.game("Y03", "Y14")
  R32.7.y <- simulate.game("Y07", "Y10")
  R32.8.y <- simulate.game("Y02", "Y15")
  
  # Round of 32
  S16.1.y <- simulate.game(R32.1.y, R32.2.y)
  S16.2.y <- simulate.game(R32.3.y, R32.4.y)
  S16.3.y <- simulate.game(R32.5.y, R32.6.y)
  S16.4.y <- simulate.game(R32.7.y, R32.8.y)
  
  # Syeet 16
  E8.1.y <- simulate.game(S16.1.y, S16.2.y)
  E8.2.y <- simulate.game(S16.3.y, S16.4.y)
  
  # Elite 8
  F4.y <- simulate.game(E8.1.y, E8.2.y)
  
  ##### Z games Round 1
  # Top Round of 64
  R32.1.z <- simulate.game("Z01", "Z16")
  R32.2.z <- simulate.game("Z08", "Z09")
  R32.3.z <- simulate.game("Z05", "Z12")
  R32.4.z <- simulate.game("Z04", "Z13")
  
  # Bottom Round of 64
  R32.5.z <- simulate.game("Z06", "Z11")
  R32.6.z <- simulate.game("Z03", "Z14")
  R32.7.z <- simulate.game("Z07", "Z10")
  R32.8.z <- simulate.game("Z02", "Z15")
  
  # Round of 32
  S16.1.z <- simulate.game(R32.1.z, R32.2.z)
  S16.2.z <- simulate.game(R32.3.z, R32.4.z)
  S16.3.z <- simulate.game(R32.5.z, R32.6.z)
  S16.4.z <- simulate.game(R32.7.z, R32.8.z)
  
  # Szeet 16
  E8.1.z <- simulate.game(S16.1.z, S16.2.z)
  E8.2.z <- simulate.game(S16.3.z, S16.4.z)
  
  # Elite 8
  F4.z <- simulate.game(E8.1.z, E8.2.z)
  
  ## Final Four!!! Game Time Baby!!!
  # Semi Finals
  F4.1 <- simulate.game(F4.w, F4.x)
  F4.2 <- simulate.game(F4.y, F4.z)
  
  # Championship Game
  Champ.1 <- simulate.game(F4.1, F4.2)
  
  #print(paste0("This is the winner ",Champ.1))
  
  results.all <- c( 
    i,  R32.1.w, 
    R32.2.w,
    R32.3.w,
    R32.4.w,
    R32.5.w,
    R32.6.w,
    R32.7.w,
    R32.8.w,
    R32.1.x,
    R32.2.x,
    R32.3.x,
    R32.4.x,
    R32.5.x,
    R32.6.x,
    R32.7.x,
    R32.8.x,
    R32.1.y, 
    R32.2.y,
    R32.3.y,
    R32.4.y,
    R32.5.y,
    R32.6.y,
    R32.7.y,
    R32.8.y,
    R32.1.z, 
    R32.2.z,
    R32.3.z,
    R32.4.z,
    R32.5.z,
    R32.6.z,
    R32.7.z,
    R32.8.z,
    S16.1.w,
    S16.2.w,
    S16.3.w,
    S16.4.w,
    S16.1.x,
    S16.2.x,
    S16.3.x,
    S16.4.x,
    S16.1.y,
    S16.2.y,
    S16.3.y,
    S16.4.y,
    S16.1.z,
    S16.2.z,
    S16.3.z,
    S16.4.z,
    E8.1.w,
    E8.2.w,
    E8.1.x,
    E8.2.x,
    E8.1.y,
    E8.2.y,
    E8.1.z,
    E8.2.z,
    F4.w, F4.x, F4.y, F4.z,
    F4.1, F4.2, Champ.1
  )
  
  simulation.results <- c(simulation.results, results.all)
  
  if( i %% 100 == 0 ) cat(paste("Simulation", i, "complete\n"))
  
  i <- i + 1 
}
tictoc::toc() # 5796.496 sec elapsed for 5,000 sims


# Results
sim.results.mat <- matrix(simulation.results, ncol=64, byrow=TRUE)
sim.results.df <- as.data.frame(sim.results.mat)
names(sim.results.df) <- c( 
  "sim", "R32.1.w", 
  "R32.2.w",
  "R32.3.w",
  "R32.4.w",
  "R32.5.w",
  "R32.6.w",
  "R32.7.w",
  "R32.8.w",
  "R32.1.x",
  "R32.2.x",
  "R32.3.x",
  "R32.4.x",
  "R32.5.x",
  "R32.6.x",
  "R32.7.x",
  "R32.8.x",
  "R32.1.y", 
  "R32.2.y",
  "R32.3.y",
  "R32.4.y",
  "R32.5.y",
  "R32.6.y",
  "R32.7.y",
  "R32.8.y",
  "R32.1.z", 
  "R32.2.z",
  "R32.3.z",
  "R32.4.z",
  "R32.5.z",
  "R32.6.z",
  "R32.7.z",
  "R32.8.z",
  "S16.1.w",
  "S16.2.w",
  "S16.3.w",
  "S16.4.w",
  "S16.1.x",
  "S16.2.x",
  "S16.3.x",
  "S16.4.x",
  "S16.1.y",
  "S16.2.y",
  "S16.3.y",
  "S16.4.y",
  "S16.1.z",
  "S16.2.z",
  "S16.3.z",
  "S16.4.z",
  "E8.1.w",
  "E8.2.w",
  "E8.1.x",
  "E8.2.x",
  "E8.1.y",
  "E8.2.y",
  "E8.1.z",
  "E8.2.z",
  "F4.w", "F4.x", "F4.y", "F4.z",
  "F4.1", "F4.2", "Champ.1"
)


#################################################
#####                                       #####
##### Create a table with all probabilities ##### -----------------------------------------------------------------------------------------------
#####                                       #####
#################################################

# NCAA Champions
champs.df <- chance.df("Champ.1")

# Semi Finals Champions
SF1.df <- chance.df("F4.1")
SF2.df <- chance.df("F4.2")
finals <- rbind(SF1.df, SF2.df)

# Final 4
w.1.df <- chance.df("F4.w")
x.1.df <- chance.df("F4.x")
y.1.df <- chance.df("F4.y")
z.1.df <- chance.df("F4.z")
Final4 <- rbind(w.1.df, x.1.df, y.1.df, z.1.df)

# Elite 8
E8w.1.df <- chance.df("E8.1.w")
E8w.2.df <- chance.df("E8.2.w")
E8x.1.df <- chance.df("E8.1.x")
E8x.2.df <- chance.df("E8.2.x")
E8y.1.df <- chance.df("E8.1.y")
E8y.2.df <- chance.df("E8.2.y")
E8z.1.df <- chance.df("E8.1.z")
E8z.2.df <- chance.df("E8.2.z")
Elite8 <- rbind(E8w.1.df, E8w.2.df, E8x.1.df, E8x.2.df, E8y.1.df, E8y.2.df, E8z.1.df, E8z.2.df)


# Sweet 16
S16w.1.df <- chance.df("S16.1.w")
S16w.2.df <- chance.df("S16.2.w")
S16w.3.df <- chance.df("S16.3.w")
S16w.4.df <- chance.df("S16.4.w")
S16x.1.df <- chance.df("S16.1.x")
S16x.2.df <- chance.df("S16.2.x")
S16x.3.df <- chance.df("S16.3.x")
S16x.4.df <- chance.df("S16.4.x")
S16y.1.df <- chance.df("S16.1.y")
S16y.2.df <- chance.df("S16.2.y")
S16y.3.df <- chance.df("S16.3.y")
S16y.4.df <- chance.df("S16.4.y")
S16z.1.df <- chance.df("S16.1.z")
S16z.2.df <- chance.df("S16.2.z")
S16z.3.df <- chance.df("S16.3.z")
S16z.4.df <- chance.df("S16.4.z")
Sweet16 <- rbind(S16w.1.df, S16w.2.df, S16w.3.df, S16w.4.df, S16x.1.df, S16x.2.df, S16x.3.df, S16x.4.df, S16y.1.df, S16y.2.df, S16y.3.df, S16y.4.df, S16z.1.df, S16z.2.df, S16z.3.df, S16z.4.df)

# Round of 32
R32w.1.df <- chance.df("R32.1.w")
R32w.2.df <- chance.df("R32.2.w")
R32w.3.df <- chance.df("R32.3.w")
R32w.4.df <- chance.df("R32.4.w")
R32w.5.df <- chance.df("R32.5.w")
R32w.6.df <- chance.df("R32.6.w")
R32w.7.df <- chance.df("R32.7.w")
R32w.8.df <- chance.df("R32.8.w")
R32x.1.df <- chance.df("R32.1.x")
R32x.2.df <- chance.df("R32.2.x")
R32x.3.df <- chance.df("R32.3.x")
R32x.4.df <- chance.df("R32.4.x")
R32x.5.df <- chance.df("R32.5.x")
R32x.6.df <- chance.df("R32.6.x")
R32x.7.df <- chance.df("R32.7.x")
R32x.8.df <- chance.df("R32.8.x")
R32y.1.df <- chance.df("R32.1.y")
R32y.2.df <- chance.df("R32.2.y")
R32y.3.df <- chance.df("R32.3.y")
R32y.4.df <- chance.df("R32.4.y")
R32y.5.df <- chance.df("R32.5.y")
R32y.6.df <- chance.df("R32.6.y")
R32y.7.df <- chance.df("R32.7.y")
R32y.8.df <- chance.df("R32.8.y")
R32z.1.df <- chance.df("R32.1.z")
R32z.2.df <- chance.df("R32.2.z")
R32z.3.df <- chance.df("R32.3.z")
R32z.4.df <- chance.df("R32.4.z")
R32z.5.df <- chance.df("R32.5.z")
R32z.6.df <- chance.df("R32.6.z")
R32z.7.df <- chance.df("R32.7.z")
R32z.8.df <- chance.df("R32.8.z")
Round32 <- rbind(R32w.1.df, R32w.2.df, R32w.3.df, R32w.4.df, R32w.5.df, R32w.6.df, R32w.7.df, R32w.8.df,
                 R32x.1.df, R32x.2.df, R32x.3.df, R32x.4.df, R32x.5.df, R32x.6.df, R32x.7.df, R32x.8.df, 
                 R32y.1.df, R32y.2.df, R32y.3.df, R32y.4.df, R32y.5.df, R32y.6.df, R32y.7.df, R32y.8.df, 
                 R32z.1.df, R32z.2.df, R32z.3.df, R32z.4.df, R32z.5.df, R32z.6.df, R32z.7.df, R32z.8.df)






# Merge all probabilities
all.chances.df <- merge(Round32, Sweet16, by="team")
names(all.chances.df) <- c("team", "Round32", "Sweet16")

all.chances.df %<>% left_join(Elite8, by = "team") %>%
  rename(Elite8 = chance) %>%
  left_join(Final4, by = "team") %>%
  rename(Final4 = chance) %>%
  left_join(finals, by = "team") %>%
  rename(Finals = chance) %>%
  left_join(champs.df, by = "team") %>%
  rename(Champs = chance) %>%
  arrange(desc(Champs), desc(Finals), desc(Final4), desc(Elite8), desc(Sweet16), desc(Round32))

# Fix percentages
all.chances.df$Sweet16 <- ifelse(is.na(all.chances.df$Sweet16), 0, all.chances.df$Sweet16)
all.chances.df$Elite8 <- ifelse(is.na(all.chances.df$Elite8), 0, all.chances.df$Elite8)
all.chances.df$Final4 <- ifelse(is.na(all.chances.df$Final4), 0, all.chances.df$Final4)
all.chances.df$Finals <- ifelse(is.na(all.chances.df$Finals), 0, all.chances.df$Finals)
all.chances.df$Champs <- ifelse(is.na(all.chances.df$Champs), 0, all.chances.df$Champs)

all.chances.df[,2:7] <- sapply(all.chances.df[,2:7], scales::percent)

# get team names
all.chances.df %<>% 
  left_join(teams, by = c("team" = "Seed")) %>%
  select(TeamName, everything(), -team, -TeamID) 


# View results
kable(all.chances.df)

# Write to a file
output_filename <- "Data/MarchMadness_probs_2022a.csv"
write_csv(all.chances.df, output_filename)


