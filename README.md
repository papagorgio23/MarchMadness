

# March Madness
Predicting 2018 March Madness Basketball games


*** 

## Monte Carlo Simulation  
#### We are going to determine what percent chance each team has on winning the tournment


***

### Load Libraries


```r
library(knitr)
library(data.table)
library(tidyverse)
library(magrittr)
```

### Load Data

I have already created a predicited probability for each team in the tournament for a kaggle competition.  

We will load those results plus a few of the data files provided in the competition.



```r
# gets seeding info
NCAATourneySeeds2018 <- fread('Data/NCAATourneySeeds.csv')
NCAATourneySeeds2018 %<>% filter(Season == 2018)
seeds <- NCAATourneySeeds2018 %>% 
  select(TeamID, Season, Seed) %>%
  mutate(seed_n = str_sub(Seed, 2, -1),
         seed_playin = str_sub(Seed, 4),
         seed_n = as.numeric(str_replace_all(seed_n, "[a-z]", "")),
         seed_region = str_sub(Seed, 1, 1),
         Seed = str_sub(Seed, 1, 3))

# gets team info
teams <- fread('Data/Teams.csv') %>% 
  select(TeamID, TeamName) %>%
  inner_join(seeds, by = "TeamID") %>% 
  select(TeamID, TeamName, Seed)

# gets the predicted results for previous project
MarchMadness <- fread('Data/MarchMadness.csv')
```


### Functions

There are several functions we have to write in order to perform the simulation.


```r
## convert_pct
convert_pct <- function(x)paste(round(100*x, 2), "%", sep="")

## simulate play in game
simulate.playin.game <- function(team1, team2){
  if(team1 > team2){
    tmp <- team1
    team1 <- team2
    team2 <- tmp
  }
  
  # Extract Probabilities for each team in the matchup
  p.1.2 <- (MarchMadness[MarchMadness$Team1 == team1 & MarchMadness$Team2 == team2, "pred"][[1]])/100
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
  
  team1 <- tourney.seeds[tourney.seeds$Seed == team1seed, "TeamID"]
  team2 <- tourney.seeds[tourney.seeds$Seed == team2seed, "TeamID"]
  
  if(team1 > team2){
    tmp <- team1
    team1 <- team2
    team2 <- tmp
  }
  
  # Extract Probabilities for each team in the matchup
  p.1.2 <- (MarchMadness[MarchMadness$Team1 == team1 & MarchMadness$Team2 == team2, "pred"][[1]])/100
  p.2.1 <- 1 - p.1.2
  
  
  # simulate Game
  game.result <- sample(c(team1, team2), size = 1, prob = c(p.1.2, p.2.1), replace=TRUE)
  
  if (game.result == team1){ 
    winner <- tourney.seeds[tourney.seeds$TeamID == team1, "Seed"]
  } else {
    winner <- tourney.seeds[tourney.seeds$TeamID == team2, "Seed"]
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
```



### Set Up


```r
set.seed(1234)
simulation.results <- c()

# Set number of simulations at 15,000
num_sims = 15000
i = 1
```

## Simulate Tourney

Here's where the magic happens.


```r
while (i <= num_sims) {
  tourney.seeds <- seeds
  play.teams <- seeds %>% filter(seed_playin == "a" | seed_playin == "b")
  play.seeds <- unique(play.teams$Seed)
  
  # play in games
  for(Z16 in play.seeds) {
    play.1.2 <- play.teams %>% filter(Seed == Z16) %>% select(TeamID)
    team1 <- play.1.2[1,]
    team2 <- play.1.2[2,]
    loser <- simulate.playin.game(team1, team2)
    tourney.seeds %<>% filter(TeamID != loser)
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
  
  i <- i + 1 
}
```

### Process Results


```r
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
```

### Table Probabilities


```r
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

all.chances.df[,2:7] <- sapply(all.chances.df[,2:7], convert_pct)

# get team names
all.chances.df %<>% 
  left_join(teams, by = c("team" = "Seed")) %>%
  select(TeamName, everything(), -team, -TeamID) 
```

### View Results


```r
# View results
kable(all.chances.df)
```



|TeamName       |Round32 |Sweet16 |Elite8 |Final4 |Finals |Champs |
|:--------------|:-------|:-------|:------|:------|:------|:------|
|Kansas         |98.69%  |88.69%  |71.55% |57.89% |36.19% |23.79% |
|Virginia       |97%     |85.56%  |69.99% |52.16% |38.43% |19.11% |
|West Virginia  |92.38%  |73%     |43.8%  |33.69% |21.59% |15.01% |
|Villanova      |97.79%  |82.65%  |47.47% |31.43% |16.21% |10.46% |
|North Carolina |98.33%  |69.37%  |61.27% |42.39% |19.69% |7.61%  |
|Gonzaga        |92.02%  |77.83%  |62.61% |32.94% |15.42% |6.1%   |
|Texas Tech     |94.77%  |58.43%  |42.07% |15.77% |7.25%  |3.81%  |
|Cincinnati     |91.77%  |61.63%  |44.93% |18.81% |10.08% |3.57%  |
|Florida        |77.52%  |36.69%  |23.06% |8.29%  |3.55%  |1.6%   |
|Rhode Island   |74.64%  |38.35%  |26.23% |8.17%  |3.17%  |1.36%  |
|Duke           |94.99%  |53.07%  |31.76% |10.58% |3.23%  |1.28%  |
|Arkansas       |50.57%  |40.97%  |18.83% |5.71%  |2.47%  |1.05%  |
|Kentucky       |85.18%  |60.7%   |18.03% |9.05%  |3.22%  |0.75%  |
|Auburn         |90.87%  |61.27%  |15.83% |8.08%  |2.15%  |0.75%  |
|Texas          |50.24%  |23.13%  |15.85% |5.25%  |2.65%  |0.64%  |
|TCU            |79.06%  |66.85%  |29.85% |8.25%  |1.81%  |0.59%  |
|Tennessee      |88.26%  |61.21%  |22.63% |6.27%  |2.77%  |0.57%  |
|Xavier         |91.41%  |59.87%  |16.73% |6.85%  |1.58%  |0.33%  |
|Clemson        |76.04%  |31.24%  |7.61%  |3.54%  |0.98%  |0.32%  |
|Ohio St        |84.57%  |18.73%  |10.52% |4%     |0.87%  |0.17%  |
|Arizona        |58.31%  |22.15%  |4.48%  |2.05%  |0.67%  |0.14%  |
|Houston        |68.52%  |40.2%   |9.07%  |3.31%  |0.81%  |0.12%  |
|Nevada         |49.76%  |13.59%  |7.68%  |2.11%  |0.73%  |0.11%  |
|Texas A&M      |70.87%  |22.03%  |11.81% |3.91%  |0.82%  |0.11%  |
|Michigan       |65.23%  |33.89%  |9.42%  |2.83%  |0.71%  |0.09%  |
|Creighton      |34.83%  |5.76%   |2.29%  |0.91%  |0.26%  |0.07%  |
|Purdue         |91.12%  |29.92%  |7.45%  |1.51%  |0.35%  |0.07%  |
|Alabama        |63.91%  |13.41%  |3.14%  |0.97%  |0.2%   |0.07%  |
|Kansas St      |65.17%  |8.39%   |3.37%  |1.27%  |0.37%  |0.05%  |
|Florida St     |56.19%  |21.83%  |6.17%  |1.26%  |0.2%   |0.05%  |
|Arizona St     |20.94%  |16.38%  |6.34%  |1.44%  |0.17%  |0.05%  |
|Syracuse       |20.94%  |16.38%  |6.34%  |1.44%  |0.17%  |0.05%  |
|Miami FL       |65.02%  |29.32%  |7.71%  |1.5%   |0.28%  |0.04%  |
|Butler         |49.43%  |28.76%  |6.59%  |1.07%  |0.2%   |0.04%  |
|Seton Hall     |68.39%  |6.97%   |2.67%  |0.83%  |0.09%  |0.04%  |
|Wichita St     |88.98%  |23%     |3.79%  |0.88%  |0.15%  |0.03%  |
|NC State       |31.61%  |3.95%   |1.65%  |0.42%  |0.06%  |0.01%  |
|Virginia Tech  |36.09%  |3.63%   |1.09%  |0.23%  |0.05%  |0.01%  |
|Murray St      |7.62%   |3.78%   |0.7%   |0.14%  |0.03%  |0.01%  |
|Providence     |29.13%  |8.44%   |4.11%  |1.05%  |0.14%  |0.01%  |
|Buffalo        |41.69%  |9.73%   |1.21%  |0.42%  |0.08%  |0.01%  |
|Oklahoma       |25.36%  |8.15%   |3.07%  |0.45%  |0.05%  |0.01%  |
|St Bonaventure |22.48%  |4.28%   |1.87%  |0.3%   |0.04%  |0.01%  |
|UCLA           |22.48%  |4.28%   |1.87%  |0.3%   |0.04%  |0.01%  |
|Montana        |34.77%  |15.3%   |2.06%  |0.51%  |0.07%  |0%     |
|San Diego St   |31.48%  |10.61%  |2.22%  |0.33%  |0.06%  |0%     |
|Missouri       |43.81%  |16.84%  |2.97%  |0.45%  |0.05%  |0%     |
|Davidson       |14.82%  |7.43%   |0.61%  |0.14%  |0.02%  |0%     |
|S Dakota St    |15.43%  |1.53%   |0.45%  |0.09%  |0.01%  |0%     |
|Michigan St    |49%     |8.75%   |1.37%  |0.07%  |0.01%  |0%     |
|UNC Greensboro |7.98%   |1.91%   |0.47%  |0.1%   |0.01%  |0%     |
|New Mexico St  |23.96%  |6.2%    |0.59%  |0.14%  |0%     |0%     |
|Bucknell       |51%     |8.02%   |1.31%  |0.11%  |0%     |0%     |
|Georgia St     |8.23%   |1.64%   |0.37%  |0.03%  |0%     |0%     |
|Loyola-Chicago |34.98%  |6.97%   |0.71%  |0.02%  |0%     |0%     |
|Wright St      |11.74%  |2.51%   |0.12%  |0.01%  |0%     |0%     |
|Iona           |5.01%   |0.43%   |0.07%  |0.01%  |0%     |0%     |
|Col Charleston |9.13%   |1.29%   |0.06%  |0.01%  |0%     |0%     |
|Penn           |1.31%   |0.39%   |0.04%  |0.01%  |0%     |0%     |
|SF Austin      |5.23%   |0.59%   |0.11%  |0%     |0%     |0%     |
|NC Central     |8.59%   |1.46%   |0.07%  |0%     |0%     |0%     |
|TX Southern    |8.59%   |1.46%   |0.07%  |0%     |0%     |0%     |
|Lipscomb       |1.67%   |0.17%   |0.03%  |0%     |0%     |0%     |
|CS Fullerton   |8.88%   |0.35%   |0.02%  |0%     |0%     |0%     |
|UMBC           |3%      |0.29%   |0.02%  |0%     |0%     |0%     |
|Long Island    |2.21%   |0.31%   |0%     |0%     |0%     |0%     |
|Radford        |2.21%   |0.31%   |0%     |0%     |0%     |0%     |
|Marshall       |11.02%  |0.22%   |0%     |0%     |0%     |0%     |

=======


