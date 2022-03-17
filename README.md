

# March Madness
Predicting 2022 March Madness Basketball games


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

I have already created a predicted probability for each team in the tournament for a kaggle competition.  

We will load those results plus a few of the data files provided in the competition.



```r
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
```


### Functions

There are several functions we have to write in order to perform the simulation.


```r
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
```



### Set Up


```r
set.seed(1234)
simulation.results <- c()

# Set number of simulations at 5,000
num_sims = 5000
i = 1
```

## Simulate Tourney

Here's where the magic happens. (5,000 Sims can take up to 90 mins... very slow but I didn't have time to fix this)


```r
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
  
  i <- i + 1 
}
tictoc::toc() # 5796.496 sec elapsed for 5,000 sims
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


|TeamName       |Round32 |Sweet16 |Elite8  |Final4  |Finals  |Champs  |
|:--------------|:-------|:-------|:-------|:-------|:-------|:-------|
|Gonzaga        |97.558% |88.098% |71.948% |55.697% |41.658% |30.188% |
|Villanova      |99.924% |76.780% |53.815% |30.748% |20.066% |9.995%  |
|Arizona        |98.550% |91.556% |61.801% |31.434% |18.515% |9.054%  |
|Kentucky       |99.898% |75.432% |40.590% |25.305% |10.809% |5.671%  |
|Texas Tech     |86.521% |66.582% |47.024% |17.726% |9.893%  |4.985%  |
|Iowa           |91.302% |68.616% |40.895% |24.695% |11.877% |4.934%  |
|Baylor         |99.898% |70.829% |45.270% |22.660% |9.664%  |4.654%  |
|Wisconsin      |87.513% |59.817% |39.598% |22.279% |10.580% |4.095%  |
|Purdue         |91.506% |52.645% |29.196% |17.904% |7.325%  |3.942%  |
|Kansas         |95.270% |67.319% |37.614% |20.524% |8.978%  |3.230%  |
|Tennessee      |86.851% |62.182% |27.060% |13.479% |7.375%  |3.103%  |
|Texas          |67.981% |35.987% |19.379% |11.597% |4.476%  |2.340%  |
|Illinois       |88.606% |51.958% |19.023% |8.037%  |4.527%  |2.085%  |
|Auburn         |99.924% |80.341% |37.462% |15.972% |5.519%  |2.009%  |
|Houston        |84.664% |42.192% |15.437% |7.986%  |4.680%  |1.653%  |
|Connecticut    |86.902% |60.936% |17.904% |9.003%  |3.967%  |1.602%  |
|Duke           |84.308% |51.399% |23.678% |8.087%  |3.332%  |1.246%  |
|Michigan       |72.864% |27.035% |8.087%  |4.145%  |2.442%  |1.043%  |
|UCLA           |79.552% |45.982% |21.745% |8.545%  |2.136%  |0.839%  |
|St Mary's CA   |65.463% |34.435% |16.684% |5.722%  |2.111%  |0.738%  |
|LSU            |68.769% |28.688% |14.446% |6.511%  |1.984%  |0.585%  |
|Ohio St        |63.911% |16.175% |6.511%  |2.238%  |0.865%  |0.254%  |
|Alabama        |50.839% |16.938% |8.952%  |2.009%  |0.687%  |0.254%  |
|Providence     |62.208% |20.677% |8.291%  |3.739%  |0.890%  |0.203%  |
|San Francisco  |43.388% |11.470% |3.255%  |1.272%  |0.407%  |0.178%  |
|Seton Hall     |55.773% |5.036%  |2.213%  |0.636%  |0.305%  |0.178%  |
|Michigan St    |62.208% |28.891% |8.444%  |1.907%  |0.610%  |0.127%  |
|Arkansas       |52.798% |19.049% |3.332%  |1.119%  |0.280%  |0.127%  |
|San Diego St   |63.784% |23.093% |8.316%  |2.976%  |0.610%  |0.102%  |
|Virginia Tech  |32.019% |10.097% |3.128%  |1.068%  |0.381%  |0.102%  |
|Vermont        |47.202% |16.455% |2.340%  |0.712%  |0.229%  |0.102%  |
|Marquette      |49.746% |16.556% |6.612%  |1.704%  |0.356%  |0.051%  |
|Indiana        |34.537% |14.954% |5.163%  |1.577%  |0.305%  |0.051%  |
|Wyoming        |34.537% |14.954% |5.163%  |1.577%  |0.305%  |0.051%  |
|Davidson       |37.792% |16.785% |4.908%  |0.916%  |0.229%  |0.051%  |
|Iowa St        |31.231% |8.800%  |4.171%  |1.322%  |0.203%  |0.051%  |
|Notre Dame     |49.161% |12.564% |5.544%  |1.348%  |0.432%  |0.025%  |
|Rutgers        |49.161% |12.564% |5.544%  |1.348%  |0.432%  |0.025%  |
|Murray St      |56.612% |13.072% |4.273%  |1.679%  |0.305%  |0.025%  |
|Boise St       |50.356% |6.943%  |2.518%  |0.839%  |0.203%  |0.025%  |
|Colorado St    |27.136% |8.698%  |2.263%  |0.509%  |0.153%  |0.025%  |
|S Dakota St    |37.792% |8.571%  |2.696%  |0.712%  |0.127%  |0.025%  |
|Loyola-Chicago |36.089% |7.045%  |1.984%  |0.356%  |0.076%  |0.025%  |
|Memphis        |49.644% |4.603%  |1.704%  |0.407%  |0.025%  |0.025%  |
|North Carolina |50.254% |12.564% |3.789%  |0.763%  |0.153%  |0.000%  |
|Creighton      |36.216% |8.647%  |1.958%  |0.458%  |0.076%  |0.000%  |
|Longwood       |13.149% |2.085%  |0.280%  |0.153%  |0.076%  |0.000%  |
|Miami FL       |62.157% |14.471% |3.204%  |0.636%  |0.051%  |0.000%  |
|TCU            |44.227% |3.255%  |0.839%  |0.229%  |0.025%  |0.000%  |
|Montana St     |13.479% |3.917%  |1.068%  |0.127%  |0.025%  |0.000%  |
|Akron          |20.448% |4.629%  |0.738%  |0.153%  |0.000%  |0.000%  |
|Colgate        |12.487% |2.696%  |0.432%  |0.102%  |0.000%  |0.000%  |
|USC            |37.843% |5.188%  |0.687%  |0.051%  |0.000%  |0.000%  |
|CS Fullerton   |15.692% |2.925%  |0.381%  |0.051%  |0.000%  |0.000%  |
|New Mexico St  |13.098% |3.561%  |0.229%  |0.051%  |0.000%  |0.000%  |
|Yale           |8.494%  |1.272%  |0.178%  |0.051%  |0.000%  |0.000%  |
|UAB            |15.336% |3.306%  |0.407%  |0.025%  |0.000%  |0.000%  |
|Chattanooga    |11.394% |2.543%  |0.280%  |0.025%  |0.000%  |0.000%  |
|Richmond       |8.698%  |2.136%  |0.178%  |0.025%  |0.000%  |0.000%  |
|TAM C. Christi |4.730%  |0.941%  |0.051%  |0.000%  |0.000%  |0.000%  |
|TX Southern    |4.730%  |0.941%  |0.051%  |0.000%  |0.000%  |0.000%  |
|Georgia St     |2.442%  |0.356%  |0.025%  |0.000%  |0.000%  |0.000%  |
|Bryant         |1.450%  |0.153%  |0.000%  |0.000%  |0.000%  |0.000%  |
|Wright St      |1.450%  |0.153%  |0.000%  |0.000%  |0.000%  |0.000%  |
|Norfolk St     |0.102%  |0.051%  |0.000%  |0.000%  |0.000%  |0.000%  |
|St Peter's     |0.102%  |0.025%  |0.000%  |0.000%  |0.000%  |0.000%  |

=======


