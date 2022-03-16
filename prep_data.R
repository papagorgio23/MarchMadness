###################################
###    PREP Kaggle Datasets    ####
###################################



# load libraries
library("here")
library("tidyverse")
library("tidymodels")
library("vip")


# load data
regular <- read_csv('Data/MRegularSeasonDetailedResults.csv')
tourney <- read_csv('Data/MNCAATourneyDetailedResults.csv')
tourney_comp <- read_csv('Data/MNCAATourneyCompactResults.csv')
teams   <- read_csv('Data/MTeams.csv')
submission <- read_csv("Data/MSampleSubmissionStage2.csv")


## Clean up the seed data to get region, seed number, and also give seeds a flag if they are ranked
seeds <- read_csv('Data/MNCAATourneySeeds.csv') %>%
  select(TeamID, Season, Seed) %>%
  mutate(
    seed_n = str_sub(Seed, 2,-1),
    seed_n = as.numeric(str_replace_all(seed_n, "[a-z]", "")),
    seed_region = str_sub(Seed, 1, 1),
    top_seeded_teams = ifelse(!is.na(seed_n), 1, 0)
  )


# combine the regular season game data with the Tournment game data
all_games <- bind_rows(regular, tourney)


## this gets the regular season details for only the teams that were in the tourney
# based off winning teams
games_win <- all_games %>%
  inner_join(seeds, by = c("WTeamID" = "TeamID", "Season" = "Season")) %>%
  rename(
    WSeed = Seed,
    WSeed_n = seed_n,
    Wseed_region = seed_region,
    Wtop_seeded_teams = top_seeded_teams
  )
# based off losing teams
games_lose <- all_games %>%
  inner_join(seeds, by = c("LTeamID" = "TeamID", "Season" = "Season"))# %>%
rename(
  LSeed = Seed,
  LSeed_n = seed_n,
  Lseed_region = seed_region,
  Ltop_seeded_teams = top_seeded_teams
)

# Joined together
all_games <-
  full_join(
    games_win,
    games_lose,
    by = c(
      "Season",
      "DayNum",
      "WTeamID",
      "WScore",
      "LTeamID",
      "LScore",
      "WLoc",
      "NumOT",
      "WFGM",
      "WFGA",
      "WFGM3",
      "WFGA3",
      "WFTM",
      "WFTA",
      "WOR",
      "WDR",
      "WAst",
      "WTO",
      "WStl",
      "WBlk",
      "WPF",
      "LFGM",
      "LFGA",
      "LFGM3",
      "LFGA3",
      "LFTM",
      "LFTA",
      "LOR",
      "LDR",
      "LAst",
      "LTO",
      "LStl",
      "LBlk",
      "LPF"
    )
  ) %>% 
  mutate(
    LLoc = case_when(
      .data$WLoc == "H" ~ "A",
      .data$WLoc == "A" ~ "H",
      TRUE ~ "N"
    ),
    ## Add advanced statistics to dataset
    Wposs       = WFGA + 0.475 * WFTA - WOR + WTO,
    Lposs       = LFGA + 0.475 * LFTA - LOR + LTO,
    pace        = (Wposs + Lposs),
    Woff_rating = round((WScore / Wposs) * 100, 2),
    Wdef_rating = round((LScore / Lposs) * 100, 2),
    Wnet_rating = Woff_rating - Wdef_rating,
    WFGM2       = WFGM - WFGM3,
    WFGA2       = WFGA - WFGA3,
    WFGperc     = round(100 * WFGM / WFGA, 2),
    WeFGperc    = round(100 * (WFGM + 0.5 * WFGM3) / WFGA, 2),
    WFG2perc    = round(100 * WFGM2 / WFGA2, 2),
    WFG3perc    = round(100 * WFGM3 / WFGA3, 2),
    WFTperc     = round(100 * WFTM / WFTA, 2),
    W2ptFreq    = round(100 * WFGA2 / WFGA, 2),
    W3ptFreq    = round(100 * WFGA3 / WFGA, 2),
    WFTFreq     = round(100 * WFTA / Wposs, 2),
    WFTrate     = round(100 * WFTA / WFGA, 2),
    WTrueShot   = round(100 * WScore / (2 * (WFGA + 0.44 * WFTA)), 2),
    WRebperc    = round(100 * (WOR + WDR) / (WOR + WDR + LOR + LDR),2),
    WOffRebperc = round(100 * WOR / (WFGA - WFGM), 2),
    WAstRatio   = round(100 * WAst / WFGM, 2),
    WAstTO      = round(100 * WAst / WTO, 2),
    WTOperc     = round(100 * WTO / Wposs, 2),
    WPPP        = round(100 * WScore / Wposs, 2),
    WSTLperc    = round(100 * WStl / Lposs, 2),
    WBlkperc    = round(100* WBlk / LFGA, 2),
    WPFperc     = round(100* WPF / LFGA, 2),
    Loff_rating = round((LScore / Lposs) * 100, 2),
    Ldef_rating = round((WScore / Wposs) * 100, 2), 
    Lnet_rating = Loff_rating - Ldef_rating,
    LFGM2       = LFGM - LFGM3,
    LFGA2       = LFGA - LFGA3,
    LFGperc     = round(100 * LFGM / LFGA, 2),
    LeFGperc    = round(100 * (LFGM + 0.5 * LFGM3) / LFGA, 2),
    LFG2perc    = round(100 * LFGM2 / LFGA2, 2),
    LFG3perc    = round(100 * LFGM3 / LFGA3, 2),
    LFTperc     = round(100 * LFTM / LFTA, 2),
    L2ptFreq    = round(100 * LFGA2 / LFGA, 2),
    L3ptFreq    = round(100 * LFGA3 / LFGA, 2),
    LFTFreq     = round(100 * LFTA / Lposs, 2),
    LFTrate     = round(100 * LFTA / LFGA, 2),
    LTrueShot   = round(100 * LScore / (2*(LFGA + 0.44 *LFTA)), 2),
    LRebperc    = round(100 * (LOR + LDR) / (WOR + WDR + LOR + LDR),2),
    LAstRatio   = round(100 * LAst / LFGM, 2),
    LAstTO      = round(100 * LAst / LTO, 2),
    LTOperc     = round(100 * LTO / Lposs, 2),
    LPPP        = round(100 * LScore / Lposs, 2),
    LSTLperc    = round(100 * LStl / Wposs, 2),
    LOffRebperc = round(100* LOR / (LFGA - LFGM), 2),
    LBlkperc    = round(100* LBlk / WFGA, 2),
    LPFperc     = round(100* LPF / WFGA, 2)
  )

# get all teams' games back together (2 rows for each game - Team vs Opp)
team_games <- bind_rows(
  all_games %>%
    select(
      Season,
      DayNum,
      TeamID = WTeamID,
      OTeamID = LTeamID,
      Score = WScore,
      OScore = LScore,
      Loc = WLoc,
      NumOT,
      FGM = WFGM,
      FGA = WFGA,
      FGperc = WFGperc,
      eFGperc = WeFGperc,
      TrueShot = WTrueShot,
      PPP = WPPP,
      FGM2 = WFGM2,
      FGA2 = WFGA2,
      FG2perc = WFG2perc,
      FG2Freq = W2ptFreq,
      FGM3 = WFGM3,
      FGA3 = WFGA3,
      FG3perc = WFG3perc,
      FG3Freq = W3ptFreq,
      FTM = WFTM,
      FTA = WFTA,
      FTperc = WFTperc,
      FTFreq = WFTFreq,
      FTrate = WFTrate,
      OR = WOR,
      DR = WDR,
      Rebperc = WRebperc,
      OffRebperc = WOffRebperc,
      Ast = WAst,
      AstRatio = WAstRatio,
      TO = WTO,
      AstTO = WAstTO,
      TOperc = WTOperc,
      Stl = WStl,
      STLperc = WSTLperc,
      Blk = WBlk,
      Blkperc = WBlkperc,
      PF = WPF,
      PFperc = WPFperc,
      Pace = pace,
      Poss = Wposs,
      Off_rating = Woff_rating,
      Def_rating = Wdef_rating,
      Net_rating = Wnet_rating,
      OFGM = LFGM,
      OFGA = LFGA,
      OFGperc = LFGperc,
      OeFGperc = LeFGperc,
      OTrueShot = LTrueShot,
      OPPP = LPPP,
      OFGM2 = LFGM2,
      OFGA2 = LFGA2,
      OFG2perc = LFG2perc,
      OFG2Freq = L2ptFreq,
      OFGM3 = LFGM3,
      OFGA3 = LFGA3,
      OFG3perc = LFG3perc,
      OFG3Freq = L3ptFreq,
      OFTM = LFTM,
      OFTA = LFTA,
      OFTperc = LFTperc,
      OFTFreq = LFTFreq,
      OFTrate = LFTrate,
      O_OR = LOR,
      ODR = LDR,
      ORebperc = LRebperc,
      O_OffRebperc = LOffRebperc,
      OAst = LAst,
      OAstRatio = LAstRatio,
      OTO = LTO,
      OAstTO = LAstTO,
      OTOperc = LTOperc,
      OStl = LStl,
      OSTLperc = LSTLperc,
      OBlk = LBlk,
      OBlkperc = LBlkperc,
      OPF = LPF,
      OPFperc = LPFperc,
      OPoss = Lposs,
      O_Off_rating = Loff_rating,
      ODef_rating = Ldef_rating,
      ONet_rating = Lnet_rating,
      Seed = WSeed,
      Seed_n = WSeed_n,
      Seed_region = Wseed_region,
      Top_seeded_teams = Wtop_seeded_teams,
      OSeed = LSeed,
      OSeed_n = LSeed_n,
      OSeed_region = Lseed_region,
      OTop_seeded_teams = Ltop_seeded_teams
    ) %>%
    mutate(Winner = 1),
  all_games %>%
    select(
      Season,
      DayNum,
      TeamID = LTeamID,
      OTeamID = WTeamID,
      Score = LScore,
      OScore = WScore,
      Loc = LLoc,
      NumOT,
      FGM = LFGM,
      FGA = LFGA,
      FGperc = LFGperc,
      eFGperc = LeFGperc,
      TrueShot = LTrueShot,
      PPP = LPPP,
      FGM2 = LFGM2,
      FGA2 = LFGA2,
      FG2perc = LFG2perc,
      FG2Freq = L2ptFreq,
      FGM3 = LFGM3,
      FGA3 = LFGA3,
      FG3perc = LFG3perc,
      FG3Freq = L3ptFreq,
      FTM = LFTM,
      FTA = LFTA,
      FTperc = LFTperc,
      FTFreq = LFTFreq,
      FTrate = LFTrate,
      OR = LOR,
      DR = LDR,
      Rebperc = LRebperc,
      OffRebperc = LOffRebperc,
      Ast = LAst,
      AstRatio = LAstRatio,
      TO = LTO,
      AstTO = LAstTO,
      TOperc = LTOperc,
      Stl = LStl,
      STLperc = LSTLperc,
      Blk = LBlk,
      Blkperc = LBlkperc,
      PF = LPF,
      PFperc = LPFperc,
      Pace = pace,
      Poss = Lposs,
      Off_rating = Loff_rating,
      Def_rating = Ldef_rating,
      Net_rating = Lnet_rating,
      OFGM = WFGM,
      OFGA = WFGA,
      OFGperc = WFGperc,
      OeFGperc = WeFGperc,
      OTrueShot = WTrueShot,
      OPPP = WPPP,
      OFGM2 = WFGM2,
      OFGA2 = WFGA2,
      OFG2perc = WFG2perc,
      OFG2Freq = W2ptFreq,
      OFGM3 = WFGM3,
      OFGA3 = WFGA3,
      OFG3perc = WFG3perc,
      OFG3Freq = W3ptFreq,
      OFTM = WFTM,
      OFTA = WFTA,
      OFTperc = WFTperc,
      OFTFreq = WFTFreq,
      OFTrate = WFTrate,
      O_OR = WOR,
      ODR = WDR,
      ORebperc = WRebperc,
      O_OffRebperc = WOffRebperc,
      OAst = WAst,
      OAstRatio = WAstRatio,
      OTO = WTO,
      OAstTO = WAstTO,
      OTOperc = WTOperc,
      OStl = WStl,
      OSTLperc = WSTLperc,
      OBlk = WBlk,
      OBlkperc = WBlkperc,
      OPF = WPF,
      OPFperc = WPFperc,
      OPoss = Wposs,
      O_Off_rating = Woff_rating,
      ODef_rating = Wdef_rating,
      ONet_rating = Wnet_rating,
      Seed = LSeed,
      Seed_n = LSeed_n,
      Seed_region = Lseed_region,
      Top_seeded_teams = Ltop_seeded_teams,
      OSeed = WSeed,
      OSeed_n = WSeed_n,
      OSeed_region = Wseed_region,
      OTop_seeded_teams = Wtop_seeded_teams
    ) %>%
    mutate(Winner = 0)
) %>% 
  left_join(teams, by= "TeamID") %>%
  arrange(Season, TeamID, DayNum)

# check NAs
na_counts <- sapply(team_games, function(y) sum(length(which(is.na(y)))))
na_counts <- data.frame(na_counts)
str(team_games)

final_data <- team_games %>% 
  mutate(
    Seed         = ifelse(is.na(Seed),  "None", Seed),
    OSeed        = ifelse(is.na(OSeed), "None", OSeed),
    Seed_n       = ifelse(is.na(Seed_n),  99, Seed_n),
    OSeed_n      = ifelse(is.na(OSeed_n), 99, OSeed_n),
    Seed_region  = ifelse(is.na(Seed_region),  "None", Seed_region),
    OSeed_region = ifelse(is.na(OSeed_region), "None", OSeed_region),
  ) %>% 
  replace(is.na(.), 0)


# Get the season averages for each team
season_team_stats_avg <- final_data %>%
  group_by(Season, TeamID, TeamName, Seed, Seed_n, Seed_region) %>%
  summarise(
    GP                     = n(),
    wins                   = sum(Winner),
    winPerc                = round(wins / GP, 2),
    AvgPoints              = round(mean(Score), 2),
    AvgPointsAllow         = round(mean(OScore), 2),
    NumOT                  = sum(NumOT),
    AvgFGM                 = round(mean(FGM), 2),
    AvgFGA                 = round(mean(FGA), 2),
    AvgFGperc              = round(mean(FGperc), 2),
    AvgeFGperc             = round(mean(eFGperc), 2),
    AvgTrueShot            = round(mean(TrueShot), 2),
    AvgPPP                 = round(mean(PPP), 2),
    AvgFGM2                = round(mean(FGM2), 2),
    AvgFGA2                = round(mean(FGA2), 2),
    AvgFG2perc             = round(mean(FG2perc), 2),
    AvgFG2Freq             = round(mean(FG2Freq), 2),
    AvgFGM3                = round(mean(FGM3), 2),
    AvgFGA3                = round(mean(FGA3), 2),
    AvgFG3perc             = round(mean(FG3perc), 2),
    AvgFG3Freq             = round(mean(FG3Freq), 2),
    AvgFTM                 = round(mean(FTM), 2),
    AvgFTA                 = round(mean(FTA), 2),
    AvgFTperc              = round(mean(FTperc), 2),
    AvgFTFreq              = round(mean(FTFreq), 2),
    AvgFTrate              = round(mean(FTrate), 2),
    AvgOR                  = round(mean(OR), 2),
    AvgDR                  = round(mean(DR), 2),
    AvgRebperc             = round(mean(Rebperc), 2),
    AvgOffRebperc          = round(mean(OffRebperc), 2),
    AvgAst                 = round(mean(Ast), 2),
    AstRatio               = round(mean(AstRatio), 2),
    AvgTO                  = round(mean(TO), 2),
    AvgAstTO               = round(mean(AstTO), 2),
    AvgTOperc              = round(mean(TOperc), 2),
    AvgStl                 = round(mean(Stl), 2),
    AvgSTLperc             = round(mean(STLperc), 2),
    AvgBlk                 = round(mean(Blk), 2),
    AvgBlkperc             = round(mean(Blkperc), 2),
    AvgPF                  = round(mean(PF), 2),
    AvgPFperc              = round(mean(PFperc), 2),
    AvgPace                = round(mean(Pace), 2),
    AvgPoss                = round(mean(Poss), 2),
    AvgOff_rating          = round(mean(Off_rating), 2),
    AvgDef_rating          = round(mean(Def_rating), 2),
    AvgNet_rating          = round(mean(Net_rating), 2),
    AvgFGMAllow            = round(mean(OFGM), 2),
    AvgFGAAllow            = round(mean(OFGA), 2),
    AvgFGpercAllow         = round(mean(OFGperc), 2),
    AvgeFGpercAllow        = round(mean(OeFGperc), 2),
    AvgTrueShotAllow       = round(mean(OTrueShot), 2),
    AvgPPPAllow            = round(mean(OPPP), 2),
    AvgFGM2Allow           = round(mean(OFGM2), 2),
    AvgFGA2Allow           = round(mean(OFGA2), 2),
    AvgFG2percAllow        = round(mean(OFG2perc), 2),
    AvgFG2FreqAllow        = round(mean(OFG2Freq), 2),
    AvgFGM3Allow           = round(mean(OFGM3), 2),
    AvgFGA3Allow           = round(mean(OFGA3), 2),
    AvgFG3percAllow        = round(mean(OFG3perc), 2),
    AvgFG3FreqAllow        = round(mean(OFG3Freq), 2),
    AvgFTMAllow            = round(mean(OFTM), 2),
    AvgFTAAllow            = round(mean(OFTA), 2),
    AvgFTpercAllow         = round(mean(OFTperc), 2),
    AvgFTFreqAllow         = round(mean(OFTFreq), 2),
    AvgFTrateAllow         = round(mean(OFTrate), 2),
    AvgORAllow             = round(mean(O_OR), 2),
    AvgDRAllow             = round(mean(ODR), 2),
    AvgRebpercAllow        = round(mean(ORebperc), 2),
    Avg_OffRebpercAllow    = round(mean(O_OffRebperc), 2),
    AvgAstAllow            = round(mean(OAst), 2),
    AvgAstRatioAllow       = round(mean(OAstRatio), 2),
    AvgTOAllow             = round(mean(OTO), 2),
    AvgAstTOAllow          = round(mean(OAstTO), 2),
    AvgTOpercAllow         = round(mean(OTOperc), 2),
    AvgStlAllow            = round(mean(OStl), 2),
    AvgSTLpercAllow        = round(mean(OSTLperc), 2),
    AvgBlkAllow            = round(mean(OBlk), 2),
    AvgBlkpercAllow        = round(mean(OBlkperc), 2),
    AvgPFAllow             = round(mean(OPF), 2),
    AvgPFpercAllow         = round(mean(OPFperc), 2),
    AvgPossAllow           = round(mean(OPoss), 2),
    Avg_Off_ratingAllow    = round(mean(O_Off_rating), 2),
    AvgDef_ratingAllow     = round(mean(ODef_rating), 2),
    AvgNet_ratingAllow     = round(mean(ONet_rating), 2),
    Top_seeded_teamsPlayed = sum(OTop_seeded_teams),
    AvgSeedPlayed          = round(sum(OSeed_n, na.rm = TRUE) / GP, 2)
  )


# Rearrange data so it matches submission file
results <- tourney_comp %>%
  filter(Season >= 2003) %>%
  mutate(
    team_id_diff = WTeamID - LTeamID,
    Team1 = case_when(team_id_diff < 0 ~ WTeamID,
                      team_id_diff > 0 ~ LTeamID),
    Team2 = case_when(team_id_diff > 0 ~ WTeamID,
                      team_id_diff < 0 ~ LTeamID),
    result = if_else(WTeamID == Team1, "Win", "Lose")
  ) %>% 
  select(
    Season,
    Team1,
    Team2,
    result
  )


# Join seeds onto the results for team1 and team2
team1_seeds <- seeds %>% 
  select(
    Season,
    seed_n,
    TeamID
  ) %>% 
  magrittr::set_colnames(c(
    "Season",
    "T1Seed",
    "Team1ID"
  ))
team2_seeds <- seeds %>% 
  select(
    Season,
    seed_n,
    TeamID
  ) %>% 
  magrittr::set_colnames(c(
    "Season",
    "T2Seed",
    "Team2ID"
  ))



## Get ratings for teams
Ratings <- season_team_stats_avg %>% 
  ungroup() %>% 
  select(
    Season,
    TeamID,
    Top_seeded_teamsPlayed,
    AvgSeedPlayed,
    AvgOff_rating,
    AvgDef_rating,
    AvgFGperc,
    AvgeFGperc,
    AvgTrueShot,
    AvgPPP,
    AvgFGpercAllow,
    AvgeFGpercAllow,
    AvgTrueShotAllow,
    AvgPPPAllow,
    AvgRebperc,
    AvgOffRebperc,
    AvgRebpercAllow,
    Avg_OffRebpercAllow,
    AvgAstTO,
    AvgAstTOAllow
  )
head(Ratings)


# Join seeds onto the results for team1 and team2
team1_rating <-
  Ratings %>% magrittr::set_colnames(
    c(
      "Season",
      "Team1ID",
      "T1Top_seeded_teamsPlayed",
      "T1AvgSeedPlayed",
      "T1AvgOff_rating",
      "T1AvgDef_rating",
      "T1AvgNet_rating",
      "T1AvgFGperc",
      "T1AvgeFGperc",
      "T1AvgTrueShot",
      "T1AvgPPP",
      "T1AvgFGpercAllow",
      "T1AvgeFGpercAllow",
      "T1AvgTrueShotAllow",
      "T1AvgPPPAllow",
      "T1AvgRebperc",
      "T1AvgOffRebperc",
      "T1AvgRebpercAllow",
      "T1Avg_OffRebpercAllow",
      "T1AvgAstTO",
      "T1AvgAstTOAllow"
    )
  )
team2_rating <-
  Ratings %>% magrittr::set_colnames(
    c(
      "Season",
      "Team2ID",
      "T2Top_seeded_teamsPlayed",
      "T2AvgSeedPlayed",
      "T2AvgOff_rating",
      "T2AvgDef_rating",
      "T2AvgNet_rating",
      "T2AvgFGperc",
      "T2AvgeFGperc",
      "T2AvgTrueShot",
      "T2AvgPPP",
      "T2AvgFGpercAllow",
      "T2AvgeFGpercAllow",
      "T2AvgTrueShotAllow",
      "T2AvgPPPAllow",
      "T2AvgRebperc",
      "T2AvgOffRebperc",
      "T2AvgRebpercAllow",
      "T2Avg_OffRebpercAllow",
      "T2AvgAstTO",
      "T2AvgAstTOAllow"
    )
  )

# Join seeds to training set
results <- results %>% 
  left_join(team1_seeds, by = c("Season", "Team1" = "Team1ID")) %>% 
  left_join(team2_seeds, by = c("Season", "Team2" = "Team2ID")) %>% 
  mutate(
    # Create relative round indicator 
    team1_seed_str = if_else(T1Seed < 9, 1,0),
    team2_seed_str = if_else(T2Seed < 9, 1,0),
    seed_diff      = T1Seed - T2Seed
  ) %>%   # get team ratings
  left_join(team1_rating, by = c("Season", "Team1" = "Team1ID")) %>% 
  left_join(team2_rating, by = c("Season", "Team2" = "Team2ID"))


str(results)
results$result <- factor(results$result, levels = c("Win", "Lose"))


### Build the Model on your own