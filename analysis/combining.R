##-----------------------------------------------------------------------------#
# Pull in data and Stan object 
##-----------------------------------------------------------------------------#


if (!exists("params")) {
  params <- list(
    model_fname = "model/ppool_combined.RData",
    data_fname = "data/model-data_s'18'19'20'21_combined_2022-03-10.rds"
  )
}


###### Stan object ---------------------------------------------------------------#
ext <- tools::file_ext(params[["model_fname"]])

if (ext == "RData") {
  load(file = here(params[["model_fname"]])) # `pm_fit`  
} else if (ext == "rds") {
  pm_fit <- readRDS(here(params[["model_fname"]]))
}


######## Data ---------------------------------------------------------------#
model_data <- readRDS(here(params[["data_fname"]]))
team_names <- colnames(model_data)[2:32]
player_names <- colnames(model_data)[33:ncol(model_data)]

df_of_draws <- as.data.frame(pm_fit) %>%
  select(starts_with(c("alfa","beta"))) %>%
  rename_with(~team_names, 1:31) %>%
  rename_with(~player_names, 32:(31+length(player_names)))


##########
 
  


##I use the get_rosters function from the HockeyR package to obtain players' team memberships.
# An alternative for getting players' team memberships, the function get_player_stats_hr, is buggy 

# If a player is traded during the season, they appear on both teams' rosters for that season. 
# For this reason, there are players (e.g. Gustav Nyquist in 2019) who get multiple columns ("Gustav.Nyquist:SJS" and "Gustav.Nyquist:DET") 
# in the player_plus_team_draws matrix which gets output 

player_plus_team_draws <- function(pm_fit, season, team_names, player_names) {
  
  #Obtain data frame of posterior samples 
  df_of_draws <- as.data.frame(pm_fit) %>%
    select(starts_with(c("alfa", "beta"))) %>% 
    rename_with(~team_names, 1:31) %>%
    rename_with(~player_names, 32:(31+length(player_names)))
  
  np = length(player_names); nt=length(team_names)
  X = matrix(0, nrow = np+nt, ncol = np)
  colnames(X) = seq_len(ncol(X))
  t = 1 #Counter for iteration through teams
  p = 1 #Counter for iteration through players
  
  for (team in team_names) {
    team_players = get_rosters(team, season)$player
    ms = match(gsub(" ", ".", team_players), player_names)
    
    for (m in ms[!is.na(ms)]) {
      X[c(t, nt+m) , p] = 1
      colnames(X)[p] = paste(player_names[m],team_names[t], sep=":") 
      p = p+1
    }  
    t = t+1
  }
  
  player_plus_team_draws = as.matrix(df_of_draws) %*% X
  return(list(X, player_plus_team_draws))  
}




L = player_plus_team_draws(pm_fit, 2019, team_names, player_names)
play_plus_team = L[[2]]

grep("Sidney.Crosby", colnames(play_plus_team))
hist(play_plus_team[,35])


grep("McDavid", colnames(play_plus_team))
hist(play_plus_team[,735])


grep("Ovechkin", colnames(play_plus_team))
hist(play_plus_team[,572])


grep("Nyquist", colnames(play_plus_team))
hist(play_plus_team[,84]) #With Sharks who had an average score differential of +0.37 during 2018-2019 season
hist(play_plus_team[,271]) #With Red Wings, who had an avearage score differential of -0.59 during the 2018-2019 season



