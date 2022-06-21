### Defensive effects for non-goalies

### Creates matrix with columns containing the posterior distribution of a different player's
### seasonal contribution to a given shot outcome (SOG or MBS) relative to the median player.
### Saves the matrix in the analysis/output folder.

### Choose the the shot outcome (SOG or MBS), the season, and the model (with or without teams)
### by changing `model_fname` and `data_fname` variables 

library(tidyverse)
library(here)
library(glue)
library(rstan)
source(here("analysis/utils.R"))


#Pull in model
mod_dir <- "model/shots/output"
model_fname <- "ppool_sh-go_nt_s'21_2022-05-09.rds" #Change this to reflect desired outcome, model, and season
pm_fit <- readRDS(here(mod_dir,model_fname))

#Pull in data
data_dir <- "data"
data_fname <- "sog-model-data_o-sh-go_s'21_2022-04-25.rds" #Change this to reflect desired outcome and season
model_data <- readRDS(here(data_dir, data_fname))


seasons = pull_seasons(model_fname)
outcome = pull_outcome(data_fname)
model = pull_modname(model_fname)
nt = ifelse(grepl("nt",model), "nt", "")


names = model_data@Dimnames[[2]]
team_names <- names[seq(3, 63, by=2)] %>%
  str_sub(start=1, end=3)

np = which(names == names[65])[2]-65 #Number of non-goalie players
ng = ncol(model_data) - 2*np - 64 #Number of total players
ng_player_names <- names[64+1:np] #Non-goalie player names 
player_names <-  names[64+np+1:(np+ng)]


mu_draws <- extract(pm_fit, pars = "mu") %>% unlist()

offense_draws <- extract(pm_fit, pars ="beta_off") %>%
  data.frame() %>%
  setNames(ng_player_names) 

defense_draws <- extract(pm_fit, pars ="beta_def") %>%
  data.frame() %>%
  setNames(player_names) %>%
  rowwise() %>%
  mutate(m = median(c_across(1:np))) %>%
  ungroup()

shift_times = model_data[, 2]

nr = nrow(model_data)
nc = ncol(model_data)

shifts = as.matrix(model_data[, 65:nc])
indices = which(shifts !=0, arr.ind =T) %>%
  data.frame() %>%
  rename(shift = row, player_index = col) %>%
  arrange(shift)


## Given a shift number, for each player on the ice for the defensive team for this shift,
## this function returns the distribution of their contribution to limiting the opposing team's shots
## relative to the median player. 
## So the returned object is a 4,000 by 5 matrix. 
## Actually, to facilitate the calculation of the running total across all the shifts in the season in the 
## for loop that follows, it will return a list whose first element is the aforementioned matrix 
## and whose second entry is a vector of the indices of these players. 
form_post <- function(shift_no) {
  players = indices %>% 
    filter(shift == shift_no)
  
  offense = offense_draws %>% 
    select(players$player_index[1:5]) %>%
    rowSums()
  defense = defense_draws %>% 
    select(players$player_index[6:11]-np) %>%
    rowSums()
  
  Z = mu_draws + offense - defense + log(shift_times[shift_no])      
  m = matrix(0, nrow = 4000, ncol = 5)
  
  for (i in 6:10) {
    Z_med = Z + pull(defense_draws, players$player_index[i]-np) - pull(defense_draws, -1)
    m[,i-5] = exp(Z_med) - exp(Z) 
  }
  
  return(list(m, players$player_index[6:10] - np))
}

season_cont = matrix(0, nrow = 4000, ncol = np)

for (j in 1:nr){
  shift_cont = form_post(j)
  season_cont[, shift_cont[[2]]] = season_cont[, shift_cont[[2]]] + shift_cont[[1]]
  print(j)
} 


matrix_fname <- glue::glue("def_{outcome}_{nt}_{seasons}_{lubridate::today()}_2.rds")
saveRDS(season_cont, here("analysis", "output", matrix_fname))