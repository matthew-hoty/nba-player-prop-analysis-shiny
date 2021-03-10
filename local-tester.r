
setwd("/Users/matthewhoty/repos/nbaStatsR/NBA-UI-Shiny")
source("utils/helpers.R")

obj <- pred_wager_edge_df()
edge_df <- obj$edge_df
player_prop_hist_df <- obj$player_prop_hist_df
schedule_df <- obj$schedule_df

source('utils/plot.R')
generate_plots(player_prop_hist_df, edge_df,1)


