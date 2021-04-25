
setwd("/Users/matthewhoty/repos/nbaStatsR/NBA-UI-Shiny")
source("utils/server/helpers.R")

obj <- pred_wager_edge_df()
edge_df <- obj$edge_df
player_prop_hist_df <- obj$player_prop_hist_df
schedule_df <- obj$schedule_df
#player_df <- obj$player_df
player_schedule_df <- obj$player_schedule_df

source('utils/server/plots/plot.R')
generate_plots(player_prop_hist_df, edge_df, player_schedule_df,1)


write.csv(schedule_all_df,'schedule.csv')
write.csv(player_prop_hist_df,"player_prop_hist_df.csv")
write.csv(edge_df, 'edge_df.csv')
