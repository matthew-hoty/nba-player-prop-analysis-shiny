library(ggplot2)
library(cowplot)
library(magick)
library(ggimage)
library(gridExtra)
library(png)
library(rsvg)
library(grid)
library(ggrepel)
library(dplyr)
library(ggalt)


generate_timeSeriesPlot <- function(df, player_schedule_df){
  df[nrow(df)+1,] <- NA
  
  opp <- player_schedule_df %>%
    filter(namePlayer == df$namePlayer[1]) %>%
    mutate(today_slugOppLoc = case_when( locationGame == 'H' ~ paste('v.',slugTeamOpp),
                                         locationGame == 'A' ~ paste('@', slugTeamOpp))
    ) %>%
    pull(today_slugOppLoc)
  
  df$dateGame[nrow(df)] <- as.Date(Sys.Date())
  df$target[nrow(df)] <- mean(df$target, na.rm=T)
  df$avg_target[nrow(df)] <- mean(df$target, na.rm=T)
  #df$sd_target <- NA
  df$sd_target[nrow(df)] <- sd(df$target, na.rm=T)
  df$currentHandicap[nrow(df)] <- mean(df$currentHandicap, na.rm=T)
  df$slugOppLoc[nrow(df)] = opp
  
  df = df %>% mutate(
    actualColor = case_when(
      success == 1 ~ "green4",
      success == 0 ~ "red4",
      is.na(success) ~ 'gray'
    )
  )
  
  scale_max = max(df$target, na.rm = T) * 1.1
  tsPlot <- ggplot(df, aes(x = dateGame, y = target)) +
    
    #Target
    geom_hline(aes(yintercept = currentHandicap,linetype = "Target")) + 
    
    #Avg
    #geom_hline(aes(yintercept = avg_target,linetype = "Avg", color = "Avg")) +
    #geom_ribbon(aes(ymin=avg_target - sd_target, ymax=avg_target + sd_target, linetype = 'Avg' ), alpha = 0.1)+
    
    # Pred CI
    geom_segment(
      data = df[nrow(df),], 
      aes(x = dateGame, xend=dateGame, y=max(0,avg_target - sd_target), yend=avg_target + sd_target), 
      size = 5,
      color = 'gray',
      lineend = 'round',
      alpha = 0.7
    ) + 
    # Actuals
    geom_xspline(size = 0.5, spline_shape=-0.5) + 
    geom_point(size = 7, color = 'white') +  
    geom_point(size = 4, color = df$actualColor, alpha = 1) + scale_colour_identity() +  
    
    #Labels
    geom_label_repel(
      aes(label=paste(target,slugOppLoc)), 
      size = 2.75,
      point.padding = 4,
      point.size = 2
    ) + 
    
    #Themes
    theme_light() + 
    theme(#panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +  # remove vertial gridlines
    theme(legend.position="top",
          legend.justification="right",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,0,-10,-10)) + 
    
    theme(text=element_text(family="Helvetica")) + 
    
    #scale_linetype_manual(values = c("Avg" = "dashed","Target" = "solid")) +
    #scale_color_manual(values = c("Avg" = "darkgreen","Target" = "black")) +
    # Axis Labels
    labs(x = "",
         y = "",
         linetype = NULL,
         color = NULL)   
  return(tsPlot)
}


generate_playerHeadshot <- function(url){
  playerHeadshotImg <- cowplot::ggdraw() + 
    cowplot::draw_image(url,clip = 'on') +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
}

generate_plotTitle <- function(playerName, marketName, marketDesc){
  #title_str <- paste(playerName,marketName)
  title_str <- marketDesc
  title_f <- textGrob(title_str, 
                      gp = gpar(fontsize = 15, fontface = 'bold',fontfamily = "Helvetica"),
                      x=0, hjust=-.05)
  return(title_f)
}


#df <- player_summary_df
generate_summaryTable <- function(df, player_schedule_df){
  summary_tbl <- df %>%
    left_join(player_schedule_df, by = c('namePlayer' = 'namePlayer', 'gameStartDate' = 'gameStartDate')) %>%
    mutate(today_slugOppLoc = case_when( locationGame == 'H' ~ paste('v.',slugTeamOpp),
                                         locationGame == 'A' ~ paste('@', slugTeamOpp))
           ) %>%
    group_by(namePlayer) %>%
    summarise(Odds = ifelse(mean(value) > 0, paste("+",mean(value),sep=''),mean(value)),
              `Matchup` = first(today_slugOppLoc), #paste(strwrap(first(gameDesc), width = 200),  collapse="\n"), #strwrap(first(gameDesc), width = 10, simplify = FALSE),
              `Avg L10` = round(mean(target),1),
              `Max L10` = round(max(target, na.rm = T),1)) %>%
    select(-namePlayer) %>%
    t()
  
  tbl <- tableGrob(summary_tbl,  cols = NULL, theme = ttheme_minimal())
  
  return(tbl)
}

generate_plots <- function(player_prop_hist_df, edge_df,player_schedule_df, input_row=1){
  if(is.null(input_row) ){
    input_row = 1
  }
  print(paste0('Generating plot for row ', input_row))

  selected_row <- edge_df[input_row,]
  player_summary_df <- player_prop_hist_df %>%
    filter(namePlayer %in% selected_row$Player &
             marketName %in% selected_row$Wager &
             marketDesc %in% selected_row$Market
             ) %>%
    mutate(avg_target = mean(target),
           sd_target = sd(target))
  
  summary_tbl <- generate_summaryTable(player_summary_df, player_schedule_df)
  tsPlot <- generate_timeSeriesPlot(player_summary_df, player_schedule_df)
  playerHeadshotImg <- generate_playerHeadshot(player_summary_df$urlPlayerHeadshot[1])
  title = generate_plotTitle(first(player_summary_df$namePlayer),first(player_summary_df$marketName), first(player_summary_df$marketDesc))
  
  layout <- rbind(c(1,3,3,3),
                  c(1,3,3,3),
                  c(2,3,3,3),
                  c(NA,3,3,3)
  )
  g<- grid.arrange(playerHeadshotImg,summary_tbl,tsPlot, 
                   nrow = 1, 
                   layout_matrix = layout,
                   top = title
  )
  
  return(g)
}