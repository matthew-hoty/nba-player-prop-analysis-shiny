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

generate_timeSeriesPlot <- function(df){
  scale_max = max(df$target, na.rm = T) * 1.1
  tsPlot <- ggplot(df, aes(x = dateGame, y = target)) +
    geom_point(size = 2) + 
    geom_line(size = .25) + 
    
    geom_hline(aes(yintercept = currentHandicap,linetype = "Target", color = 'Target')) + 

    geom_hline(aes(yintercept = avg_target,linetype = "Avg", color = "Avg")) + 
    
    geom_label_repel(aes(label=paste(target,slugOppLoc))) + 

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
    
    scale_linetype_manual(values = c("Avg" = "dashed","Target" = "solid")) +
    scale_color_manual(values = c("Avg" = "darkgreen","Target" = "black")) +
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

generate_plotTitle <- function(playerName, marketName){
  title_str <- paste(playerName,marketName)
  title_f <- textGrob(title_str, 
                      gp = gpar(fontsize = 15, fontface = 'bold',fontfamily = "Helvetica"),
                      x=0, hjust=-.05)
  return(title_f)
}

generate_summaryTable <- function(df){
  summary_tbl <- df %>%
    group_by(namePlayer) %>%
    summarise(Odds = ifelse(mean(value) > 0, paste("+",mean(value),sep=''),mean(value)),
              `Matchup` = 'TBD', #paste(strwrap(first(gameDesc), width = 200),  collapse="\n"), #strwrap(first(gameDesc), width = 10, simplify = FALSE),
              `Avg L10` = mean(target),
              `Max L10` = max(target, na.rm = T)) %>%
    select(-namePlayer) %>%
    t()
  
  tbl <- tableGrob(summary_tbl,  cols = NULL, theme = ttheme_minimal())
  
  return(tbl)
}

generate_plots <- function(player_wager_df, breakdown_df,input_row=1){
  if(is.null(input_row) ){
    input_row = 1
  }

  selected_row <- breakdown_df[input_row,]
  player_summary_df <- player_wager_df %>%
    filter(namePlayer %in% selected_row$Player) %>%
    filter(marketName %in% selected_row$Wager) %>%
    mutate(avg_target = mean(target))
  
  summary_tbl <- generate_summaryTable(player_summary_df)
  tsPlot <- generate_timeSeriesPlot(player_summary_df)
  playerHeadshotImg <- generate_playerHeadshot(player_summary_df$urlPlayerHeadshot[1])
  title = generate_plotTitle(first(player_summary_df$namePlayer),first(player_summary_df$marketName))
  
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