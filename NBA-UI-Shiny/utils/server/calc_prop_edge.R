#### Calculate Edge ####
#df <- player_prop_hist_df

calculate_edge <- function(df){
  edge_df <- df %>%
    group_by(marketName, namePlayer, marketDesc) %>%
    summarise(
      odds = mean(value),
      est_probs = round(mean(success_weight),2),
      wager_probs = mean(calc_wager_prob(odds)),
      wager_dec = mean(convert_odds_to_dec(odds)),
      wager_payout = mean(calc_wager_payout(odds)),
      est_odds = convert_probs_to_odds(est_probs),
      est_payout = mean(calc_wager_payout(est_odds)),
      wager_ev = (wager_probs*wager_payout),
      est_ev = (est_probs * est_payout),
      pred_upside = round(wager_ev - est_ev,2),
      edge = ((est_probs * wager_dec) - 1),
      ev = (wager_payout * est_probs) - (100 * (1-est_probs))
    ) %>%
    arrange(desc(pred_upside)) %>%
    select(`Player` = namePlayer, 
           `Wager` = marketName, 
           `Market` = marketDesc,
           `Odds` = odds, 
           `My Prob` = est_probs,
           `House Prob` = wager_probs,
           `My Value` = pred_upside)
}