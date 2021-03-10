#https://www.aceodds.com/bet-calculator/odds-converter.html

calc_wager_prob <- function(odds) {
  if(odds == 1){
    odds = .999999
  }
  if(odds > 0){
    value = 100 / (odds + 100)
  }else{
    value = (odds * -1) / (odds*-1 + 100)
  }
  return(round(value,2))
}

calc_wager_payout <- function(odds){
  if(odds > 0){
    value = 100 * ((1 + (odds / 100)) - 1)
  }else{
    value = 100 * ((1 - (100 / odds)) - 1)
  } 
  return(round(value,0))
}

convert_probs_to_odds <- function(probs){
  if(probs == 1){
    probs = .999999
  }
  if(probs < .5){
    value = (100 / (probs*100 / 100)) - 100
    #return((100 - (100*probs)) / probs)
  }else{
    value = ((probs*100) / (1 - ((probs*100)/100))) * -1
  }
  return(round(value,0))
}

convert_odds_to_dec <- function(odds){
  if(odds > 0){
    value = 1 + (odds/100)
    #return((100 - (100*probs)) / probs)
  }else{
    value = 1 - (100 / odds)
  }
  return(round(value,2))
}
