library(dplyr)
library(tidyverse)
library(stringr)

clean <- function(year) {
  allweeks <- read_csv(paste0("data/dst/", year, "/raw/allweeks.csv"))
  allweeks <- allweeks %>% group_by(Player) %>%
    summarize(Pos = "DST", GP = sum(G), Sacks = sum(SACK),
              GoodBadPct = (sum(FPTS >= 8.5) - sum(FPTS <= 3.5)) / GP, GreatTerriblePct = (sum(FPTS >= 15) - sum(FPTS <= 0)) / GP,
              StdDevFPTS = sd(FPTS), MedianFPTS = median(FPTS), MeanFPTS = mean(FPTS), AdjMeanFPTS = MeanFPTS/(StdDevFPTS^(1/4)) * 1.535,
              Score = MedianFPTS*.25 + AdjMeanFPTS*.65 + GoodBadPct*3 + GreatTerriblePct*3 + (MeanFPTS - MedianFPTS)*.25) %>%
    arrange(-Score) %>% filter(GP > 2) %>% select(Pos, Player, GP, Sacks, Score)
  names(allweeks)[names(allweeks) == "GP"] <- paste0("GP", year)
  names(allweeks)[names(allweeks) == "Sacks"] <- paste0("Sacks", year)
  names(allweeks)[names(allweeks) == "Score"] <- paste0("Score", year)
  return(allweeks)
}

DST2019 <- clean(2019)
DST2020 <- clean(2020)
DST2021 <- clean(2021)
DST2022 <- clean(2022)
allyears <- DST2022 %>% full_join(DST2021) %>% left_join(DST2020) %>% left_join(DST2019)
allscore <- data.frame(matrix(ncol = 15, nrow = 0))
factor22 <- .6
factor21 <- .25
factor20 <- .1
factor19 <- .05
for(i in 1:nrow(allyears)) {
  row <- allyears[i,] %>% mutate(Score = Score2022*factor22 + Score2021*factor21 + Score2020*factor20 + Score2019*factor19)
  allscore <- rbind(allscore, row)
}
allscore <- allscore %>% select(Pos, Player, Score2022, Score2021, Score2020, Score2019, Score) %>% arrange(-Score)
write_csv(allscore, "data/dst/allscore.csv")