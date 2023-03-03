library(dplyr)
library(tidyverse)
library(stringr)

clean <- function(year) {
  allweeks <- read_csv(paste0("data/qb/", year, "/raw/allweeks.csv")) %>% filter(PassATT >= 5 | RushATT >= 5)
  allweeks <- allweeks %>% group_by(Player) %>%
    summarize(Pos = "QB", GP = sum(G), PassATT = sum(PassATT), MedianFPTS = median(FPTS), 
              MeanFPTS = mean(FPTS), StdDevFPTS = sd(FPTS), AdjMeanFPTS = MeanFPTS/(StdDevFPTS^(1/4)) * 1.6425,
              GoodBadPct = (sum(FPTS >= 17.5) - sum(FPTS <= 12.5)) / GP,
              GreatTerriblePct = (sum(FPTS >= 22.5) - sum(FPTS <= 7.5)) / GP,
              GPct = 1 - ifelse(year != 2022 & GP < 10, (10 - GP)*.03, ifelse(year == 2022 & GP < 10, (10 - GP)*.03, 0)),
              Score = (MedianFPTS*.25 + AdjMeanFPTS*.65 + GoodBadPct*3 + GreatTerriblePct*3 + (MeanFPTS - MedianFPTS)*.25)*GPct) %>%
    arrange(-Score) %>% filter(PassATT >= 100) %>% select(Pos, Player, GP, PassATT, Score)
  names(allweeks)[names(allweeks) == "GP"] <- paste0("GP", year)
  names(allweeks)[names(allweeks) == "PassATT"] <- paste0("PassATT", year)
  names(allweeks)[names(allweeks) == "Score"] <- paste0("Score", year)
  return(allweeks)
}

QB2019 <- clean(2019)
QB2020 <- clean(2020)
QB2021 <- clean(2021)
QB2022 <- clean(2022)
allyears <- QB2022 %>% full_join(QB2021) %>% left_join(QB2020) %>% left_join(QB2019)
allyears <- allyears %>% filter(!(str_detect(Player, "(FA)") & is.na(GP2022)))
allscore <- data.frame(matrix(ncol = 15, nrow = 0))
for(i in 1:nrow(allyears)) {
  factor22 <- .6
  factor21 <- .25
  factor20 <- .1
  factor19 <- .05
  row <- allyears[i,]
  if (is.na(row[5])) {
    row[5] <- 0
    factor22 <- 0
    factor21 <- .625
    factor20 <- .25
    factor19 <- .125
    if (is.na(row[8])) {
      row[8] <- 0
      factor22 <- 0
      factor21 <- 0
      factor20 <- .6667
      factor19 <- .3333
      if (is.na(row[11])) {
        row[11] <- 0
        factor22 <- 0
        factor21 <- 0
        factor20 <- 0
        factor19 <- 1
        if (is.na(row[14])) {
          row[14] <- 0
          factor22 <- 0
          factor21 <- 0
          factor20 <- 0
          factor19 <- 0
        } 
      } else if (is.na(row[14])) {
        row[14] <- 0
        factor22 <- 0
        factor21 <- 0
        factor20 <- 1
        factor19 <- 0
      } 
    } else if (is.na(row[11])) {
      row[11] <- 0
      factor22 <- 0
      factor21 <- .8333
      factor20 <- 0
      factor19 <- .1667
      if (is.na(row[14])) {
        row[14] <- 0
        factor22 <- 0
        factor21 <- 1
        factor20 <- 0
        factor19 <- 0
      } 
    } else if (is.na(row[14])) {
      row[14] <- 0
      factor22 <- 0
      factor21 <- .7143
      factor20 <- .2857
      factor19 <- 0
    } 
  } else if (is.na(row[8])) {
    row[8] <- 0
    factor22 <- .8
    factor21 <- 0
    factor20 <- .1333
    factor19 <- .0667
    if (is.na(row[11])) {
      row[11] <- 0
      factor22 <- .9231
      factor21 <- 0
      factor20 <- 0
      factor19 <- .0769
      if (is.na(row[14])) {
        row[14] <- 0
        factor22 <- 1
        factor21 <- 0
        factor20 <- 0
        factor19 <- 0
      } 
    } else if (is.na(row[14])) {
      row[14] <- 0
      factor22 <- .8571
      factor21 <- 0
      factor20 <- .1429
      factor19 <- 0
    } 
  } else if (is.na(row[11])) {
    row[11] <- 0
    factor22 <- .6667
    factor21 <- .2778
    factor20 <- 0
    factor19 <- .0555
    if (is.na(row[14])) {
      row[14] <- 0
      factor22 <- .7059
      factor21 <- .2941
      factor20 <- 0
      factor19 <- 0
    } 
  } else if (is.na(row[14])) {
    row[14] <- 0
    factor22 <- .6316
    factor21 <- .2631
    factor20 <- .1053
    factor19 <- 0
  } 
  row <- row %>% mutate(Score = Score2022*factor22 + Score2021*factor21 + Score2020*factor20 + Score2019*factor19)
  if (row$Score2022 == 0) {
    row$Score = row$Score*.82
  }
  allscore <- rbind(allscore, row)
}
allscore <- allscore %>% select(Pos, Player, Score2022, Score2021, Score2020, Score2019, Score) %>% arrange(-Score)
write_csv(allscore, "data/qb/allscore.csv")