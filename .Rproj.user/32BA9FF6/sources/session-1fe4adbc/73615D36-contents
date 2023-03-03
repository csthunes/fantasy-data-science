library(dplyr)
library(tidyverse)

qb <- read_csv("data/qb/allscore.csv")
rb <- read_csv("data/rb/allscore.csv")
wr <- read_csv("data/wr/allscore.csv")
te <- read_csv("data/te/allscore.csv")
k <- read_csv("data/k/allscore.csv")
dst <- read_csv("data/dst/allscore.csv")

all <- qb %>% full_join(rb) %>% full_join(wr) %>% full_join(te) %>% full_join(k) %>% full_join(dst)

#situation <- read_csv("data/situation.csv")
rookies <- read_csv("data/rookies.csv")
all <- all %>% rbind(rookies)
all <- all %>% mutate(ValueFactor = ifelse(Pos == "QB", 2.5, ifelse(Pos == "WR", 5, ifelse(Pos == "TE", 5, ifelse(Pos == "K", 4.5, ifelse(Pos == "DST", 5, 5))))), Value = Score*ValueFactor) %>% arrange(-Value)
all <- all %>% mutate(AdjValue = Score*ValueFactor) %>%
  arrange(-AdjValue) %>% mutate(Rank = row_number(), Round = ifelse(Rank <= 150, ceiling(Rank / 10), NA)) %>%
  mutate_if(is.numeric, round, digits = 3)

qbRankings <- all %>% filter(Pos == "QB") %>% mutate(PosRank = paste0("QB", row_number()), Role = ifelse(row_number() <= 10, "QB1", ifelse(row_number() <= 20, "QB2", "Depth"))) %>% 
  select(Round, Rank, Pos, Player, PosRank, Role, everything(), -Value)
write_csv(qbRankings, "data/qb_rankings.csv")
rbRankings <- all %>% filter(Pos == "RB") %>% mutate(PosRank = paste0("RB", row_number()), Role = ifelse(row_number() <= 10, "RB1", ifelse(row_number() <= 20, "RB2", ifelse(row_number() <= 30, "RB3", ifelse(row_number() <= 40, "RB4", "Depth"))))) %>% 
  select(Round, Rank, Pos, Player, PosRank, Role, everything(), -Value)
write_csv(rbRankings, "data/rb_rankings_half.csv")
wrRankings <- all %>% filter(Pos == "WR") %>% mutate(PosRank = paste0("WR", row_number()), Role = ifelse(row_number() <= 10, "WR1", ifelse(row_number() <= 20, "WR2", ifelse(row_number() <= 30, "WR3", ifelse(row_number() <= 40, "WR4", "Depth"))))) %>% 
  select(Round, Rank, Pos, Player, PosRank, Role, everything(), -Value)
write_csv(wrRankings, "data/wr_rankings_half.csv")
teRankings <- all %>% filter(Pos == "TE") %>% mutate(PosRank = paste0("TE", row_number()), Role = ifelse(row_number() <= 10, "TE1", ifelse(row_number() <= 20, "TE2", "Depth"))) %>% 
  select(Round, Rank, Pos, Player, PosRank, Role, everything(), -Value)
write_csv(teRankings, "data/te_rankings_half.csv")
kRankings <- all %>% filter(Pos == "K") %>% mutate(PosRank = paste0("K", row_number()), Role = ifelse(row_number() <= 10, "K1", ifelse(row_number() <= 20, "K2", "Depth"))) %>% 
  select(Round, Rank, Pos, Player, PosRank, Role, everything(), -Value)
write_csv(kRankings, "data/k_rankings.csv")
dstRankings <- all %>% filter(Pos == "DST") %>% mutate(PosRank = paste0("DST", row_number()), Role = ifelse(row_number() <= 10, "DST1", ifelse(row_number() <= 20, "DST2", "Depth"))) %>% 
  select(Round, Rank, Pos, Player, PosRank, Role, everything(), -Value)
write_csv(dstRankings, "data/dst_rankings.csv")

allRankings <- qbRankings %>% rbind(rbRankings) %>% rbind(wrRankings) %>% rbind(teRankings) %>% rbind(kRankings) %>% rbind(dstRankings) %>% arrange(Rank)
write_csv(allRankings, "data/value_rankings_half.csv")

mocks <- allRankings %>% left_join(read_csv("data/mocks.csv"))
mocks <- mocks %>% mutate(NFLMDAvg = (ifelse(is.na(NFLMD1), 16, NFLMD1) + ifelse(is.na(NFLMD2), 16, NFLMD2) + ifelse(is.na(NFLMD3), 16, NFLMD3)) / 3, Diff = NFLMDAvg - ifelse(is.na(Round), 16, Round)) %>%
  mutate_if(is.numeric, round, digits = 3)
mocks$NFLMDAvg = round(mocks$NFLMDAvg)
write_csv(mocks, "data/value_rankings_mocks.csv")
valuepicks <- mocks %>% filter(Diff >= 0)
write_csv(valuepicks, "data/value_picks.csv")