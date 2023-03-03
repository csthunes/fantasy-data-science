library(dplyr)
library(tidyverse)
library(rvest)
library(janitor)
library(hablar)

getQBs <- function(year, week) {
  if(missing(week)) {
    url <- paste0("https://www.fantasypros.com/nfl/stats/qb.php?scoring=HALF&year=", year)
  } else {
    url <- paste0("https://www.fantasypros.com/nfl/stats/qb.php?scoring=HALF&range=week&year=", year, "&week=", week)
  }
  url %>% read_html %>% html_table(header = TRUE) %>% data.frame -> qbs.df
  names(qbs.df) <- qbs.df[1,]
  qbs.df <- qbs.df[-1,]
  qbs.df <- qbs.df %>% rename(PassATT = 4, PassYDS = 6, "PassY/A" = 7, PassTD = 8, RushATT = 11, RushYDS = 12, RushTD = 13) %>% filter(G >= "1") %>% retype()
  if(missing(week)) {
    qbs.df <- qbs.df %>% mutate(Season = year)
  } else {
    qbs.df <- qbs.df %>% mutate(Season = year, Week = week)
  }
  return(qbs.df)
}

allyears.df <- data.frame(matrix(ncol = 20, nrow = 0))
for (i in 2019:2022) {
  full.df <- getQBs(i)
  write_csv(full.df, paste0("data/qb/", i, "/raw/full.csv"))
  allyears.df <- rbind(allyears.df, full.df)
  allweeks.df <- data.frame(matrix(ncol = 20, nrow = 0))
  if (i == 2022) {
    for (j in 1:18) {
      qbs.df <- getQBs(2022, j)
      write_csv(qbs.df, paste0("data/qb/", 2022, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, qbs.df)
    }
  } else {
    for (j in 1:17) {
      qbs.df <- getQBs(i, j)
      write_csv(qbs.df, paste0("data/qb/", i, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, qbs.df)
    }
    if (i >= 2021) {
      qbs.df <- getQBs(2021, 18)
      write_csv(qbs.df, paste0("data/qb/", i, "/raw/wk18.csv"))
      allweeks.df <- rbind(allweeks.df, qbs.df)
    }
  }
  allweeks.df <- allweeks.df %>% arrange(Player, -FPTS)
  write_csv(allweeks.df, paste0("data/qb/", i, "/raw/allweeks.csv"))
}
allyears.df <- allyears.df %>% arrange(Player, -FPTS)
write_csv(allyears.df, "data/qb/allyears.csv")



getRBs <- function(year, week) {
  if(missing(week)) {
    url <- paste0("https://www.fantasypros.com/nfl/stats/rb.php?scoring=HALF&year=", year)
  } else {
    url <- paste0("https://www.fantasypros.com/nfl/stats/rb.php?scoring=HALF&range=week&year=", year, "&week=", week)
  }
  url %>% read_html %>% html_table(header = TRUE) %>% data.frame -> rbs.df
  names(rbs.df) <- rbs.df[1,]
  rbs.df <- rbs.df[-1,]
  rbs.df <- rbs.df %>% rename(RushYDS = 4, RushTD = 8, RecYDS = 11, RecTD = 13) %>% filter(G >= "1") %>% retype()
  if(missing(week)) {
    rbs.df <- rbs.df %>% mutate(Season = year)
  } else {
    rbs.df <- rbs.df %>% mutate(Season = year, Week = week)
  }
  return(rbs.df)
}

allyears.df <- data.frame(matrix(ncol = 20, nrow = 0))
for (i in 2019:2022) {
  full.df <- getRBs(i)
  write_csv(full.df, paste0("data/rb/", i, "/raw/full.csv"))
  allyears.df <- rbind(allyears.df, full.df)
  allweeks.df <- data.frame(matrix(ncol = 20, nrow = 0))
  if (i == 2022) {
    for (j in 1:18) {
      rbs.df <- getRBs(i, j)
      write_csv(rbs.df, paste0("data/rb/", i, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, rbs.df)
    }
  } else {
    for (j in 1:17) {
      rbs.df <- getRBs(i, j)
      write_csv(rbs.df, paste0("data/rb/", i, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, rbs.df)
    }
    if (i >= 2021) {
      rbs.df <- getRBs(2021, 18)
      write_csv(rbs.df, paste0("data/rb/", i, "/raw/wk18.csv"))
      allweeks.df <- rbind(allweeks.df, rbs.df)
    }
  }
  allweeks.df <- allweeks.df %>% arrange(Player, -FPTS)
  write_csv(allweeks.df, paste0("data/rb/", i, "/raw/allweeks.csv"))
}
allyears.df <- allyears.df %>% arrange(Player, -FPTS)
write_csv(allyears.df, "data/rb/allyears.csv")



getWRs <- function(year, week) {
  if(missing(week)) {
    url <- paste0("https://www.fantasypros.com/nfl/stats/wr.php?scoring=HALF&year=", year)
  } else {
    url <- paste0("https://www.fantasypros.com/nfl/stats/wr.php?scoring=HALF&range=week&year=", year, "&week=", week)
  }
  url %>% read_html %>% html_table(header = TRUE) %>% data.frame -> wrs.df
  names(wrs.df) <- wrs.df[1,]
  wrs.df <- wrs.df[-1,]
  wrs.df <- wrs.df %>% rename(RecYDS = 5, RecTD = 9, RushYDS = 11, RushTD = 12) %>% filter(G >= "1") %>% retype()
  if(missing(week)) {
    wrs.df <- wrs.df %>% mutate(Season = year)
  } else {
    wrs.df <- wrs.df %>% mutate(Season = year, Week = week)
  }
  return(wrs.df)
}

allyears.df <- data.frame(matrix(ncol = 19, nrow = 0))
for (i in 2019:2022) {
  full.df <- getWRs(i)
  write_csv(full.df, paste0("data/wr/", i, "/raw/full.csv"))
  allyears.df <- rbind(allyears.df, full.df)
  allweeks.df <- data.frame(matrix(ncol = 19, nrow = 0))
  if (i == 2022) {
    for (j in 1:18) {
      wrs.df <- getWRs(i, j)
      write_csv(wrs.df, paste0("data/wr/", i, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, wrs.df)
    }
  } else {
    for (j in 1:17) {
      wrs.df <- getWRs(i, j)
      write_csv(wrs.df, paste0("data/wr/", i, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, wrs.df)
    }
    if (i >= 2021) {
      wrs.df <- getWRs(2021, 18)
      write_csv(wrs.df, paste0("data/wr/", i, "/raw/wk18.csv"))
      allweeks.df <- rbind(allweeks.df, wrs.df)
    }
  }
  allweeks.df <- allweeks.df %>% arrange(Player, -FPTS)
  write_csv(allweeks.df, paste0("data/wr/", i, "/raw/allweeks.csv"))
}
allyears.df <- allyears.df %>% arrange(Player, -FPTS)
write_csv(allyears.df, "data/wr/allyears.csv")



getTEs <- function(year, week) {
  if(missing(week)) {
    url <- paste0("https://www.fantasypros.com/nfl/stats/te.php?scoring=HALF&year=", year)
  } else {
    url <- paste0("https://www.fantasypros.com/nfl/stats/te.php?scoring=HALF&range=week&year=", year, "&week=", week)
  }
  url %>% read_html %>% html_table(header = TRUE) %>% data.frame -> tes.df
  names(tes.df) <- tes.df[1,]
  tes.df <- tes.df[-1,]
  tes.df <- tes.df %>% rename(RecYDS = 5, RecTD = 9, RushYDS = 11, RushTD = 12) %>% filter(G >= "1") %>% retype()
  if(missing(week)) {
    tes.df <- tes.df %>% mutate(Season = year)
  } else {
    tes.df <- tes.df %>% mutate(Season = year, Week = week)
  }
  return(tes.df)
}

allyears.df <- data.frame(matrix(ncol = 19, nrow = 0))
for (i in 2019:2022) {
  full.df <- getTEs(i)
  write_csv(full.df, paste0("data/te/", i, "/raw/full.csv"))
  allyears.df <- rbind(allyears.df, full.df)
  allweeks.df <- data.frame(matrix(ncol = 19, nrow = 0))
  if (i == 2022) {
    for (j in 1:18) {
      tes.df <- getTEs(i, j)
      write_csv(tes.df, paste0("data/te/", i, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, tes.df)
    }
  } else {
    for (j in 1:17) {
      tes.df <- getTEs(i, j)
      write_csv(tes.df, paste0("data/te/", i, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, tes.df)
    }
    if (i >= 2021) {
      tes.df <- getTEs(2021, 18)
      write_csv(tes.df, paste0("data/te/", i, "/raw/wk18.csv"))
      allweeks.df <- rbind(allweeks.df, tes.df)
    }
  }
  allweeks.df <- allweeks.df %>% arrange(Player, -FPTS)
  write_csv(allweeks.df, paste0("data/te/", i, "/raw/allweeks.csv"))
}
allyears.df <- allyears.df %>% arrange(Player, -FPTS)
write_csv(allyears.df, "data/te/allyears.csv")



getKs <- function(year, week) {
  if(missing(week)) {
    url <- paste0("https://www.fantasypros.com/nfl/stats/k.php?scoring=HALF&year=", year)
  } else {
    url <- paste0("https://www.fantasypros.com/nfl/stats/k.php?scoring=HALF&range=week&year=", year, "&week=", week)
  }
  url %>% read_html %>% html_table(header = TRUE) %>% data.frame -> ks.df
  ks.df <- ks.df %>% rename("1-19" = 7, "20-29" = 8, "30-39" = 9, "40-49" = 10, "50+" = 11, "FPTS/G" = 16) %>% filter(G >= "1") %>% retype()
  if(missing(week)) {
    ks.df <- ks.df %>% mutate(Season = year)
  } else {
    ks.df <- ks.df %>% mutate(Season = year, Week = week)
  }
  return(ks.df)
}

allyears.df <- data.frame(matrix(ncol = 19, nrow = 0))
for (i in 2019:2022) {
  full.df <- getKs(i)
  write_csv(full.df, paste0("data/k/", i, "/raw/full.csv"))
  allyears.df <- rbind(allyears.df, full.df)
  allweeks.df <- data.frame(matrix(ncol = 19, nrow = 0))
  if (i == 2022) {
    for (j in 1:18) {
      ks.df <- getKs(i, j)
      write_csv(ks.df, paste0("data/k/", i, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, ks.df)
    }
  } else {
    for (j in 1:17) {
      ks.df <- getKs(i, j)
      write_csv(ks.df, paste0("data/k/", i, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, ks.df)
    }
    if (i >= 2021) {
      ks.df <- getKs(2021, 18)
      write_csv(ks.df, paste0("data/k/", i, "/raw/wk18.csv"))
      allweeks.df <- rbind(allweeks.df, ks.df)
    }
  }
  allweeks.df <- allweeks.df %>% arrange(Player, -FPTS)
  write_csv(allweeks.df, paste0("data/k/", i, "/raw/allweeks.csv"))
}
allyears.df <- allyears.df %>% arrange(Player, -FPTS)
write_csv(allyears.df, "data/k/allyears.csv")



getDSTs <- function(year, week) {
  if(missing(week)) {
    url <- paste0("https://www.fantasypros.com/nfl/stats/dst.php?scoring=HALF&year=", year)
  } else {
    url <- paste0("https://www.fantasypros.com/nfl/stats/dst.php?scoring=HALF&range=week&year=", year, "&week=", week)
  }
  url %>% read_html %>% html_table(header = TRUE) %>% data.frame -> dsts.df
  dsts.df <- dsts.df %>% rename(DefTD = 7, SpcTD = 9, "FPTS/G" = 12) %>% filter(G >= "1") %>% retype()
  if(missing(week)) {
    dsts.df <- dsts.df %>% mutate(Season = year)
  } else {
    dsts.df <- dsts.df %>% mutate(Season = year, Week = week)
  }
  return(dsts.df)
}

allyears.df <- data.frame(matrix(ncol = 15, nrow = 0))
for (i in 2019:2022) {
  full.df <- getDSTs(i)
  write_csv(full.df, paste0("data/dst/", i, "/raw/full.csv"))
  allyears.df <- rbind(allyears.df, full.df)
  allweeks.df <- data.frame(matrix(ncol = 15, nrow = 0))
  if (i == 2022) {
    for (j in 1:18) {
      dsts.df <- getDSTs(i, j)
      write_csv(dsts.df, paste0("data/dst/", i, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, dsts.df)
    }
  } else {
    for (j in 1:17) {
      dsts.df <- getDSTs(i, j)
      write_csv(dsts.df, paste0("data/dst/", i, "/raw/wk", j, ".csv"))
      allweeks.df <- rbind(allweeks.df, dsts.df)
    }
    if (i >= 2021) {
      dsts.df <- getDSTs(2021, 18)
      write_csv(dsts.df, paste0("data/dst/", i, "/raw/wk18.csv"))
      allweeks.df <- rbind(allweeks.df, dsts.df)
    }
  }
  allweeks.df <- allweeks.df %>% arrange(Player, -FPTS)
  write_csv(allweeks.df, paste0("data/dst/", i, "/raw/allweeks.csv"))
}
allyears.df <- allyears.df %>% arrange(Player, -FPTS)
write_csv(allyears.df, "data/dst/allyears.csv")