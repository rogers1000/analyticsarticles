
library(tidyverse)
library(nflverse)

load_rosters_weekly()

yearly_team_roster <- load_rosters_weekly(seasons = c(2016:2021)) |>
  mutate(yearly_weekly_team = paste0(season,"_",week,"_",team)) |>
  select(gsis_id,yearly_weekly_team)

down_personnel_usage <- load_participation(seasons = 2021, include_pbp = TRUE) |>
  filter(!is.na(down),!is.na(offense_personnel),n_offense == 11) |>
  filter(!str_detect(offense_personnel,"OL")) |>
  filter(!str_detect(offense_personnel,"QB")) |>
  filter(!str_detect(offense_personnel,"K")) |>
  filter(!str_detect(offense_personnel,"DL")) |>
  filter(!str_detect(offense_personnel,"P")) |>
  filter(!str_detect(offense_personnel,"LS")) |>
  filter(!str_detect(offense_personnel,"DB")) |>
select(season,week,down,posteam,offense_personnel,offense_players) |>
  separate(offense_personnel, into = c("RB","TE","WR"), sep = ", ") |>
  mutate(RB = as.double(str_remove(RB," RB")),
         WR = as.double(str_remove(WR, " WR")),
         TE = as.double(str_remove(TE, " TE"))) |>
  filter(!is.na(TE)) |>
  separate(offense_players,into = c("P1","P2","P3","P4","P5","P6","P7",
                                    "P8","P9","P10","P11"), sep = ";") |>
  mutate(yearly_weekly_team = paste0(season,"_",week,"_",posteam)) |>
  mutate(row_num = row_number())

view(down_personnel_usage)

team_down_count <- down_personnel_usage |>
  group_by(posteam,down) |>
  summarise(count = n())

view(team_down_count)
  
P1 <- dplyr::pull(down_personnel_usage,P1)
P2 <- dplyr::pull(down_personnel_usage,P2)
P3 <- dplyr::pull(down_personnel_usage,P3)
P4 <- dplyr::pull(down_personnel_usage,P4)
P5 <- dplyr::pull(down_personnel_usage,P5)
P6 <- dplyr::pull(down_personnel_usage,P6)
P7 <- dplyr::pull(down_personnel_usage,P7)
P8 <- dplyr::pull(down_personnel_usage,P8)
P9 <- dplyr::pull(down_personnel_usage,P9)
P10 <- dplyr::pull(down_personnel_usage,P10)
P11 <- dplyr::pull(down_personnel_usage,P11)
yearly_weekly_team <- dplyr::pull(down_personnel_usage,yearly_weekly_team)
row_num <- dplyr::pull(down_personnel_usage,row_num)

all_Ps_df <- data.frame(gsis_id = c(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11), row_num = row_num, yearly_weekly_team = yearly_weekly_team) |>
  select(-row_num) |>
  unique()

view(all_Ps_df)

test <- all_Ps_df |>
  left_join(down_personnel_usage, by = c("yearly_weekly_team"))

view(test)

#?separate()

view(down_personnel_usage)

write.csv(down_personnel_usage,"C:\\Users\\zacro\\Documents\\Code Nerd Shit\\Big Data Bowl 2023\\test.csv")
