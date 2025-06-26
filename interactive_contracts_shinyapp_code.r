
library(tidyverse)
library(nflverse)
library(shiny)
library(gt)
library(gtExtras)
library(shinydashboard)

### NFL NAMES

clean_positions <- read_csv("https://raw.githubusercontent.com/rogers1000/analyticsarticles/main/OTC_2023_rookie_deals_4%20-%20Sheet4.csv")

# position needs fixing before can be put into prod app 

contracts <- load_contracts() |>
  filter(is_active == TRUE) |>
  select(player,position,apy,cols) |>
  distinct() |>
  tidyr::unnest(cols) |>
  filter(year!='Total') |>
  filter(year == "2023") |>
  select(player,position,apy,cap2023 = cap_number) |>
  left_join(clean_positions, by = c("position" = "Position")) |>
  mutate(Clean_Position = ifelse(is.na(Clean_Position),"Unknown",Clean_Position))

nfl_player_filter <- load_rosters(seasons = 2022) |>
  select(full_name,team) |>
  unique() |>
  left_join(contracts, by = c("full_name" = "player")) |>
  mutate(team = ifelse(is.na(apy),"FA",team))

team_filter <- "BAL"

pos_filter <- "QB"

test <- nfl_player_filter |>
  mutate(team_filter = ifelse((team_filter == "CH") | 
                                (team_filter == team),1,0)) |>
  mutate(pos_filter = ifelse((pos_filter == "CH") |
                               (pos_filter == Clean_Position),1,0)) |>
  filter(team_filter == 1, pos_filter == 1)
  

nfl_player_names <- c("Click Here",dplyr::pull(nfl_player_filter,full_name) |> unique())

nfl_team_names <- c("Click Here",dplyr::pull(nfl_player_filter,team) |> unique(),"FA")

nfl_positions <- c("Click Here","QB","RB","WR","TE","OT","OG","OC",
                   "DI","ED","LB","CB","SAF","K","P","LS","Unknown")

ui <- dashboardPage(title = "Interactive Rookie Contracts", skin = "purple",
                    dashboardHeader(title = "Rookie Contracts"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Interactive Rookie Contract", tabName = "IRC")
                      )
                    ),
                    dashboardBody(
  fluidPage(
  
  titlePanel("Interactive Rookie Contract Table by ZacRogers.co.uk"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "it_contract_focus",
                  label = "Sort By",
                  choices = c("APY" = "apy",
                              "Cap Cost 2023" = "cap2023"),
                  selected = c("APY")),
      selectInput(inputId = "it_position",
                  label = "Rookie Position",
                  choices = c("QB","RB","WR","OT","OG","OC",
                              "DI","ED","LB","CB","SAF","K","P","LS"),
                  selected = c("QB"))
      ,sliderInput(inputId = "it_draft_pick",
                  label = "Rookie Draft Pick",
                  min = 1,
                  max = 261,
                  value = 1,
                  step = 1),
      selectInput(inputId = "it_position_filter",
                  label = "Filter Table by Position",
                  choices = c("Yes","No"),
                  selected = c("No")),
      selectInput(inputId = "it_rookie_contract_filter",
                  label = "Filter Table by Rookie Contract",
                  choices = c("Yes","No"),
                  selected = c("No")),
      selectInput(inputId = "it_nfl_player1_name",
                  label = "NFL Player 1 Name",
                  choices = c("Click Here"),
                  #choices = (nfl_player_filter |>
                  #             mutate(team_filter = ifelse((input$it_nfl_player1_team == "Click Here") | 
                  #                                           (input$it_nfl_player1_team == team),1,0)) |>
                  #             mutate(pos_filter = ifelse((input$it_nfl_player1_pos == "Click Here") |
                  #                                          (input$it_nfl_player1_pos == Clean_Position),1,0)) |>
                  #             filter(team_filter == 1, pos_filter == 1)),
                  selected = c("Click Here")),
      selectInput(inputId = "it_nfl_player1_team",
                  label = "NFL Player 1 Team",
                  choices = nfl_team_names,
                  selected = c("Click Here")),
      selectInput(inputId = "it_nfl_player1_pos",
                  label = "NFL Player 1 Position",
                  choices = nfl_positions,
                  selected = c("Click Here")),
      numericInput(inputId = "it_nfl_player1_apy",
                   label = "NFL Player 1 APY",
                   value = 0,
                   min = 0,
                   max = 55000000),
      numericInput(inputId = "it_nfl_player1_cost2023",
                   label = "NFL Player 1 2023 Cap Cost",
                   value = 0,
                   min = 0,
                   max = 55000000)
      
    ),
    
    mainPanel(
      
      gt_output(outputId = "it_summary_table"),
      gt_output(outputId = "interactive_table")
      
    )
  )
)
))

server <- function(input,output) {
  output$it_summary_table <- render_gt(
    {it_summary_clean_positions <- read_csv("https://raw.githubusercontent.com/rogers1000/analyticsarticles/main/OTC_2023_rookie_deals_4%20-%20Sheet4.csv")
    otc_2023_rookie_deals <- read_csv("https://raw.githubusercontent.com/rogers1000/analyticsarticles/main/OTC_2023_rookie_deals_6%20-%20Sheet6.csv") |>
      rename(pick = PICK) |>
      rename(cap2023 = "2023_CAP") |>
      mutate(apy = TOTAL_VALUE/4) |>
      select(pick,apy,cap2023) |>
      mutate(rookie23 = TRUE)
    
    it_summary_user_selected_draft_pick <- data.frame(player = input$it_draft_pick,
                                                      position = c("QB","RB","WR","TE","OT","OG","OC",
                                                                   "DI","ED","LB","CB","SAF"),
                                                      pick = input$it_draft_pick,
                                                      is_active = TRUE,
                                                      year_signed = 2023,
                                                      draft_year = 2023,
                                                      rookie_contract = TRUE,
                                                      inputted_player = TRUE
    ) |>
      left_join(otc_2023_rookie_deals, by = "pick") |>
      select(player,position,is_active,year_signed,apy,cap2023,draft_year,rookie_contract,inputted_player,rookie23)
    
    it_summary_df_interactive_contracts_rank <- load_contracts() |>
      select(player,position,is_active,year_signed,apy,draft_year,cols) |>
      mutate(rookie_contract = ifelse(year_signed == draft_year,TRUE,FALSE)) |>
      filter(is_active == TRUE) |>
      mutate(inputted_player = FALSE) |>
      mutate(rookie23 = FALSE) |>
      mutate(apy = apy*1000000) |>
      distinct() |>
      tidyr::unnest(cols) |>
      filter(year!='Total') |>
      filter(year == "2023") |>
      select(player,position,is_active,year_signed,apy,cap2023 = cap_number,draft_year,rookie_contract,inputted_player,rookie23) |>
      rbind(it_summary_user_selected_draft_pick) |>
      left_join(it_summary_clean_positions, by = c("position" = "Position")) |>
      group_by(Clean_Position) |>
      arrange(-apy) |>
      mutate(position_contract_rank = round(rank(desc(apy)),0)) |>
      ungroup() |>
      arrange(-apy) |>
      mutate(contract_rank = round(rank(desc(apy)),0)) |>
      mutate(rookie_contract = str_to_title(rookie_contract)) |>
      #mutate(apy = apy*1000000) |>
      pivot_wider(names_from = Clean_Position, values_from = .data[[input$it_contract_focus]],
                  values_fill = 0) |>
      filter(player == input$it_draft_pick) |>
      group_by(player) |>
      summarise(QB = max(QB),RB = max(RB),WR = max(WR),TE = max(TE),OT = max(OT),
                OG = max(OG),OC = max(OC),DI = max(DI),ED = max(ED),LB = max(LB),
                CB = max(CB),SAF = max(SAF)) |>
      rename(Pick = player)
    
    it_summary_df_interactive_contracts_perc <- load_contracts() |>
      select(player,position,is_active,year_signed,apy,draft_year,cols) |>
      mutate(rookie_contract = ifelse(year_signed == draft_year,TRUE,FALSE)) |>
      filter(is_active == TRUE) |>
      mutate(inputted_player = FALSE) |>
      mutate(rookie23 = FALSE) |>
      mutate(apy = apy*1000000) |>
      distinct() |>
      tidyr::unnest(cols) |>
      filter(year!='Total') |>
      filter(year == "2023") |>
      select(player,position,is_active,year_signed,apy,cap2023 = cap_number,draft_year,rookie_contract,inputted_player,rookie23) |>
      mutate(cap2023 = cap2023*1000000) |>
      rbind(it_summary_user_selected_draft_pick) |>
      left_join(it_summary_clean_positions, by = c("position" = "Position")) |>
      group_by(Clean_Position) |>
      arrange(-apy) |>
      mutate(max_pos_apy = max(apy)) |>
      mutate(max_pos_cap2023 = max(cap2023)) |>
      mutate(position_contract_rank = round(rank(desc(apy)),0)) |>
      ungroup() |>
      mutate(per_pos_max_apy = round(apy/max_pos_apy*100,0)) |>
      mutate(per_pos_max_cap2023 = round(cap2023/max_pos_cap2023*100,0)) |>
      arrange(-apy) |>
      mutate(contract_rank = round(rank(desc(apy)),0)) |>
      mutate(rookie_contract = str_to_title(rookie_contract)) |>
      #mutate(apy = apy*1000000) |>
      pivot_wider(names_from = Clean_Position, values_from = .data[[input$it_contract_focus]],
                  values_fill = 0) |>
      filter(player == input$it_draft_pick) |>
      group_by(player) |>
      summarise(QB = max(QB),RB = max(RB),WR = max(WR),TE = max(TE),OT = max(OT),
                OG = max(OG),OC = max(OC),DI = max(DI),ED = max(ED),LB = max(LB),
                CB = max(CB),SAF = max(SAF)) |>
      rename(Pick = player)
    
    pivot_description <- data.frame(rownum = c(1,2), desc = c("Rank","Percent"))
    
    it_summary_df_interactive_contracts <- rbind(it_summary_df_interactive_contracts_rank,it_summary_df_interactive_contracts_perc) |>
      mutate(rownum = row_number()) |>
      left_join(pivot_description, by = "rownum")
    
gt_it_summary <- gt(it_summary_df_interactive_contracts) |>
  cols_align(align = "center") |>
  cols_move(desc,Pick) |>
  cols_hide(rownum) |>
  cols_label(desc = "Desc") |>
  #gt_color_rows(columns = c("QB","RB","WR","TE","OT","OG","OC",
  #                          "DI","ED","LB","CB","SAF"),
  #              pal_type = "continuous") |>
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(QB))))
}
  )
  
#?gt_color_cols()
  
#### ABCDE PLAYER NAMES TABLE  
  
  output$interactive_table <- render_gt(
    {clean_positions <- read_csv("https://raw.githubusercontent.com/rogers1000/analyticsarticles/main/OTC_2023_rookie_deals_4%20-%20Sheet4.csv")
    otc_2023_rookie_deals <- read_csv("https://raw.githubusercontent.com/rogers1000/analyticsarticles/main/OTC_2023_rookie_deals_6%20-%20Sheet6.csv") |>
      rename(pick = PICK) |>
      mutate(apy = TOTAL_VALUE/4) |>
      rename(cap2023 = "2023_CAP") |>
      select(pick,apy,cap2023) |>
      mutate(rookie23 = TRUE)
    
    user_selected_draft_pick <- data.frame(player = "ROOKIE",
                                           position = c("QB","RB","WR","TE","OT","OG","OC",
                                                        "DI","ED","LB","CB","SAF"),
                                           #pick = 1,
                                           pick = input$it_draft_pick,
                                           is_active = TRUE,
                                           year_signed = 2023,
                                           draft_year = 2023,
                                           rookie_contract = TRUE,
                                           inputted_player = TRUE
                                           ) |>
      #filter(position == "QB") |>
      filter(position == input$it_position) |>
      left_join(otc_2023_rookie_deals, by = "pick") |>
      select(player,position,is_active,year_signed,apy,cap2023,draft_year,rookie_contract,inputted_player,rookie23)

    
    df_interactive_contracts <- load_contracts() |>
      select(player,position,is_active,year_signed,apy,draft_year,cols) |>
      mutate(rookie_contract = ifelse(year_signed == draft_year,TRUE,FALSE)) |>
      filter(is_active == TRUE) |>
      mutate(inputted_player = FALSE) |>
      mutate(rookie23 = FALSE) |>
      mutate(apy = apy*1000000) |>
      distinct() |>
      tidyr::unnest(cols) |>
      filter(year!='Total') |>
      filter(year == "2023") |>
      select(player,position,is_active,year_signed,apy,cap2023 = cap_number,draft_year,rookie_contract,inputted_player,rookie23) |>
      mutate(cap2023 = cap2023*1000000) |>
      rbind(user_selected_draft_pick) |>
      left_join(clean_positions, by = c("position" = "Position")) |>
      group_by(Clean_Position) |>
      arrange(-apy) |>
      mutate(position_contract_rank_apy = round(rank(desc(apy)),0)) |>
      arrange(-cap2023) |>
      mutate(position_contract_rank_cap2023 = round(rank(desc(cap2023)),0)) |>
      mutate(max_pos_apy = max(apy)) |>
      mutate(max_pos_cap2023 = max(cap2023)) |>
      ungroup() |>
      mutate(per_pos_max_apy = apy/max_pos_apy) |>
      mutate(per_pos_max_cap2023 = cap2023/max_pos_cap2023) |>
      arrange(-apy) |>
      mutate(contract_rank_apy = round(rank(desc(apy)),0)) |>
      arrange(-cap2023) |>
      mutate(contract_rank_cap2023 = round(rank(desc(cap2023)),0)) |>
      mutate(rookie_contract = str_to_title(rookie_contract)) |>
      mutate(position_rank_reactive = ifelse(input$it_contract_focus == "apy",position_contract_rank_apy,position_contract_rank_cap2023)) |>
      mutate(rank_reactive = ifelse(input$it_contract_focus == "apy",contract_rank_apy,contract_rank_cap2023)) |>
      arrange(desc(.data[[input$it_contract_focus]])) |>
      #mutate(apy = apy*1000000) |>
      mutate(filter_position = ifelse(
        (input$it_position_filter == "Yes" & Clean_Position == input$it_position)
        | (input$it_position_filter == "No"),1,0)) |>
      filter(filter_position == 1) |>
      mutate(filter_rc = ifelse(
        (input$it_rookie_contract_filter == "Yes" & rookie_contract == "True")
        | (input$it_rookie_contract_filter == "No"),1,0)) |>
      filter(filter_rc == 1) |>
    
    gt_it <- gt(df_interactive_contracts) |>
      cols_hide(c(position,is_active,draft_year,inputted_player
                  ,filter_position,filter_rc,rookie23,max_pos_apy,
                  max_pos_cap2023
                  )) |>
      cols_move_to_start(rank_reactive) |>
      cols_move(position_rank_reactive,rank_reactive) |>
      cols_move(Clean_Position,player) |>
      cols_move(rookie_contract,year_signed) |>
      cols_move(per_pos_max_apy,apy) |>
      cols_label(position_rank_reactive = "Position Rank",
                 player = "Player Name",
                 year_signed = "Contract Signed",
                 apy = "APY",
                 cap2023 = "Cap Cost 2023",
                 rookie_contract = "Rookie Contract",
                 Clean_Position = "Position",
                 rank_reactive = "Rank",
                 per_pos_max_apy = "% Max APY",
                 per_pos_max_cap2023 = "% Max Cap Cost 2023") |>
      cols_align(align = "center") |>
      fmt_currency(apy,suffixing = TRUE) |>
      fmt_currency(cap2023,suffixing = TRUE) |>
      #fmt_currency(max_pos_cap2023,suffixing = TRUE) |>
      fmt_percent(per_pos_max_apy) |>
      fmt_percent(per_pos_max_cap2023) |>
      gt_highlight_rows(rows = rookie23 == TRUE, fill = "#D0312D", alpha = 0.6) |>
      tab_header(title = md("**2023 Player Contracts**")) |>
      tab_style(
        style = list(
          cell_borders(
            sides = "left",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(player))))
    }
  )
}

shinyApp(ui = ui, server = server)

