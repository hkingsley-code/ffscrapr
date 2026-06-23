# server.R

server <- function(input, output, session) {

  # ── Helpers ──────────────────────────────────────────────────────────────

  fmt_pts  <- function(x) formatC(round(x, 1), format = "f", digits = 1, big.mark = ",")
  fmt_pct  <- function(x) paste0(round(x * 100, 1), "%")

  result_colour <- function(res) {
    case_when(
      res == "W" ~ "#d4edda",
      res == "L" ~ "#f8d7da",
      res == "T" ~ "#fff3cd",
      TRUE       ~ "#ffffff"
    )
  }

  # ────────────────────────────────────────────────────────────────────────────
  # Tab 1: Weekly Scores
  # ────────────────────────────────────────────────────────────────────────────

  # Update team choices when season changes
  observe({
    req(input$ws_season)
    teams_in_season <- schedules_named %>%
      filter(season == as.integer(input$ws_season), !is.na(display_name)) %>%
      pull(display_name) %>%
      unique() %>%
      sort()
    updateSelectInput(
      session, "ws_teams",
      choices  = c("All teams" = "all", setNames(teams_in_season, teams_in_season)),
      selected = "all"
    )
  })

  ws_data <- reactive({
    req(input$ws_season)
    df <- schedules_named %>%
      filter(season == as.integer(input$ws_season), !is.na(franchise_score))

    if (!is.null(input$ws_teams) && input$ws_teams != "all") {
      df <- df %>% filter(display_name == input$ws_teams)
    }
    df
  })

  output$ws_chart <- renderPlotly({
    df <- ws_data()
    req(nrow(df) > 0)

    # Cumulative points over weeks per team
    cum_df <- df %>%
      arrange(display_name, week) %>%
      group_by(display_name) %>%
      mutate(cumulative_pts = cumsum(coalesce(franchise_score, 0))) %>%
      ungroup()

    p <- ggplot(cum_df, aes(x = week, y = cumulative_pts, colour = display_name,
                            group = display_name,
                            text = paste0(display_name, "<br>Week ", week,
                                          "<br>Weekly: ", round(franchise_score, 1),
                                          "<br>Cumulative: ", round(cumulative_pts, 1)))) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = "Week", y = "Cumulative Points", colour = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "right")

    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest") %>%
      config(displayModeBar = FALSE)
  })

  output$ws_table <- renderDT({
    df <- ws_data() %>%
      select(
        Week     = week,
        Team     = display_name,
        Score    = franchise_score,
        Result   = result,
        Opponent = opponent_name,
        `Opp Score` = opponent_score
      ) %>%
      arrange(Week, Team)

    datatable(
      df,
      rownames  = FALSE,
      filter    = "top",
      options   = list(pageLength = 20, dom = "lrtip")
    ) %>%
      formatStyle(
        "Result",
        backgroundColor = styleEqual(
          c("W", "L", "T"),
          c("#d4edda", "#f8d7da", "#fff3cd")
        )
      ) %>%
      formatRound(c("Score", "Opp Score"), digits = 1)
  })

  # ────────────────────────────────────────────────────────────────────────────
  # Tab 2: Historical Records
  # ────────────────────────────────────────────────────────────────────────────

  alltime_records <- reactive({
    standings_named %>%
      group_by(franchise_id) %>%
      summarise(
        seasons       = n(),
        total_wins    = sum(coalesce(h2h_wins,   0L), na.rm = TRUE),
        total_losses  = sum(coalesce(h2h_losses, 0L), na.rm = TRUE),
        total_ties    = sum(coalesce(h2h_ties,   0L), na.rm = TRUE),
        total_pf      = sum(coalesce(points_for,     0), na.rm = TRUE),
        total_pa      = sum(coalesce(points_against, 0), na.rm = TRUE),
        championships = sum(coalesce(league_rank == 1L, FALSE), na.rm = TRUE),
        .groups       = "drop"
      ) %>%
      mutate(
        total_games = total_wins + total_losses + total_ties,
        win_pct     = if_else(total_games > 0,
                              total_wins / total_games, NA_real_)
      ) %>%
      left_join(franchise_canonical %>% select(franchise_id, display_name),
                by = "franchise_id") %>%
      arrange(desc(total_wins))
  })

  output$records_summary_cards <- renderUI({
    at <- alltime_records()
    top_wins  <- at %>% slice_max(total_wins, n = 1)
    top_pts   <- at %>% slice_max(total_pf,   n = 1)
    top_champ <- at %>% slice_max(championships, n = 1)

    fluidRow(
      column(4,
        div(class = "card text-center mb-3",
          div(class = "card-body",
            h6(class = "card-subtitle text-muted", "Most All-Time Wins"),
            h4(class = "card-title", top_wins$display_name),
            p(class = "card-text",
              top_wins$total_wins, "W – ", top_wins$total_losses, "L")
          )
        )
      ),
      column(4,
        div(class = "card text-center mb-3",
          div(class = "card-body",
            h6(class = "card-subtitle text-muted", "Most Total Points"),
            h4(class = "card-title", top_pts$display_name),
            p(class = "card-text", fmt_pts(top_pts$total_pf), " pts")
          )
        )
      ),
      column(4,
        div(class = "card text-center mb-3",
          div(class = "card-body",
            h6(class = "card-subtitle text-muted", "Most Championships"),
            h4(class = "card-title", top_champ$display_name),
            p(class = "card-text", top_champ$championships,
              if (top_champ$championships == 1) " title" else " titles")
          )
        )
      )
    )
  })

  output$hr_alltime_table <- renderDT({
    at <- alltime_records() %>%
      transmute(
        Team          = display_name,
        Seasons       = seasons,
        W             = total_wins,
        L             = total_losses,
        T             = total_ties,
        `Win%`        = fmt_pct(win_pct),
        `Points For`  = fmt_pts(total_pf),
        `Points Agst` = fmt_pts(total_pa),
        Championships = championships
      )

    datatable(at, rownames = FALSE,
              options = list(pageLength = 20, dom = "lrtip")) %>%
      formatStyle("Championships",
                  backgroundColor = styleInterval(0, c("white", "#fff3cd")))
  })

  output$hr_champs_table <- renderDT({
    champs <- standings_named %>%
      filter(league_rank == 1L) %>%
      arrange(desc(season)) %>%
      transmute(
        Season       = season,
        Champion     = coalesce(display_name, franchise_name),
        `Points For` = fmt_pts(points_for),
        W            = h2h_wins,
        L            = h2h_losses
      )

    datatable(champs, rownames = FALSE,
              options = list(pageLength = 20, dom = "lrtip"))
  })

  output$hr_season_table <- renderDT({
    req(input$hr_season)
    df <- standings_named %>%
      filter(season == as.integer(input$hr_season)) %>%
      arrange(coalesce(league_rank, 99L)) %>%
      transmute(
        Rank         = league_rank,
        Team         = coalesce(display_name, franchise_name),
        W            = h2h_wins,
        L            = h2h_losses,
        T            = h2h_ties,
        `Win%`       = fmt_pct(h2h_winpct),
        `Points For` = fmt_pts(points_for),
        `Pts Agst`   = fmt_pts(points_against)
      )

    datatable(df, rownames = FALSE,
              options = list(pageLength = 20, dom = "lrtip"))
  })

  output$hr_heatmap <- renderPlotly({
    heat_df <- standings_named %>%
      mutate(team = coalesce(display_name, franchise_name)) %>%
      filter(!is.na(league_rank), !is.na(team)) %>%
      select(season, team, rank = league_rank)

    n_teams <- heat_df %>% pull(rank) %>% max(na.rm = TRUE)

    p <- ggplot(heat_df, aes(x = season, y = team, fill = rank,
                             text = paste0(team, "<br>", season, ": Rank #", rank))) +
      geom_tile(colour = "white", linewidth = 0.5) +
      scale_fill_gradient(low = "#2196f3", high = "#f8d7da",
                          name = "Final Rank", breaks = seq(1, n_teams)) +
      scale_x_continuous(breaks = unique(heat_df$season)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right")

    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest") %>%
      config(displayModeBar = FALSE)
  })

  # ────────────────────────────────────────────────────────────────────────────
  # Tab 3: Trades
  # ────────────────────────────────────────────────────────────────────────────

  output$tr_table <- renderDT({
    req(HAS_TRADE_DATA)
    df <- all_transactions

    if (!is.null(input$tr_season) && input$tr_season != "all") {
      df <- df %>% filter(season == as.integer(input$tr_season))
    }
    if (!is.null(input$tr_team) && input$tr_team != "all") {
      matching_ids <- franchise_canonical %>%
        filter(display_name == input$tr_team) %>%
        pull(franchise_id)
      df <- df %>% filter(franchise_id %in% matching_ids)
    }
    if (!is.null(input$tr_player) && nchar(input$tr_player) > 0) {
      search_str <- tolower(trimws(input$tr_player))
      df <- df %>% filter(grepl(search_str, tolower(coalesce(player_name, ""))))
    }

    # Resolve trade partner name
    df <- df %>%
      left_join(
        all_franchises %>%
          select(season, franchise_id, partner_display = franchise_name),
        by = c("season", "trade_partner" = "franchise_id")
      ) %>%
      arrange(desc(timestamp)) %>%
      transmute(
        Date         = format(timestamp, "%Y-%m-%d"),
        Season       = season,
        Team         = franchise_name,
        Direction    = case_when(
          type_desc == "traded_away" ~ "Sent",
          type_desc == "traded_for"  ~ "Received",
          TRUE                       ~ type_desc
        ),
        Player       = coalesce(player_name, as.character(player_id)),
        Position     = pos,
        `MLB Team`   = team,
        `Trade Partner` = coalesce(partner_display, as.character(trade_partner))
      )

    datatable(
      df, rownames = FALSE, filter = "top",
      options = list(pageLength = 25, dom = "lrtip")
    ) %>%
      formatStyle(
        "Direction",
        backgroundColor = styleEqual(
          c("Sent", "Received"),
          c("#f8d7da", "#d4edda")
        )
      )
  })

  # ────────────────────────────────────────────────────────────────────────────
  # Tab 4: Total Points
  # ────────────────────────────────────────────────────────────────────────────

  tp_data <- reactive({
    req(length(STATS_SEASONS) > 0)
    df <- all_weekly_stats

    if (!is.null(input$tp_season) && input$tp_season != "all") {
      df <- df %>% filter(season == as.integer(input$tp_season))
    }

    df %>%
      group_by(user_name) %>%
      summarise(
        weeks           = n(),
        hitting_points  = sum(coalesce(hitting_points,  0), na.rm = TRUE),
        pitching_points = sum(coalesce(pitching_points, 0), na.rm = TRUE),
        total_points    = sum(coalesce(total_points,    0), na.rm = TRUE),
        .groups         = "drop"
      ) %>%
      mutate(avg_per_week = total_points / pmax(weeks, 1)) %>%
      arrange(desc(total_points))
  })

  output$tp_bar <- renderPlotly({
    df <- tp_data()
    req(nrow(df) > 0)

    # Reshape for stacked bar
    bar_df <- df %>%
      pivot_longer(c(hitting_points, pitching_points),
                   names_to  = "category",
                   values_to = "points") %>%
      mutate(
        category = recode(category,
                          hitting_points  = "Hitting",
                          pitching_points = "Pitching"),
        user_name = factor(user_name, levels = rev(df$user_name))
      )

    p <- ggplot(bar_df, aes(x = user_name, y = points, fill = category,
                            text = paste0(user_name, "<br>", category, ": ", round(points, 1)))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c(Hitting = "#2196f3", Pitching = "#ff9800")) +
      labs(x = NULL, y = "Fantasy Points", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")

    ggplotly(p, tooltip = "text") %>%
      layout(barmode = "stack", hovermode = "closest") %>%
      config(displayModeBar = FALSE)
  })

  output$tp_table <- renderDT({
    df <- tp_data() %>%
      transmute(
        Owner           = user_name,
        `Total Points`  = fmt_pts(total_points),
        Hitting         = fmt_pts(hitting_points),
        Pitching        = fmt_pts(pitching_points),
        Weeks           = weeks,
        `Avg / Week`    = fmt_pts(avg_per_week)
      )

    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 20, dom = "lrtip")
    )
  })
}
