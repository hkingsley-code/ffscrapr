# ui.R

ui <- navbarPage(
  title = LEAGUE_NAME,
  id    = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # ── Tab 1 (home): Total Points ────────────────────────────────────────────
  tabPanel(
    "Total Points",
    icon = icon("chart-bar"),
    fluidPage(
      if (length(STATS_SEASONS) == 0) {
        fluidRow(
          column(12,
            div(
              class = "alert alert-warning",
              icon("circle-exclamation"), " ",
              "Detailed weekly stats are only available from 2018 onward.",
              " Fallback: season-level Points For from standings is shown below."
            )
          )
        )
      } else {
        tagList(
          fluidRow(
            column(3,
              wellPanel(
                selectInput(
                  "tp_season", "Season",
                  choices  = c("All seasons (cumulative)" = "all", rev(STATS_SEASONS)),
                  selected = max(STATS_SEASONS)
                )
              )
            ),
            column(9,
              h4("Points Leaderboard"),
              plotlyOutput("tp_bar", height = "360px"),
              hr(),
              h4("Detailed Breakdown"),
              DTOutput("tp_table")
            )
          )
        )
      }
    )
  ),

  # ── Tab 2: Weekly Scores ──────────────────────────────────────────────────
  tabPanel(
    "Weekly Scores",
    icon = icon("calendar-week"),
    fluidPage(
      fluidRow(
        column(3,
          selectInput(
            "ws_season", "Season",
            choices  = rev(ALL_SEASONS),
            selected = max(ALL_SEASONS)
          )
        ),
        column(3,
          selectInput(
            "ws_teams", "Teams",
            choices  = c("All teams" = "all"),
            selected = "all",
            multiple = FALSE
          )
        )
      ),
      # Dedicated full-width, tall area for the season-race chart
      fluidRow(
        column(12,
          h4("Season Race — Cumulative Points"),
          plotlyOutput("ws_chart", height = "620px")
        )
      ),
      hr(),
      fluidRow(
        column(12,
          h4("Matchup Results"),
          DTOutput("ws_table")
        )
      )
    )
  ),

  # ── Tab 3: Head-to-Head ───────────────────────────────────────────────────
  tabPanel(
    "Head-to-Head",
    icon = icon("people-arrows"),
    fluidPage(
      fluidRow(
        column(3,
          wellPanel(
            radioButtons(
              "h2h_scope", "Scope",
              choices  = c("Current season" = "current", "All-time" = "all"),
              selected = "current"
            ),
            selectInput(
              "h2h_owner", "Owner (for table)",
              choices  = ALL_OWNERS,
              selected = ALL_OWNERS[1]
            ),
            p(class = "text-muted",
              "Counts every completed matchup (regular season + playoffs). ",
              "The manually-run 2016/2017 first-playoff week is excluded.")
          )
        ),
        column(9,
          h4("Win % Matrix (row owner vs column opponent)"),
          plotlyOutput("h2h_matrix", height = "560px")
        )
      ),
      hr(),
      fluidRow(
        column(12,
          h4("Head-to-Head Record for Selected Owner"),
          DTOutput("h2h_table")
        )
      )
    )
  ),

  # ── Tab 4: Historical Records ─────────────────────────────────────────────
  tabPanel(
    "Historical Records",
    icon = icon("trophy"),
    fluidPage(
      fluidRow(
        column(12,
          h4("All-Time Records"),
          uiOutput("records_summary_cards"),
          hr()
        )
      ),
      tabsetPanel(
        tabPanel("All-Time Standings",
          br(),
          DTOutput("hr_alltime_table")
        ),
        tabPanel("Championship History",
          br(),
          DTOutput("hr_champs_table")
        ),
        tabPanel("Season-by-Season",
          br(),
          fluidRow(
            column(3,
              selectInput(
                "hr_season", "Season",
                choices  = rev(ALL_SEASONS),
                selected = max(ALL_SEASONS)
              )
            )
          ),
          DTOutput("hr_season_table")
        ),
        tabPanel("Standings Over Time",
          br(),
          plotlyOutput("hr_heatmap", height = "680px")
        )
      )
    )
  ),

  # ── Tab 5: Trades ─────────────────────────────────────────────────────────
  tabPanel(
    "Trades",
    icon = icon("arrows-left-right"),
    fluidPage(
      if (!HAS_TRADE_DATA) {
        fluidRow(
          column(12,
            div(
              class = "alert alert-warning",
              icon("circle-exclamation"), " ",
              strong("Trade history requires ESPN login cookies."),
              " Re-run the fetch script with ",
              code("ESPN_S2"), " and ", code("SWID"),
              " environment variables set. See ",
              tags$code("vignettes/espn_authentication.Rmd"), " for instructions.",
              br(), br(),
              "Once fetched, trades are available for seasons 2019 and later."
            )
          )
        )
      } else {
        tagList(
          fluidRow(
            column(12,
              div(
                class = "alert alert-info",
                icon("circle-info"), " ",
                "ESPN only retains trade/transaction history for the ",
                strong("current active season"), " through this API. Past seasons' trade logs ",
                "are not retrievable even with valid login credentials — this is a limitation ",
                "of ESPN's data retention, not something this app can work around. As each season ",
                "completes, its trade history will remain available going forward from that point."
              )
            )
          ),
          fluidRow(
            column(3,
              wellPanel(
                selectInput(
                  "tr_season", "Season",
                  choices  = c("All seasons" = "all", rev(TXN_SEASONS)),
                  selected = "all"
                ),
                selectInput(
                  "tr_team", "Team",
                  choices  = c("All teams" = "all", ALL_TEAMS),
                  selected = "all"
                ),
                textInput("tr_player", "Player name search", placeholder = "e.g. Ohtani")
              )
            ),
            column(9,
              h4("Trade Log"),
              p(class = "text-muted",
                "Each trade appears twice — once as sent, once as received.",
                "Trades at the same timestamp are part of the same deal."
              ),
              DTOutput("tr_table")
            )
          )
        )
      }
    )
  )
)
