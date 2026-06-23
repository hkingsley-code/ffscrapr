# ui.R

ui <- navbarPage(
  title = LEAGUE_NAME,
  id    = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # ── Tab 1: Weekly Scores ──────────────────────────────────────────────────
  tabPanel(
    "Weekly Scores",
    icon = icon("calendar-week"),
    fluidPage(
      fluidRow(
        column(3,
          wellPanel(
            selectInput(
              "ws_season", "Season",
              choices  = rev(ALL_SEASONS),
              selected = max(ALL_SEASONS)
            ),
            selectInput(
              "ws_teams", "Teams",
              choices  = c("All teams" = "all"),
              selected = "all",
              multiple = FALSE
            )
          )
        ),
        column(9,
          h4("Week-by-Week Scores"),
          plotlyOutput("ws_chart", height = "360px"),
          hr(),
          h4("Matchup Results"),
          DTOutput("ws_table")
        )
      )
    )
  ),

  # ── Tab 2: Historical Records ─────────────────────────────────────────────
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
          plotlyOutput("hr_heatmap", height = "420px")
        )
      )
    )
  ),

  # ── Tab 3: Trades ─────────────────────────────────────────────────────────
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
  ),

  # ── Tab 4: Total Points ───────────────────────────────────────────────────
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
                  selected = "all"
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
  )
)
