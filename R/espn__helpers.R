#### ESPN helper functions ####

#' ESPN Lineup Slot map
#'
#' This is for the starting lineup specifically - primary positions is accessible via `.espn_pos_map`
#'
#' @keywords internal
#' @seealso <https://github.com/cwendt94/espn-api/blob/master/espn_api/football/constant.py>
.espn_lineupslot_map <- function() {
  c(
    "0" = "QB",
    "1" = "TQB",
    "2" = "RB",
    "3" = "RB/WR",
    "4" = "WR",
    "5" = "WR/TE",
    "6" = "TE",
    "7" = "OP",
    "8" = "DT",
    "9" = "DE",
    "10" = "LB",
    "11" = "DL",
    "12" = "CB",
    "13" = "S",
    "14" = "DB",
    "15" = "DP",
    "16" = "DST",
    "17" = "K",
    "18" = "P",
    "19" = "HC",
    "20" = "BE",
    "21" = "IR",
    "22" = "XYZ",
    "23" = "RB/WR/TE",
    "24" = "ER",
    "25" = "Rookie",
    "QB" = 0,
    "TQB" = 1,
    "RB" = 2,
    "RB/WR" = 3,
    "WR" = 4,
    "WR/TE" = 5,
    "TE" = 6,
    "OP" = 7,
    "DT" = 8,
    "DE" = 9,
    "LB" = 10,
    "DL" = 11,
    "CB" = 12,
    "S" = 13,
    "DB" = 14,
    "DP" = 15,
    "DST" = 16,
    "K" = 17,
    "P" = 18,
    "HC" = 19,
    "BE" = 20,
    "IR" = 21,
    "XYZ" = 22,
    "RB/WR/TE" = 23,
    "ER" = 24,
    "Rookie" = 25
  )
}

#' ESPN Primary Position map
#'
#' Decoded by hand - if you have an IDP ESPN league please open a GitHub issue
#' and pass along the league info so we can expand this.
#'
#' @keywords internal

.espn_pos_map <- function() {
  c(
    '0' = 'C',
    '1' = '1B',
    '2' = '2B',
    '3' = '3B',
    '4' = 'SS',
    '5' = 'OF',
    '6' = '2B/SS',
    '7' = '1B/3B',
    '8' = 'LF',
    '9' = 'CF',
    '10' = 'RF',
    '11' = 'DH',
    '12' = 'UTIL',
    '13' = 'P',
    '14' = 'SP',
    '15' = 'RP',
    '16' = 'BE',
    '17' = 'IL',
    '19' = 'IF',
    'C' = '0',
    '1B' = '1',
    '2B' = '2',
    '3B' = '3',
    'SS' = '4',
    'OF' = '5',
    '2B/SS' = '6',
    '1B/3B' = '7',
    'LF' = '8',
    'CF' = '9',
    'RF' = '10',
    'DH' = '11',
    'UTIL' = '12',
    'P' = '13',
    'SP' = '14',
    'RP' = '15',
    'BE' = '16',
    'IL' = '17',
    'IF' = '19'
  )
}

#' ESPN Team ID map
#'
#' Opinionatedly conforming to DynastyProcess standards, which match to MyFantasyLeague.
#' Abbreviations are consistently three letters.
#'
#' @keywords internal
#' @seealso <https://github.com/cwendt94/espn-api/blob/master/espn_api/football/constant.py>
.espn_team_map <- function() {
  c(
    '0' = 'FA',
    '1' = 'Bal',
    '2' = 'Bos',
    '3' = 'LAA',
    '4' = 'ChW',
    '5' = 'Cle',
    '6' = 'Det',
    '7' = 'KC',
    '8' = 'Mil',
    '9' = 'Min',
    '10' = 'NYY',
    '11' = 'Oak',
    '12' = 'Sea',
    '13' = 'Tex',
    '14' = 'Tor',
    '15' = 'Atl',
    '16' = 'ChC',
    '17' = 'Cin',
    '18' = 'Hou',
    '19' = 'LAD',
    '20' = 'Wsh',
    '21' = 'NYM',
    '22' = 'Phi',
    '23' = 'Pit',
    '24' = 'StL',
    '25' = 'SD',
    '26' = 'SF',
    '27' = 'Col',
    '28' = 'Mia',
    '29' = 'Ari',
    '30' = 'TB',
    'FA' = '0',
    'Bal' = '1',
    'Bos'= '2',
    'LAA' = '3',
    'ChW' = '4',
    'Cle' = '5',
    'Det' = '6',
    'KC' = '7',
    'Mil' = '8',
    'Min' = '9',
    'NYY' = '10',
    'Oak' = '11',
    'Sea' = '12',
    'Tex' = '13',
    'Tor' = '14',
    'Atl' = '15',
    'ChC' = '16',
    'Cin' = '17',
    'Hou' = '18',
    'LAD' = '19',
    'Wsh' = '20',
    'NYM' = '21',
    'Phi' = '22',
    'Pit' = '23',
    'StL' = '24',
    'SD' = '25',
    'SF' = '26',
    'Col' = '27',
    'Mia' = '28',
    'Ari' = '29',
    'TB' = '30'



  )
}


#' ESPN Stat ID map
#'
#' @keywords internal
#' @seealso <https://github.com/cwendt94/espn-api/blob/master/espn_api/football/constant.py>
.espn_stat_map <- function() {
  c(
    "3" = "passingYards",
    "4" = "passingTouchdowns",
    "8" = "passing25Yards",
    "19" = "passing2PtConversions",
    "20" = "passingInterceptions",
    "24" = "rushingYards",
    "25" = "rushingTouchdowns",
    "26" = "rushing2PtConversions",
    "28" = "rushing10Yards",
    "35" = "rushing40YardTD",
    "37" = "rushing100YardGame",
    "38" = "rushing200YardGame",
    "42" = "receivingYards",
    "43" = "receivingTouchdowns",
    "44" = "receiving2PtConversions",
    "45" = "receiving40YardTD",
    "48" = "receiving10Yards",
    "53" = "receivingReceptions",
    "56" = "receiving100YardGame",
    "57" = "receiving200YardGame",
    "63" = "fumbleRecoveryTouchdown",
    "72" = "lostFumbles",
    "74" = "madeFieldGoalsFrom50Plus",
    "77" = "madeFieldGoalsFrom40To49",
    "79" = "missedFieldGoalsFrom40To49",
    "80" = "madeFieldGoalsFromUnder40",
    "82" = "missedFieldGoalsFromUnder40",
    "85" = "missedFieldGoals",
    "86" = "madeExtraPoints",
    "88" = "missedExtraPoints",
    "89" = "defensive0PointsAllowed",
    "90" = "defensive1To6PointsAllowed",
    "91" = "defensive7To13PointsAllowed",
    "92" = "defensive14To17PointsAllowed",
    "93" = "defensiveBlockedKickForTouchdowns",
    "95" = "defensiveInterceptions",
    "96" = "defensiveFumbles",
    "97" = "defensiveBlockedKicks",
    "98" = "defensiveSafeties",
    "99" = "defensiveSacks",
    "101" = "kickoffReturnTouchdown",
    "102" = "puntReturnTouchdown",
    "103" = "fumbleReturnTouchdown",
    "104" = "interceptionReturnTouchdown",
    "114" = "kickoffReturnYards",
    "115" = "puntReturnYards",
    "122" = "defensive22To27PointsAllowed",
    "123" = "defensive28To34PointsAllowed",
    "124" = "defensive35To45PointsAllowed",
    "125" = "defensive46+PointsAllowed",
    "128" = "defensive000To099YardsAllowed",
    "129" = "defensive100To199YardsAllowed",
    "130" = "defensive200To299YardsAllowed",
    "132" = "defensive350To399YardsAllowed",
    "133" = "defensive400To449YardsAllowed",
    "134" = "defensive450To499YardsAllowed",
    "135" = "defensive500To549YardsAllowed",
    "136" = "defensiveOver550YardsAllowed",

    # Punter Stats
    "140" = "puntsInsideThe10", # PT10
    "141" = "puntsInsideThe20", # PT20
    "148" = "puntAverage44.0+", # PTA44
    "149" = "puntAverage42.0-43.9", # PTA42
    "150" = "puntAverage40.0-41.9", # PTA40

    # Head Coach stats
    "161" = "25+pointsWinMargin", # WM25
    "162" = "20-24pointWinMargin", # WM20
    "163" = "15-19pointWinMargin", # WM15
    "164" = "10-14pointWinMargin", # WM10
    "165" = "5-9pointWinMargin", # WM5
    "166" = "1-4pointWinMargin", # WM1

    "155" = "TeamWin", # TW

    "171" = "20-24pointLossMargin", # LM20
    "172" = "25+pointLossMargin", # LM25

    "198" = "madeFieldGoalsFrom50To59",
    "200" = "missedFieldGoalsFrom50To59",
    "201" = "madeFieldGoalsFrom60Plus",
    "203" = "missedFieldGoalsFrom60Plus",
    "206" = "2PtConversionReturnedForTouchdown",
    "209" = "1PtSafety"
  )
}

#' ESPN Activity/Transaction Mapping
#'
#' @keywords internal
#'
#' @seealso <https://github.com/cwendt94/espn-api/blob/master/espn_api/football/constant.py#L82-92>

.espn_activity_map <- function() {
  c(
    "178" = "FREE_AGENT|added",
    "179" = "FREE_AGENT|dropped",
    "180" = "BBID_WAIVER|added",
    "181" = "BBID_WAIVER|dropped",
    "239" = "DROP|dropped",
    "244" = "TRADE|traded_away",
    "FREE_AGENT|added" = "178",
    "BBID_WAIVER|added" = "180",
    "TRADE|traded_away" = "244"
  )
}
