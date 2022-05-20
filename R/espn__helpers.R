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
    '0' = 'AB',
    '1' = 'H',
    '2' = 'AVG',
    '3' = '2B',
    '4' = '3B',
    '5' = 'HR',
    '6' = 'XBH', # 2B + 3B + HR
    '7' = '1B',
    '8' = 'TB', # 1 * COUNT(1B) + 2 * COUNT(2B) + 3 * COUNT(3B) + 4 * COUNT(HR)
    '9' = 'SLG',
    '10' = 'B_BB',
    '11' = 'B_IBB',
    '12' = 'HBP',
    '13' = 'SF', # Sacrifice Fly
    '14' = 'SH', # Sacrifice Hit - i.e. Sacrifice Bunt
    '15' = 'SAC', # total sacrifices = SF + SH
    '16' = 'PA',
    '17' = 'OBP',
    '18' = 'OPS', # OBP + SLG
    '19' = 'RC', # Runs Created = TB * (H + BB) / (AB + BB)
    '20' = 'R',
    '21' = 'RBI',
    # 22' = '',
    '23' = 'SB',
    '24' = 'CS',
    '25' = 'SB-CS', # net steals
    '26' = 'GDP',
    '27' = 'B_SO', # batter strike-outs
    '28' = 'PS', # pitches seen
    '29' = 'PPA', # pitches per plate appearance = PS / PA
    # 30' = '',
    '31' = 'CYC',
    '32' = 'GP', # pitcher games pitched
    '33' = 'GS', # games started
    '34' = 'OUTS',  # divide by 3 for IP
    '35' = 'TBF',
    '36' = 'P',  # pitches
    '37' = 'P_H',
    '38' = 'OBA', # Opponent Batting Average
    '39' = 'P_BB',
    '40' = 'P_IBB', # intentional walks allowed
    '41' = 'WHIP',
    '42' = 'HBP',
    '43' = 'OOBP', # Opponent On-Base Percentage
    '44' = 'P_R',
    '45' = 'ER',
    '46' = 'P_HR',
    '47' = 'ERA',
    '48' = 'K',
    '49' = 'K/9',
    '50' = 'WP',
    '51' = 'BLK',
    '52' = 'PK', # pickoff
    '53' = 'W',
    '54' = 'L',
    '55' = 'WPCT', # Win Percentage
    '56' = 'SVO', # Save opportunity
    '57' = 'SV',
    '58' = 'BLSV', # BLown SaVe
    '59' = 'SV%', # Save percentage
    '60' = 'HLD',
    # 61' = '',
    '62' = 'CG',
    '63' = 'QS', # Quality Starts
    # 64' = '',
    '65' = 'NH', # No-hitters
    '66' = 'PG', # Perfect Games
    '67' = 'TC', # Total Chances = PO + A + E
    '68' = 'PO', # Put Outs
    '69' = 'A', # Assists
    '70' = 'OFA', # Outfield Assists
    '71' = 'FPCT', # Fielding Percentage
    '72' = 'E',
    '73' = 'DP', # Double plays turned
    # Not sure what to call the next four
    # 74 is games played where the batter's team won
    # 75 is the same except when the team lost
    # 76 and 77 are the same except for pitchers
    '74' = 'B_G_W',
    '75' = 'B_G_L',
    '76' = 'P_G_W',
    '77' = 'P_G_L',
    # 78' = ,
    # 79' = ,
    # 80' = ,
    '81' = 'G', # Games Played
    '82' = 'K/BB', # Strikeout to Walk Ratio
    '83' = 'SVHD', # Saves + Holds
    '99' = 'STARTER'
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
