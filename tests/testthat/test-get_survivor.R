#### Tests for get_survivor ####

# Build a minimal schedule tibble for use across tests.
# 3 teams (A, B, C), 4 weeks of scores.
#   Week 1: B lowest (50)  -> B eliminated
#   Week 2: A lowest among {A, C} (70) -> A eliminated
#   Week 3: Only C remains; C is "eliminated" (week 3)
#   Week 4: No teams left
mock_schedule <- tibble::tibble(
  week            = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
  franchise_id    = c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A", "B", "C"),
  franchise_score = c(100, 50, 90, 70, 80, 95, 85, 60, 110, 75, 55, 100),
  result          = rep("W", 12)
)

# Franchises lookup tibble (simulates ff_franchises() output)
mock_franchises <- tibble::tibble(
  franchise_id   = c("A", "B", "C"),
  franchise_name = c("Alpha", "Bravo", "Charlie")
)

# Schedule that already has franchise_name (e.g. Fleaflicker)
mock_schedule_named <- mock_schedule %>%
  dplyr::left_join(mock_franchises, by = "franchise_id")

# ── Tests: no franchise names — falls back to franchise_id as name value ───

test_that("get_survivor always returns franchise_name column (fallback to id value)", {
  result <- suppressMessages(get_survivor(mock_schedule, week = 1))

  expect_type(result, "list")
  expect_named(result, c("loser", "survivors"))

  expect_true(tibble::is_tibble(result$loser))
  expect_true(tibble::is_tibble(result$survivors))

  # Output always has franchise_name, never franchise_id
  expect_true("franchise_name" %in% names(result$loser))
  expect_true("franchise_score" %in% names(result$loser))
  expect_false("franchise_id" %in% names(result$loser))

  expect_true("franchise_name" %in% names(result$survivors))
  expect_false("franchise_id" %in% names(result$survivors))
})

test_that("get_survivor fallback uses franchise_id value as franchise_name", {
  result <- suppressMessages(get_survivor(mock_schedule, week = 1))

  # B (id "B") scored 50 — lowest in week 1; name falls back to id value
  expect_equal(result$loser$franchise_name, "B")
  expect_equal(result$loser$franchise_score, 50)
  expect_setequal(result$survivors$franchise_name, c("A", "C"))
})

test_that("get_survivor skips previously eliminated teams in later weeks", {
  result <- suppressMessages(get_survivor(mock_schedule, week = 2))

  # B was eliminated in week 1; week 2 is A (70) vs C (95) only
  expect_equal(result$loser$franchise_name, "A")
  expect_equal(result$loser$franchise_score, 70)
  expect_equal(result$survivors$franchise_name, "C")
})

test_that("get_survivor works when only one team remains", {
  result <- suppressMessages(get_survivor(mock_schedule, week = 3))

  expect_equal(result$loser$franchise_name, "C")
  expect_equal(nrow(result$survivors), 0)
})

test_that("get_survivor returns 'all teams have been eliminated' when no teams left", {
  result <- suppressMessages(get_survivor(mock_schedule, week = 4))

  expect_equal(result, "all teams have been eliminated")
})

test_that("get_survivor ignores NA scores (unplayed weeks)", {
  schedule_with_na <- tibble::tibble(
    week            = c(1, 1, 1),
    franchise_id    = c("A", "B", "C"),
    franchise_score = c(100, NA, 90),
    result          = c("W", NA, "W")
  )

  result <- suppressMessages(get_survivor(schedule_with_na, week = 1))

  # B has NA score so is excluded; C (90) is lowest among A (100) and C (90)
  expect_equal(result$loser$franchise_name, "C")
  expect_equal(result$loser$franchise_score, 90)
  expect_setequal(result$survivors$franchise_name, c("A", "B"))
})

# ── Tests: franchise names via `franchises` parameter ──────────────────────

test_that("get_survivor uses franchise_name when franchises tibble is supplied", {
  result <- get_survivor(mock_schedule, week = 1, franchises = mock_franchises)

  expect_equal(result$loser$franchise_name, "Bravo")
  expect_equal(result$loser$franchise_score, 50)
  expect_setequal(result$survivors$franchise_name, c("Alpha", "Charlie"))
})

test_that("get_survivor with franchises param skips eliminated teams correctly", {
  result <- get_survivor(mock_schedule, week = 2, franchises = mock_franchises)

  expect_equal(result$loser$franchise_name, "Alpha")
  expect_equal(result$survivors$franchise_name, "Charlie")
})

test_that("get_survivor returns 'all teams have been eliminated' string with franchises param", {
  result <- get_survivor(mock_schedule, week = 4, franchises = mock_franchises)

  expect_equal(result, "all teams have been eliminated")
})

# ── Tests: franchise names already in schedule (e.g. Fleaflicker) ──────────

test_that("get_survivor uses franchise_name when already present in schedule", {
  result <- get_survivor(mock_schedule_named, week = 1)

  expect_equal(result$loser$franchise_name, "Bravo")
  expect_setequal(result$survivors$franchise_name, c("Alpha", "Charlie"))
})

test_that("get_survivor ignores franchises param when schedule already has franchise_name", {
  other_franchises <- tibble::tibble(
    franchise_id   = c("A", "B", "C"),
    franchise_name = c("Team1", "Team2", "Team3")
  )
  result <- get_survivor(mock_schedule_named, week = 1, franchises = other_franchises)

  # Names from schedule take priority over separately passed franchises
  expect_equal(result$loser$franchise_name, "Bravo")
})
