#### Tests for get_survivor ####
# Tests call .get_survivor_logic() directly so no live API connection is needed.

# 3 teams, 4 weeks
#   Week 1: B lowest (50)  -> B eliminated
#   Week 2: A lowest among {A, C} (70) -> A eliminated
#   Week 3: Only C remains -> C eliminated
#   Week 4: No teams left
mock_schedule <- tibble::tibble(
  week            = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
  franchise_id    = c("1", "2", "3", "1", "2", "3", "1", "2", "3", "1", "2", "3"),
  franchise_score = c(100, 50, 90, 70, 80, 95, 85, 60, 110, 75, 55, 100),
  result          = rep("W", 12)
)

mock_franchises <- tibble::tibble(
  franchise_id   = c("1", "2", "3"),
  franchise_name = c("Alpha FC", "Bravo FC", "Charlie FC"),
  user_name      = c("Alice", "Bob", "Carol")
)

# MFL-style franchises — no user_name column
mock_franchises_no_user <- tibble::tibble(
  franchise_id   = c("1", "2", "3"),
  franchise_name = c("Alpha FC", "Bravo FC", "Charlie FC")
)

# ── Basic elimination logic ─────────────────────────────────────────────────

test_that(".get_survivor_logic returns correct loser and survivors in week 1", {
  result <- ffscrapr:::.get_survivor_logic(mock_schedule, mock_franchises, week = 1)

  expect_type(result, "list")
  expect_named(result, c("loser", "survivors"))

  # Franchise 2 (Bob) scored 50 — lowest in week 1
  expect_equal(result$loser$user_name, "Bob")
  expect_equal(result$loser$franchise_score, 50)

  expect_setequal(result$survivors$user_name, c("Alice", "Carol"))
})

test_that(".get_survivor_logic skips previously eliminated teams in later weeks", {
  result <- ffscrapr:::.get_survivor_logic(mock_schedule, mock_franchises, week = 2)

  # Bob eliminated in week 1; week 2 is Alice (70) vs Carol (95) only
  expect_equal(result$loser$user_name, "Alice")
  expect_equal(result$loser$franchise_score, 70)
  expect_equal(result$survivors$user_name, "Carol")
})

test_that(".get_survivor_logic works when only one team remains", {
  result <- ffscrapr:::.get_survivor_logic(mock_schedule, mock_franchises, week = 3)

  expect_equal(result$loser$user_name, "Carol")
  expect_equal(nrow(result$survivors), 0)
})

test_that(".get_survivor_logic returns string when all teams eliminated", {
  result <- ffscrapr:::.get_survivor_logic(mock_schedule, mock_franchises, week = 4)

  expect_equal(result, "all teams have been eliminated")
})

# ── Output column names ─────────────────────────────────────────────────────

test_that(".get_survivor_logic always returns user_name column, never franchise_id", {
  result <- ffscrapr:::.get_survivor_logic(mock_schedule, mock_franchises, week = 1)

  expect_true("user_name" %in% names(result$loser))
  expect_true("franchise_score" %in% names(result$loser))
  expect_false("franchise_id" %in% names(result$loser))
  expect_false("franchise_name" %in% names(result$loser))

  expect_true("user_name" %in% names(result$survivors))
  expect_false("franchise_id" %in% names(result$survivors))

  expect_equal(nrow(result$loser), 1)
})

# ── NA score handling ───────────────────────────────────────────────────────

test_that(".get_survivor_logic ignores NA scores (unplayed games)", {
  sched_na <- tibble::tibble(
    week            = c(1, 1, 1),
    franchise_id    = c("1", "2", "3"),
    franchise_score = c(100, NA, 90),
    result          = c("W", NA, "W")
  )

  result <- ffscrapr:::.get_survivor_logic(sched_na, mock_franchises, week = 1)

  # Franchise 2 (Bob) excluded due to NA; Carol (90) is lowest among Alice/Carol
  expect_equal(result$loser$user_name, "Carol")
  expect_equal(result$loser$franchise_score, 90)
  expect_setequal(result$survivors$user_name, c("Alice", "Bob"))
})

# ── MFL fallback: franchise_name used when user_name absent ────────────────

test_that(".get_survivor_logic falls back to franchise_name when user_name not in franchises", {
  result <- ffscrapr:::.get_survivor_logic(
    mock_schedule, mock_franchises_no_user, week = 1
  )

  # user_name column absent in franchises — should coalesce to franchise_name
  expect_equal(result$loser$user_name, "Bravo FC")
  expect_setequal(result$survivors$user_name, c("Alpha FC", "Charlie FC"))
})

# ── Integer franchise_id type coercion (ESPN) ───────────────────────────────

test_that(".get_survivor_logic handles integer franchise_id from ESPN schedule", {
  sched_int <- tibble::tibble(
    week            = c(1L, 1L, 1L),
    franchise_id    = c(1L, 2L, 3L),   # integer, as ESPN returns
    franchise_score = c(100, 50, 90),
    result          = rep("W", 3)
  )
  fran_dbl <- tibble::tibble(
    franchise_id   = c(1, 2, 3),        # double, as hoist() returns
    franchise_name = c("Alpha FC", "Bravo FC", "Charlie FC"),
    user_name      = c("Alice", "Bob", "Carol")
  )

  result <- ffscrapr:::.get_survivor_logic(sched_int, fran_dbl, week = 1)

  expect_equal(result$loser$user_name, "Bob")
  expect_equal(result$loser$franchise_score, 50)
})
