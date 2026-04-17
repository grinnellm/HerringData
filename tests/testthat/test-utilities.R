test_that("Rolling mean", {
  expect_setequal(
    mean_na_roll(x = c(NA, NA, 2, NA, 4, 5, 6, NA, NA, NA, 10)),
    c(NA, NA, 2, 2, 4, 5, 6, 3.8, 4.16, 4.592, 10)
  )
  expect_setequal(
    mean_na_roll(x = c(NA, NA, 2, NA, 4, 5, 6, NA, NA, NA, 10), n = 2),
    c(NA, NA, 2, 2, 4, 5, 6, 5.5, 5.75, 5.625, 10)
  )
  expect_setequal(
    mean_na_roll(x = c(NA, NA, 2, NA, 4, 5, 6, NA, NA, NA, 10), n = 1),
    c(NA, NA, 2, 2, 4, 5, 6, 6, 6, 6, 10)
  )
})

test_that("Season to year", {
  expect_setequal(
    season_to_year(dat = c(20123, 20134, 20145)), c(2013, 2014, 2015)
  )
  expect_warning(season_to_year(dat = 201456))
  expect_warning(season_to_year(dat = 206))
})

test_that("Plus group", {
  expect_setequal(plus_group(x = c(9, 10, 11)), c(9, 10, 10))
  expect_setequal(plus_group(x = c(9, 10, 11), max_age = 9), c(9, 9, 9))
  expect_setequal(plus_group(x = c(9, 10, 11), max_age = 11), c(9, 10, 11))
})

test_that("Check sections", {
  data(codes_group)
  codes_group_ok <- codes_group %>%
    filter(Section %in% c("001", "002", "003", "004", "005"))
  expect_warning(check_sections(dat = codes_group, dat_name = "Groups"))
  expect_no_warning(check_sections(dat = codes_group_ok, dat_name = "Groups"))
  expect_type(check_sections(dat = codes_group_ok, dat_name = "Groups"), "list")
  expect_warning(
    expect_equal(
      dim(check_sections(dat = codes_group, dat_name = "Groups")), c(14, 2)
    )
  )
})
