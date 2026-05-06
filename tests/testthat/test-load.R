test_that("Load biological data", {
  suppressMessages(suppressWarnings(library(SpawnIndex)))
  suppressMessages(suppressWarnings(library(DBI)))
  data(pars)
  data(codes_group)
  data(database_info)
  expect_no_warning(bio_raw <- load_bio(quiet = TRUE))
  expect_type(bio_raw, "list")
  expect_true("tbl" %in% class(bio_raw))
  saveRDS(
    object = bio_raw,
    file = file.path(
      system.file("testdata", package = "HerringData"), "bio_raw.rds"
    )
  )
})

test_that("Load catch data", {
  suppressMessages(suppressWarnings(library(SpawnIndex)))
  suppressMessages(suppressWarnings(library(DBI)))
  data(pars)
  data(codes_group)
  data(conv_factors)
  data(database_info)
  expect_no_warning(catch_raw <- load_catch(quiet = TRUE))
  expect_type(catch_raw, "list")
  expect_true("tbl" %in% class(catch_raw))
  saveRDS(
    object = catch_raw,
    file = file.path(
      system.file("testdata", package = "HerringData"), "catch_raw.rds"
    )
  )
})

test_that("Load spawn index data", {
  suppressMessages(suppressWarnings(library(SpawnIndex)))
  suppressMessages(suppressWarnings(library(DBI)))
  data(pars)
  data(codes_group)
  data(conv_factors)
  data(database_info)
  expect_no_warning(spawn_raw <- load_spawn(quiet = TRUE))
  expect_type(spawn_raw, "list")
  expect_true("tbl" %in% class(spawn_raw))
  saveRDS(
    object = spawn_raw,
    file = file.path(
      system.file("testdata", package = "HerringData"), "spawn_raw.rds"
    )
  )
})
