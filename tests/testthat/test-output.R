test_that("SISCAH input: biological data", {
  suppressMessages(suppressWarnings(library(SpawnIndex)))
  suppressMessages(suppressWarnings(library(DBI)))
  suppressMessages(suppressWarnings(library(magrittr)))
  suppressMessages(suppressWarnings(library(here)))
  suppressMessages(suppressWarnings(library(dplyr)))
  data(pars)
  data(codes_group)
  data(database_info)
  data(undefined_sections)
  if (file.exists(here("Examples", "bio_raw.rds"))) {
    bio_raw <- readRDS(file = here("Examples", "bio_raw.rds"))
  } else {
    example(load_bio)
  }
  expect_no_warning(
    bio <- bio_raw %>%
      filter(Region == "PRD") %>%
      siscah_bio(structure = "Section")
  )
  expect_type(bio, "list")
  expect_equal(length(bio), 3)
  expect_true("tbl" %in% class(bio[[1]]))
  expect_true("tbl" %in% class(bio[[2]]))
  expect_true("tbl" %in% class(bio[[3]]))
})

test_that("SISCAH input: catch data", {
  suppressMessages(suppressWarnings(library(SpawnIndex)))
  suppressMessages(suppressWarnings(library(DBI)))
  suppressMessages(suppressWarnings(library(magrittr)))
  suppressMessages(suppressWarnings(library(here)))
  suppressMessages(suppressWarnings(library(dplyr)))
  data(pars)
  data(codes_group)
  data(conv_factors)
  data(database_info)
  data(undefined_sections)
  if (file.exists(here("Examples", "catch_raw.rds"))) {
    catch_raw <- readRDS(file = here("Examples", "catch_raw.rds"))
  } else {
    example(load_catch)
  }
  expect_warning(
    catch <- catch_raw %>%
      filter(Region == "PRD") %>%
      siscah_catch(structure = "Section")
  )
  expect_type(catch, "list")
  expect_true("tbl" %in% class(catch))
})

test_that("SISCAH input: spawn data", {
  suppressMessages(suppressWarnings(library(SpawnIndex)))
  suppressMessages(suppressWarnings(library(DBI)))
  suppressMessages(suppressWarnings(library(magrittr)))
  suppressMessages(suppressWarnings(library(here)))
  suppressMessages(suppressWarnings(library(dplyr)))
  data(pars)
  data(codes_group)
  data(conv_factors)
  data(database_info)
  data(undefined_sections)
  if (file.exists(here("Examples", "spawn_raw.rds"))) {
    spawn_raw <- readRDS(file = here("Examples", "spawn_raw.rds"))
  } else {
    example(load_spawn)
  }
  expect_no_warning(
    spawn <- spawn_raw %>%
      filter(Region == "PRD") %>%
      siscah_spawn(structure = "Section")
  )
  expect_type(spawn, "list")
  expect_true("tbl" %in% class(spawn))
})
