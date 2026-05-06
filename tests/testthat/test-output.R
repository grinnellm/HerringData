test_that("SISCAH input: biological data", {
  suppressMessages(suppressWarnings(library(magrittr)))
  suppressMessages(suppressWarnings(library(dplyr)))
  data(undefined_sections)
  bio_raw <- readRDS(
    file = file.path(
      system.file("testdata", package = "HerringData"), "bio_raw.rds"
    )
  )
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
  suppressMessages(suppressWarnings(library(magrittr)))
  suppressMessages(suppressWarnings(library(dplyr)))
  data(undefined_sections)
  catch_raw <- readRDS(
    file = file.path(
      system.file("testdata", package = "HerringData"), "catch_raw.rds"
    )
  )
  expect_warning(
    catch <- catch_raw %>%
      filter(Region == "PRD") %>%
      siscah_catch(structure = "Section")
  )
  expect_type(catch, "list")
  expect_true("tbl" %in% class(catch))
})

test_that("SISCAH input: spawn data", {
  suppressMessages(suppressWarnings(library(magrittr)))
  suppressMessages(suppressWarnings(library(dplyr)))
  data(undefined_sections)
  spawn_raw <- readRDS(
    file = file.path(
      system.file("testdata", package = "HerringData"), "spawn_raw.rds"
    )
  )
  expect_no_warning(
    spawn <- spawn_raw %>%
      filter(Region == "PRD") %>%
      siscah_spawn(structure = "Section")
  )
  expect_type(spawn, "list")
  expect_true("tbl" %in% class(spawn))
})
