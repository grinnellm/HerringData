test_that("Disposal codes", {
  data("codes_disposal")
  expect_type(codes_disposal, "list")
  expect_equal(dim(codes_disposal), c(9, 6))
  expect_named(
    codes_disposal,
    c("DisposalCode", "DisposalLong", "Disposal", "DisposalSort", "DisposalJOT",
      "DisposalJOTDesc")
  )
  expect_setequal(codes_disposal$DisposalCode, 0:8)
})

test_that("Gear codes", {
  data("codes_gear")
  expect_type(codes_gear, "list")
  expect_equal(dim(codes_gear), c(8, 5))
  expect_named(
    codes_gear, c("GearCode", "Gear", "GearSort", "GearSort2", "GearSort3")
  )
  expect_setequal(codes_gear$GearCode, c(1, 19, 20, 21, 29, 50, 59, 70))
})

test_that("Group codes", {
  data("codes_group")
  expect_type(codes_group, "list")
  expect_equal(dim(codes_group), c(89, 2))
  expect_named(codes_group, c("Section", "Group"))
  expect_type(codes_group$Section, "character")
  expect_type(codes_group$Group, "character")
})

test_that("Period codes", {
  data("codes_period")
  expect_type(codes_period, "list")
  expect_equal(dim(codes_period), c(4, 2))
  expect_named(codes_period, c("Gear", "Period"))
  expect_setequal(codes_period$Gear, 1:4)
  expect_setequal(codes_period$Period, c("Other", "RoeSN", "RoeGN", "SOK"))
})

test_that("Source codes", {
  data("codes_source")
  expect_type(codes_source, "list")
  expect_equal(dim(codes_source), c(8, 3))
  expect_named(codes_source, c("SourceCode", "SampleSource", "SampleSource2"))
  expect_setequal(codes_source$SourceCode, 0:7)
  expect_setequal(
    codes_source$SampleSource,
    c("Roe fishery", "Bait fishery", "Nearshore", "Research - offshore",
      "Other", "Test fishery", "Food fishery", "Reduction fishery")
  )
})

test_that("Conversion factors", {
  data("conv_factors")
  expect_type(conv_factors, "list")
  expect_equal(length(conv_factors), 3)
  expect_named(conv_factors, c("st2t", "ft2m", "lb2kg"))
  expect_setequal(conv_factors, c(0.90718474, 0.3048, 0.453592))
})

test_that("Database information", {
  data("database_info")
  expect_type(database_info, "list")
  expect_equal(length(database_info), 9)
  expect_equal(length(unlist(database_info)), 170)
  expect_named(
    database_info,
    c("herring_conn", "area_loc", "bio_loc", "catch_loc", "width_loc",
      "surf_loc", "macro_loc", "under_loc", "all_loc")
  )
})

test_that("Unbalanced sampling", {
  data("unbalanced_sampling")
  expect_type(unbalanced_sampling, "list")
  expect_equal(length(unbalanced_sampling), 1)
  expect_named(unbalanced_sampling, "CC")
  expect_setequal(unbalanced_sampling$CC$Sections, c("085", "086"))
  expect_setequal(unbalanced_sampling$CC$Years, c(2014, 2015))
})

test_that("Undefined Sections", {
  data("undefined_sections")
  expect_type(undefined_sections, "list")
  expect_equal(dim(undefined_sections), c(28, 2))
  expect_named(undefined_sections, c("StatArea", "Section"))
  expect_type(undefined_sections$StatArea, "character")
  expect_type(undefined_sections$Section, "character")
  expect_equal(sum(as.numeric(undefined_sections$StatArea)), 385)
  expect_equal(sum(as.numeric(undefined_sections$Section)), 3850)

})
