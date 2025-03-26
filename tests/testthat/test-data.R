test_that("Disposal codes", {
  data("codes_disposal")
  expect_type(codes_disposal, "list")
  expect_equal(dim(codes_disposal), c(9, 6))
  expect_named(codes_disposal,
               c("DisposalCode", "DisposalLong", "Disposal", "DisposalSort",
                 "DisposalJOT", "DisposalJOTDesc"
               )
  )
  expect_setequal(codes_disposal$DisposalCode, 0:8)
})
