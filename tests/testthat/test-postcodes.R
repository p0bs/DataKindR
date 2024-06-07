test_that("trivial case works", {
  postcode_trivial <- "ME1 2RE"
  expect_equal(postcodes(postcode_value = postcode_trivial), postcode_trivial)
})
