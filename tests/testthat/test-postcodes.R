test_that("trivial cases work", {

  postcode_trivial <- "ME1 2re "
  expect_equal(postcodes(postcode_trivial), "ME1 2RE")
  expect_equal(postcodes(postcode_trivial, postcode_type = "region"), "ME")
  expect_equal(postcodes(postcode_trivial, postcode_type = "area"), "ME1")
  expect_equal(postcodes(postcode_trivial, postcode_type = "locale"), "ME1 2")

})

test_that("stranger cases work", {

  expect_equal(postcodes(" WC1a 1aD "), "WC1A 1AD")
  expect_equal(postcodes(" WC1a 1aD ", postcode_type = "region"), "WC")
  expect_equal(postcodes(" WC1a 1aD ", postcode_type = "area"), "WC1A")
  expect_equal(postcodes(" WC1a 1aD ", postcode_type = "locale"), "WC1A 1")

  expect_equal(postcodes("N1 0DT"), "N1 0DT")
  expect_equal(postcodes("N1 0DT", postcode_type = "region"), "N")
  expect_equal(postcodes("N1 0DT", postcode_type = "area"), "N1")
  expect_equal(postcodes("N1 0DT", postcode_type = "locale"), "N1 0")

  expect_equal(postcodes("TN12 0QS"), "TN12 0QS")
  expect_equal(postcodes("TN12 0QS", postcode_type = "region"), "TN")
  expect_equal(postcodes("TN12 0QS", postcode_type = "area"), "TN12")
  expect_equal(postcodes("TN12 0QS", postcode_type = "locale"), "TN12 0")

})
