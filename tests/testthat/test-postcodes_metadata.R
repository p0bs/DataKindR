test_that("typical example works", {

  output_typical <- postcodes_metadata("ME1 2RE")

  expect_equal(output_typical$pcd[1], "ME1 2RE")
  expect_equal(output_typical$rgn[1], "E12000008")
  expect_equal(output_typical$msoa21[1], "E02003339")
})

test_that("silly examples fail", {

  expect_error(postcodes_metadata(" ME1 2RE"))
})
