test_that("custom_palette works", {
  expect_error(
    custom_palette(palette = "redblue"),
    NA
  )
  expect_warning(custom_palette(palette = "high_contrast")(9))
  expect_equal(
    custom_palette(palette = "redblue")(2),
    c("#319CD7", "#F76063")
  )
  expect_length(
    custom_palette(palette = "viridis")(12),
    12
  )
})
