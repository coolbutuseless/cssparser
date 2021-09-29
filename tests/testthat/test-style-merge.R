


test_that("style_merge works", {
  style1 <- list(color = 'blue', `font-weight` = 'bold')
  style2 <- list(color = 'red', `font-size`="12px")
  res <- style_merge(style1, style2)

  expect_equal(
    res,
    list(
      color = "red",
      `font-weight` = 'bold',
      `font-size` = '12px'
    )
  )

})
