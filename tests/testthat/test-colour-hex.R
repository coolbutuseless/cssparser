
hex_tests <- c(
  "#00000000" = "#00000000",
  '#fff'      = '#FFFFFFFF',
  '#1234'     = '#11223344',
  '#123456'   = '#123456FF'
)


test_that("hex expansion works", {

  for (i in seq_along(hex_tests)) {

    initial  <- names(hex_tests)[i]
    expected <- hex_tests[[i]]

    final <- parse_colour_hex(initial)

    expect_equal(final, expected)
  }

  expect_error(
    parse_colour_hex("123"),
    "Not"
  )



  expect_error(
    parse_colour_hex("#12345"),
    "Not"
  )


})
