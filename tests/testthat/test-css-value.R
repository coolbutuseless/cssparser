


test_that("css value works", {
  expect_equal(css_length_as_pixels(css_string_as_css_length("12")), 12)
  expect_equal(css_length_as_pixels(css_string_as_css_length("12pt")), 12)
  expect_equal(css_length_as_pixels(css_string_as_css_length("12pc")), 12)
  expect_equal(css_length_as_pixels(css_string_as_css_length("12px")), 12)
  expect_equal(css_length_as_pixels(css_string_as_css_length("12in")), 12 * 96)
  expect_equal(css_length_as_pixels(css_string_as_css_length("12cm")), 12 * 96/2.54)
  expect_equal(css_length_as_pixels(css_string_as_css_length("12mm")), 12 * 96/2.54/10)

  expect_equal(css_length_as_pixels(css_string_as_css_length("2em")), 2 * 12)
  expect_equal(css_length_as_pixels(css_string_as_css_length("2ex")), 2 * 12)
  expect_equal(css_length_as_pixels(css_string_as_css_length("2ch")), 2 * 12)
  expect_equal(css_length_as_pixels(css_string_as_css_length("2rem")), 2 * 12)

  expect_warning(
    css_string_as_css_length(NULL),
    "Not a number"
  )

  expect_equal(
    css_string_as_css_length('none'),
    0
  )


  expect_equal(
    css_length_as_pixels(12),
    12
  )

  expect_warning(
    css_length_as_pixels(css_length(12, 'zebs')),
    "unknown suffix"
  )

  expect_equal(
    css_string_as_pixels("12px"),
    12
  )

  expect_equal(
    css_string_as_pixels(12),
    12
  )

  expect_equal(
    css_string_as_pixels("12%"),
    0.12
  )

  expect_equal(
    css_string_as_pixels(NULL),
    0
  )


})
