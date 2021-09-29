

test_that("inline style parsing works", {

  expect_identical(
    read_inline_style(""),
    NULL
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # simple case
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- read_inline_style("a: b; c: d;")

  expect_identical(
    unclass(res),
    list(a='b', c='d')
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Real-life case. trailing ";" may be missing!
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  style_string <- "stroke:red; fill: black; stroke-width:12"
  res <- read_inline_style(style_string)

  expect_identical(
    unclass(res),
    list(
      stroke         = 'red',
      fill           = 'black',
      `stroke-width` = "12"
    )
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Mismatched declaraions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  expect_error(
    read_inline_style("a: b; c"),
    "parsing"
  )

  expect_error(
    read_inline_style("a: b; c : "),
    "parsing"
  )

  expect_error(
    read_inline_style("a:: b; c :d "),
    "parsing"
  )

  expect_error(
    read_inline_style("a=b; c:d;"),
    "parsing"
  )

  res <- read_inline_style("{a: b; c: d;}")

  expect_identical(
    unclass(res),
    list(a='b', c='d')
  )
})
