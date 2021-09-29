


test_that("firefox user-css parsing works", {

  filename <- "./css/firefox-html.css"
  rules_text <- paste(readLines(filename), collapse = "\n")

  rules <- read_css(rules_text)

  rules <- rules[names(rules) == 'marquee']
  expect_length(rules, 2)

  expect_equal(
    rules[[2]],
    list(
      `inline-size`    = "-moz-available",
      `display`        = "inline-block",
      `vertical-align` = "text-bottom",
      `text-align`     = "start"
    )
  )

})
