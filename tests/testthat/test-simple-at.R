test_that("simple at works", {
  rules_text <- '
  @import url;
  .greg {
    fill: red;
  }'

  res <- read_css(rules_text)

  expect_equal(
    res,
    list(
      `@import` = 'url',
      .greg = list(
        fill = 'red'
      )
    )
  )

})

