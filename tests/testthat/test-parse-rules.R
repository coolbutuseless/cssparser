

test_that("rule parsing works works", {

  rules_text <- "
  blockquote:before {
    content: '';
    content: none;
  }
  table, chair {
    border-collapse: collapse;
    border-spacing: 0;
  }
  @font-face {
     something: goes here;
  }
  :pseudosomething {
    something: goes here;
  }
  "


  res <- read_css(rules_text)

  expect_equal(
    unclass(res),
    list(
      `blockquote:before` = list(
        content = "''",
        content = 'none'
      ),
      `table` = list(
        `border-collapse` = 'collapse',
        `border-spacing` = '0'
      ),
      `chair` = list(
        `border-collapse` = 'collapse',
        `border-spacing` = '0'
      ),
      `@font-face` = list(
        something = 'goes here'
      ),
      `:pseudosomething` = list(
        something = 'goes here'
      )
    )
  )

  expect_error(read_css(c(1, 2)))
})


test_that("bad selector start", {
  rules_text <- "
  ;this is bad {
    content: '';
    content: none;
  }
  "

  expect_error(
    res <- read_css(rules_text),
    "Unexpected token"
  )


})


test_that("read_css from file", {
  css <- read_css("./css/simple.css")

  expect_equal(
    css[['#greg']]$color,
    'blue'
  )
})





