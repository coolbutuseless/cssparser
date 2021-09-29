

test_that("@media queries work", {

  rules_text <- "
@media screen and (max-width: 800px) {
  .aside p {
    font-size: .75em;
    }
  .section .latest-shot {
    background: url(../img/bg-light.jpg);
    }
}"

  res <- read_css(rules_text)

  expect_identical(
    unclass(res),
    list(
      `@media screen and (max-width: 800px)` = list(
        `.aside p` = list(`font-size` = ".75em"),
        `.section .latest-shot` = list(
          background = 'url(../img/bg-light.jpg)'
        )
      )
    )
  )

})
