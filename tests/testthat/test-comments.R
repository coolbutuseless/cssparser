


test_that("comments excluded correctly", {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rules_text <- '
  .class1 {
    color: red;
    fill: blue;
  }'

  res <- read_css(rules_text)

  expect_identical(
    unclass(res),
    list(.class1 = list(
      color = 'red',
      fill = 'blue'
    ))
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rules_text <- '
  /* This is a comment */
  .class1 {
    color: red;
    fill: blue;
  }'


  res <- read_css(rules_text)
  expect_identical(
    unclass(res),
    list(.class1 = list(
      color = 'red',
      fill = 'blue'
    ))
  )



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rules_text <- '
  /* This is a comment */
  .class1 {
    color: red;  /* so is this */
    fill: blue;
  }'


  res <- read_css(rules_text)
  expect_identical(
    unclass(res),
    list(.class1 = list(
      color = 'red',
      fill = 'blue'
    ))
  )



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rules_text <- '
  /* This is a comment */
  .class1 {
    color: red;  /* so is this */
    fill: blue;
  }

  /* multi
  line
  comment
  */

  '


  res <- read_css(rules_text)
  expect_identical(
    unclass(res),
    list(.class1 = list(
      color = 'red',
      fill = 'blue'
    ))
  )


  })
