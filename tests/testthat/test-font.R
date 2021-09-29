

font_test <- list(
  list(
    string = '1.2em "Fira Sans", sans-serif;',
    result = c(
      size = '1.2em', family_dquote = 'Fira Sans', family_raw = 'sans-serif'
    )
  ),

  list(
    string = 'italic 1.2em "Fira Sans", serif;',
    result = c(
      style = 'italic', size = '1.2em', family_dquote = 'Fira Sans', family_raw = 'serif'
    )
  ),

  list(
    string = 'italic small-caps bold 16px/2 cursive;',
    result = c(
      style = 'italic', variant = 'small-caps', weight = 'bold',
      size = '16px', lineheight = '2', family_raw = 'cursive'
    )
  ),

  list(
    string = 'small-caps bold condensed 24px/1 sans-serif;',
    result = c(
      variant = 'small-caps', weight = 'bold',  stretch = 'condensed', size = '24px',
      lineheight = '1', family_raw = 'sans-serif'
    )
  ),

  list(
    string = 'caption;',
    result = c(
      system_font = 'caption'
    )
  )
#
#   list(
#     string = "",
#     result = list(
#
#     )
#   )


)



test_that("font lex patterns works", {

  for (i in seq_along(font_test)) {
    tokens <- lex(font_test[[i]]$string, font_patterns)
    tokens <- tokens[!names(tokens) %in% c('whitespace', 'comma', 'semicolon')]
    expect_equal(tokens, font_test[[i]]$result)
  }

})


test_that("split_font_shorthand works", {

  font_text <- "italic small-caps bold condensed 16px/2 cursive;"
  font <- split_font_shorthand(font_text)

  expect_equal(
    font,
    list(
      `font-size`        = '16px',
      `font-stretch`     = 'condensed',
      `font-style`       = 'italic',
      `font-variant`     = 'small-caps',
      `font-weight`      = 'bold',
      `line-height`      = '2',
      `font-size-adjust` = 'none',
      `font-kerning`     = 'auto',
      `font-family`      = 'cursive'
    )
  )


})












