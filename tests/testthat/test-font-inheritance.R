
# 'italic 1.2em "Fira Sans", serif;'
# 'small-caps bold condensed 24px/1 sans-serif;'

modify_list <- function(x, y) {
  for (n in names(y)) {
    x[[n]] <- y[[n]]
  }
  x
}


font_short1 <- list( font = 'small-caps bold condensed 24px/1 sans-serif;' )
font_short2 <- list( font = 'italic 1.2em "Fira Sans", serif;' )

test_that("font shorthand inheritance works works", {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If both styles have 'font' shorthands, then the 2nd takes precedence
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  final_style <- style_merge(
    font_short1,
    font_short2
  )

  expect_equal(final_style, font_short2)



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Only the second
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  final_style <- style_merge(
    list(),
    font_short2
  )

  expect_equal(final_style, font_short2)



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Only the first
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  final_style <- style_merge(
    font_short1,
    list()
  )

  expect_equal(final_style, font_short1)



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Only the first
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  final_style <- style_merge(
    font_short1,
    list(`font-weight` = 'bold')
  )

  expected <- modify_list(
    split_font_shorthand(font_short1),
    list(`font-weight` = 'bold')
  )

  expect_equal(
    final_style,
    expected
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Only the first
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  final_style <- style_merge(
    list(`font-weight` = 'bold'),
    font_short2
  )

  expected <- split_font_shorthand(font_short2)

  final_style <- final_style[order(names(final_style))]
  expected    <- expected   [order(names(expected   ))]


  expect_equal(
    final_style,
    expected
  )


})
