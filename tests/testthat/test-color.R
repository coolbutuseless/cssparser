


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# alpha_set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("alpha_set() works", {

  expect_equal(
    alpha_set('#00000000', 1),
    '#000000FF'
  )

  expect_equal(
    alpha_set('#00000000', 0.5),
    '#00000080'
  )

  expect_error(
    alpha_set('#000000', 0.5)
  )

})



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# alpha_mul
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("alpha_mul() works", {

  expect_equal(
    alpha_mul('#00000000', 1),
    '#00000000'
  )

  expect_equal(
    alpha_mul('#000000FF', 0.5),
    '#00000080'
  )

  expect_error(
    alpha_mul('#000000', 0.5)
  )
})




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# css_colour_to_hex
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("css_coluor_to_hex works", {

  expect_equal(
    css_colour_to_hex("rgb(1, 2, 3)"),
    "#010203FF"
  )

  expect_equal(
    css_colour_to_hex("rgb(1, 2, 3, 0.5)"),
    "#01020380"
  )

  expect_equal(
    css_colour_to_hex("rgb(1, 2, 3, 100%)"),
    "#010203FF"
  )

  expect_equal(
    css_colour_to_hex("hsl(30, 100%, 50%, 0.6)"),
    "#FF800099"
  )

  expect_warning(css_colour_to_hex("hcl(12)"))
  expect_warning(css_colour_to_hex("lab(12)"))
  expect_warning(css_colour_to_hex("color(12)"))

  expect_equal(
    css_colour_to_hex("#123"),
    "#112233FF"
  )

  expect_equal(
    css_colour_to_hex("silver"),
    "#C0C0C0FF"
  )


  expect_equal(
    css_colour_to_hex("none"),
    "#00000000"
  )

  expect_equal(
    css_colour_to_hex("red"),
    "#FF0000FF"
  )

  expect_warning(css_colour_to_hex("currentColor"))

})










