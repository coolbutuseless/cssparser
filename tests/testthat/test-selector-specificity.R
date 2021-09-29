

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test cases from CSS documentation
# https://drafts.csswg.org/selectors-3/#specificity
# A selector's specificity is calculated as follows:
#
#  count the number of ID selectors in the selector (= a)
#  count the number of class selectors, attributes selectors, and pseudo-classes in the selector (= b)
#  count the number of type selectors and pseudo-elements in the selector (= c)
#  ignore the universal selector
#
# Selectors inside the negation pseudo-class are counted like any other,
#  but the negation itself does not count as a pseudo-class.
#
# Concatenating the three numbers a-b-c (in a number system with a large base)
# gives the specificity.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
specs <- c(
    `*`              = 000000,   #  a=0 b=0 c=0 -> specificity =   0
    `LI`             = 000001,   #  a=0 b=0 c=1 -> specificity =   1
    `UL LI`          = 000002,   #  a=0 b=0 c=2 -> specificity =   2
    `UL OL+LI`       = 000003,   #  a=0 b=0 c=3 -> specificity =   3
    `H1 + *[REL=up]` = 000101,   #  a=0 b=1 c=1 -> specificity =  11
    `UL OL LI.red`   = 000103,   #  a=0 b=1 c=3 -> specificity =  13
    `LI.red.level`   = 000201,   #  a=0 b=2 c=1 -> specificity =  21
    `#x34y`          = 010000,   #  a=1 b=0 c=0 -> specificity = 100
    `#s12:not(FOO)`  = 010001    #  a=1 b=0 c=1 -> specificity = 101
)

test_that("selector_specificity works", {

  for (i in seq_along(specs)) {

    selector <- names(specs)[i]
    expected_specificity <- specs[[i]]

    expect_equal(
      selector_specificity(selector),
      expected_specificity
    )
  }

})
