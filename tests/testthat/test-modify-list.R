

test_that("recursive modify list works", {

  list1 <- list(
    a = list(
      a1 = 1,
      a2 = 2
    ),
    b = list(
      b1 = list(
        bb1 = 10,
        bb2 = 20
      )
    ),
    c = 1,
    d = 2
  )



  list2 <- list(
    a = list(
      a1 = 1,
      a2 = 21
    ),
    b = list(
      b1 = list(
        bb2 = 201
      )
    ),
    c = 11,
    d = list(
      d1 = 10
    )
  )


  res <- css_merge_core(list1, list2)

  expect_equal(
    res,
    list(
      a = list(
        a1 = 1,
        a2 = 21
      ),
      b = list(
        b1 = list(
          bb1 = 10,
          bb2 = 201
        )
      ),
      c = 11,
      d = list(
        d1 = 10
      )
    )
  )

})



test_that("recursive modify list respects important attribute", {

  a2 <- 2
  attr(a2, 'important') <- TRUE


  list1 <- list(
    a = list(
      a1 = 1,
      a2 = a2
    ),
    b = list(
      b1 = list(
        bb1 = 10,
        bb2 = 20
      )
    ),
    c = 1,
    d = 2
  )



  list2 <- list(
    a = list(
      a1 = 1,
      a2 = 21
    ),
    b = list(
      b1 = list(
        bb2 = 201
      )
    ),
    c = 11,
    d = list(
      d1 = 10
    )
  )


  res <- css_merge_core(list1, list2)

  expect_equal(
    res,
    list(
      a = list(
        a1 = 1,
        a2 = a2
      ),
      b = list(
        b1 = list(
          bb1 = 10,
          bb2 = 201
        )
      ),
      c = 11,
      d = list(
        d1 = 10
      )
    )
  )
})


test_that("modify list on actual", {

  m1 <- ".greg {
    fill: red !important;
    color: blue;
    stroke: white !important;
  }"

  m2 <- ".greg {
    fill: yellow;
    color: green;
    stroke: black !important;
  }"

  rules1 <- read_css(m1)
  rules2 <- read_css(m2)

  res <- css_merge_core(rules1, rules2)

  important_red <- 'red'
  attr(important_red, 'important') <- TRUE

  important_black <- 'black'
  attr(important_black, 'important') <- TRUE

  expect_equal(
    res,
    list(
      .greg = list(
        fill = important_red,
        color = 'green',
        stroke = important_black
      )
    )
  )


})



test_that("css_merge cascade works", {

  l1 <- list(a = 1, b = 1, c = 1)
  l2 <- list(       b = 2)
  l3 <- list(              c = 3, d = 4)
  l4 <- list(                     d = 5, e = 6)

  res1 <- css_merge(l1, l2, l3, l4)

  expect_equal(
    res1,
    list(a = 1, b = 2, c = 3, d = 5, e = 6)
  )





})






