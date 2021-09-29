

test_that("stream works", {
  tokens <- c(a= 1, b= 2, b= 3, b= 4, c= 5, c= 6, b= 7,
              d= 8, d= 9, e=10, f=11, g=12, h=13, i=14,
              j=15, j=16, j=17, k=18, l=19, m=20, n=21)
  stream <- init_stream(tokens)

  expect_equal(
    stream$peek(2),
    c(a=1, b= 2)
  )

  expect_equal(
    stream$distance_before_type('c'),
    4
  )

  expect_equal(
    stream$consume(4),
    c(a=1, b=2, b=3, b=4)
  )

  expect_equal(
    stream$idx,
    5
  )

  expect_equal(
    stream$peek(4),
    c(c = 5, c = 6, b= 7, d = 8)
  )

  stream$skip(2)

  expect_equal(
    stream$peek(2),
    c(b = 7, d=8)
  )

  expect_false(stream$eos())

  expect_error(stream$peek(30), "within_bounds")
  expect_error(stream$skip(30), "within_bounds")
  expect_error(stream$consume(30), "within_bounds")

  stream$consume(15)
  expect_true(stream$idx == stream$N + 1L)
  expect_true(stream$eos())

  expect_error(
    init_stream(c(1, 2, 3)),
    "named"
  )


})



test_that("substream works", {
  tokens <- c(a= 1, b= 2, b= 3, b= 4, c= 5, c= 6, b= 7,
              d= 8, d= 9, e=10, f=11, g=12, h=13, i=14,
              j=15, j=16, j=17, k=18, l=19, m=20, n=21)
  stream <- init_stream(tokens)

  stream$skip(3)
  n <- stream$distance_before_type('d')
  stream$skip(n)

  expect_equal(
    stream$peek(3),
    c(d=8, d=9, e=10)
  )

  ss <- stream$substream(5)$copy_without_type(c('e', 'f'))
  expect_equal(
    ss$tokens,
    c(d=8, d=9, g=12)
  )

  expect_equal(
    stream$peek(1),
    c(h = 13)
  )





})
