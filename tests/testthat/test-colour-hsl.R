

hsl_tests <- list(
  "hsl(30, 100%, 50%, 0.6)"   = "#FF800099",
  "hsla(30, 100%, 50%, 0.6)"  = "#FF800099",
  "hsl(30 100% 50% / 0.6)"    = "#FF800099",
  "hsla(30 100% 50% / 0.6)"   = "#FF800099",
  "hsl(30.0 100% 50% / 60%)"  = "#FF800099",
  "hsla(30.2 100% 50% / 60%)" = "#FF800099",
  "hsla(30.2 100% 50% )"      = "#FF8000FF"
)


test_that("hsl() hsla() works", {

  for (i in seq_along(hsl_tests)) {

    hsl_call  <- names(hsl_tests)[i]
    hex_alpha <- hsl_tests[[i]]

    res <- parse_colour_hsl_to_hex(hsl_call)

    expect_equal(res, hex_alpha)
  }
})
