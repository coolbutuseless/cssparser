

rgb_tests <- list(
  "rgb(34, 12, 64, 0.6)"      = "#220C4099",
  "rgba(34, 12, 64, 0.6)"     = "#220C4099",
  "rgb(34 12 64 / 0.6)"       = "#220C4099",
  "rgba(34 12 64 / 0.3)"      = "#220C404C",
  "rgb(34.6 12 64 / 60%)"     = "#230C4099",
  "rgba(34.6 12 64 / 30%)"    = "#230C404C",
  "rgba(34.6 100% 64 / 30%)"  = "#23FF404C"
)


test_that("rgb() rgba() works", {

  for (i in seq_along(rgb_tests)) {

    rgb_call  <- names(rgb_tests)[i]
    hex_alpha <- rgb_tests[[i]]

    res <- parse_colour_rgb_to_hex(rgb_call)

    expect_equal(res, hex_alpha)
  }
})
