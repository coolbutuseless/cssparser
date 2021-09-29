




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Style sheet
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stylesheet <- '
body {
   color: blue;
}
.light {
  font-weight: lighter;
}
.light.speed {
  font-weight: normal;
}
div > p {
  color: hotpink;
}
'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HTML document
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
html <- xml2::read_html('
<body>
  <html>
  <h1> hello </h1>
  <p> Nothing happens in this p </p>
  <div id="main" transform="1" style="color:white; width:50%">
    <div id="sidebar" transform="2">Sidebar</div>
    <div id="content">Main content</div>
    <p> This paragraph </p>
    <ul></ul>
  </div>
  <div id="footer" class="light" style="color:green;"> goodbye </div>
  </html>
</body>
')

rules     <- read_css(stylesheet)




test_that("transform accumulation works", {

  style <- css_apply(html, rules, svg = TRUE)

  expect_identical(
    style[['/html/body/div[1]/div[1]']]$transform,
    c("1", "2")
  )


  expect_identical(
    style[['/html/body/div[1]/div[2]']]$transform,
    c("1")
  )


})





