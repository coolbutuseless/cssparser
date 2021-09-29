


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
  <div id="main" style="color:white; width:50%">
    <div id="sidebar">Sidebar</div>
    <div id="content">Main content</div>
    <p> This paragraph </p>
    <ul></ul>
  </div>
  <div id="footer" class="light" style="color:green;"> goodbye </div>
  </html>
</body>
')

rules     <- read_css(stylesheet)




test_that("style_tree works", {

  style <- css_apply(html, rules)

  expect_identical(
    style[['/html/body/div[1]/ul']]$color,
    "white"
  )


  expect_identical(
    style[['/html/body/div[1]/p']]$color,
    "hotpink"
  )


})





