
stylesheet  <- ".greg {
    fill: red;
    color: blue;
  }

  #mary, #jo, #ann:hover {
    fill: white 1px;
    color:   purple;
  }"

css <- read_css(stylesheet)

css


xml <- xml2::read_xml('
  <html>
    <p id="mary"> hello </p>
  </html>
')



html <- xml2::read_html('
  <html>
    <p id="mary"> hello </p>
  </html>
')



test_that("multiplication works", {

  xml <- css_apply_inline(xml, css)

  node <- xml2::xml_find_all(xml, "/html//p")

  istyle <- xml2::xml_attr(node, 'style')

  expect_equal(
    istyle,
    "fill:white 1px; color:purple;"
  )
})
