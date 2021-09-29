
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cssparser

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
[![R-CMD-check](https://github.com/coolbutuseless/cssparser/workflows/R-CMD-check/badge.svg)](https://github.com/coolbutuseless/cssparser/actions)
<!-- badges: end -->

<img src="man/figures/titlecard.png">

`cssparser` is a CSS parser (written in vanilla R) which can:

-   Parse CSS into R lists
-   Combine CSS
-   Apply CSS to determine final computed styles of elements in an HTML
    document

## What’s in the box

-   `read_css()` read a cascading style sheet consisting of multiple
    selectors into an R list
-   `read_inline_style()` read a single inline style string into an R
    list
-   `css_apply()` determine the calculated style for each element in an
    html document
-   `css_apply_inline()` add the calculated style an inline `<style>`
    tag to every element in an HTML document
-   `css_merge()`, `style_merge()` recursively merge multiple style
    sheets or styles
-   `css_pretty_print()`, `style_pretty_print()` helpers to view the R
    list structures in a way that may be more meaningful if you are
    comparing the information to its original CSS form.

## Vignettes

-   [Read CSS from
    file/string](https://coolbutuseless.github.io/package/cssparser/articles/read-css.html)
-   [Apply CSS to an HTML
    document](https://coolbutuseless.github.io/package/cssparser/articles/apply-css.html)
-   [Merge multiple CSS
    stylesheets](https://coolbutuseless.github.io/package/cssparser/articles/css-merge.html)
-   [A toy HTML layout engine in
    R](https://coolbutuseless.github.io/package/cssparser/articles/web-page-render-frames.html)
-   [Another `read_css()`
    example](https://coolbutuseless.github.io/package/cssparser/articles/another-example.html)

## Installation

You can install from
[GitHub](https://github.com/coolbutuseless/cssparser) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/cssparser')
```

## Limitations

-   Shorthand properties. There are many shorthand properties in CSS
    which are compact representations of multiple other CSS properties.
    E.g.
    [`border`](https://developer.mozilla.org/en-US/docs/Web/CSS/border)
    is is a way of combining `border-width`, `border-style` and
    `border-color` in a single property. These shorthand properties have
    particular inheritance rules, and currently only the `font`
    shorthand property is the only one which is expanded and inhertied
    correctly.
-   The `all` selector is not implemented yet.
-   The CSS values like `inherit` and `unset` are not yet handled.
-   The `@`-prefixed selectors (e.g. `@media`) aren’t really handled
    well. They exist in the parsed data, but have not been tested
-   pseudo-elements and pseudo-classes do not take part in the cascading
    of styles
-   there is no concept of page size, and it is expected that if you
    want to calculate the final `width` of any particular element,
    you’ll have to do your own device-specific shenanigans.

## Example: Parse CSS

``` r
library(cssparser)

stylesheet <- '
#main {
  background-color: red !important;
  color: blue;
}

.footer {
  border-color: rgb(50 100 255);
  font-weight: lighter;
  margin-left: 1cm;
}

.footer > p {
  background-color: green;
}
'

css <- cssparser::read_css(stylesheet)
css
```

    >   $`#main`
    >   $`#main`$`background-color`
    >   [1] "red"
    >   attr(,"important")
    >   [1] TRUE
    >   
    >   $`#main`$color
    >   [1] "blue"
    >   
    >   
    >   $.footer
    >   $.footer$`border-color`
    >   [1] "rgb(50 100 255)"
    >   
    >   $.footer$`font-weight`
    >   [1] "lighter"
    >   
    >   $.footer$`margin-left`
    >   [1] "1cm"
    >   
    >   
    >   $`.footer > p`
    >   $`.footer > p`$`background-color`
    >   [1] "green"

``` r
cssparser::css_pretty_print(css)
```

    >   #main {
    >     background-color: red !important;
    >     color: blue;
    >   }
    >   
    >   .footer {
    >     border-color: rgb(50 100 255);
    >     font-weight: lighter;
    >     margin-left: 1cm;
    >   }
    >   
    >   .footer > p {
    >     background-color: green;
    >   }

## Example: Extract values from CSS in R

The object returned by `read_css()` is a nested list of name/value
pairs.

The top-level name is the CSS Selector (e.g. `#main`). The value it
points to is another named list - property/value pairs
e.g. `color = blue`

To extract a single style from the CSS object, just subset it as you
would for any R list

``` r
css[['.footer']][['font-weight']]
```

    >   [1] "lighter"

``` r
css[['#main']][['background-color']]  # Note the 'important' attribute on this CSS declaration
```

    >   [1] "red"
    >   attr(,"important")
    >   [1] TRUE

## Example: Extract/Convert values

Some tools are included for extracting parsing CSS values into R values
e.g.

-   `css_colour_to_hex()` will convert `rgb()` and `hsl()` strings to R
    colours. As well as CSS colour names and 3 letter hex colours
    (e.g. `#fab` -&gt; `#ffaabb`)
-   `css_string_as_pixels()` has a (naive) go at converting length
    values to a number of pixels (assuming 96dpi display). This is a
    wrapper around two other functions:
    -   `css_string_as_css_length()` to convert a string to a numeric
        value, and include the `unit` as an attribute.
    -   `css_length_as_pixels()` to convert a value with a `unit`
        attribute to a length in pixels

``` r
colour <- css[['.footer']][['border-color']]
colour
```

    >   [1] "rgb(50 100 255)"

``` r
cssparser::css_colour_to_hex( colour )
```

    >   [1] "#3264FFFF"

``` r
value <- css[['.footer']][['margin-left']]
value
```

    >   [1] "1cm"

``` r
cssparser::css_string_as_css_length(value)
```

    >   [1] 1
    >   attr(,"unit")
    >   [1] "cm"

``` r
cssparser::css_string_as_pixels(value)
```

    >   [1] 37.79528

## Example: Apply CSS to html

Given `css` and an `html/xml` document, `cssparser` can produce a named
list containing the computed style for every element.

The names in this list are the `xpaths` for each element, and the values
are the named list of property/value declarations on this element.

The calculation of the style for each element takes into account:

-   `<tag>`, `#id` and `.class` selectors
-   selector combinators like `#main > div`
-   CSS inheritance from parent elements

In the `computed_styles` for the following HTML document, note the
following:

-   The first `<div>` (with `id="main"`) has
    `xpath = "/html/body/div[1]"` has a `background-color` of `red` as
    the `!important` flag was set in the stylesheet and this overrides
    the inline style `background-color: yellow`
-   The CSS selector `.footer > p` has correctly applied
    `background-color = "green"` to only the second `<p>` in the
    document (with `xpath = "/html/body/div[2]/p"`)
-   The `border`-related properties on the `<div>` elements are not
    evident on the child `<p>` elements as border properties are not
    inherited.

``` r
html <- xml2::read_html('
<html>
<div id="main" class="thin" style="border: 2px; background-color: yellow;">
  <p>Lorem Ipsum</p>
</div>
<div class="footer">
  <p>Goodbye</p>
</div>

</html>')


computed_styles <- cssparser::css_apply(html, css)
cssparser::css_pretty_print(computed_styles)
```

    >   /html/body/div[2] {
    >     border-color: rgb(50 100 255);
    >     font-weight: lighter;
    >     margin-left: 1cm;
    >   }
    >   
    >   /html/body/div[2]/p {
    >     font-weight: lighter;
    >     background-color: green;
    >   }
    >   
    >   /html/body/div[1] {
    >     background-color: red !important;
    >     color: blue;
    >     border: 2px;
    >   }
    >   
    >   /html {
    >   }
    >   
    >   /html/body {
    >   }
    >   
    >   /html/body/div[1]/p {
    >     color: blue;
    >   }

## Example: Manually merging CSS styles

The following code shows a manual way of combining styles from a number
of different sources using `style_merge()`

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# StyleSheet
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stylesheet <- '
#greg {
  fill: red !important;
  color: blue;
}

.thin {
  border: 1px;
  border-color: hsl(30 50% 40%);
  font-weight: lighter;
}'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example HTML
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
html <- xml2::read_html('
<div id="greg" class="thin" style="border: 2px; fill: yellow;">
  Lorem Ipsum
</div>')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert stylesheet into rulesets (i.e. R lists indexed by selector)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css <- read_css(stylesheet)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Manually calculate style for this element by merging the various styles
# which apply to this element based upon the CSS selectors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
greg_final <- style_merge(
  css[['div']],
  css[[".thin"]],
  css[['#greg']], 
  read_inline_style("border: 2px; fill: yellow;")
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note: "!important" property carried through.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cssparser::style_pretty_print(greg_final)
```

    >   border: 2px;
    >   border-color: hsl(30 50% 40%);
    >   font-weight: lighter;
    >   fill: red !important;
    >   color: blue;

``` r
cssparser::css_colour_to_hex(greg_final$`border-color`)
```

    >   [1] "#996633FF"

## Related Software

-   [katana](https://github.com/hackers-painters/katana-parser) C
    library for parsing CSS
    -   [{tsuka}](https://github.com/hrbrmstr/tsuka) a wrapper around
        katana for R

## Acknowledgements

-   R Core for developing and maintaining the language.
-   CRAN maintainers, for patiently shepherding packages onto CRAN and
    maintaining the repository
