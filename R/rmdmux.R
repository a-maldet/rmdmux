#' @include utils.R
#' @include create_examples.R
NULL

resolve_default_css <- function(css) {
  css_map <- c(
    rmdmux_paged_default = pkg_resource("stylesheets", "rmdmux_paged_default.sass"),
    rmdmux_plain_default = pkg_resource("stylesheets", "rmdmux_plain_default.sass")
  )
  sapply(
    css,
    function(x) {
      if (x %in% names(css_map)) {
        css_map[x]
      } else {
        x
      }
    }
  )
}

#' HTML output formats for reports
#' 
#' The following **html output formats** are included in the **rmdmux** package:
#' - [html_rmdmux_paged()]: A **paginated html** output, which can be printed to **pdf**
#' - [html_rmdmux_plain()]: A **plain html** output, which is fully accessible
#'   (eg. for screen readers etc.)
#' All output formats of the **rmdmux** package support the usage of
#' [styledTables][styledTables::styled_table()].
#' For a better understanding, see section **Supported yaml parameters** or 
#' have a look at the  look at the minimal example, which can be created with the command
#' [create_example_1()].
#' 
#' @section Supported yaml parameters:
#' The following yaml parameters are supported by the `rmdmux` output formats:
#' - `title`: Title of the document
#' - `subtitle`: Optional subtitle of the document
#' - `author`: Optional field holding one ore more author names.
#'    Either a character vector or an unnamed list, where each list entry has
#'    the entries `name` and `affiliation`.
#' - `date`: Optional date string
#' - `lang`: HTML language definition. Inserted in `<html lang="XX">` tag of the html document.
#' - `params`: Optional list of parameters, which should be available inside of the **rmd chunks**.
#' - `output`: Specify one or more output formats:
#'     - `html_rmdmux_paged` or `html_rmdmux_plain`:
#'         - `css`: Additional **css**, **scss** or **sass** files, which should
#'           be appended to the output file.
#'         - `toc`: Should a **table of contents** be included?
#'         - `md_extensions: -autolink_bare_uris`: Prohibit pandoc from automatically
#'           replacing all urls in the **yaml** header by `<a href="...">` elements
#'         - `pandoc_args: ["-M", "paged-footnotes=true"]`: This option shoud only be
#'           used for `html_rmdmux_paged`: This causes the footnotes to be printed
#'           at the bottom of each page and not as a collection at
#'           the end of the document.
#' - `lot`: Should a **list of table** be included?
#' - `lof`: Should a **list of figures** be included?
#' - `toc-title`: Title of the **table of contents**
#' - `lot-title`: Title of the **list of tables**
#' - `lof-title`: Title of the **list of figures**
#' - `paged-footnotes`: Print footnotes at the bottom of each page
#'    (Cauton: This option is valid only for `html_rmdmux_paged` =>
#'    Better use `pandoc_args` solution listed above)
#' - `title-page`: Optional list object defining the title page
#'   - `cover-image`: Optional list object defining a cover image, which will
#'     be positioned below the report title.
#'     - `src`: file path of the cover image
#'     - `alt`: alternative text for the cover image
#'   - `cover-logo`: Optional list object defining a cover logo, which will
#'     be positioned in the page corner of the title page of the paginated output:
#'     - `src`: file path of the cover image
#'     - `alt`: alternative text for the cover image
#'   - `footer`: Optional list object defining the footer of the title page
#'     - `left`: Optional text for the left part of page footer
#'     - `middle`: Optional text for the center part of page footer
#'     - `right`: Optional text for the right part of the page footer 
#' - `imprint`: Configure the **imprint** section
#'     - `heading`: Optional string holding the title of the **imprint** section
#'     - `title-label`: Optional label for the title field
#'     - `edition-label`: Optional label for the edition field
#'     - `edition`: Optional string holding information about the report edition
#'     - `publisher-label`: Optional label for the publisher field
#'     - `publisher`: Optional character vector holding information about the publisher
#'     - `address-label`: Optional label for the address field
#'     - `address`: Optional character vector holding the address as multiple lines of text
#'     - `author-label`: Optional label for the author field
#'     - `editor-label`: Optional label for the editor field
#'     - `editor`: Optional field holding one ore more editor names.
#'       Either a character vector or an unnamed list, where each list entry has
#'       the entries `name` and `affiliation`.
#'     - `email-label`: Optional label for the email field
#'     - `email`: Optional string holding the email address
#'     - `phone-label`: Optional label for the phone number field
#'     - `phone`: Optional string holding the phone number
#'     - `url-label`: Optional label for the **url** field
#'     - `url`: Optional string holding the **url** of the publisher
#'     - `url-qrcode`: Optional list object defining a QR-code image, which will
#'       be positioned next to the `url`.
#'       - `src`: file path to the image file for the QR-code
#'       - `alt`: alternative text for the QR-code image
#'     - `date-label`: Optional label for the date field
#'     - `place-label`: Optional label for the place field
#'     - `place`: Optional character vector holding information about the publishing place
#'     - `remark`: Optional character vector holding one ore more paragraphs
#'       of remarks, which will be appended at theend of the imprint section
#'     - `copyright-label`: Optional label for the copyrights field
#'     - `copyright`: Optional character vector holding information about
#'       credits and copyrights
#'     - `remarks`: Optional character vector holding additional remarks,
#'       which will be included at the end of the imprint.
#' @section Translation of report element labels:
#' In order to translate report element labels into other languates
#' (e.g. `"Abbildung"` is german for `"Figure"` etc.) we use the `_bookdown.yml`
#' file, in which we can configure all important report labels. This file
#' must be located in the same folder as the **rmd** files.
#' For a detailed description see \url{https://bookdown.org/yihui/bookdown/internationalization.html}
#' @inheritParams rmarkdown::html_document
#' @inheritParams bookdown::html_document2
#' @export
#' @rdname rmdmux
html_rmdmux_paged <- function(
    ...,
    template = pkg_resource('html', 'rmdmux_paged.html'),
    css = NULL,
    toc = TRUE,
    md_extensions = "-autolink_bare_uris",
    number_sections = TRUE,
    fig_caption = TRUE
) {
  fmt <- pagedown_html_format(
    ...,
    template = template,
    theme = NULL,
    css = c(
      resolve_default_css(css)
    ),
    md_extensions = md_extensions,
    toc = toc,
    number_sections = number_sections,
    fig_caption = fig_caption,
    .pagedjs = TRUE,
    .pandoc_args = c(
      pagedown_lua_filters('uri-to-fn.lua', 'loft.lua', 'footnotes.lua'),
      styledTables::lua_remove_filter(),
      pagedown_pandoc_chapter_name_args()
    )
  )
  fmt$knitr$opts_knit[['rmdmux_output_format']] <- "html_rmdmux_paged"
  fmt$knitr$knit_hooks$plot = insert_fig_footer
  fmt
}


#' @export
#' @rdname rmdmux
html_rmdmux_plain <- function(
    ...,
    template = pkg_resource('html', 'rmdmux_plain.html'),
    css = NULL,
    toc = TRUE,
    md_extensions = "-autolink_bare_uris",
    number_sections = TRUE,
    fig_caption = TRUE
) {
  fmt <- pagedown_html_format(
    ...,
    template = template,
    theme = NULL,
    css = c(
      resolve_default_css(css)
    ),
    md_extensions = md_extensions,
    toc = toc,
    number_sections = number_sections,
    fig_caption = fig_caption,
    .pandoc_args = c(
      pagedown_lua_filters('uri-to-fn.lua', 'loft.lua', 'footnotes.lua'),
      styledTables::lua_remove_filter(),
      pagedown_pandoc_chapter_name_args()
    )
  )
  fmt$knitr$opts_knit[['rmdmux_output_format']] <- "html_rmdmux_plain"
  fmt$knitr$knit_hooks$plot = insert_fig_footer
  fmt
}

#' Get the actual `output format` when knitting the document
#' 
#' The following function can be executed inside of an Rmd chunk in order to
#' specify the output format which is currently rendered:
#' - `rmdmux_get_output_format()`: Returns a detailed string holding either the
#'   value `"html_rmdmux_paged"` or `"html_rmdmux_plain"` (or `NULL` if neither
#'   applies).
#' - `rmdmux_is_paged()`: Returns a logical flag, defining if the current output
#'   format is `html_rmdmux_paged`
#' - `rmdmux_is_paged()`: Returns a logical flag, defining if the current output
#'   format is `html_rmdmux_plain`
#' @export
#' @rdname rmdmux_output_format
rmdmux_get_output_format <-function() {
  knitr::opts_knit$get("rmdmux_output_format")
}

#' @export
#' @rdname rmdmux_output_format
rmdmux_is_paged <- function() {
  isTRUE(rmdmux_get_output_format() == "html_rmdmux_paged")
}

#' @export
#' @rdname rmdmux_output_format
rmdmux_is_plain <- function() {
  isTRUE(rmdmux_get_output_format() == "html_rmdmux_plain")
}

#' Allow footer text for figures to be set with `fig.footer` chunk option.
insert_fig_footer <- function(x, options) {  
  x <- knitr::hook_plot_md(x, options)
  if(!is.null(options$fig.footer)) {  
    insert_text <- sprintf('<p class="fig-footer">%s</p>\n</div>', options$fig.footer)
    x <- gsub("</div>", insert_text, x, fixed = TRUE)
  }
  x
}

