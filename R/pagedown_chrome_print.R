#############################################################################
######    Code taken from the R-package `pagedown` and slightly modified   #######
######    https://github.com/rstudio/pagedown/blob/main/R/chrome.R         #######
######    Original author: Yihui Xie et al.                                #######
#############################################################################

#' Print a web page to PDF or capture a screenshot using the headless Chrome
#'
#' This function was taken from the [pagedown package][pagedown::chrome_print()]
#' (see [pagedown::chrome_print()] for more details).
#' The only modification was, that now it is possible to pass the argument
#' `output_format`.
#'  
#' Print an HTML page to PDF or capture a PNG/JPEG screenshot through the Chrome
#' DevTools Protocol. Google Chrome or Microsoft Edge (or Chromium on Linux)
#' must be installed prior to using this function.
#' @param output_format The R Markdown output format to convert to
#'   (see also [rmarkdown::render()]). Usually it is one of the following:
#'   - `"all"`: This will render all formats defined within the **yaml header** of the file.
#'   - `NULL`: This will render the first format defined within the **yaml header** of the file.
#'   - `"rmdmux::html_rmdmux_paged"`: This will use the `html_rmdmux_paged` output format for rendering.
#'   - `"rmdmux::html_rmdmux_plain"`: This will use the `html_rmdmux_paged` output format for rendering.
#' @param input A URL or local file path to an HTML page, or a path to a local
#'   file that can be rendered to HTML via [rmarkdown::render()]
#'   (e.g., an R Markdown document or an R script). If the \code{input} is to be
#'   rendered via \code{rmarkdown::render()} and you need to pass any arguments
#'   to it, you can pass the whole \code{render()} call to
#'   \code{chrome_print()}, e.g., if you need to use the \code{params} argument:
#'   \code{pagedown::chrome_print(rmarkdown::render('input.Rmd', params =
#'   list(foo = 1:10)))}. This is because \code{render()} returns the HTML file,
#'   which can be passed to \code{chrome_print()}.
#' @param output The output filename. For a local web page \file{foo/bar.html},
#'   the default PDF output is \file{foo/bar.pdf}; for a remote URL
#'   \file{https://www.example.org/foo/bar.html}, the default output will be
#'   \file{bar.pdf} under the current working directory. The same rules apply
#'   for screenshots.
#' @param wait The number of seconds to wait for the page to load before
#'   printing (in certain cases, the page may not be immediately ready for
#'   printing, especially there are JavaScript applications on the page, so you
#'   may need to wait for a longer time).
#' @param browser Path to Google Chrome, Microsoft Edge or Chromium. This
#'   function will try to find it automatically via [find_chrome()]
#'   if the path is not explicitly provided and the environment variable
#'   \code{PAGEDOWN_CHROME} is not set.
#' @param format The output format.
#' @param options A list of page options. See
#'   \code{https://chromedevtools.github.io/devtools-protocol/tot/Page#method-printToPDF}
#'    for the full list of options for PDF output, and
#'   \code{https://chromedevtools.github.io/devtools-protocol/tot/Page#method-captureScreenshot}
#'    for options for screenshots. Note that for PDF output, we have changed the
#'   defaults of \code{printBackground} (\code{TRUE}),
#'   \code{preferCSSPageSize} (\code{TRUE}) and when available
#'   \code{transferMode} (\code{ReturnAsStream}) in this function.
#' @param selector A CSS selector used when capturing a screenshot.
#' @param box_model The CSS box model used when capturing a screenshot.
#' @param scale The scale factor used for screenshot.
#' @param work_dir Name of headless Chrome working directory. If the default
#'   temporary directory doesn't work, you may try to use a subdirectory of your
#'   home directory.
#' @param timeout The number of seconds before canceling the document
#'   generation. Use a larger value if the document takes longer to build.
#' @param extra_args Extra command-line arguments to be passed to Chrome.
#' @param verbose Level of verbosity: \code{0} means no messages; \code{1} means
#'   to print out some auxiliary messages (e.g., parameters for capturing
#'   screenshots); \code{2} (or \code{TRUE}) means all messages, including those
#'   from the Chrome processes and WebSocket connections.
#' @param async Execute \code{chrome_print()} asynchronously? If \code{TRUE},
#'   \code{chrome_print()} returns a [promises::promise()] value (the
#'   \pkg{promises} package has to be installed in this case).
#' @param outline If not \code{FALSE}, \code{chrome_print()} will add the
#'   bookmarks to the generated \code{pdf} file, based on the table of contents
#'   informations. This feature is only available for output formats based on
#'   [pagedown::html_paged()]. It is enabled by default, as long as the
#'   Ghostscript executable can be detected by [tools::find_gs_cmd()].
#' @param encoding Not used. This argument is required by RStudio IDE.
#' @references
#' \url{https://developers.google.com/web/updates/2017/04/headless-chrome}
#' @return Path of the output file (invisibly). If \code{async} is \code{TRUE},
#'   this is a [promises::promise()] value.
#' @export
chrome_print = function(
    input,
    output = xfun::with_ext(input, format),
    output_format = NULL,
    wait = 2,
    browser = 'google-chrome',
    format = c('pdf', 'png', 'jpeg'),
    options = list(),
    selector = 'body',
    box_model = c('border', 'content', 'margin', 'padding'),
    scale = 1, 
    work_dir = tempfile(),
    timeout = 30, 
    extra_args = c('--disable-gpu'),
    verbose = 0, 
    async = FALSE,
    outline = gs_available(),
    encoding
) {
  is_rstudio_knit =
    !interactive() && !is.na(Sys.getenv('RSTUDIO', NA)) &&
    !missing(encoding) && length(match.call()) == 3
  if (is_rstudio_knit) verbose = 1
  
  format = match.arg(format)
  
  if (missing(browser) && is.na(browser <- Sys.getenv('PAGEDOWN_CHROME', NA))) {
    browser = find_chrome()
  } else {
    if (!file.exists(browser)) browser = Sys.which(browser)
  }
  if (!utils::file_test('-x', browser)) stop('The browser is not executable: ', browser)
  if (isTRUE(verbose)) verbose = 2
  if (verbose >= 1) message('Using the browser "', browser, '"')
  
  # check that work_dir does not exist because it will be deleted at the end
  if (dir.exists(work_dir)) stop('The directory ', work_dir, ' already exists.')
  work_dir = normalizePath(work_dir, mustWork = FALSE)
  
  # for windows, use the --no-sandbox option
  extra_args = unique(c(
    extra_args, proxy_args(), if (xfun::is_windows()) '--no-sandbox',
    '--headless', '--no-first-run', '--no-default-browser-check', '--hide-scrollbars'
  ))
  
  debug_port = servr::random_port(NULL)
  log_file = if (getOption('pagedown.chrome.log', FALSE)) {
    sprintf('chrome-stderr-%s.log', format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
  }
  ps = processx::process$new(browser, c(
    paste0('--remote-debugging-port=', debug_port),
    paste0('--user-data-dir=', work_dir), extra_args
  ), stderr = log_file)
  kill_chrome = function(...) {
    if (verbose >= 1) message('Closing browser')
    if (ps$is_alive()) ps$kill()
    if (verbose >= 1) message('Cleaning browser working directory')
    unlink(work_dir, recursive = TRUE)
  }
  on.exit(kill_chrome(), add = TRUE)
  
  remote_protocol_ok = is_remote_protocol_ok(debug_port, verbose = verbose)
  stream_pdf_available = isTRUE(xfun::attr(remote_protocol_ok, 'stream_pdf_available'))
  
  if (!remote_protocol_ok)
    stop('A more recent version of Chrome is required. ')
  
  if (format == 'pdf' && stream_pdf_available)
    options = merge_list(list(transferMode = 'ReturnAsStream'), as.list(options))
  
  # If !async, use a private event loop to drive the websocket. This is
  # necessary to separate later callbacks relevant to chrome_print, from any
  # other callbacks that have been scheduled before entering chrome_print; the
  # latter must not be invoked while inside of any synchronous function,
  # including chrome_print(async=FALSE).
  #
  # It's also critical that none of the code inside with_temp_loop waits on a
  # promise that originates from outside the with_temp_loop, as it will cause
  # the code inside to hang. And finally, no promise from inside with_temp_loop
  # should escape to the outside either, as with_temp_loop uses a truly "temp"
  # loop--it will be destroyed when with_temp_loop completes.
  #
  # Therefore, if async, it's important NOT to use a private event loop.
  with_temp_loop_maybe <- if (async) identity else later::with_temp_loop
  
  with_temp_loop_maybe({
    
    ws = websocket::WebSocket$new(get_entrypoint(debug_port, verbose), autoConnect = FALSE)
    ws$onClose(kill_chrome)
    ws$onError(kill_chrome)
    close_ws = function() {
      if (verbose >= 1) message('Closing websocket connection')
      ws$close()
    }
    
    svr = NULL # init svr variable
    if (file.exists(input)) {
      is_html = function(x) grepl('[.]html?$', x)
      with_msg_maybe = if (is_rstudio_knit) suppressMessages else identity
      url = if (is_html(input)) input else with_msg_maybe(rmarkdown::render(
        input, output_format = output_format, envir = parent.frame(), encoding = 'UTF-8'
      ))
      if (!is_html(url)) stop(
        "The file '", url, "' should have the '.html' or '.htm' extension."
      )
      svr = servr::httd(
        dirname(url), daemon = TRUE, browser = FALSE, verbose = verbose >= 1,
        port = servr::random_port(NULL), initpath = httpuv::encodeURIComponent(basename(url))
      )
      stop_server = function(...) {
        if (verbose >= 1) message('Closing local webserver')
        svr$stop_server()
      }
      on.exit(stop_server(), add = TRUE)
      ws$onClose(stop_server)
      ws$onError(stop_server)
      url = svr$url
    } else url = input  # the input is not a local file; assume it is just a URL
    
    # remove hash/query parameters in url
    if (missing(output) && !file.exists(input))
      output = xfun::with_ext(basename(gsub('[#?].*', '', url)), format)
    output2 = normalizePath(output, mustWork = FALSE)
    if (!dir.exists(d <- dirname(output2)) && !dir.create(d, recursive = TRUE)) stop(
      'Cannot create the directory for the output file: ', d
    )
    
    if ((format == 'pdf') && !all(c(missing(selector), missing(box_model), missing(scale))))
      warning('For "pdf" format, arguments `selector`, `box_model` and `scale` are ignored.', call. = FALSE)
    
    box_model = match.arg(box_model)
    
    pr = NULL
    res_fun = function(value) {} # default: do nothing
    rej_fun = function(reason) {} # default: do nothing
    if (async) {
      pr_print = promises::promise(function(resolve, reject) {
        res_fun <<- resolve
        rej_fun <<- function(reason) reject(paste('Failed to generate output. Reason:', reason))
      })
      pr_timeout = promises::promise(function(resolve, reject) {
        later::later(
          ~reject(paste('Failed to generate output in', timeout, 'seconds (timeout).')),
          timeout
        )
      })
      pr = promises::promise_race(pr_print, pr_timeout)
      promises::finally(pr, close_ws)
    }
    
    t0 = Sys.time(); token = new.env(parent = emptyenv())
    on.exit({
      close_ws()
      kill_chrome()
      if (!is.null(svr)) stop_server()
    })
    print_page(ws, url, output2, wait, verbose, token, format, options, selector, box_model, scale, outline, res_fun, rej_fun)
    
    if (async) {
      on.exit()
      return(pr)
    }
    
    while (!isTRUE(token$done)) {
      if (!is.null(e <- token$error)) stop('Failed to generate output. Reason: ', e)
      if (as.numeric(difftime(Sys.time(), t0, units = 'secs')) > timeout) stop(
        'Failed to generate output in ', timeout, ' seconds (timeout).'
      )
      later::run_now(); if (!is.null(svr)) run_servr()
    }
    
    if (is_rstudio_knit) message('\nOutput created: ', basename(output))
    
    invisible(output)
  })
}

gs_available <- utils::getFromNamespace("gs_available", "pagedown")
proxy_args <- utils::getFromNamespace("proxy_args", "pagedown")
find_chrome <- utils::getFromNamespace("find_chrome", "pagedown")
print_page <- utils::getFromNamespace("print_page", "pagedown")
get_entrypoint <- utils::getFromNamespace("get_entrypoint", "pagedown")
is_remote_protocol_ok <- utils::getFromNamespace("is_remote_protocol_ok", "pagedown")
merge_list <- utils::getFromNamespace("merge_list", "pagedown")
run_servr <- utils::getFromNamespace("run_servr", "pagedown")
