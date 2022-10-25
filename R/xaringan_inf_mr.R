#' Render and serve paginated markdown files
#' 
#' This function was taken from the [xaringan package][xaringan::inf_mr()]
#' (see [xaringan::inf_mr()] for more details).
#'  
#' This function starts a background process, which does the following things:
#' - if an **Rmd** file is changed, automatically rerender the **html** file
#' - serve the **html** file and all included files (images, stylesheets, javascript libraries etc.)
#'   via an **http server**.
#'  
#' The rendered **html** file can be viewed by opening the **url** shown in the
#' console output (e.g. `127.0.0.1:4321/my_dir/my_report.html`) in **Google chrome**.
#'  
#' In order to stop the background process, please call [daemon_stop()].
#'  
#' Be aware that, if the **Rmd** files are not located in the working directory,
#' then you have also pass in `knit_root_dir`
#' (e.g.: `inf_mr("my_dir/my_report.Rmd", knit_root_dir = normalizePath("my_dir"))`).
#' **current working directory**.
#' @inheritParams xaringan::inf_mr
#' @export
inf_mr <- xaringan::inf_mr

#' Stop rendering with [inf_mr()]
#' 
#' This function was taken from the [servr package][servr::daemon_stop()]
#' (see [servr::daemon_stop()] for more details).
#'  
#' This function stops the background process (**http server** and rerendering)
#' started by calling [inf_mr()].
#' @inheritParams servr::daemon_stop
#' @export
daemon_stop <- servr::daemon_stop