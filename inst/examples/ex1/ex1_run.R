### Create ex1_main.pdf ###
# This code prints the paginated html output to a pdf file
rmdmux::chrome_print(
  input = "ex1_main.Rmd",
  output = "ex1_paged.pdf",
  output_format = "rmdmux::html_rmdmux_paged"
)

### Create ex1_plain.html ###
# This code creates the plain html output file
# This output file is also accessible to people who are visually impaired and
# are using assistive reading technologies like screen readers.
rmarkdown::render(
  input = "ex1_main.Rmd",
  output_file = "ex1_plain.html",
  output_format = "rmdmux::html_rmdmux_plain"
)

### Create ex1_plain.html WITH ADDITIONAL EXCEL###
# This code does the same as above, but additionally:
# - stores all tables in a folder "xlsx_tables"
# - creates an Excel file holding all tables stored in "xlsx_tables".
#   The table counters in the Excel file are the same as in `ex1_plain.html`
library(xlsxcollection)
xlsx_dir <- "xlsx_tables"
rmarkdown::render(
  input = "ex1_main.Rmd",
  output_file = "ex1_plain.html",
  output_format = "rmdmux::html_rmdmux_plain",
  params = list(xlsx_dir = xlsx_dir)
)
# create an Excel file containing all tables of the ListOfTables in `ex1_plain.html`
xlsxcol <- xlsxcollection_list_stored_tables(xlsx_dir)
if (length(xlsxcol) > 0) {
  xlsxcol %>%
    xlsxcollection_read_stored_tables %>%
    xlsxcollection_use_html_table_counter(html_path = "ex1_plain.html") %>%
    xlsxcollection_create_excel(
      xlsx_path = "ex1_tables.xlsx",
      toc_caption = "Tabellen in Berichtvorlage-1",
      toc_format = function(x) {
        x %>%
          set_excel_col_width(30, col_id = 1)
      }
    )
}

###  Start developing mode ###
# This code line does the following things:
#   - starts an http-server in the background
#   - create the paged html file
#   - the http-server now serves the created html file and all included
#     javascript libraries, css files etc.
#   - the html file can be viewed by opening the url printed in the console
#     (usually something like `http://127.0.0.1:4321`) in Google-Chrome
#   - if ex1_main.Rmd or ex1_child1.Rmd or ex1_child2.Rmd are changed, the 
#     html file is automatically updated and the updated file can be viewed in
#     Google-Chrome by simply refreshing the browser after the rendering
#     of the updated Rmd files is finished.
# 
# This workflow is useful, when working on a new report, since the output is
# automatically updated, when an RMD file is changed.
# 
# Be aware:
#   - If you start a 2nd process, a new url is used!
#   - in order work properly the RMD files should be in the working directory
rmdmux::inf_mr("ex1_main.Rmd", output_format = "rmdmux::html_rmdmux_paged")
# Stop all started http-servers
rmdmux::daemon_stop()
