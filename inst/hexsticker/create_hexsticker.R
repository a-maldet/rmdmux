devtools::load_all()
# create the poodle logo of the styledTables package
# install.packages("hexSticker")
hexSticker::sticker(
  system.file("hexsticker", "rmdmux_logo.png", package = "rmdmux"),
  package="rmdmux", 
  p_size=16,
  p_y = 0.6,
  s_x=1,
  s_y=1.2,
  h_fill = "#ffffff",
  p_color = "#000000",
  h_color = "#4bbeb0",
  s_width=0.7,
  s_height=0.7,
  filename=file.path(system.file("hexsticker", package = "rmdmux"), "rmdmux_hexsticker_raw.png")
)
