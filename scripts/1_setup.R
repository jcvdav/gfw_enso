#This script installs the packages needed to run all the code in this repository

install.packages(pkgs = c(
  "DBI",
  "dbplyr",
  "devtools",
  "ggrepel",
  "here",
  "lubridate",
  "magrittr",
  "raster",
  "sf",
  "furrr"
  )
)

# Install commarobust from https://github.com/acoppock/commarobust
devtools::install_github("acoppock/commarobust")

# Install startR from https://github.com/jcvdav/startR
devtools::install_github("jcvdav/startR")



