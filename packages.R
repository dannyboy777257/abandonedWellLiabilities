packages <- c("leaflet", "gt", "plotly", "purrr", "readr", "sf", "tidyverse")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE)
}