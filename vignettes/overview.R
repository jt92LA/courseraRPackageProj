## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(courseraRPackageProj)

## ---- fig.show='hold'----------------------------------------------------
filename <- make_filename(2013)
file_dir <- paste("../inst/extdata",filename, sep = "/")
fars_read(file_dir)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

