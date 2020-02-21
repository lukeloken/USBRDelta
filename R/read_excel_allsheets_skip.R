

library(readxl)
library(openxlsx)

#Function to read in all sheets from an excel file
read_excel_allsheets_skip <- function(filename, tibble = FALSE, ...) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, ...))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
