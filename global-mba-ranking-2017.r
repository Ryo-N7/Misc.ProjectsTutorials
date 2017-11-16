library(pdftools)
library(tidyverse)


txt <- pdf_text("https://rankings.ft.com/pdf/global-mba-ranking-2017.pdf")
head(txt)
txt <- as.tibble(txt)
