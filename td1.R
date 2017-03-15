#' ---
#' title: "TD 1 stats"
#' author: "Bruno Fischer Colonimos"
#' output:
#'         pdf_document:
#'         number_sections: yes
#' toc: yes
#' html_notebook:
#'         number_sections: yes
#' theme: readable
#' toc: yes
#' ---

# data

dir("data")

df <- read.csv2(file.path("data", "1_1.csv"))
