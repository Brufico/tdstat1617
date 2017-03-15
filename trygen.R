#'---
#'title: "trying generate R markdown text"
#'subtitle: "Presenting the results of the simple analysis"
#'author: "Bruno Fischer Colonimos"
#'date: "21/2/2017"
#'abstract: |
#'      Blabla.
#'      blablabla.
#'output:
#'  html_document:
#'    number_sections: yes
#'    toc: yes
#'    theme: readable
#'  pdf_document:
#'    toc: yes
#'  word_document: default
#'---

#' parameters

fname <- "cat1_text.rmd"
outputf <- "addfile.rmd"
varname <- "Arrondissement1"

#' reading lines from text file
lns <- readLines(file.path(fname))

# substituting a variable name instead of "<variable>"

nlns <- gsub(pattern = "<variable>", replacement = varname , x = lns)

# writeLines(text = nlns,con = outputf) # no
# cat(nlns, file = "addfile.rmd", sep="\n", append = TRUE) # OK

# or (OK)
# fileconn <- file( "addfile.rmd" ,open="a")
# writeLines(text = nlns,con = fileconn)
# close(fileconn)

# or (NO)
sink( "addfile.rmd" , append = TRUE)
writeLines(text = nlns)
sink()


