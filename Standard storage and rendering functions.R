#'---
#'title: "Standard storage and rendering functions"
#'subtitle: "Improving and standardizing the result storage"
#'author: "Bruno Fischer Colonimos"
#'date: "12/05/2017"
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


#'
#' Storage of results
#' ==================
#'

#' Storage names
#' -------------

# make a name with a list of (variable) names
make.a.name <- function(varvect, varsep = sfdefault("varsep")) {
        paste(varvect, collapse = varsep)
}

# Reverse: split a name in elements names
splitname <- function(cname, splistring = sfdefault("varsep")) {
        strsplit(cname, splistring)
}

# testing
vlist <- c("var", "faco", "cafvac") # list of variable names
mn <- make.a.name(c("fname", vlist)) # make a name with c( function name + list of variables )
strsplit(mn,sfdefault("varsep")) # split the name in elements names

# test splitname
splitname(mn)



# Make a "safe" name with a name : verify if it is already
# used in storage . if it is, add an integer at the end, else return the name
make.safe.name <- function(rootname) {
        verifyname <- function(name) {
                !(name %in% allresnames())
        }
        make.safe.name0 <- function(rootname, name, int) {
                if(verifyname(name)) {
                        name
                } else {
                        make.safe.name0(rootname, paste0(rootname,int) , int + 1)
                }
        }

        if (verifyname(rootname)) {
                rootname
        } else {
                make.safe.name0(rootname, rootname, 1)
        }
}


# testing
make.safe.name("hehe")
make.safe.name("hi")





# Rendering results
# =================

# Text output parameters:
# -----------------------
# set a "textfilrs" sub directory that will contain all text patterns
# name of each text pattern files = namefun_txt.Rmd
#
#
#  At the beginning: read all textfiles and store the corresponding lines under the function name
# dir("textfiles")
# function
read.txt.patterns <- function(fnamevect) {
        textpatterns <- lapply(fnamevect,
                                FUN = function(fname, dname = "textfiles") {
                                        # make file name
                                        filename <- paste0(fname, "_text.Rmd")
                                        filepath <- file.path(dname, filename)
                                        fex <- file.exists(filepath)
                                        # message(fname)
                                        # message(filepath)
                                        # message(fex)
                                        # fex
                                        # reading lines from text file
                                        lns <- readLines(filepath)
                                        list(funname = fname,
                                             lns = lns)
                                }
                                )
        # put names
        names(textpatterns) <- sapply(textpatterns, function(elt){elt$funname})
        textpatterns
}

# initialization
textpatterns <- read.txt.patterns(c("cat1", "verbatim", "default"))

# essais
# textpatterns["cat1"]
# textpatterns[["default"]]$lns


# accessor
get.text.pattern <- function(funname, textpatterns) {
        p <- textpatterns[[funname]]
        p$lns
}

# essai
# get.text.pattern("cat1", textpatterns)

# get and customize ==> Improve please

get.text.lines <- function(funname, varnames) {
        lns <- get.text.pattern(funname, textpatterns) # problème textpatterns
        # replace variable text by custom text
        lns <- gsub(pattern = "<function>", replacement = funname , x = lns)
        lns <- gsub(pattern = "<name>", replacement = varnames[1] , x = lns) #compatibility ?
        lns <- gsub(pattern = "<variable>", replacement = varnames[1] , x = lns)
        lns <- gsub(pattern = "<variable1>", replacement = varnames[1] , x = lns)
        lns <- gsub(pattern = "<variable2>", replacement = varnames[2] , x = lns)
        lns <- gsub(pattern = "<variable3>", replacement = varnames[3] , x = lns)
        lns <- gsub(pattern = "<variable1rest>",
                    replacement = paste0(varnames, collapse = ", " ), x = lns)
        lns <- gsub(pattern = "<variable2rest>",
                    replacement = paste0(varnames[-1], collapse = ", " ), x = lns)
        lns <- gsub(pattern = "<variable3rest>",
                    replacement = paste0(varnames[c(-1, -2)], collapse = ", " ), x = lns)
        # return
        lns
}


# example
get.text.lines("cat1", c('zozo', 'zizi'))
get.text.lines("verbatim", c('zaza', 'zazie'))


























# Not now
# =======
# update a recorded result (only change certain fields)
#
# update.record  <-  function(...) {environment()}
#
# ev <- update.record( graph1 = "zoo", hoarg = 1)
# ls(ev)
# ????????????????????????????
# make.result(plot)
#
#
# update.result.name  <-  function(name) {
#        res <- result(name)
#
# }

# update.result( graph1 = "zoo", hoarg = 1)
# nm
# result(nm[3])
