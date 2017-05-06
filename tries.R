#'---
#'title: "Tries"
#'subtitle: "Various tries"
#'author: "Bruno Fischer Colonimos"
#'date: "01/05/2017"
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


# beware duplicate names
nam <- "name_§_flooz"
nam


# testing data: store some results.
r1 <- make.result(name = "aha !") # discrepancy internal name of result =/= storage name
result("hehe", r1)

result("hihihi !", make.result(name = "hoho", plot1 = c(1,2,3) ) )

result("hihihi !")

result("hehe1", r1)

allres()
nm <- allresnames()
nm

# beware duplicate names !!!





tryres <- function(var1, var2) {
        var3 <- var1 + var2
        c(var1 = var1, var2 = var2, var3 = var3)
}

try <- tryres( var1 = 2, var2 = 10)

try
try["var1"]


# updating list components
nl <- list(a = 1, b = c( 2, 3), d = "blabla")
nl$b
nl$x <- "new"
nl
nl$a <- NULL
nl

nl$b <- 1:3
nl


# Now seriously
# -------------


# make a name with a list of (variable) names
make.a.name <- function(varvect, varsep = sfdefault("varsep")) {
        paste(varvect, collapse = varsep)
}

make.a.name(c("var", "faco", "cafvac"))



# verifyname("hehei") make a "safe" name with a  names : verify if it is already
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

make.safe.name("hehe")
make.safe.name("hi")


# store result:
# =============

# result creation (already done)
# ---------------
#
# make.result(
# ...
#       funname = "FunX", # name of the function which produced the result
#       varnames = c("var1", "var2", ...), # vector (or better = named vector) of variable names
# ...
# )

# result storage (with coherent internal and external names)
# ---------------

# store.result(res)
#       funname <- res$funname
#       lvar <- res$varnames
#       name0 <- make_a_name( c(lvar[1], funname, lvar[-1]))
#       name1 <- make_safe_name(name0)
#       res$name <- name1
#       result(name1, res)






# Get all results and make a .rmd file for them (ref trygen.R)
# ----------------------------------------------
#
# res.to.text(filepath) {
#      sapply(allresnames(),
#           Fun = function(name) {
#                    res <-  result(name)
#                    funname <- res$funname
#                    txtlines <- gettextlines(funname, res$varnames)
#                    printlines(txtlines, filepath)
#                    TRUE # success flag
#               }
#       )
# }


#  how to get textlines
#
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

# get and customize

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
