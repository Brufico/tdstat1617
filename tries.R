
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


