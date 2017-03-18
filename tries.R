
# beware duplicate names
nam <- "name_§_flooz"
nam

r1 <- make.result(name = "aha !")
result("hehe", r1)

result("hihihi !", make.result(name = "hoho", plot1 = c(1,2,3) ) )

result("hihihi !")

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

ls()

