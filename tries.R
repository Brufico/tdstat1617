
tryres <- function(var1, var2) {
        var3 <- var1 + var2
        c(var1 = var1, var2 = var2, var3 = var3)
}

try <- tryres( var1 = 2, var2 = 10)

try
try["var1"]

