
k <- runif(n = 15, min=30,max = 1200)
round(k, 0)

causes <- c("faux contact", "défaillance du moteur",
            "carte électronique défectueuse", "autre panne mécanique", "autre panne électrique")


s <- sample(causes, 20,replace = TRUE)
cat(paste(s,  collapse = ", "))


p <- rpois(25,0.5)+1
p


pnorm(1.5)
