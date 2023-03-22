library(MCDA)

scores <- matrix( c(-0.2,-2.3,-2.4,-1,3,9,10,7), 
                  nrow = 4, dimnames = list(c("School-A","School-B","School-C", "School-D"),
                                            c("Location","Quality")))
q <- c( 0.2, 1)
p <- c(   1, 2)
v <- c( 3.5, 4)
w <- c(0.25, 0.75)

res <- ELECTRE3(scores, q, p, v, w)
print(res)
