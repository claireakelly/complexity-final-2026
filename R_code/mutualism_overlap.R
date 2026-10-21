library(Grind)
set.seed(878) #determined with sample(1:1000, 1)

dependence <- function(t, state, parms) { #same function as for mutualism model, but without the mutualism term in the equation.
  with(as.list(c(state, parms)), {
    Th <- state[1:nr_var]
    dTh <- r * Th * (1 - Th/k)
    return(list(c(dTh)))
  })
}


#code from book; here used for creating dependency matrix D
nr_var <- 12
nr_of_pp <- 500
factors <- 3
D <- matrix(0, nr_var, nr_var)
#low <- (11/300); high <- (22/300) #reproduce hw assignment 5 question 8
low <- (3/80); high <- (6/80) #new; almost same as above but simplified parameters for presentation.

cat <- cut(1:nr_var, factors)
for(i in 1:nr_var) {
  for (j in 1:nr_var) {
    if (cat[i] == cat[j])
      D[i, j] <- high else D[i, j] <- low
  }
}

D[diag(nr_var) == 1] <- (1 - (3*high + 8*low))
#end of specification D


data <- matrix(0, nr_of_pp, nr_var)

for(i in 1:nr_of_pp){
  r <- D %*% rnorm(nr_var, .2, .05) #overlap in growth
  k <- D %*% rnorm(nr_var, 10, 2) #overlap in limit; seems VERY important
  th0 <- D %*% rnorm(nr_var, 2, 0.1) #overlap in initial state
  s  <- th0; p <- c()
  data[i,] <- run(odes = dependence, tmax = 60, 
                  timeplot = (i==1), legend = FALSE) 
}
#hist(cor(data)[cor(data) < 1], main = 'positive manifold', xlab = 'between test correlations', col = 'grey50') #disabled to not have plot window filled up with a histogram, can be re-enabled safely.

select <- matrix(FALSE, 12, 12)
select[1:4, 1:4] <- T; select[5:8, 5:8] <- T; select[9:12, 9:12] <- T
mean(c(cor(data)[select])) #correlations within clusters only
mean(c(cor(data)[!select])) #correlations between clusters only
mean(cor(data)[cor(data) < 1]) #correlations across the board with diagonal excluded (others dont hit 1 unless pars adjusted)


write.table(file='group project - dependence model grind simulated.txt', data, row.names = FALSE, sep='\t')