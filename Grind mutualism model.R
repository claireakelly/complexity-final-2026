library(Grind)
set.seed(878) #determined with sample(1:1000, 1)

mutualism <- function(t, state, parms){
  with(as.list(c(state, parms)),{
    X <- state[1:nr_var]
    dX <- a * X * (1 - X/k) + a * (X * M %*% X)/k 
    return(list(dX))
  })
}

#layout(matrix(1:2, 1, 2)) #disabled for easy comparison of the timeplots between models
nr_var <- 12
nr_of_pp <- 500
data <- matrix(0, nr_of_pp, nr_var)

factors <- 3
M <- matrix(0, nr_var, nr_var)
low <- .03; high <- .1 #my assignment 5 question 8 solution parameters. Taking the values from the paper (.2, .8) seems to break the network plot.

cat <- cut(1:nr_var, factors)
for(i in 1:nr_var) {
  for (j in 1:nr_var) {
    if (cat[i] == cat[j])
      M[i, j] <- high else M[i, j] <- low
  }
}
M[diag(nr_var) == 1] <- 0


for(i in 1:nr_of_pp){
  a <- rnorm(nr_var, .2, .05) 
  k <- rnorm(nr_var, 10, 2)
  x0 <- rnorm(nr_var, 2, 0.1)
  s  <- x0; p <- c()
  data[i,] <- run(odes = mutualism , tmax = 60, 
                  timeplot = (i==1), legend = FALSE) 
}
#hist(cor(data)[cor(data) < 1], main = 'positive manifold', xlab = 'between test correlations', col = 'grey50') #disabled for comparing timeplots

select <- matrix(FALSE, 12, 12)
select[1:4, 1:4] <- T; select[5:8, 5:8] <- T; select[9:12, 9:12] <- T
mean(c(cor(data)[select])) #correlations within clusters only
mean(c(cor(data)[!select])) #correlations between clusters only
mean(cor(data)[cor(data) < 1]) #correlations across the board with diagonal excluded (others dont hit 1 unless pars adjusted (I believeee))


write.table(file='group project - mutualism model.txt', data, row.names = FALSE, sep='\t')