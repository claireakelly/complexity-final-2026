# Simulate basic Thomson Bonds
simulate_bonds <- function(N, X, P, K = 1){
  # Initialize data structures
  # Saved x_i scores
  X_num <- X
  X <- matrix(data = NA, nrow = X, ncol = 1)

  # Vector of contributions, e_i
  ## Drawn from std. norm. distribution
  E <- matrix(rnorm(N, mean = 0, sd = 1), ncol = 1)

  # Probability of selection

  ## Define function to select active bonds
  prob <- function(x) {
    # Value of 1 indicates active bond
    rbinom(n = 1, size = 1, prob = P)
  }

  #####CLUSTERING

  ## 1/3 of bonds to clusters
  bond_cluster <- c(
    rep(1, 33),
    rep(2, 33),
    rep(3, 34)
  )
  ##each item to its associated "higher prob" cluster
  item_group <- rep(1:3, each = 4) ##here hard coded for 12 items

  high_p <- .7
  low_p <- .3

  #########

  ## Create indicator matrix
  A <- matrix(NA, nrow = X_num, ncol = N)
  ##A <- apply(A, c(1,2), prob)


  # Number of passes to be completed
  K = 1 # for standard Thomson model

  # Calculate item scores-- not in the right place here
  scores <- A %*% E
  colnames(scores) <- "scores"


  bonds <- matrix(NA, nrow = X_num, ncol = N)
  for (i in 1:X_num){
    for(j in 1:N){
      if (item_group[i] == bond_cluster[j]) {
        p <- high_p
      } else {
        p <- low_p
      }
      A[i,j] <- rbinom(1, size = 1, prob = p)
    }
  }

  colnames(bonds) <- colnames(bonds, prefix = "bond_", do.NULL = FALSE)
  bonds <- cbind(bonds, scores)


  return(bonds)
}


# Function checks

## N of bonds
N <- 100
## N of items
X <- 12
## P of bond being selected
P <- .3
## Number of passes
K = 1 # For standard Thomson model
## how many individuals should be simulated
size <- 500
##cluster size
c <- 4

set.seed(1234)
# initialize matrix
data <- matrix(data = NA, nrow = size, ncol = X)
cd
# simulate bonds and save item scores
for (i in 1:size){
  iter <- simulate_bonds(N, X, P)
  data[i,] <- iter[,N+1]
}

# fix column names
colnames(data) <- c(colnames(data, prefix = "item_score_", do.NULL = FALSE))


# write txt for Jasp
write.table(file='bonds.txt', data, row.names = FALSE, sep='\t')
