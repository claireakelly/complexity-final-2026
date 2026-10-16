# Simulate basic Thomson Bonds
simulate_bonds <- function(N, X, P, K = 1){
  # Initialize data structures
  # Saved x_i scores
  X_num <- X
  X <- matrix(data = NA, nrow = X, ncol = 1)

  # Vector of contributions, e_i
  ## Drawn from std. norm. distribution
  E <- matrix(rnorm(N, mean = 0, sd = 1), ncol = 1)

  # Probability of bond selection

  #####CLUSTERING

  ## Hard code for 3 approx. balanced clusters and N = 1000 bonds
  ##### CAK: I have code from my thesis that I can dredge up and modify to allow this to not be hard coded, but I don't know if thats really necessarily rn
  bond_cluster <- c(
    rep(1, 333),
    rep(2, 333),
    rep(3, 334)
  )
  
  ## Each item to its associated "higher prob" cluster
  item_group <- rep(1:3, each = 4) ## here hard coded for 12 items

  high_p <- .7
  low_p <- .3

  # Initialize and populate probability and indicator matrix
  cluster_prob <- matrix(NA, nrow = X_num, ncol = N)
  A <- matrix(NA, nrow = X_num, ncol = N)
  
  for (i in 1:X_num){
    for(j in 1:N){
      if (item_group[i] == bond_cluster[j]) {
        cluster_prob[i,j] <- high_p
        p <- high_p
      } else {
        cluster_prob[i,j] <- low_p
        p <- low_p
      }
      A[i,j] <- rbinom(1, size = 1, prob = p)
    }
  }


  # Number of passes to be completed
  K = 1 # for standard Thomson model
  
  # Calculate item scores-- not in the right place here
  scores <- A %*% E
  colnames(scores) <- "scores"
  
  # Create matrix of actively contributed effects
  bonds <- matrix(NA, nrow = X_num, ncol = N)
  for (i in 1:nrow(A)){
    for(j in 1:ncol(A)){
      if (A[i,j] == 1) {
        bonds[i,j] <- E[i]
      } else {
        bonds[i,j] <- 0
        
      }
    }
  }
  colnames(bonds) <- colnames(bonds, prefix = "bond_", do.NULL = FALSE)
  bonds <- cbind(bonds, scores)


  return(bonds)
}


# Function checks

## N of bonds
N <- 1000
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

# simulate bonds and save item scores
for (i in 1:size){
  iter <- simulate_bonds(N, X, P)
  data[i,] <- iter[,N+1]
}

# fix column names
colnames(data) <- c(colnames(data, prefix = "item_score_", do.NULL = FALSE))


cor_data <- cor(data)

# write txt for Jasp
write.table(file='bonds_clustered.txt', data, row.names = FALSE, sep='\t')
