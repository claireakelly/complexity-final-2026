# Simulate basic Thomson Bonds 
simulate_bonds <- function(N, X, P, K = 1){
# Initialize data structures
# Saved x_i scores
X_num <- X
X <- matrix(data = NA, nrow = X, ncol = 1)

# Vector of bond contributions, e_i
## Drawn from norm. distribution (fix at sd = 1 for simplicity)
E <- matrix(rnorm(N, mean = 0, sd = 1), ncol = 1)

# Probability of selection

## Define function to select active bonds
prob <- function(x) {
  # Value of 1 indicates active bond
  rbinom(n = 1, size = 1, prob = P)
}

## Create indicator matrix
A <- matrix(NA, nrow = X_num, ncol = N)
A <- apply(A, c(1,2), prob)

# Number of passes to be completed
K = 1 # for standard Thomson model

# Calculate item scores
scores <- A %*% E
colnames(scores) <- "scores"

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

# correlation matrix is how we would expect
## tetrad differences are close to zero (but not zero bc of sampling noise)
## all correlations are positive, ie positive manifold

cor_data <- cor(data)


# write txt for Jasp
write.table(file='bonds.txt', data, row.names = FALSE, sep='\t')