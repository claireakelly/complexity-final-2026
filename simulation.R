# Simulate basic Thomson Bonds 
simulate_bonds <- function(N, X, expectation, K = 1){
# Initialize data structures

# Saved x_i scores
X <- matrix(data = NA, nrow = X, ncol = 1)

# Vector of contributions, e_i
## Drawn from std. norm. distribution
E <- matrix(rnorm(N, mean = 0, sd = 1), ncol = 1) #### Should be 1 if std. norm, but book writes as sd_i so not sure if this is supposed to vary??

# Probability of selection
#P <- matrix(data = NA, nrow = X_num) ## Currently this number for all bonds in a given item, but p can vary between items
##### Need to do more reading on Expectations in probability theory before we can change this
##### hard code so that P is the number of bonds to be selected for each item, with which bonds sampled being drawn from a binomial random variable from a dist with expectation 3 (i.e., average 3 bonds selected)
expectation <- 3

# Define function to select active bonds
P <- function(x) {
  # x is the current row
  rbinom(n = length(x), size = 1, prob = expectation / N)
}

# Create indicator matrix
A <- t(apply(A, 1, P))


# Number of passes to be completed
K = 1 # for standard Thomson model

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
  #colnames(bonds)[i] <- c(paste("bond", i, sep = "_"))
}

colnames(bonds) <- colnames(bonds, prefix = "bond_", do.NULL = FALSE)
bonds <- cbind(bonds, scores)


return(bonds)
}

# Function checks

## N of bonds
N <- 10
## N of items
X <- 5
## Avg. active bonds per item
expectation <- 3
## Number of passes
K = 1 # For standard Thomson model

model <- simulate_bonds(N, X, expectation)