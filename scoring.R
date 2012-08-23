# CHECK THAT THE MATH IS ALL CORRECT
# DETERMINE WHETHER IT WILL WORK FOR NODES WITH NO PARENTS



DPSM <- function(BN, dataset, alpha=10) {
	# General Dirichlet Prior Score Metric
	# The Dirichlet orders for all sets of parameters are set to the constant alpha.
	# dataset should be a dataframe that contains data generated from/used to generate the network BN
	# In dataset, the variables will take on the values 0 and 1.

	N = BN$size
	
	score <- 1
	for (i in 1:N) {
		ri <- 2  # Binary, so each variable takes on two values.
		parents <- BN$nodes[[i]]$parents
		numParents <- length(parents)
		qi <- 2 ** numParents  # Number of possible configurations of the parents of i. As this is binary each parent can take two values.
		
		product_qi <- 1
		zeroData <- c()  # Parameter estimation data for when k=0.
		oneData <- c()  # Parameter estimation data for when k=1.
		for (j in 0:(qi-1)) {
			parentConfig <- intToBits(j)[1:numParents]
			alpha_ij <- 0
			n_ij <- 0
			
			product_ri <- 1
			for (k in 0:(ri-1)) {				
				tempSubset <- dataset
				if (numParents != 0) {
					for (s in 1:numParents) {
						L = tempSubset[, parents[s]] == as.character(as.integer(parentConfig[s]))
						tempSubset <- tempSubset[L, ]
					}
				}
				L = tempSubset[, i] == as.character(k)
				tempSubset <- tempSubset[L, ]
				alpha_ijk <- alpha
				n_ijk <- nrow(tempSubset)
				product_ri <- product_ri * (gamma(alpha_ijk + n_ijk) / gamma(alpha_ijk))
				
				alpha_ij <- alpha_ij + alpha_ijk
				n_ij <- n_ij + n_ijk
				
				if (k == 0) {
					zeroNumerator <- alpha_ijk + n_ijk
				} else {
					oneNumerator <- alpha_ijk + n_ijk
				}
			}
			
			# Calculate the parameter estimate for ijk.
			zeroData <- c(zeroData, zeroNumerator / (alpha_ij + n_ij))
			oneData <- c(oneData, oneNumerator / (alpha_ij + n_ij))
			
			product_qi <- product_qi * (gamma(alpha_ij) / gamma(alpha_ij + n_ij)) * product_ri
		}
		
		# Generate the CPT for i.
		cpt <- data.frame(zero=zeroData, one=oneData, row.names=0:(qi-1))
		BN$nodes[[i]]$CPT <- cpt
		
		score <- score * product_qi
	}
	
	BN$score <- score
	BN
}

BDeu <- function(BN, dataset, alpha=1) {
	# Uniform Bayesian Dirichlet equivalence score metric (not really uniform prior, just means 'the same value of all hyperparamters for a variable')
}