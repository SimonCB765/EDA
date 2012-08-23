perturb <- function(BN, degree=BN$size) {
	# Perturb the network BN by making a number of modifications equal to the number supplied in degree.
	# A modification is either:
	# the addition of an edge
	# the removal of an edge
	# the reversal of an edge
	# If the network is empty or complete then removal/reversal or addition respectively will not be possible.
	# In this case the operation chosen may fail to perform. For example, if the network is empty and edge removal is chosen,
	# then the removal doesn't occur. In this case, the number of perturbations performed will be < degree.
	# Additionally, if there is already an edge from node i to node j, and addition of an edge from node i to node j is
	# selected as the perturbation to perform, then no perturbation will occur.
	
	newBN <- BN
	
	for (i in 1:degree) {
		choice <- sample(c(1, 2, 3), size=1)
		switch(choice, newBN <- add_random_edge(newBN), newBN <- remove_random_edge(newBN), newBN <- reverse_random_edge(newBN))
	}
	
	newBN
}

add_random_edge <- function(BN) {
	# Add an edge from node i to node j.
	orderToTry <- sample(BN$size)
	for (i in orderToTry) {
		for (j in orderToTry) {
			if (i == j) next
			if (i %in% BN$nodes[[j]]$parents) next
			perturbedBN <- add_edge(BN, i, j)
			cyclePresent <- cycle_check(perturbedBN)
			if (!cyclePresent) return(perturbedBN)
		}
	}
	# If this point is reached, then there were no possible changes that could be made.
	return(BN)
}

remove_random_edge <- function(BN) {
	# Remove an edge from node i to node j.
	orderToTry <- sample(BN$size)
	for (i in orderToTry) {
		for (j in orderToTry) {
			if (i == j) next
			if (!(i %in% BN$nodes[[j]]$parents)) next
			perturbedBN <- remove_edge(BN, i, j)
			cyclePresent <- cycle_check(perturbedBN)
			if (!cyclePresent) return(perturbedBN)
		}
	}
	# If this point is reached, then there were no possible changes that could be made.
	return(BN)
}

reverse_random_edge <- function(BN) {
	# Reverse an edge from node i to node j.
	orderToTry <- sample(BN$size)
	for (i in orderToTry) {
		for (j in orderToTry) {
			if (i == j) next
			if (!(i %in% BN$nodes[[j]]$parents)) next
			perturbedBN <- reverse_edge(BN, i, j)
			cyclePresent <- cycle_check(perturbedBN)
			if (!cyclePresent) return(perturbedBN)
		}
	}
	# If this point is reached, then there were no possible changes that could be made.
	return(BN)
}