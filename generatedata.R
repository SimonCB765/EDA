generate_data <- function(BN, dataPointsToGenerate=10) {
	ordering <- topological_oreder(BN)
	numNodes <- BN$size
	outputData <- matrix(0, dataPointsToGenerate, numNodes)
	for (i in 1:dataPointsToGenerate) {
		datapoint <- vector(mode='list', length=numNodes)
		rand <- runif(numNodes)  # Generate a random number between 0 and 1 for each node in the network.
		for (j in ordering) {
			randJ <- rand[j]
			# Get parents.
			parents <- BN$nodes[[j]]$parents
			if (length(parents) == 0) {
				# The node has no parents.
				if (randJ < BN$nodes[[j]]$CPT$zero) {
					datapoint[[j]] <- 0
				} else {
					datapoint[[j]] <- 1
				}
			} else {
				# The node has parents. Determine the parent configuration.
				parentValue <- 1  # Start with 1 rather than 0 in order to index the CPT properly (no 0 indices).
				index <- 0
				for (k in parents) {
					parentValue <- parentValue + ((2 ** index) * datapoint[[k]])
					index <- index + 1
				}
				if (randJ < BN$nodes[[j]]$CPT$zero[parentValue]) {
					datapoint[[j]] <- 0
				} else {
					datapoint[[j]] <- 1
				}
			}
		}
		outputData[i,] <- unlist(datapoint)
	}
	outputData <- data.frame(outputData)
	names(outputData) <- names(BN$nodes)
	outputData
}

topological_oreder <- function(BN) {
	# http://en.wikipedia.org/wiki/Topological_sorting#Algorithms

	L <- c()
	getChildren <- function(elem, net) net$nodes[[elem]]$children
	childNodes <- unique(c(Map(getChildren, 1:BN$size, MoreArgs=list(net=BN)), recursive=TRUE))
	S <- setdiff(1:BN$size, childNodes)
	
	while (length(S) > 0) {
		size <- length(S)
		n <- S[size]
		S <- S[-size]
		L <- c(L, n)
		children <- BN$nodes[[n]]$children
		BN$nodes[[n]]$children <- integer(0)
		for (i in children) {
			parents <- BN$nodes[[i]]$parents
			BN$nodes[[i]]$parents <- parents[which(parents != n)]  # Remove n from i's parents.
			if (length(BN$nodes[[i]]$parents) == 0) {
				# If i has no more incoming edges, then add it to S.
				S <- c(S, i)
			}
		}
	}
	
	# Return L.
	L
}