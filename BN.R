BN <- function(trainingData=NULL, savedStructure=NULL) {
	# trainingData should be a dataframe with each variable a factor
	# savedStrucutre should be the location of a BN written out to disk
	if (is.null(savedStructure)) {
		# Generate a new empty network.
		numNodes <- ncol(trainingData)
		bayesNet <- list()
		bayesNet$nodes <- list()
		bayesNet$size <- numNodes
		bayesNet$score <- 0

		for (i in 1:numNodes) {
			levelNames <- levels(trainingData[,i])
			numberLevels <- length(levelNames)
			bayesNet$nodes[[i]] <- list('id'=i, 'name'=colnames(trainingData)[i], 'levels'=numberLevels, 'levelNames'=levelNames, 'parents'=integer(0), 'children'=integer(0), 'CPT'=data.frame(zero=1/numberLevels, one=1/numberLevels, row.names=0))
		}

		names(bayesNet$nodes) <- colnames(trainingData)
		class(bayesNet) <- 'BayesianNetwork'
	} else {
		# Load in the saved structure information. Initialises the network to a previously saved network.
	}
	
	bayesNet

}

print.BN <- function(network) {
	cat('The network consists of', network$size, 'nodes.', sep=' ')
	cat('\n')
	cat('NodeID\tNodeName\n')
	for (i in 1:network$size) {
		cat(i, network$nodes[[i]]$name, sep='\t')
		cat('\n')
	}
}