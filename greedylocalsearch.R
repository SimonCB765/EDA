greedy_local_search <- function(BN, dataset) {
	# Generate all networks that have either:
	# 1 edge added
	# 1 edge removed
	# 1 edge reversed
	
	# Record the best network.
	best <- BN
	
	nodes <- BN$size
	
	for (i in nodes) {
		for (j in nodes) {
			if (i == j) next  # If i and j are the same node, then there can be no edges added, removed or reversed.
			
			if (j %in% BN$nodes[[i]]$children) {
				# If there is already an edge going from i to j.
				# Remove an edge between i and j.
				removedBN <- remove_edge(BN, i, j)
				removedBN <- DPSM(removedBN, dataset)
				if (best$score < removedBN$score) best <- removedBN
				
				# Reverse an edge between i and j, and check if a cycle is induced.
				reversedBN <- reverse_edge(BN, i, j)
				cyclePresent <- cycle_check(reversedBN)
				if (!cyclePresent) {
					reversedBN <- DPSM(reversedBN, dataset)
					if (best$score < reversedBN$score) best <- reversedBN
				}
			} else {
				# If there is no edge going from i to j, and check if a cycle is induced.
				# Add an edge between i and j.
				addedBN <- add_edge(BN, i, j)
				cyclePresent <- cycle_check(addedBN)
				if (!cyclePresent) {
					addedBN <- DPSM(addedBN, dataset)
					if (best$score < addedBN$score) best <- addedBN
				}
			}
		}
	}
	
	best
}