greedy_with_restart <- function(BN, dataset, restartsToDo, degree=BN$size) {
	# Perform greedy search with random restarts.
	
	# Record the best network found.
	best <- BN
	
	for (i in 1:restartsToDo) {
		# Perform the desired number of restarts.
		startNet <- perturb(BN, degree)  # Perturb the original network.
		startNet <- DPSM(startNet, dataset)  # Calculate the score of the perturbation.
		searchResult <- greedy_local_search(startNet, dataset)  # Perform greedy local search on the perturbation.
		while (startNet$score < searchResult$score) {
			# While the greedy search is producing improvements.
			startNet <- searchResult
			searchResult <- greedy_local_search(startNet, dataset)
		}
		if (best$score < searchResult$score) {
			# If the result of the greedy search on this restart is the best result yet, then record that fact.
			best <- searchResult
		}
	}
	
	# Return the best network.
	best
}