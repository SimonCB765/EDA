add_edge <- function(BN, nodeFrom, nodeTo) {
	# nodeFrom and nodeTo should be numeric IDs for the nodes in the network.
	if (nodeFrom != nodeTo) {
		if (!(nodeFrom %in% BN$nodes[[nodeTo]]$parents)) {
			# If nodeFrom is not already a parent of nodeTo, then the edge can be added.
			# Add nodeFrom to nodeTo's vector of parents.
			BN$nodes[[nodeTo]]$parents <- append(BN$nodes[[nodeTo]]$parents, nodeFrom)
			# Add nodeTo to nodeFrom's vector of children.
			BN$nodes[[nodeFrom]]$children <- append(BN$nodes[[nodeFrom]]$children, nodeTo)
		}
	} else {
		cat('The two nodes to add an edge between must be different. No changes were made.\n')
	}
	BN
}

remove_edge <- function(BN, nodeFrom, nodeTo) {
	# nodeFrom and nodeTo should be numeric IDs for the nodes in the network.
	if (nodeFrom != nodeTo) {
		if (nodeFrom %in% BN$nodes[[nodeTo]]$parents) {
			# If nodeFrom is already a parent of nodeTo, then the edge can be removed.
			parents <- BN$nodes[[nodeTo]]$parents
			children <- BN$nodes[[nodeFrom]]$children
			# Remove nodeFrom from nodeTo's vector of parents.
			BN$nodes[[nodeTo]]$parents <- parents[which(parents != nodeFrom)]
			# Remove nodeTo from nodeFrom's vector of children.
			BN$nodes[[nodeFrom]]$children <- children[which(children != nodeTo)]
		}
	} else {
		cat('The two nodes to remove an edge between must be different. No changes were made.\n')
	}
	BN
}

reverse_edge <- function(BN, nodeFrom, nodeTo) {
	# nodeFrom and nodeTo should be numeric IDs for the nodes in the network.
	if (nodeFrom != nodeTo) {
		BN <- remove_edge(BN, nodeFrom, nodeTo)
		BN <- add_edge(BN, nodeTo, nodeFrom)
	} else {
		cat('The two nodes to remove an edge between must be different. No changes were made.\n')
	}
	BN
}