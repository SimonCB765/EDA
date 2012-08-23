cycle_check <- function(BN) {
	# Use Tarjan's strongly connected components algorithm to find cycles.
	# (http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm)
	#
	#index := 0
	#S := empty
	#for each v in V do
	#	if (v.index is undefined) then
	#		strongconnect(v)
	#	end if
	#repeat
	#
	index <- 0
	S <- vector(mode='integer', length=0)
	vertices <- vector(mode='list', length=BN$size)
	for (i in 1:length(vertices)) vertices[[i]] <- list('index'=NULL, 'lowlink'=NULL)
	for (i in 1:BN$size) {
		if (is.null(vertices[[i]]$index)) {
			result <- strong_connect(i, index, vertices, S, BN)
			if (result$cyclePresent) return(TRUE)
			index <- result$index
			vertices <- result$vertices
			S <- result$S
		}
	}
	
	# If this point is reached, then no cycles were found.
	return(FALSE)
}

strong_connect <- function(vertex, index, vertices, S, BN) {

	#
	#v.index := index
    #v.lowlink := index
    #index := index + 1
    #S.push(v)
	#
	vertices[[vertex]]$index <- index
	vertices[[vertex]]$lowlink <- index
	index <- index + 1
	S <- c(S, vertex)
	
	# Considers all of vertex's children.
	#
	#for each (v, w) in E do
	#	if (w.index is undefined) then
	#		// Successor w has not yet been visited; recurse on it
	#		strongconnect(w)
	#		v.lowlink := min(v.lowlink, w.lowlink)
	#	else if (w is in S) then
	#		// Successor w is in stack S and hence in the current SCC
	#		v.lowlink := min(v.lowlink, w.index)
	#	end if
    #repeat
	#
	for (child in BN$nodes[[vertex]]$children) {
		if (is.null(vertices[[child]]$index)) {
			# The child has not been visited yet. Recurse on it.
			result <- strong_connect(child, index, vertices, S, BN)
			if (result$cyclePresent) return(result)
			index <- result$index
			vertices <- result$vertices
			S <- result$S
			vertices[[vertex]]$lowlink <- min(vertices[[vertex]]$lowlink, vertices[[child]]$lowlink)
		} else if (child %in% S) {
			# The child is in the stack S already, and hence in the current strongly connected component.
			vertices[[vertex]]$lowlink <- min(vertices[[vertex]]$lowlink, vertices[[child]]$index)
		}
	}
	
	# If vertex is a root node, then pop the stack and generate a strongly connected component.
	#
	#if (v.lowlink = v.index) then
	#	start a new strongly connected component
	#	repeat
	#		w := S.pop()
	#		add w to current strongly connected component
	#	until (w = v)
	#	output the current strongly connected component
    #end if
	#
	if (vertices[[vertex]]$lowlink == vertices[[vertex]]$index) {
		# Start a new strongly connected component.
		SCC <- vector(mode='integer', length=0)
		repeat {
			w <- S[length(S)]
			SCC <- c(SCC, w)
			S <- S[-length(S)]
			if (w == vertex) break
		}
		cyclePresent <- length(SCC) > 1
		list('index'=index, 'vertices'=vertices, 'S'=S, 'cyclePresent'=cyclePresent)
	} else {
		list('index'=index, 'vertices'=vertices, 'S'=S, 'cyclePresent'=FALSE)
	}
}