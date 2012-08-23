main <- function(dataset, outputDataset, sourceDir, restartsToDo=20, datapointsToGenerate=100) {
	# dataset is the location of the data to learn the network from. The data must contain a header. The header for each column must be the name of the variable that the column represetns.
	# outputDataset is the location to write the new dataset from the learned network to (can be the same as the input dataset, but will then overwrite)
	# sourceDir is the location of the directory that contains the source files to run the EDA.
	# restartsToDo is the number of restarts of the greedy local search to do
	# datapointsToGenerate is the number of new datapoints to generate from the learned network.

	# Load in the files needed for computation.
	sapply(list.files(pattern='[.]R$', path=sourceDir, full.names=TRUE), source)
	
	# Load in the dataset.
	inputData <- read.table(dataset, header=TRUE, colClasses=c('factor'))
	
	# Generate the empty network.
	network <- BN(inputData)
	
	# Generate the network that fits the data the 'best'.
	network <- greedy_with_restart(network, inputData, restartsToDo)
	
	# Generate the new dataset from the 'best' network.
	outputData <- generate_data(network, datapointsToGenerate)
	
	# Write out the output data.
	write.table(outputData, outputDataset, quote=FALSE)
}



# make sure alpha in the scoring algorithm does not outwiegh the counts too much, as if this is the case then the paramter estimates become rubbish (stick to about 50-50)