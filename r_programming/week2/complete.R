# Write a function that reads a directory full of 
# files and reports the number of completely observed 
# cases in each data file. The function should return 
# a data frame where the first column is the name of 
# the file and the second column is the number of 
# complete cases. A prototype of this function follows

complete <- function(directory, monitor_ids = 1:332) {

	
	# id nobs
	# 1 117
	# 1041

	observation_counts <- integer(0)

	for(monitor_id in monitor_ids) {
		observation_counts <- c(observation_counts, c(monitor_id, count_observations(directory, monitor_id)))
	}

	observation_counts <- matrix(observation_counts, ncol = 2, byrow = TRUE, dimnames = list(c(),c("id", "nobs")))

	data.frame(observation_counts)
}

count_observations <- function(directory, monitor_id) {
	
	monitor_id = str_pad(monitor_id, 3, pad = "0")	
	path <- file.path(directory, monitor_id)
	filename <- paste(path, ".csv", sep = "")
	#print(paste("loading file:", filename))
	
	if (file.exists(filename)) {
		monitor <- read.csv(filename)		
		nob <- nrow(na.omit(monitor))
		nob	
	}
}

# tests <- function(){
# 	if (complete("specdata", c(2, 4, 8, 10, 12)) == 4.064) {
# 		results <- c(results, "PASSED")
# 	} else {
# 		results <- c(results, "FAILED")
# 	}
# }
