# Write a function that takes a directory of data 
# files and a threshold for complete cases and 
# calculates the correlation between sulfate and 
# nitrate for monitor locations where the number 
# of completely observed cases (on all variables) 
# is greater than the threshold. The function 
# should return a vector of correlations for the 
# monitors that meet the threshold requirement. If 
# no monitors meet the threshold requirement, then 
# the function should return a numeric vector of 
# length 0. A prototype of this function follows

corr <- function(directory, threshold = 0) {
 	#threshold = numeric(1) indicating the number
 	# of completely observed observations (rows)
 	# required to compute correlation between
 	# nitrate and sulfate

# complete_observations <- complete.cases(monitor)
# obs <- monitor[complete_observations,]

 	# x is a dataframe of data where observed cases is > threshold
 	
 	correlations <- fetch_complete_observations(directory, threshold)
	# complete_observations <- fetch_complete_observations(directory, threshold)
 	# correlations <- cor(complete_observations$sulfate, complete_observations$nitrate, use = "complete.obs", method = "pearson")
 	# return numeric vector of correlations
}

fetch_complete_observations <- function(directory, threshold) {

	# complete_observations <- data.frame()
	correlations <- numeric(0)
	
	filenames <- list.files(directory)
	for (filename in filenames) {
		full_path <- file.path(directory, filename)
		observations <- read.csv(full_path)
		complete_bools <- complete.cases(observations)
		total_complete_cases <- sum(complete_bools)
		if (total_complete_cases > threshold) {
			# print(sum(complete_bools))
			# print(observations[complete_bools,])
			#complete_observations <- c(complete_observations, observations[complete_bools,])	

			correlations <- c(correlations, cor(observations$sulfate, observations$nitrate, use = "complete.obs", method = "pearson"))

		} else {
			#print("Skipping monitor...")
		}
		
	}	
	# complete_observations
	correlations
}

