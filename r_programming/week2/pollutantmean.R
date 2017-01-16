library(stringr)

# Write a function named 'pollutantmean' that 
# calculates the mean of a pollutant (sulfate or nitrate) 
# across a specified list of monitors. The function 
# 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
# Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
# particulate matter data from the directory specified in the 'directory' 
# argument and returns the mean of the pollutant across all of the monitors, 
# ignoring any missing values coded as NA. A prototype of the function is 
# as follows

# directory => "specdata"
# pollutant => "sulfate" or "nitrate"
pollutantmean <- function(directory, pollutant, monitor_ids = 1:332) {

	observations <- integer(0)

	for(monitor_id in monitor_ids) {
		observations <- c(observations, fetch_observations(directory, pollutant, monitor_id))		
	}
	
	print(paste("total observations found:", length(observations)))
	mean(observations)
}

fetch_observations <- function(directory, pollutant, monitor_id) {
	
	monitor_id = str_pad(monitor_id, 3, pad = "0")	
	path <- file.path(directory, monitor_id)
	filename <- paste(path, ".csv", sep = "")
	print(paste("loading file:", filename))
	
	if (file.exists(filename)) {
		monitor <- read.csv(filename)		
		#print(summary(monitor))
		
		observations <- monitor[,pollutant][!is.na(monitor[,pollutant])]		
		print(paste("observations found:", length(observations)))
		observations
	}
}

tests <- function(){
	results <- character(0)
	# if (pollutantmean("/Users/jana/_Jana/coursera/specdata", "sulfate", 100) == "/Users/jana/_Jana/coursera/specdata/100"){
		# results <- c(results, "PASSED")
	# } else {
		# results <- c(results, "FAILED")
	# }
	
	if (round(pollutantmean("specdata", "sulfate", 1:10), 3) == 4.064) {
		results <- c(results, "PASSED")
	} else {
		results <- c(results, "FAILED")
	}
	if (round(pollutantmean("specdata", "nitrate", 70:72), 3) == 1.706) {
		results <- c(results, "PASSED")
	} else {
		results <- c(results, "FAILED")
	}
	if (round(pollutantmean("specdata", "nitrate", 23), 3) == 1.281) {
		results <- c(results, "PASSED")
	} else {
		results <- c(results, "FAILED")
	}
	print(results)
}

#pollutantmean("specdata", "sulfate", 1:10) =>  4.064
#pollutantmean("specdata", "nitrate", 70:72) =>  1.706
#pollutantmean("specdata", "nitrate", 23) => 1.281

