pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".


        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

	if (!pollutant == "sulfate" & !pollutant == "nitrate")
	{
		print("Error, is not possible to calculate the mean of a value different to sulfate or nitrate")
	}

	first <- TRUE
	
	for (i in id) 
	{
		
		if (i < 10) {  file_name <- paste("./",directory,"/","00",i,".csv",sep="") }
		else if (i<100) {  file_name <- paste("./",directory,"/","0",i,".csv",sep="") }
		else {  file_name <- paste("./",directory,"/",i,".csv",sep="") }

		lecture <- read.csv(file_name,head=TRUE)

		if (first) { complete <- lecture 
			     first <- !first}
		else { complete <- rbind(complete,lecture)}
		
	}

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!

	if (pollutant == "sulfate") { mean(complete$sulfate, na.rm = TRUE)}
		else { mean(complete$nitrate, na.rm = TRUE)}

}
