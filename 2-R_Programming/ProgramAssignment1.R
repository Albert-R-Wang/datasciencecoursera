### Programming Assignment 1: Air Pollution ###
# Coursera Data Science specialization (provided by Johns Hopkins University)
# Course 2: R programming Module 2

### Part 1 ###
# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors.

pollutantmean <- function(directory, pollutant, id = 1:332){
  # 'directory' is a character vector of length 1 indicating the location of the CSV files
  
  
  # 'pollutant' is a character vector of length 1 indicating the name of the pollutant for which we will calculate the mean; either "sulfate" or "nitrate".
  
  # 'id' is an integer vector indicating the monitor ID numbers to be used
  
  # Return the mean of the pollutant across all monitors list in the 'id' vector (ignoring NA values)
  # NOTE: Do not round the result!
  
  
  id_chr = sprintf("%03d", id) # Pad with leading zeros to ensure a total width of 3 characters, matching the file name format
  files = paste(getwd(), directory, paste(id_chr, ".csv", sep = ""), sep = "/") # get directory for all the files specified
  
  # Initialize an empty vector to collect pollutant values
  allValues <- c()
  
  # Loop through each file
  for (file in files){
    data = read.csv(file)
    
    if (pollutant == "sulfate"){
      allValues = c(allValues, data$sulfate)
    } else if (pollutant == "nitrate"){
      allValues = c(allValues, data$nitrate)
    } else {
      stop("Pollutant not found")
    }
    
  }
  
  # Calculate mean, removing NA values
  mean(allValues, na.rm = T)
  
}

# Example use
pollutantmean("specdata", "nitrate", 70:72)


### Part 2 ###
#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file.

complete <- function(directory, id = 1:332){
  # 'directory is a character vector of length 1 indicating the location of the CSV files
  
  # 'id' is an integer vector indicating the monitor ID numbers to be used
  
  # Return a data frame of the form:
  # id nobs
  # 1 117
  # 2 1041
  # ...
  # Where 'id is the monitor ID number and 'nobs' is the number of complete cases
  
  id_chr = sprintf("%03d", id) # Pad with leading zeros to ensure a total width of 3 characters, matching the file name format
  files = paste(getwd(), directory, paste(id_chr, ".csv", sep = ""), sep = "/") # get directory for all the files specified
  
  completeList = list()
  n = 1
  for (file in files){
    data = read.csv(file)
    nobs = complete.cases(data) #find rows have no missing values across the entire sequence. Can specify specific columns by doing data[,cols], where cols specify the columns
    nobs = sum(nobs) #to get the number of TRUE (complete cases). Can also use table() to find out the numbers of both TRUE and FALSE
    completeList[[n]] = data.frame(id = id[n], nobs = nobs) #record values for each loop
    n = n+1
  }
  
  # Combine into one data frame after the loop
  do.call(rbind, completeList)
}


# Example use
complete("specdata", 30:25)



### Part 3 ###
# Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. 

corr <- function(directory, threshold = 0){
  # 'directory' is a character vector of length 1 indicating the location of the CSV files
  
  # 'threshold' is a numeric vector of length 1 indicating the number of completely observed observations (on all variables) required to compute the correlation between nitrate and sulfate; default is 0
  
  # Return a numeric vector of correlations
  
  # Using all the files
  id = 1:332
  
  # Find out the number of complete cases for all files
  completeTable = complete(directory, id)
  
  # Identify files with a number of complete cases greater than the specified threshold
  thresholdTable = subset(completeTable, completeTable$nobs>threshold)
  
  # If no files meet the threshold requirement, then the function should return a numeric vector of length 0
  if (nrow(thresholdTable)==0){
    corVector = numeric(0) #numeric vector of length 0
    return(corVector)
  }
  
  # Extract the ID for these files
  thresholdID = thresholdTable$id
  
  
  id_chr = sprintf("%03d", thresholdID) # Pad with leading zeros to ensure a total width of 3 characters, matching the file name format
  files = paste(getwd(), directory, paste(id_chr, ".csv", sep = ""), sep = "/") # get directory for all the files specified
  
  # Initialize an empty vector to collect cor results
  corVector = c()
  
  # Loop through each file
  for (file in files){
    data = read.csv(file)
    
    # Calculate the correlation between sulfate and nitrate, using only complete cases
    corData = cor(data$sulfate, data$nitrate, use = "complete.obs")
    
    corVector = c(corVector, corData)
  }
  
  corVector
}

# Example use
cr <- corr("specdata", 400)
head(cr)

summary(cr)

# No files meet the threshold requirement
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
