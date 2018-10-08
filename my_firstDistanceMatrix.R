##########################################################################################################
# R session attempting to transform top500 overdispersion matrix to distanceMatrix
##########################################################################################################

library(tidyverse)  #importing tidyverse library
my_file <- read.csv(file='exported_data.overd.csv',head=TRUE,sep=",",stringsAsFactors=TRUE) #importing .csv file

row.names(my_file) <- my_file[,1] # Set gene names as row.names, ie. 0th column of table
my_file <- my_file[,-1]   # 
my_file <- my_file[,names(which(is.na(colSums(my_file))==FALSE))] 

install.packages("distances")

#working
my_distances1 <- dist(my_file, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
m <- as.matrix(my_distances1)
m1 <- t(apply(m, 1, function(x)(x-min(x))/(max(x)-min(x)))) #normalize
write.table(x = m1, file = "distanceMatrix2.txt")  #need to write this fucker to a file

##########################################################################################################
#how to normalize these values?? 
max(m[5], na.rm = TRUE)
col_max <- apply(m, 2, function(x) max(x, na.rm = TRUE))
m1 <- t(apply(m, 1, function(x)(x/max(x)))) #naive normalization func

#firstAttempt
matrix <- data.frame(t(combn(rownames(my_file),2)), as.numeric(my_distances1))
write.csv(matrix, file = "distanceMatrix.csv")

#secondAttempt
matrix <- distance_matrix(distances, indices = NULL)
cellDistance <- row.names(my_file)[distance]

#thirdAttempt
data.cellDistance <- my_file[cellDistance[1:100],]
write.table(x = data.cellDistance, file = "data.cellDistance.txt")  #need to write this fucker to a file
##########################################################################################################