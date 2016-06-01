#Load Data

scenario <- read.csv('scenario_19.csv', row.names = NULL, header = FALSE, stringsAsFactors = FALSE)
psycho <- read.csv('psycho_scenario_19.csv', row.names = NULL, header = FALSE, stringsAsFactors = FALSE)
activitiesPath <- read.csv('time_scenario_19.csv', row.names = NULL, header = FALSE,stringsAsFactors = FALSE)



### function for processing the CSVs into form convenient for the analysis. Order each file by the name of students...
### ... to avoid the case when one students ends the experiment after the second one starts it.


# process the scenario file, it contains two lines for each students, one for the pretest and one for the postest
processScenario <- function(file){
  file <- file[order(file$V1),]
  rownames(file) <- 1:nrow(file)
  if(file[1,1] != file[2,1]){
    file <- file[-1,]
  }
  
  for(i in 2:nrow(file)-1){
    if (!(file[i,1] == file[i-1,1] || file[i,1] == file[i+1,1])){
      file[i,1] <- NA
    }
  }
  if(file[nrow(file),1] != file[nrow(file)-1,1]){
    file <- file[-nrow(file),]
  }
  file <- na.omit(file)
  rownames(file) <- 1:nrow(file)
  
  return (file)
  
}

# process the file about psychological results, each student should have 10 lines corresponding to questions of the test
processPsycho <- function(file, students){
  file <- file[order(file$V1),]
  rownames(file) <- 1:nrow(file)
  
  for(i in 1:nrow(file)){
    if(!(file[i,1] %in% students)){
      file[i,1] <- NA
    }
  }
  file <- na.omit(file)
  rownames(file) <- 1:nrow(file)
  
  return (file)
  
}

processPath <- function(file, students){
  file <- file[order(file$V1,file$V5),]
  rownames(file) <- 1:nrow(file)
  
  for(i in 1:nrow(file)){
    if(!(file[i,1] %in% students)){
      file[i,1] <- NA
    }
  }
  file <- na.omit(file)
  rownames(file) <- 1:nrow(file)
  
  
  return (file)
}

### get the list of students that have completed the experiment
getStudents <- function(scenario){
  students_nb <- nrow(scenario)/2  # see processScenario to understand
  students <- rep(NA, students_nb)
  
  for(i in seq(2,nrow(scenario),2)){
    students[i/2] <-scenario[i,1]
  }
  return (students)
}

scenario <- processScenario(scenario)
students <- getStudents(scenario)
students_nb <- length(students)
psycho <- processPsycho(psycho, students)
activitiesPath <- processPath(activitiesPath, students)

# create a table that summurizes the data 

psychological_results <- rep(NA, students_nb)
openness_score <- rep(0, students_nb)
conscientiousness_score<- rep(0, students_nb)
pre_test_score<- rep(0, students_nb)
path<- rep(NA, students_nb)
post_test_score<- rep(0, students_nb)
learning_gain <- rep(0, students_nb)
table <- data.frame(students,psychological_results,openness_score,conscientiousness_score,pre_test_score,path,post_test_score, learning_gain)



# score the psychological test
for(k in seq(1,nrow(psycho),10)){
  l = k+9
  for(j in k:l){
    if(psycho[j,6] == "Very Inaccurate"){
      table[ceiling(k/10),2] = paste(table[ceiling(k/10),2],sep = ',',1)
    }else if(psycho[j,6] == "Inaccurate"){
      table[ceiling(k/10),2] = paste(table[ceiling(k/10),2],sep = ',',2)
    }else if(psycho[j,6] == "Neutral"){
      table[ceiling(k/10),2] = paste(table[ceiling(k/10),2],sep = ',',3)
    }else if(psycho[j,6] == "Accurate"){
      table[ceiling(k/10),2] = paste(table[ceiling(k/10),2],sep = ',',4)
    }else if(psycho[j,6] == "Very accurate"){
      table[ceiling(k/10),2] = paste(table[ceiling(k/10),2],sep = ',',5)
    }
  }
}
#score openness and conscientiousness knowing that the first 5 numbers are for openness...
# ...and the last 5 are for conscientiousness
for(i in 1:students_nb){
  table[i,2] <- substring(table[i,2],3,22)
  for(j in seq(2,10,2)){
    table[i,3] = table[i,3]+ strtoi(substring(table[i,2],j,j))
  }
  
  for(k in seq(12,20,2)){
    table[i,4] <- table[i,4] + strtoi(substring(table[i,2],k,k))
  }
}

# remove the fisrt comma ()
table[,2] <- substring(table[,2],2,20)

### get the preTest and posTest scores
for(i in seq(1,nrow(scenario),2)){
  table[ceiling(i/2),5] <- scenario[i,6]
  table[ceiling(i/2),7] <- scenario[i+1,6]
  
}
table$learning_gain <- table$post_test_score - table$pre_test_score

j <- 0
for (i in seq(1,nrow(activitiesPath)-4,5)){
  j <-j+1
  if(activitiesPath[i+2,3] == 13 && activitiesPath[i+3,3] == 14){
    table[j,6] <- "A"
  }else if (activitiesPath[i+2,3] == 15 && activitiesPath[i+3,3] == 16){
    table[j,6] <- "B"
  }else if (activitiesPath[i+2,3] == 13 && activitiesPath[i+3,3] == 16){
    table[j,6] <- "C"
  }else if (activitiesPath[i+2,3] == 15 && activitiesPath[i+3,3] == 14){
    table[j,6] <- "D"
  }
  
}


### Build linear Model

# take two vectors as arguments and returns the distance between them
predictError <- function(x1,x2){
  tmp <- (x1 - x2) ^ 2
  dist <- sqrt(sum(tmp))
  return(dist)
}

# Take the table of data and the path as arguments, builds the model using 80% of the data and test with 20%
predictPath<- function(table, pathStr){
  
  path <- table
  for(i in 1:nrow(table)){
    if (!(path[i,6] == pathStr)){
      path[i,6] <- NA
    }
  }
  path <- na.omit(path)
  rownames(path) <- 1:nrow(path)
  path_m<- path[sample(nrow(path), floor(0.8*nrow(path))),]
  rownames(path_m) <- 1:nrow(path_m)
  
  path_t <- path
  for(i in 1:nrow(path_t)){
    if(path_t[i,1] %in% path_m$students){
      path_t[i,] <- NA
    }
  }
  path_t <- na.omit(path_t)
  rownames(path_t) <- 1:nrow(path_t)
  
  # Linear Model
  model<- lm(learning_gain~openness_score + conscientiousness_score, data = path_m)
  p <- predict(model,path_t)
  print(paste("============================================= PATH" ,pathStr," ================================================"))

  print(path_t)   #print the data used for testing the model
  print("Prediction:")
  print(p)
  print(paste("Error: ", predictError(path_t$learning_gain,p)))
  
  return (p)
}

pathA_prediction <- predictPath(table,"A")
pathB_prediction <- predictPath(table,"B")
pathC_prediction <- predictPath(table,"C")
pathD_prediction <- predictPath(table,"D")


