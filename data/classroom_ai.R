### choose the experiement to analyse
theme <- 0

while(!(theme == 1 || theme == 2 )){
  theme <- as.integer(readline("For Epidemics enter 1, For Falling Balls enter 2:  "))
}
#Load Data
if(theme ==1){
  scenario <- read.csv('scenario_19.csv', row.names = NULL, header = FALSE, stringsAsFactors = FALSE)
  psycho <- read.csv('psycho_scenario_19.csv', row.names = NULL, header = FALSE, stringsAsFactors = FALSE)
  activitiesPath <- read.csv('time_scenario_19.csv', row.names = NULL, header = FALSE,stringsAsFactors = FALSE)

}else if(theme == 2){
  scenario <- read.csv('scenario_22.csv', row.names = NULL, header = FALSE, stringsAsFactors = FALSE)
  psycho <- read.csv('psycho_scenario_22.csv', row.names = NULL, header = FALSE, stringsAsFactors = FALSE)
  activitiesPath <- read.csv('time_scenario_22.csv', row.names = NULL, header = FALSE,stringsAsFactors = FALSE)
}
 
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

# process the file about the scenario path
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

### Use the activities' id to find the path followed by each student
mapPath <- function(theme,file, table){
j<-0
if(theme ==1){
for (i in seq(1,nrow(file)-4,5)){
  j <-j+1
  if(file[i+2,3] == 13 && file[i+3,3] == 14){
    table[j,6] <- "video-video"
  }else if (file[i+2,3] == 15 && file[i+3,3] == 16){
    table[j,6] <- "text-text"
  }else if (file[i+2,3] == 13 && file[i+3,3] == 16){
    table[j,6] <- "video-text"
  }else if (file[i+2,3] == 15 && file[i+3,3] == 14){
    table[j,6] <- "text-video"
  }
}
}else if(theme == 2){
  for (i in seq(1,nrow(file)-4,5)){
    j <-j+1
    if(file[i+2,3] == 21 && file[i+3,3] == 20){
      table[j,6] <- "video2-text1"
    }else if (file[i+2,3] == 22 && file[i+3,3] == 19){
      table[j,6] <- "video1-text2"
    }
    
  }
}
  return(table)
}

table <- mapPath(theme, activitiesPath, table)
### Build linear Model

# take two vectors as arguments and returns the distance between them
predictError <- function(x1,x2){
  tmp <- (x1 - x2) ^ 2
  dist <- sqrt(sum(tmp))
  return(dist)
}

### Take the table of data and the path as arguments, builds the model using 80% of the data and test with 20%
createData<- function(data, pathStr){
  path <- data
  path <- path[,-2]
  for(i in 1:nrow(path)){
    if (!(path[i,5] == pathStr)){
      path[i,5] <- NA
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

  result <- list(path_m, path_t)

  return(result)
}

### Adapt the table to assign path
createDataToAssign<- function(data, pathStr){
  path <- data
  path <- path[,-2]
  for(i in 1:nrow(path)){
    if (!(path[i,5] == pathStr)){
      path[i,5] <- NA
    }
  }
  path <- na.omit(path)
  rownames(path) <- 1:nrow(path)

  return(path)

}

### Build a linear model
predictPath<- function(data,test, pathStr,choice){
  # Linear Model
  model<- lm(learning_gain~openness_score + conscientiousness_score, data = data)
  p <- predict(model,test)
  print(paste("============================================= PATH" ,pathStr," ================================================"))
 if(choice == 1){
   print(test)   #print the data used for testing the model
 }
  print("Prediction:")
  print(p)
  if(choice == 1){
    print(paste("Error: ", predictError(test$learning_gain,p)))
}
  return (p)
}

### Assign path to a student in a way that maximizes the learning gain using the linear model 
assignPath <- function(theme, data, student_id, op_score, cons_score){
  gain <- 0
  pre_score <- 0
  post_score <- 0

  if(theme == 1){
  path <- "video-video"
  dfA <- data.frame(student_id,op_score,cons_score,pre_score,path, post_score,gain)
  colnames(dfA) <- c("students", "openness_score", "conscientiousness_score", "pre_test_score", "path", "post_test_score", "learning_gain")
  path <- "text-text"
  dfB <- data.frame(student_id,op_score,cons_score,pre_score,path, post_score,gain)
  colnames(dfB) <- c("students", "openness_score", "conscientiousness_score", "pre_test_score", "path", "post_test_score", "learning_gain")
  path <- "video-text"
  dfC <- data.frame(student_id,op_score,cons_score,pre_score,path, post_score,gain)
  colnames(dfC) <- c("students", "openness_score", "conscientiousness_score", "pre_test_score", "path", "post_test_score", "learning_gain")

  path <- "text-video"
  dfD <- data.frame(student_id,op_score,cons_score,pre_score,path, post_score,gain)
  colnames(dfD) <- c("students", "openness_score", "conscientiousness_score", "pre_test_score", "path", "post_test_score", "learning_gain")

  dataA <- createDataToAssign(data,"video-video")
  pathA_prediction <- predictPath(dataA,dfA,"video-video",2)

  dataB <- createDataToAssign(data,"text-text")
  pathB_prediction <- predictPath(dataB,dfB,"text-text",2)

  dataC <- createDataToAssign(data,"video-text")
  pathC_prediction <- predictPath(dataC,dfC,"video-text",2)

  dataD <- createDataToAssign(data,"text-video")
  pathD_prediction <- predictPath(dataD,dfD,"text-video",2)

  predictions <- c(rbind(pathA_prediction)[1], rbind(pathB_prediction)[1], rbind(pathC_prediction)[1], rbind(pathD_prediction)[1])
  predictions <- predictions[predictions <=1]
  maximum <- max(predictions)
  if(maximum == pathA_prediction){
    print(paste("For student",student_id,"you should assign the video-video path, the expected learning_gain is:",maximum))
  }else if(maximum == pathB_prediction){
   print(paste("For student", student_id,"you should assign the text-text path, the expected learning_gain is:",maximum))
  }else if(maximum == pathC_prediction){
    print(paste("For student",student_id, "you should assign the video-text path, the expected learning_gain is:",maximum))
  }else{
    print(paste("For student", student_id, "you should assign the text-video path, the expected learning_gain is: ",maximum))
  }

  } else if(theme == 2){
    path <- "video2-text1"
    dfA <- data.frame(student_id,op_score,cons_score,pre_score,path, post_score,gain)
    colnames(dfA) <- c("students", "openness_score", "conscientiousness_score", "pre_test_score", "path", "post_test_score", "learning_gain")
    path <- "video1-text2"
    dfB <- data.frame(student_id,op_score,cons_score,pre_score,path, post_score,gain)
    colnames(dfB) <- c("students", "openness_score", "conscientiousness_score", "pre_test_score", "path", "post_test_score", "learning_gain")
    
    dataA <- createDataToAssign(data,"video2-text1")
    pathA_prediction <- predictPath(dataA,dfA,"video2-text1",2)
    
    dataB <- createDataToAssign(data,"video1-text2")
    pathB_prediction <- predictPath(dataB,dfB,"video1-text2",2)
    
    predictions <- c(rbind(pathA_prediction)[1], rbind(pathB_prediction)[1])
    maximum <- max(predictions)
    
    if(maximum == pathA_prediction){
      print(paste("For student",student_id,"you should assign the video2-text1 path, the expected learning_gain is:",maximum))
    }else if(maximum == pathB_prediction){
      print(paste("For student", student_id,"you should assign the video1-text2 path, the expected learning_gain is:",maximum))
}

  }
}
### "main"

choice <-0

### if choice is 1, we evaluate predictions. If it is 2 we assign path to new student

while(!(choice == 1 || choice == 2)){
  choice <- as.integer(readline("For evaluating predictions enter 1. To assign path enter 2: "))
}

if(choice == 1){
  if(theme == 1 ){
    dataA <- createData(table,"video-video")
    dataA_model <-data.frame(dataA[1])
    dataA_test <-data.frame(dataA[2])
    pathA_prediction <- predictPath(dataA_model,dataA_test,"video-video",1)
    
    dataB <- createData(table,"text-text")
    dataB_model <-data.frame(dataB[1])
    dataB_test <-data.frame(dataB[2])
    pathB_prediction <- predictPath(dataB_model,dataB_test,"text-text",1)
    
    dataC <- createData(table,"video-text")
    dataC_model <-data.frame(dataC[1])
    dataC_test <-data.frame(dataC[2])
    pathC_prediction <- predictPath(dataC_model,dataC_test,"video-text",1)
    
    dataD <- createData(table,"text-video")
    dataD_model <-data.frame(dataD[1])
    dataD_test <-data.frame(dataD[2])
    pathD_prediction <- predictPath(dataD_model,dataD_test,"text-video",1)
    
  }
  
  
  if(theme == 2){
    dataA <- createData(table,"video1-text2")
    dataA_model <-data.frame(dataA[1])
    dataA_test <-data.frame(dataA[2])
    pathA_prediction <- predictPath(dataA_model,dataA_test,"video1-text2",1)
    
    dataB <- createData(table,"video2-text1")
    dataB_model <-data.frame(dataB[1])
    dataB_test <-data.frame(dataB[2])
    pathB_prediction <- predictPath(dataB_model,dataB_test,"video2-text1",1)
  }
}else{
  student_id <- as.integer(readline("student id: "))
  op <- as.integer(readline("openness score: "))
  cons <- as.integer(readline("conscientiousness score:  "))
  assignPath(theme,table, student_id ,op,cons)
  
}


