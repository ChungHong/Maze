setwd("D:/Desktop/Meeting/exercise")
rm(list = ls());gc()


require(purrr)
require(parallel)
rm(list=ls());gc()


################# Create Maze
# start.number: the number for starting(default is 1)
# end.number: the number for the end(default is 9)
# dim.number: the number of rows for maze(default is 3)
Make_maze <- function(start.number = 1, end.number = 9, dim.number = 3){
  if((end.number %% dim.number) != 0){
    print("Sorry. The dimention can not match your end number. Maybe you can set another end number or change your number of dimention.")
  }else{
    Endpoint <<- end.number
    maze.dody <- matrix(start.number:end.number,
                        nrow = dim.number,
                        byrow = TRUE)
    maze.wall <- cbind(rep(0, dim.number), maze.dody, rep(0, dim.number))
    maze <- rbind(rep(0, ncol(maze.wall)), maze.wall, rep(0, ncol(maze.wall)))
    return(maze)
  }
}

################# Directions for mouse could run in maze.
# i and j is the position in maze
Choose_direction <- function(i, j){
  direction <- c() # record the mouse's direction in maze
  
  if(MAZE[i, j] != Endpoint){
    ifelse(MAZE[i + 1, j] == 0,
           direction <- c(direction, "Non"),
           direction <- c(direction, "L./"))# left
    ifelse(MAZE[i + 1, j + 2] == 0,
           direction <- c(direction, "Non"),
           direction <- c(direction, "R./"))# right
    ifelse(MAZE[i + 2, j + 1] == 0,
           direction <- c(direction, "Non"),
           direction <- c(direction, "D./"))# down
    ifelse(MAZE[i, j + 1] == 0,
           direction <- c(direction, "Non"),
           direction <- c(direction, "U./"))# up
  }else{
    direction <- "NoN"
  }
  
  Direction <- sample(x = direction[direction != "Non"], size = 1)
  return(Direction)
}

################# Mouse begin to walk and I use the iterative method to demo it.
# x and y is the first position for mouse in maze to start to walk(default is 1)
Walk <- function(x = 1, y = 1){
  Direction <- Choose_direction(x, y)
  mouse <- MAZE[x + 1, y + 1] # the position of mouse

  if(mouse != Endpoint){
    if(Direction == "U./"){
      x <- x - 1
    }else if(Direction == "D./"){
      x <- x + 1
    }else if(Direction == "L./"){
      y <- y - 1
    }else if(Direction == "R./"){
      y <- y + 1
    }else {cat("End")}
    runs <<- runs + 1
    #print(Direction)
    Walk(x, y)
    }else {
      gc()
      return(runs)
    }
}


MAZE <- Make_maze()
runs <- 0

Time1 <- system.time(result1 <- sapply(1:1e2, function(i){
                                  runs <<- 0
                                  Walk()}))
Time1.5 <- system.time(result1.5 <- map_dbl(1:1e2, function(i){
                                      runs <<- 0
                                      Walk()}))
gc()

Time2 <- system.time(result2 <- sapply(1:1e4, function(i){
                                  runs <<- 0
                                  Walk()}))
Time2.5 <- system.time(result2.5 <- map_dbl(1:1e4, function(i){
                                      runs <<- 0
                                      Walk()}))
gc()



cpu.cores <- detectCores()               #計算核心數
cl <- makeCluster(cpu.cores - 1)           #使用的核心數
#將"變數"及"函數"丟進核心中
clusterExport(cl, c("MAZE", "Endpoint", "runs",
                    "Make_maze", "Choose_direction", "Walk"))

clusterSetRNGStream(cl, 1)               #平行計算的隨機種子
time1 <- system.time(result_P <- parSapply(cl, 1:1e6, function(i){
                                                        runs <<- 0
                                                        Walk()}));stopCluster(cl)
mean(result_P) # 17.9932
save(result_P, file = "million.Rdata")



