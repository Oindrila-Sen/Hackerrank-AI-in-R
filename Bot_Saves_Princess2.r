# Enter your code here. Read input from STDIN. Print output to STDOUT
nextMove <- function(n, r , c, grid){
  p <- n * n
  for( i in 1:p){
    #cat(i)
    inputline <- unlist(strsplit(grid,""))[i + 3]
    #cat(inputline)
    #cat("\n")
    if (inputline == "p"){
      a = as.integer(i)
      #cat (a)
      #cat("\n")
      if (a <= n){
        rp = 0
        cp = a-1
      }
      else if (a%%n == 0){
        rp = (a/n) - 1
        cp = 4
      }
      else{
        rp = floor(a/n)
        cp = (a%%n) -1
      }
    }
  }
  #cat(rp)
  #cat("\n")
  # cat(cp)
  #cat("\n")
  dir <- ' '
  if(r != rp){
    if(r-rp>0){
      dir = "UP"
      #r = r-1
    }
    else{
      dir = "DOWN"
      #r = r + 1
    }
  }
  else{
    if(c - cp >0){
      dir = "LEFT"
      #c = c - 1
    }
    else{
      dir = "RIGHT"
      #c = c + 1
    }  
  }
  #cat(r)
  #cat("\n")
  #cat(c)
  #cat("\n")
  n <- 0
  r <- 0
  c <- 0
  cat(dir)
}

stdin <- file('input')
open(stdin)
input <- readLines(stdin, warn = FALSE)
input <- unlist(strsplit(input, split = " "))

n <- as.integer(input[1])
r <- as.integer(input[2])
c <- as.integer(input[3])
grid <- input
#cat(n)
#cat(str(stdin))
#cat (r)
#cat(c)
#cat("\n")
#cat(str(input))
#cat(grid)
nextMove(n, r , c, grid)
close(stdin)