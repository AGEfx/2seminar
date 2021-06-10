v <- c(0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,-1,0,0,-1,0,
       0,0,0,0,0,0,0,0,0,0,0,0,-1,0,-1,-1,0,0,0,-1,
       0,0,0,0,0,0,0,0,0,0,0,-1,0,-1,0,-1,-1,"f",0,0,
       -1,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,-1,0,0,0,-1,0,0,0,0,0,0,0,-1,0,
       0,0,-1,-1,0,0,-1,0,0,0,0,0,0,-1,0,0,0,-1,0,0,
       0,0,0,0,-1,0,0,0,-1,0,0,0,0,0,0,-1,0,-1,-1,-1,
       -1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,0,-1,0,
       0,"s",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
m <- matrix(v, nrow = 10, ncol = 20, byrow = TRUE)
found <- FALSE

f <- function(x) {
  up <- x + c(-1, 0)
  if (check(up)) {
    vup <- m[up[1], up[2]]
    
    if (vup == 0) {
      if (vup == "f")
        return(TRUE)
      
      m[up[1], up[2]] <<- d
      print(m)
    }
  }
  
  down <- x + c(1, 0)
  if (check(down)) {
    vdown <- m[down[1], down[2]]
    if (vdown == 0) {
      if (vdown == "f")
        return(TRUE)
      m[down[1], down[2]] <<- d
    }
  }
  
  left <- x + c(0, -1)
  if (check(left)) {
    vleft <- m[left[1], left[2]]
    if (vleft == 0) {
      if (vleft == "f")
        return(TRUE)
      m[left[1], left[2]] <<- d
    }
  }
  
  right <- x + c(0, 1)
  
  if (check(right)) {
    vright <- m[right[1], right[2]]
    
    if (vright == 0) {
      if (vright == "f")
        return(TRUE)
      m[right[1], right[2]] <<- d
    }
  }
  return(FALSE)
}

check <- function(x){ #проверка на выход за пределы матрицы
  if(x[1] > 0 && x[1] <= nrow(m) && x[2] > 0 && x[2] <= ncol(m)){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

d = 1 #длина волны
for(i in 1:100){
  print(d)
  if(found==TRUE)break
    if(d>1){
      for (i in 1:nrow(m)){
        if(found==TRUE) break
        for(j in 1:ncol(m)){
        if(found==TRUE)break
        if(m[i,j] == d-1){
          e <- c(i,j)
          if(f(e)==TRUE) found <- TRUE
        }
      }
      }
    }
    else{
      s <- which(m=="s", arr.ind=TRUE)
      
      f(c(s[1],s[2]))
    }
  d <- d + 1
  print(m)
}
print(m)



