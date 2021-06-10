e1 <- c(1, 0)
e2 <- c(0, 1)

h <- 2 #приращение
d <- 2 #коэффициент уменьшения шага
e <- 0.001 #точность

k <- 0

b1 <- c(-8, -9) #начальная точка


F <- function(x) {
  return((x[1] - 2) ^ 2 + (x[2] - 9) ^ 2 + x[1] * x[2])
}



while(h > e){
  k <- k + 1
  b2 <- research(b1)
  if(F(b2) < F(b1)){
    b3 <- b2 + (b2 - b1)
    if(F(b3) < F(b2)){
      b1 <- b3
    }else{
      b1 <- b2 
    }
  } else {
    h <- h / d
  }
}




research <- function(b2){ 
  p1 <- b2 + h * e1
  p2 <- b2 - h * e1
  
  if (F(p1) < F(b2)) {
    b2 <- p1
  } else if (F(p2) < F(b2)) {
    b2 <- p2
  }
  
  
  p1 <- b2 + h * e2
  p2 <- b2 - h * e2
  if (F(p1) < F(b2)) {
    b2 <- p1
  } else if (F(p2) < F(b2)) {
    b2 <- p2
  }
  return(b2)
}
cat("Значение в минимальной точке: ", F(b2),"\n")

cat("Минимальная точка: ", b2,"\n")

cat("Число итераций: ", k)



