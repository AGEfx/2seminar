sircle <- function(a,b,e){ #a,b - ???????, ? - ??????
  f <- function(x){#???????? ???????
    return(x^2)
  }
  abc <- c() #посещенные точки
  i <- 0 #кол-во итераций
  while((b-a) > e){ 
    x1 <- b+0.618*(a-b)
    x2 <- a+0.618*(b-a)
    if (f(x1)<f(x2)){ #если значение функции в x1 меньше значения в x2, значит - x2 лежит дальше от нашего минимума и ее нужно переобозначить
      b <- x2
      i <- i + 1
      abc <- append(abc,x2)
    }
    else{
      a <- x1
      i <- i + 1
      abc <- append(abc,x1)
    }
  }
  min <- (a + b) / 2 #мы дошли до маленького расстояния между a и b, теперь находим середину
  fmin <- f(min)
  xf <- -10:10
  yf <- f(xf)
  plot(xf,yf,type = "l",col = "green",sub = "график x^2")
  points(c(abc[1:5]),c(f(abc[1:5])))
  points(c(abc[i],abc[i-1],abc[i-2],abc[i-3],abc[i-4]),c(f(abc[i]),f(abc[i-1]),f(abc[i-2]),f(abc[i-3]),f(abc[i-4])))
  return(c(min,fmin,i))
}
sircle(-10,10,1)