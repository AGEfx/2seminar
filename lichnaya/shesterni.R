n <- 5
m <- 5
mat <- matrix(c(1,2,2,3,3,4,4,5,5,1), ncol= 2, nrow = m, byrow=TRUE)

shesterni <- c(rep(-1,n))
shesterni[1] <- 0
current <- c(1) #текущие шестерни
nnext <- c() #шестерни сцепленные с текущими
znach = 1 #значение для шестерней, обозначающее сторону вращения(либо в одну - 0, либо в другую - 1)
f <- TRUE #можно найти еще сцепленные шестерни(флаг для выхода из цикла)
possible <- TRUE #возможно ли прокрутить
while(f == TRUE){
  f <- FALSE
  for(i in current){
    for(x in 1:nrow(mat)){
      if(mat[x,1] == i){ #если текущая есть в матрицу
        if(shesterni[mat[x,2]] == znach){
          #если её метка не поменяется(то есть не поменяется направление вращения), то выходим из цикла(иначе мы пойдём по кругу)
        }else if(shesterni[mat[x,2]] == -1){ #если шестерня не помечена
          shesterni[mat[x,2]] <- znach #даём значение шестерне, с которой сцеплена текущая
          nnext <- append(nnext,mat[x,2]) #добавляем новую шестерню в следующие
          f <- TRUE
        }else{ #если шестерня уже была помечена
          possible <- FALSE
        }
      }
    }
  }
  current <- nnext
  nnext <- c()
  if(znach == 0){ #меняем значение на противоположное
    znach <- 1
  }else{
    znach <- 0
  }
}
if(possible){
  print("Возможно")
  print(length(shesterni[shesterni!=-1])) #вывод количества
}else{
  print("невозможно")
}
print(shesterni)

