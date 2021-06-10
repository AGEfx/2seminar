A = matrix(c(56,43,345,32,35,14,75,35,34), nrow=3,ncol=3,byrow = TRUE)
A


#Поиск по столбцу

det <- function(matrix) {
  dim <- dim(matrix)[1] 
  sum <- 0
  if (length(matrix) == 1) {
    sum <- sum + matrix[1]
  }
  else{
    for (i in 1:dim) {
      minor <- matrix[-i,2:dim]
      if (i %% 2 == 0) {
        sum <- sum + matrix[i] * -1 * det(minor)
      } else{
        sum <- sum + matrix[i] * det(minor)
      }
    }
  }
  return(sum)
}

determinant <- det(A)
print(determinant)