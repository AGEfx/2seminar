e1 <- c(1,0)
e2 <- c(0,1)

h <- 1 #приращение 

e <- 0.1 #точность


d <- 2 #коэффициент уменьшения шага

f <- function(p){
    #return((p[1]-3)^2 + (p[2]+1)^2)
    return(8*p[1]^2+4*p[1]*p[2]+5*p[2]^2)
}

hooke_jeeves <- function(point){
    old_point <- point
    new_point <- point
    difference <- c(1,1) #разница между старой и новой точкой
    
    while(h > e){
        while(difference != c(0,0)){
            point <- old_point + h*e1
            print(f(point))
            if(f(point) < f(new_point)){
                new_point <- point
            }
            point <- old_point - h*e1
            print(f(point))
            if(f(point) < f(new_point)){
                new_point <- point
            }
            
            point <- old_point + h*e2
            print(f(point))
            if(f(point) < f(new_point)){
                new_point <- point
            }
            
            point <- old_point - h*e2
            print(f(point))
            if(f(point) < f(new_point)){
                new_point <- point
            }
            
            difference <- new_point - old_point
            old_point <- new_point
        }
        
        h <- h / d
    }
    return(new_point)
}

start_point <- c(-4,-4)

end_point <- hooke_jeeves(start_point)

print(end_point)

print(f(end_point))

