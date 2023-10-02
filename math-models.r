#equations and their graphs

#2x + 3y = 8

#solve for y

#3y = 8 -2x

x = 1:5

y = (8-2*x)/3
plot(x,y,type="b",col="green")




#straight line
# y = mx + b

m = 4
b = 2
x = 1:20


y = m*x + b

plot(x,y,type="b",xlim=c(0,20),ylim=c(0,20))

for ( m in 0:15){
  x = 1:20
  y = m*x + b
  lines(x,y,type="l",col="red")
}

#if m = 2

#example
# x = 1
# y =




#quadratic equation
#https://quantifyinghealth.com/plot-a-quadratic-function-in-r/
#A quadratic function is a function of the form: ax2+bx+c where a≠0
#f(x)=x2+2x–20

a = 1
b = 2
c = -3
f = function(x) {
  a*x^2 + b*x + c
}


#Plotting the quadratic function f(x)
#First, we have to choose a domain over which we want to plot f(x).

#Let’s try -10 ≤ x ≤ 10:

# domain over which we want to plot f(x)
x = -10:10
# plot f(x)
plot(x, f(x), type = 'l',xlim = range(-10:10), ylim = range(-10:10)) 


# type = 'l' plots a line instead of points
# plot the x and y axes
abline(h = 0)
abline(v = 0)

#Finding the vertex
#The vertex V of a quadratic equation, in this case the lowest point on the graph of f(x), is: V(−b2a,f(−b2a))


find.vertex = function(a, b, c) {
  x_vertex = -b/(2 * a)
  y_vertex = f(x_vertex)
  c(x_vertex, y_vertex)
}
V = find.vertex(a, b, c)


# add the vertex to the plot
points(x = V[1], y = V[2],
       pch = 18, cex = 2) # pch controls the form of the point and cex controls its size
# add a label next to the point
text(x = V[1], y = V[2],
     labels = "Vertex", pos = 3) # pos = 3 places the text above the point

# Finding the x-intercepts of f(x)
# The x-intercepts are the solutions of the quadratic equation f(x) = 0; they can be found by using the quadratic formula:
#   
#   x=−b±b2–4ac√2a
# The quantity b2–4ac
# is called the discriminant:
#   
#   if the discriminant is positive, then f(x) has 2 solutions (i.e. 2 x-intercepts).
# if the discriminant is zero, then f(x) has 1 solution (i.e. 1 x-intercept).
# if the discriminant is negative, then f(x) has no real solutions (i.e. does not intersect the x-axis).


# find the x-intercepts of f(x)
find.roots = function(a, b, c) {
  discriminant = b^2 - 4 * a * c
  if (discriminant > 0) {
    c((-b - sqrt(discriminant))/(2 * a), (-b + sqrt(discriminant))/(2 * a))
  }
  else if (discriminant == 0) {
    -b / (2 * a)
  }
  else {
    NaN
  }
}


solutions = find.roots(a, b, c)
# print(solutions) outputs: -5.582576  3.582576


# Adding the x-intercepts to the plot:

# add the x-intercepts to the plot
points(x = solutions, y = rep(0, length(solutions)), # x and y coordinates of the x-intercepts
       pch = 18, cex = 2, col = 'red')
text(x = solutions, y = rep(0, length(solutions)),
     labels = rep("x-intercept", length(solutions)),
     pos = 3, col = 'red')



# INPUTS =================================================================
# A typical school bus has 50 seats
bus_capacity = 50

# The number of students signing up for the trip may range from 350 to 500
# We will compute a result for each increment of 10 sign-ups
number_of_students = seq(from = 350, to = 500, by = 10)

# MODEL ==================================================================
# Compute the number of buses by first dividing the number of students 
# by the bus capacity and then rounding up the results
number_of_bus = ceiling(number_of_students/bus_capacity)

# OUTPUTS ================================================================
# Display results as a plot

plot(x    = number_of_students,   y    = number_of_bus, 
     xlab = "Number of students", ylab = "Number of buses needed",
     main = "Number of buses needed by number of students",
     pch  = 16, col = "coral")


###-------------------------------------------------------------------------##

x=seq(1:10)
y=sin(x)
plot(y,x)

plot(2,1)


plot(1:5 ,c(2,4,6,8,10))

students = 200:300
adults = ceiling(students / 15)
riders = students + adults

capacity_big = 50
capacity_small = 20

remainder = riders %% capacity_big

if(remainder == 0){
  buses_big = riders %/% capacity_big
  buses_small = 0
} else if (remainder <= 20){  
  buses_big = riders %/% capacity_big
  buses_small = 1
} else {
  buses_big = riders %/% capacity_big + 1
  buses_small = 0
}


plot(x = students, y = buses_big + buses_small)




a <- 5
b <- 6

a < b


#Crocodile
croctime <- function(x){ 5 * sqrt(36 + x^2) + 4 * (20 - x)}
curve(croctime, 0, 20, las = 1, lwd = 5, col = "#68A959",
      xlab = "distance", ylab = "time")



#chaos game
 plot(0:2,0:2,type = "n")
 x <- c(0,1,2)
 y <- c(0,2,0)
 points(x,y,pch = 19)
 labels <- c("A","B","C")
 text(x+.05,y,labels)
 segments(0,0,2,0)
 segments(0,0,1,2)
 segments(1,2,2,0)
 points(1,1,pch = 19)
 
 
 

 px <- 1
 py <- 1


 for(i in 2:10000){
   r <- sample(1:3,1)
  
    if (r == 1){
     x <- (px+0)/2
     y <- (py+0)/2
     c <- "red"
     }
   else if (r == 2){
     x <- (px+1)/2
     y <- (py+2)/2
     c <- "green"
     
   }else{
     x <- (px+2)/2
     y <- (py+0)/2
     c <- "blue"
   }
    
   points(x,y,pch = 19,col = c)
    px <- x
    py <- y
 }

 
 

 plot(sin, -pi, 2*pi) #plot the curve of y = sin(x) from -pi to 2*pi 
 square <- function(x) x*x #Define a function
 plot(square, -3, 2)# Plot the defined function
 
 # Plot a 3D surface using the following code
 x <- y <- seq(-1, 1, length=100)
 Z <- outer(x, y, function(x, y){1-x^2-y^2})
 # outer(x,y, function) gives the z values defined on x, y grid
 persp(x = x, y = y, z = Z, theta = 310)

 
 area <- function(r){
   pi * r ^ 2
 }

 
 
 
 for (month in 1:5) {
   if (month < 3) {
     print(paste('Winter, month', month))
   } else {
     print(paste('Spring, month', month))
   }
 }
 
 for (x in 1:5) {
   print(x)
 }