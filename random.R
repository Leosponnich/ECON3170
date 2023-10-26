a <- 75
b <- a*3
v <- c(25,17,24,25,15,17,19,25)

my_plot <- plot(v)
my_table <- tableGrob(v)

grid.arrange(my_plot, my_table)

e <- rnorm(10,mean=5,sd=2)
e
w <- runif(5,min=0,max=10)
w

func <- function(x){
  return( x +x)
}

func(32)


dice <-function(n, s=6){
  return(sum(sample(1:s,n, T)))
}
dice(34, 10)