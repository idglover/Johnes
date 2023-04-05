####FUNCTION####

plotnorm <- function(u,s){
  n1 <- u - (3 * s)
  n2 <- u + (3 * s)
  
  x <- seq(n1, n2, ((n2-n1)/1000))
  
  y <- dnorm(x, u, s)
  
  print(ggplot(data.frame(x,y),
         aes(x = x,
             y = y)) +
    geom_point() +
    labs(title = paste0("Normal (",u,",",s,")")))
}

####GAUSSIAN####

u <- 0.3
s <- 0.1

n1 <- u - (3 * s)
n2 <- u + (3 * s)

x <- seq(n1, n2, ((n2-n1)/1000))
  
y <- dnorm(x, u, s)

ggplot(data.frame(x,y),
       aes(x = x,
           y = y)) +
  geom_point() +
  labs(title = paste0("Normal (",u,",",s,")"))


####GAMMA####

shape = 4
rate = 1

x <- seq(0,30,0.01)

y <- dgamma(x, shape, rate)

ggplot(data.frame(x,y),
       aes(x = x,
           y = y)) +
  geom_point() +
  labs(title = paste0("Gamma (",shape,",",rate,")"))

####STUDENT'S T####

nu <- 3
mu = 1
sig = 2.5

x <- seq(-10,10,0.01)

y <- dstudent_t(x, nu, mu, sig)

ggplot(data.frame(x,y),
       aes(x = x,
           y = y)) +
  geom_point() +
  labs(title = paste0("Student's T (",nu,",",mu,",",sig,")"))
