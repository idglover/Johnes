####EXTRACT ONE TIMPOINT FOR EACH ANIMAL####

data_sens_age <- data_combi[data_combi$Target_Meyer == "1",]

data_sens_age <- data_sens_age[,c(4,459,
                                  58:67,
                                   69:78,
                                   80:89,
                                   91:100,
                                   102:111,
                                   113:122,
                                   124:133,
                                   135:144,
                                   146:155,
                                   157:166,
                                   168:177,
                                   179:188,
                                   190:199,
                                   201:210,
                                   212:221,
                                   223:232,
                                   234:243,
                                   245:254,
                                   256:265,
                                   267:276,
                                   278:287,
                                   289:298,
                                   300:309,
                                   311:320,
                                   322:331,
                                   333:342,
                                   344:353,
                                   355:364,
                                   366:375,
                                   377:386,
                                   388:397,
                                   399:408,
                                   410:419,
                                   421:430,
                                   432:441)]





data_sens_age <- unite(data_sens_age, dateagetitre1, c(3:12), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre2, c(4:13), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre3, c(5:14), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre4, c(6:15), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre5, c(7:16), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre6, c(8:17), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre7, c(9:18), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre8, c(10:19), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre9, c(11:20), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre10, c(12:21), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre11, c(13:22), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre12, c(14:23), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre13, c(15:24), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre14, c(16:25), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre15, c(17:26), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre16, c(18:27), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre17, c(19:28), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre18, c(20:29), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre19, c(21:30), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre20, c(22:31), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre21, c(23:32), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre22, c(24:33), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre23, c(25:34), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre24, c(26:35), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre25, c(27:36), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre26, c(28:37), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre27, c(29:38), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre28, c(30:39), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre29, c(31:40), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre30, c(32:41), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre31, c(33:42), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre32, c(34:43), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre33, c(35:44), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre34, c(36:45), remove = TRUE, sep = ":")
data_sens_age <- unite(data_sens_age, dateagetitre35, c(37:46), remove = TRUE, sep = ":")







data_sens_age <- gather(data_sens_age, covsandtitre, value, -c(calfeartag, Farm))

data_sens_age <- data_sens_age[order(data_sens_age$calfeartag),]

data_sens_age <- separate(data_sens_age, value, c("date", "age", "parity", "dim", "yield", "butterfat", "protein", "lactose", "cellcount", "titre"), sep = ":")

data_sens_age <- data_sens_age[data_sens_age$age != "NA",]

random_samples <- data.frame(calfeartag = character(0),
                             farm = character(0),
                             covsandtitre = character(0),
                             date = character(0),
                             age = numeric(0),
                             parity = numeric(0),
                             dim = numeric(0),
                             yield = numeric(0),
                             butterfat = numeric(0),
                             protein = numeric(0),
                             lactose = numeric(0),
                             cellcount = numeric(0),
                             titre = numeric(0))

for(i in 1:length(unique(data_sens_age$calfeartag))) {
  uniquetag <- unique(data_sens_age$calfeartag)[i]
  print(uniquetag)
  tempdata <- data_sens_age[data_sens_age$calfeartag == uniquetag,]
  samp <- tempdata[sample(nrow(tempdata), 1),]
  print(samp)
  random_samples[i,] <- samp
}






random_samples$age <- as.numeric(random_samples$age)


random_samples$dim <- as.numeric(random_samples$dim)

random_samples$titre <- as.numeric(random_samples$titre)

random_samples$class <- factor(ifelse(random_samples$titre >= 30, 1, 0))

####GNM AGE SPECIFIC SENSITIVITY MODEL######

 
#A <- gnm(y ~ 1 + Exp(age), family = "binomial", data = random_samples) # NOT WORKING!


######BUGS AGE SPECIFIC SENSITIVITY MODEL#####

age <- as.vector(random_samples$age)
class <- as.vector(random_samples$class)
n <- nrow(random_samples)

AgeSpecSensModel <- function() {
 
    for (i in 1:n) {
      class[i] ~ dbern(p[i])
      logit(p[i]) <- alpha - (beta * exp(-gamma*age[i]))
      
    }
    ## Priors
    alpha ~ dnorm(0.0,1.0E-4)
    beta ~ dnorm(0.0,1.0E-4)
    gamma ~ dnorm(0.0,1.0E-4)
    
    
  }






data <- list ("n", "age", "class")


inits1 <- list(alpha = 0.7, beta = 1, gamma = 0.03)
inits2 <- list(alpha = 0.2, beta = 0.5, gamma = 0.1)
inits <- list(inits1, inits2)





mod <- bugs(data,
            inits,
            parameters.to.save = c("alpha", "beta", "gamma"),
            n.iter = 100000,
            model.file = AgeSpecSensModel_dim,
            n.chains = 2,
            n.burnin = 10000,
            debug = TRUE,
            codaPkg = TRUE)

AgeSpecSensModel_coda <- read.bugs(mod)

plot(AgeSpecSensModel_coda)

gelman.diag(AgeSpecSensModel_coda)

a <- 0.6743     
b <- 22.98
c <- 0.07499

random_samples$odds <- exp(a - (b*exp(-c*random_samples$age)))
random_samples$pred <- random_samples$odds / (1 + random_samples$odds)

ggplot(random_samples, aes(x = age, y = pred)) +
  geom_point() +
  labs(x = "DIM", y = "ELISA Sensitivity")

CaliPlot(random_samples$pred, random_samples$class, nbins = 10)


###BUGS SENSITIVITY MODELS WITH COVARIATES####

dim <- as.vector(random_samples$dim)
age <- as.vector(random_samples$age)
class <- as.vector(random_samples$class)
n <- nrow(random_samples)

DIMSpecSensModel <- function() {
  
  for (i in 1:n) {
    class[i] ~ dbern(p[i])
    logit(p[i]) <- alpha * dim[i] + beta*dim[i]*dim[i]
    
  }
  ## Priors
  alpha ~ dnorm(0.0,1.0E-4)
  beta ~ dnorm(0.0,1.0E-4)
  }




AgeSpecSensModel_dim <- function() {
  
  for (i in 1:n) {
    class[i] ~ dbern(p[i])
    logit(p[i]) <- alpha - (beta * exp(-gamma*age[i])) + delta * dim[i]
 
  }
  ## Priors
  alpha ~ dnorm(0.0,1.0E-4)
  beta ~ dnorm(0.0,1.0E-4)
  gamma ~ dnorm(0.0,1.0E-4)
  delta ~ dnorm(0.0,1.0E-4)
}






data <- list("n", "age", "dim", "class")

inits1 <- list(alpha = 0.7, beta = 0.1, gamma = 0.05, delta = 0.05)
inits2 <- list(alpha = 0.2, beta = 0.2, gamma = 0.01, delta = 0.1)


#inits1 <- list(alpha = 0.7, beta = 1, gamma = 0.03, delta = 0.1)
#inits2 <- list(alpha = 0.2, beta = 0.5, gamma = 0.1, delta = 0.05)
inits <- list(inits1, inits2)





mod <- bugs(data,
            inits,
            parameters.to.save = c("alpha", "beta", "gamma", "delta"),
            n.iter = 100000,
            model.file = AgeSpecSensModel_dim,
            n.chains = 2,
            n.burnin = 10000,
            debug = TRUE,
            codaPkg = TRUE)

DIMSpecSensModel_coda <- read.bugs(mod)

AgeSpecSensModel_dim_coda <- read.bugs(mod)

plot(AgeSpecSensModel_dim_coda)

gelman.diag(AgeSpecSensModel_dim_coda)

a <- 0.001067     

random_samples$odds <- exp(a*random_samples$dim)
random_samples$pred <- random_samples$odds / (1 + random_samples$odds)

ggplot(random_samples, aes(x = age, y = pred)) +
  geom_point() +
  labs(x = "Age (m)", y = "ELISA Sensitivity")

ggplot(random_samples, aes(x = dim, y = pred)) +
  geom_point() +
  labs(x = "DIM", y = "ELISA Sensitivity")


CaliPlot(random_samples$pred, random_samples$class, nbins = 10)

