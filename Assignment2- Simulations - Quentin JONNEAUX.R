# Create empty vectors to store 1000 simulations of deviations of x and y axis for each day
End.day.dev.x <- rep(NA,1000)
End.day.dev.y <- rep(NA,1000)

for(j in 1:1000){# run the random walk 1000 times
  
  align.x <- 0 # x aligned at start of day
  align.y <- 0 # y aligned at start of day
  # For a batch of 220 samples
  for(i in 1:220){
    # Apply a random deviation per distribution and angles provided
    delta.r <-rnorm(1,0.05, 0.13) # Store needle movement deviation
    delta.theta <- runif(1, (-pi/8), (3*pi/8)) # Store needle angular deviation
    dev.x <- delta.r * cos(delta.theta) # random deviation in x direction (applying formula to convert polar to cartesian coords)
    dev.y <- delta.r * sin(delta.theta) # random deviation in y direction (applying formula to convert polar to cartesian coords)
    align.x <- align.x+dev.x # add this to previous steps - current alignment x
    align.y <- align.y+dev.y # add this to previous steps - current alignment y
  }
  # Store deviations for each dimensions in vectors
  End.day.dev.x[j] <- align.x
  End.day.dev.y[j] <- align.y
} 

#create data frame for ggplot
dev.data <- data.frame(Dev.x = End.day.dev.x, Dev.y = End.day.dev.y) 
library(ggplot2)

#create scatterplot of final allignments and show limit spec
ggplot(dev.data, aes(x=Dev.x, y=Dev.y))+
  geom_point(aes(colour = ifelse(Dev.x<8.5 & Dev.y < 3.5, "Valid", ifelse(Dev.x < 10 & Dev.y < 5, "Innacurate", "Damaged"))))+
  geom_hline(yintercept=3.5, color = "orange")+
  annotate("text", x=2, y=3.6, label="innacurate", color="orange")+
  geom_hline(yintercept=5, color = "red")+
  annotate("text", x=2, y=5.1, label="Fail", color="red")+
  geom_vline(xintercept=8.5, color = "orange")+
  annotate("text", x=8.5, y=7.5, label="innacurate", color="orange")+
  geom_vline(xintercept=10, color = "red")+
  annotate("text", x=10, y=7.5, label="Fail", color="red")+
  labs(title = "Deviation simulations scatterplot") +
  guides(color = guide_legend(title = "End of day machine states")) 

# Filter data for machine being inacurate or damaged
in.or.fail.data <- filter(dev.data,End.day.dev.x>=8.5 | End.day.dev.y>=3.5)
# Dividing count on simulation total
nrow(in.or.fail.data)/1000

# Creating hist with innacurate line to analyse distribution
hist(End.day.dev.x, main = "Histogram of Deviations on X axis at end of day", xlab = "Deviations in mm")
abline(v=8.5,col="red")
hist(End.day.dev.y, main = "Histogram of Deviations on Y axis at end of day", xlab = "Deviations in mm")
abline(v=3.5,col="red")

#####################################

#function to just calculate how many runs it takes before alignment is out of spec
# if it remains within spec, function return the value n+1
when.outside <- function(n){#simulate if/when misalignment occurs
  delta.r <-rnorm(n,0.05, 0.13) # Store needle movement deviation
  delta.theta <- runif(n, (-pi/8), (3*pi/8)) # Store needle angular deviation
  dev.x <- delta.r * cos(delta.theta) # random deviation in x direction (applying formula to convert polar to cartesian coords)
  dev.y <- delta.r * sin(delta.theta) # random deviation in y direction (applying formula to convert polar to cartesian coords)
  # Store cumulative sums of each deviations
  cum.dev.x <- cumsum(dev.x)
  cum.dev.y<- cumsum(dev.y)
  
  index <- 1:n
  # Store cumulative sum of test when machine becomes innacurate (if 221, always passing)
  nretest.dev.x <-ifelse(is.na(index[cum.dev.x>=8.5][1]),n+1,index[cum.dev.x>=8.5][1])
  nretest.dev.y <-ifelse(is.na(index[cum.dev.y>=3.5][1]),n+1,index[cum.dev.y>=3.5][1])
  nretest <- ifelse(nretest.dev.x >= nretest.dev.y, nretest.dev.y, nretest.dev.x)
  # Store cumulative sum of test when machine becomes damaged (if 221, always passing)
  nfail.dev.x <-ifelse(is.na(index[cum.dev.x>=10][1]),n+1,index[cum.dev.x>=10][1])
  nfail.dev.y <-ifelse(is.na(index[cum.dev.y>=5][1]),n+1,index[cum.dev.y>=5][1])
  nfail <- ifelse(nfail.dev.x >= nfail.dev.y, nfail.dev.y, nfail.dev.x)
  
  # return vectors of number of tests when innacurate and when damaged
  nums <- c(nretest,nfail)
}

# Replicate 1000 for simulation
sim <- replicate(1000, when.outside(220))
#Store each vector
retest.row <-sim[1,]
fail.row <-sim[2,]
# Create a data frame with those vector
sim2 <-data_frame(retest=numeric(1000),fail=numeric(1000))
sim2$retest <- retest.row
sim2$fail <- fail.row

# when misaligned (n+1 [201] implies still within bounds at end of day)
hist(sim2$retest, main = "Histogram of test when test becomes innacurate", xlab = "Number of tests")
sum(sim2$retest<221)/1000
#60% will become misaligned at some point in the day
hist(sim2$fail, main = "Histogram of test when machine becomes damaged", xlab = "Number of tests")
sum(sim2$fail<221)/1000


profit <- function(n){#function to simulate profit/loss
  ofline.cost <- 10000
  run.profit <- 200
  run.retest <- 300
  run.loss <- 500
  sim <- replicate(1000, when.outside(n))#simulate 1000 times
  # Create a cost vector with profit calculation depending on scenario
  costvec <- ifelse(sim[1,]<=n,
                    ifelse(sim[2,]<=n,
                           run.profit*sim[1,]-run.retest*((n-sim[1,])-(n-sim[2,]))-run.loss*(n-sim[1,])-ofline.cost,
                           run.profit*sim[1,]-run.retest*(n-sim[1,])-run.loss*(n-sim[1,])),
                    n*run.profit)
  mean(costvec)#calculate mean profit over all simulations
}

# Expected profits for simulation
profit(220)

#run this expected mean profit simulation for various values if target n
N=0:220
ECOST = sapply(N, profit)

#visualise results
plot(N, ECOST, main = "Expected Profits against Number of runs per day scatterplot",xlab ="Number of runs per day", ylab ="Expected profit in Euro")
abline(h = max(ECOST), col="red")
text(x=5, y=max(ECOST), 'Max Expected Cost')
abline(v = N[which.max(ECOST)], col="red")
text(x=N[which.max(ECOST)], y=0, 'Optimal Number of test')
abline(h = ECOST[length(ECOST)], col="red")
text(x=5, y=ECOST[length(ECOST)], 'Expected Cost for whole batch')
abline(v = max(N), col="red")
text(x=max(N), y=0, 'Whole Batch')



# Expected Max Profit
max(ECOST)
# Optimal number of runs per day
N[ECOST==max(ECOST)]
