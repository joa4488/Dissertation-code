#### Simulation for Trial Design Tenscare ####
### Part 1: Simulations for initial sample size calculations ###
# Author: Joachim Dejonckheere
# Date: Sep 2022

## Initialize code ##
library(MASS) # for MVRNORM
library(mice) # for the ampute function
library(lattice) # for the Trellis plots
library (haven) # to read in SAS datasets later on

setwd("P:/JoachimD/TC/Datasets")
set.seed(1234)
size <- c(50, 70, 90, 110, 135, 180)
N.sim <- 2000
ratio <- 0.5
base <- 22 #baseline ADAS score for both groups
incr <- c(8) #Increases for the placebo group
meandiffs <- c(0, 0.15, 0.3, 0.4, 0.5) 
sds <- c(4, 6, 9) #SD
corrs <- c(0.15, 0.3, 0.45) #Correlation between 2 adjacent measurements
droprates <- c(0.3)

## Create missingness pattern matrix
pat1 <- c(y0=1, y1=1, y2=1, y3=1, y4=1, y5=1)
pat2 <- c(y0=1, y1=1, y2=1, y3=1, y4=1, y5=0)
pat3 <- c(y0=1, y1=1, y2=1, y3=1, y4=0, y5=0)
pat4 <- c(y0=1, y1=1, y2=1, y3=0, y4=0, y5=0)
pat5 <- c(y0=1, y1=1, y2=0, y3=0, y4=0, y5=0)
pat6 <- c(y0=1, y1=0, y2=0, y3=0, y4=0, y5=0)
pat <- cbind(corr=1, n=1, meandiff=1, sd=1, sim=1, trt=1,rbind(pat2, pat3, pat4, pat5, pat6))

# Perform the simulations
source("P:/JoachimD/TC/Programs/Simulation code.R") 

# Create and Export data to CSV
#fulldata <- do.call(rbind.data.frame, full)
#write.csv(fulldata, file="mmrm_full.csv", row.names = FALSE, na=".")
#rm("full", "fulldata")

#mcardata <- do.call(rbind.data.frame, mcar)
#write.csv(mcardata, file="mmrm_mcar.csv", row.names = FALSE, na=".")
#rm("mcar", "mcardata")

#mardata <- do.call(rbind.data.frame, mar)
#write.csv(mardata, file="mmrm_mar.csv", row.names = FALSE, na=".")
#rm("mar", "mardata")

#mnardata <- do.call(rbind.data.frame, mnar)
#write.csv(mnardata, file="mmrm_mnar.csv", row.names = FALSE, na=".")
#rm("mnar", "mnardata")


## Check simulated datasets whether the assumptions are accurately reflected

### Check if the SDs are reflected in the simulated datasets - OK
check_std <- as.data.frame(with(totdata_small, 
                                setNames 
                                ( aggregate (y,list(corr, n, ratio, meandiff, sd, sim, trt, time), sd),
                                  varnames)))
check_std1 <- as.data.frame(with(check_std, 
                                ( aggregate (aval,list(corr, n, ratio, meandiff, sd, trt, time), mean))))

### Check if the correlations are reflected in the simulated datasets


### The following checks (for outputing in the dissertation) will only be performed on the small sample size

totdata_small <- totdata[totdata$n == 50 & totdata$ratio == 0.7 & totdata$sd == 9,]
summary(totdata_small)

varnames <- c("corr", "n", "ratio", "meandiff", "sd", "sim", "trt", "time", "aval")

check_n <- as.data.frame(with(totdata_small, 
           setNames 
           ( aggregate (y,list(corr, n, ratio, meandiff, sd, sim, trt, time), NROW),
             varnames)))
head(check_n)

check_mean <- as.data.frame(with(totdata_small, 
                              setNames 
                              ( aggregate (y,list(corr, n, ratio, meandiff, sd, sim, trt, time), mean),
                                varnames)))

check_std <- as.data.frame(with(totdata_small, 
                              setNames 
                              ( aggregate (y,list(corr, n, ratio, meandiff, sd, sim, trt, time), sd),
                                varnames)))


#names(check_n)[names(check_n) == 'x'] <- 'n' 

## Boxplots of means over time - what does the decline look like + are they normally distributed?
### only for the smallest sample size?
bwplot(aval ~ time | meandiff * corr , 
       data=check_mean,
       groups = trt,
       type='density',
       horizontal = TRUE,
       xlab='Time',
       ylab='ADAS Score',
       layout = c(3,3),
       main='Mean ADAS score over the simulated datasets (for n=50, SD=9 and Rratio=0.7)',
       box.width = 1/3,
       panel = panel.superpose,
       panel.groups = function(x, y,..., group.number) {
         panel.bwplot(x,y + (group.number-1.5)/3,...)
       })

?bwplot
### Boxplots of SDs over time - what does the decline look like + are they normally distributed?



