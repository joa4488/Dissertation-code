# Author: Joachim Dejonckheere
# Purpose: Dissertation for Paster of Science in Statistical Data Analysis
# Academic year: 2023-2024

j <- 0
for(meandiff in meandiffs)
{ 
  full <- list()
  mcar <- list()
  mar <- list()
  mnar <- list()
  
  for(n in size)
  { 
  # for(incr in incrs)
  #{ 
      for(sd in sds)
      { 
        for(corr in corrs)
        { 
          ## Calculated covariance for 2 adjacent measurements  
          cov1 <- corr * (sd^2)
          cov2 <- (corr^2) * (sd^2)
          cov3 <- (corr^3) * (sd^2)
          cov4 <- (corr^4) * (sd^2)
          cov5 <- (corr^5) * (sd^2)
          
          ## Assuming autocorrelation 
          row1 <- c(sd^2, cov1, cov2, cov3, cov4, cov5)
          row2 <- c(row1[2],row1[1:5])
          row3 <- c(row1[3:2],row1[1:4])
          row4 <- c(row1[4:2],row1[1:3])
          row5 <- c(row1[5:2],row1[1:2])
          row6 <- c(row1[6:2],row1[1:1])
          cov <- matrix( c(row1, row2, row3, row4, row5, row6), 
                         nrow=6, ncol=6)
          
          for(i in 1:N.sim)
          { 
            j <- j+1            
            
            #Simulate dataset
            basevector0 <- c(y0=base, y1=base+1/5*incr, y2=base+2/5*incr, y3=base+3/5*incr, 
                             y4=base+4/5*incr, y5=base+5/5*incr )#linear increase for placebo
            basevector1 <- c(y0=base, y1=base+1/5*incr*(1-meandiff), y2=base+2/5*incr*(1-meandiff),
                             y3=base+3/5*incr*(1-meandiff), y4=base+4/5*incr*(1-meandiff),
                             y5=base+5/5*incr*(1-meandiff))
                             #slower increase for active arm (with threatment effect also growing gradually)
            data0 <- cbind(trt=0, mvrnorm(round(n*(1-ratio)), mu=basevector0, Sigma=cov))
            data1 <- cbind(trt=1, mvrnorm(round(n*ratio), mu=basevector1, Sigma=cov))
            
            widedata <- data.frame (corr, n, meandiff, sd, sim=i, rbind (data0,data1))
            
            # cumulate all data in a list
            ## Complete data
            full [[j]]<- widedata
            
            ## MCAR data
            mcar [[j]] <- ampute (widedata,
                                  prop= 0.3,
                                  patterns=pat,
                                  mech = 'MCAR')$amp
            ## MAR data
            ## Create weight matrix for MAR
            wmar2 <- c(y0=0, y1=0, y2=0, y3=0.2, y4=0.8, y5=0)
            wmar3 <- c(y0=0, y1=0, y2=0.2, y3=0.8, y4=0, y5=0)
            wmar4 <- c(y0=0, y1=0.2, y2=0.8, y3=0, y4=0, y5=0)
            wmar5 <- c(y0=0.2, y1=0.8, y2=0, y3=0, y4=0, y5=0)
            wmar6 <- c(y0=1, y1=0, y2=0, y3=0, y4=0, y5=0)
            wmar <- cbind(corr=1, n=1, meandiff=1, sd=1, sim=1, trt=1, rbind(wmar2, wmar3, wmar4, wmar5, wmar6))
            
            mar [[j]] <- ampute (widedata,
                                  prop= 0.3,
                                  patterns=pat,
                                  cont = TRUE,#Continuous weight function
                                  weights = wmar,
                                  type = c('RIGHT','RIGHT','RIGHT','RIGHT','RIGHT'),# A high weighted sums score enlarges the probability of being dropout
                                  mech = 'MAR')$amp

            ## MCAR data
            ## Create weight matrix for MCAR
            wmnar <- abs(pat - 1) # We specify the weights for MNAR as the apposite of the pattern matrix
            
            mnar [[j]] <- ampute (widedata,
                                 prop= 0.3,
                                 patterns=pat,
                                 cont = TRUE,#Continuous weight function
                                 weights = wmnar,
                                 type = c('RIGHT','RIGHT','RIGHT','RIGHT','RIGHT'),# A high weighted sums score enlarges the probability of being dropout
                                 mech = 'MNAR')$amp
          }
        }
      }
  }
  # Create and Export data to CSV
  fulldata <- do.call(rbind.data.frame, full)
  fullcsv <-gsub(" ", "", paste ("mmrm_full_",meandiff,".csv"))
  write.csv(fulldata, file=fullcsv, row.names = FALSE, na=".")
  rm("full", "fulldata")
  
  mcardata <- do.call(rbind.data.frame, mcar)
  mcarcsv <-gsub(" ", "", paste ("mmrm_mcar_",meandiff,".csv"))
  write.csv(mcardata, file=mcarcsv, row.names = FALSE, na=".")
  rm("mcar", "mcardata")
  
  mardata <- do.call(rbind.data.frame, mar)
  marcsv <-gsub(" ", "", paste ("mmrm_mar_",meandiff,".csv"))
  write.csv(mardata, file=marcsv, row.names = FALSE, na=".")
  rm("mar", "mardata")
  
  mnardata <- do.call(rbind.data.frame, mnar)
  mnarcsv <-gsub(" ", "", paste ("mmrm_mnar_",meandiff,".csv"))
  write.csv(mnardata, file=mnarcsv, row.names = FALSE, na=".")
  rm("mnar", "mnardata")
  
  }
#}

## Create long format of the data
#longdata <- reshape(cumudata, varying=c("y1", "y2", "y3", "y4", "y5"),
#                    direction="long", sep="", idvar="usubjid")
#totdata <- longdata[order(longdata$corr, longdata$n, longdata$incr,
#                           longdata$meandiff,longdata$sd,longdata$sim,
#                           longdata$usubjid, longdata$time),]




