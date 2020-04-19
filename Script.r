#Warm-Up: Linear Regression Pre-Lab
library(ggplot2)

#This dataset of calories burned and other variables

myData = read.csv("exercise.csv")

#------------------------------------------------------------------------------- 
# Two group comparison - predicting calories burned by gender
#------------------------------------------------------------------------------- 
#variables from dataset
xGrp="Gender"
yName="Calories"
fileNameRoot = "Exercise-Ttest-" 
graphFileType = "eps" 

# Data exploration

Fm <- subset(myData, Gender == "female")
M <- subset(myData, Gender == "male")

par(mfrow=c(1,2))
plot(density(Fm$Calories), main = "Female")
plot(density(M$Calories) , main = "Male")

#for reference
#plot(density(Fm$Duration), main = "Female")
#plot(density(M$Duration) , main = "Male")


#------------------------------------------------------------------------------- 

#Fisherian t- test
t.test(Calories~Gender, data=myData)


#------------------------------------------------------------------------------- 
#Bayesian analysis

source("Jags-Ymet-Xnom2grp-MrobustHet.R")

# Generate the MCMC chain:
#startTime = proc.time()
mcmcCoda = genMCMC( data=myData , xName=xGrp , yName=yName , 
                     numSavedSteps=20000 , saveName=fileNameRoot )
#stopTime = proc.time()
#duration = stopTime - startTime
#show(duration)
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , 
                        compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda2 , data=myData , xName=xgrp , yName=yName , 
          compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 

#------------------------------------------------------------------------------- 
# Linear regression comparison - predicting calories by exercise duration
#------------------------------------------------------------------------------- 


par(mfrow=c(1))
#How will that impact our model and subsequent analysis?
lattice::xyplot(Duration~Calories, data = myData,xlab = "Calories",
                ylab = "Duration (min)",
                type = c("p","r"),
                main = "Calories burned by exercise time")


#fisherian linear model
slrFisher = lm(Duration~Calories, data = myData)
summary(slrFisher)
confint(slrFisher)

#look at variable dist

par(mfrow=c(1,2))
plot(density(myData$Calories), main = "calories")
plot(density(myData$Duration) , main = "Duration")

#------------------------------------------------------------------------------- 

xName="Duration"
yName="Calories"
fileNameRoot = "Exercise-SLR-" 
graphFileType = "eps" 

source("Jags-Ymet-Xmet-Mrobust.R")

mcmcCoda2 = genMCMC( data=myData , xName=xName , yName=yName , 
                    numSavedSteps=20000 , saveName=fileNameRoot )


#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda2) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda2 , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda2 , 
                        compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda2 , data=myData , xName=xName , yName=yName , 
          compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 

