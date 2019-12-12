#Exercise 10
#Population Model to investigate evolution of drug resistance in tumors
#Jake Fry and Mariana Suarez

#Initialize Variables
rN = 0.1 #normal cell daily growth rate
rM = 0.1 #mutant cell daily growth rate
K = 1000000 #carrying capacity
timesteps=750 #time (in days)
med_added = FALSE #bool to indicate if drug has been added
first_mutant = FALSE #bool to indicate if the mutant has been added

#Create Vectors (filled with 0s)
Nt=numeric(length=timesteps)
Mt=numeric(length=timesteps)

#Initialize first values
Nt[1] = 1 #Need to start at 1 for base population
Mt[1] = 0

#Populate Nt and Mt vectors
for(t in 1:(timesteps-1)){
  #Growth Equations
  Nt[t+1] <- Nt[t]+(rN*Nt[t]) * (1-((Nt[t]+Mt[t])/K))
  Mt[t+1] <- Mt[t]+(rM*Mt[t]) * (1-((Nt[t]+Mt[t])/K))
  
  #When normal count reaches 100, add a mutant cell
  if (Nt[t+1]>=100 && first_mutant != TRUE){
    Mt[t+1]=1
    first_mutant = TRUE
  }
  
  #Assume reach equilibrium at time 300 days
  if (t==300 && med_added!=TRUE){
    #Change the rates to be that appropriate to the new growth rates when drug is applied
    rN = -.1
    rM = 0.05
    med_added = TRUE
  }
}

#Create dataframe
normalPopu<-data.frame(time=1:length(Nt),Nt=Nt)
mutantPopu<-data.frame(time=1:length(Mt),Mt=Mt)

#Plot
library(ggplot2)
simPlot <- ggplot()+geom_line(data=mutantPopu,aes(x=time,y=Mt),col='red')+geom_line(data=normalPopu,aes(x=time,y=Nt),col='blue')
simPlot <- sim +labs(y="Number of Cells",x="Time (days)",title="Cell Population Simulation") 
simPlot
