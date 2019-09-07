RandomLoadingTime = function (){ #Generates loading time from uniform random variable
  rv = runif(1,0,1)
  if(rv < 0.3){
    return (5)
  }
  else if (rv < 0.8 ){ 
    return (10)
  }
  else return (15)
}

RandomWeighingTime = function(){ #Generates weighing time from uniform random variable
  rv = runif(1,0,1)
  if(rv < 0.7){
    return (12)
  }
  else return (16)
}

RandomTravelTime = function(){ #Generates travel time from uniform random variable
  rv = runif(1,0,1)
  if(rv < 0.4){
    return (40)
  }
  else if (rv < 0.7){
    return(60)
  }
  else if(rv < 0.9){
    return (80)
  }
  else return(100)
}

rl = 1 #number of simulations
end = 1000 #End time of simulation
BL = rep(0,length = rl) #Create vector to record total busy time for loaders from time 0 to end
BW = rep(0, length = rl) #Create vector to record total busy time of scales from time 0 to end

for(i in 1:rl){ #Initiate sweep of simulations
  
  print(c("Starting Sweep", i))
  t = 0 #Clock set to time of first arrival
  LQ = 3 #Number of trucks in loading queue at t = 0
  L = 2 #Number of trucks being loaded at t = 0
  WQ = 0 #Number of trucks in weighing queue at t = 0
  W = 1 #Number of trucks being weighed at t = 0
  SCL1 = 0 #Initialise service commencement time with one truck being loaded
  SCL2 = 0 #Initialise service commencement time with two trucks being loaded
  SCW = 0 #Initialise service commecement for weighing scales
  totalDeliveries = 0 #Initialise variable to keep track of completed deliveries
  
  FEL = data.frame(times = c(RandomLoadingTime(), RandomTravelTime(),  RandomWeighingTime(), end), event = c("EL","ALQ", "EW", "End"))
  #Generate Future Event List including travel time, loading time, weighing time, and end event
  
  event = "EL" #Nature of first event
  
  while(event != "End"){ #Continuing procedure until end event
    
    FEL = FEL[order(FEL[,1]),] #Order the Future Event List so successive events appear as successive rows
    t = FEL[1,1] #Update clock to new time of next event
    event = FEL[1,2] #Record event type
    FEL[1,] = NA #Remove current event from future event list
    
    if(event == "EL"){ #Initiate logic for circumstance next event is end of loading
      
      print(c("Event: End of Loading at time:",t)) #Print time of end loading event
      
      if(LQ == 0){ #Event logic if there is no loading queue for the loader at time of end loading event
        
        if(L == 2){ #Event logic if there were two trucks being loaded at time of end loading event
          
          L = L-1 #Reducing number of trucks being loaded as loading queue is empty
          BL[i] = BL[i] + t + t - SCL2 - SCL2 #Update time loader has been busy with two trucks
          SCL2 = 0 #Resetting commencement time for two trucks being loaded to 0
          SCL1 = t #Starting the timer for one truck being loaded
          
        } #End of event logic for end loading event with two trucks being load and loading queue = 0
        
        else if(L == 1){ #Event logic if there was only one truck being loaded at time of end loading event
          
          L = L-1 #Reducing number of trucks being loaded as laoding queue is empty
          BL[i] = BL[i] + t - SCL1 #Update time loader has been b0usy with one truck
          SCL1 = 0 #Reset commencement time of one truck being loaded to 0
          
        } #End of event logic for end loading event with one truck being load and loading queue = 0
      }
      
      else if(LQ > 0){ #Event logic if there is a loading queue at time of end loading event
        
        if(L == 2){ #Event logic if there were two trucks being loaded at time of end loading event
          
          LQ = LQ-1 #Reduce number of trucks in loading queue
          BL[i] = BL[i] + t + t - SCL2 -SCL2 #Update time loader has been busy with two trucks
          SCL2 = t #Resetting commencement time for two trucks being loaded to the current time
          newEL = data.frame(times = c(t + RandomLoadingTime()), event = c("EL")) #Create new end loading event for incoming truck
          FEL = rbind(FEL, newEL) #Add new end loading event to future event list
          
        }
        
        else if(L == 1){ #Event logic if there was one truck being loaded at time of end loading event
          
          LQ = LQ-1 #Reduce number of trucks in loading 
          SCL1 = t #Resetting commecement time for one truck being loaded to the current time
          newEL = data.frame(times = c(t + RandomLoadingTime()), event = c("EL")) #Create new end loading event for incoming truck
          FEL = rbind(FEL, newEL) #Add new end loading event to future event list
          
        }
        
      }
      
      if(W == 1){ #Event Logic for truck finished loading going to occupied weighing scales
        
        WQ = WQ + 1 #Truck joins weighing queue
        
      }
      
      else if(WQ == 0){ #Event logic for truck finished loading going to empty weighing scales
        
        W = 1 #Truck begins being weighed
        SCW = t #Set time of weighing commencement to be the current time
        newEW = data.frame(times = c(t + RandomWeighingTime()), event = c("EW")) #Create new end weighing event for incoming truck
        FEL = rbind(FEL, newEW) #Add new end weighing event to future event list
        
      }
      
      else if(WQ > 0){ #Event logic if  there is a weighing queue and there is no truck currently being weighed
        
        w = 1
        SCW = t #Set time of weighing commencement to be the current time
        newEW = data.frame(times = c(t + RandomWeighingTime()), event = c("EW"))
        #Create new end weighing event for truck at front of queue
        FEL = rbind(FEL, newEW) #Add new end weighing event to future event list
        
      }
      
    } #End logic for end loading event
    
    if(event == "EW"){ #Event logic for end weighing event
      
      print(c("Event: End of Weighing at time:",t)) #Print time of end weighing event
      BW[i] = BW[i] + t - SCW #Update busy time of weighing scales
      
      if(WQ == 0){ #Event logic for end weighing event if weighing queue is empty
        W = 0 #Scales is idle if queue is empty
      }
      
      else if(WQ > 0){ #Event logic for end weighing event if queue is not empty
        
        WQ = WQ - 1 #Weighing queue decreases by one as next truck goes to scales
        SCW = t #Weighing commencement time is set to current time
        newEW = data.frame(times = c(t + RandomWeighingTime()), event = c("EW"))
        #Create new end weighing event for truck at front of queue
        FEL = rbind(FEL, newEW) #Add new end weighing event to future event list
        
      } 
      
      newALQ = data.frame(times = c(t + RandomTravelTime()), event = c("ALQ")) 
      # Create new arrival at loading queue event for truck that has ended weighing
      FEL = rbind(FEL, newALQ) #Add new end weighing event to future event list
      
    } #End logic for end weighing event
    
    if(event == "ALQ"){ #Event logic for arrival at loading queue event
      
      print(c("Event: Arrival at Loading Queue at time:",t)) #Print time of arrival at loading queue event
      totalDeliveries = totalDeliveries + 1 #Increase total deliveries by 1 for arrival at loading queue event
      
      if(L == 2){ #Event logic for arrival at loading queue event if both loaders are busy
        
        LQ= LQ + 1 #Increase loading queue by one as loaders are busy
        
      } #End logic for arrival at loading queue with both loaders busy
      
      else if (LQ == 0){ #Event Logic for arrival at loading queue event if there is no queue
        
        if(L == 0){ #Event logic for arrival at loading queue event if there are no trucks currently being loaded
          
          L = L + 1	#Increase trucks being loaded by 1 for newly arrived truck
          SCL1 = time	#Set time of loading commencement for 1 truck equal to the current time
          newEL = data.frame(times = c(t + RandomLoadingTime()), event = c("EL")) 
          #Create new end loading event for newly arrived truck		
          FEL = rbind(FEL,newEL) #Add new end loading event to future event list
          
        } #End of event logic for arrival at loading queue event with no queue and empty loaders
        
        else if(L == 1){ #Event logic for arrival at loading queue event with one truck currently being loaded
          
          L = L + 1	#Increase trucks being loaded by 1
          BL[i] = BL[i] + t - SCL1 #Add current truck's loading time to total loading time
          SCL1 = 0 #Set loading time for one truck being loaded to 0
          SCL2 = t #Set commencement time for 2 trucks being loded to current time
          newEL = data.frame(times = c(t + RandomLoadingTime()), event=c("EL"))	 #Create new end loading event for newly arrived truck	
          FEL = rbind(FEL,newEL) #Add new end loading event to future event list
          
        } #End of event logic for arrival at loading queue event with one truck being loaded
        
      }	#End of event logic for arrival at loading queue event with no queue
      
      else if((LQ > 0) && (L == 1)) { #Event logic for arrival at loading queue event with a queue and one truck being loaded
        
        L = L + 1 #Add truck to loaders
        BL[i] = BL[i] +t - SCL1 #Add current trucks loading time to total loading time
        SCL1 = 0 #Set loading time for one truck being loaded to 0
        SCL2 = t #Set commencement time for 2 trucks being loded to current time		
        newEL = data.frame(times = c(t + RandomLoadingTime()), event=c("EL"))		
        #Create new end loading event for truck entering loader from queue
        FEL = rbind(FEL,newEL) #Add new end loading event to future event list
        
      }				
      
      else if((LQ > 0) && (L == 0)){ #Event logic for arrival at loading queue event with a queue and no trucks being loaded
        L = L + 1	#Add truck to loader
        LQ = LQ - 1	#Decrease loading queue by 1
        SCL1 = time	 #Start loading timer for 1 truck being loaded.		
        newEL = data.frame(times = c(t + RandomLoadingTime()), event=c("EL"))		
        #Create new end loading event for truck entering loader from queue
        FEL = rbind(FEL,newEL) #Add new end loading event to future event list
        
      }	#End of event logic for arrival at loading queue event with a queue and two empty loaders
      
    }	#End of event logic for arrival at loader queue event
    
    if(event == "End"){	#Event logic for end event
      
      print(c("Event: End of Simulation at time:",t)) #Print time of simulation end
      
      if(L == 1){	#Event logic for one truck being loaded at end event
        
        BL[i] = BL[i] + t - SCL1 #Update total busy time for loaders for one truck in loader at time of end event		
        
      }								
      
      if(L == 2){	#Event logic for two trucks being loaded at end event
        
        BL[i] = BL[i] + t + t - SCL2 - SCL2 #Update total busy time for loaders for two trucks in loader at time of end event
        
      }	#End event logic for 2 trucks being loaded at time of end event
      
      if(W == 1){	#Event logic for truck being weighed at time of end event
        
        BW[i] = BW[i] + t - SCW #Update total busy time of scales for truck being weighed at time of end event
        
      }	
    } #End of event logic for end event
    
    FEL=na.omit(FEL) #Remove any empty records from future event list
    
    #Print values of system state variables at the occurrence of each event
    print(c('Trucks in loading queue:',LQ))
    print(c('Trucks being loaded:',L))
    print(c('Trucks in weighing queue:',WQ))
    print(c('Trucks being weighed:',W))
    print(c('Total Deliveries Completed:', totalDeliveries))
    writeLines("  ")
    
    
  }	#End sweep
  
  print(c("END OF SIMULATION STATISTICS:"))
  print(c('Total Deliveries Completed:', totalDeliveries))
  print(c('Total Weighing Time:',BW[1]))
  print(c('Total Loading Time:', BL[1]))
  
}