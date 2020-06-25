library(truncnorm)

# Data Generation Process
sim.time=function(nbuyer=100, prob.type=c(0.4, 0.6), 
                  pay.rate=3600/40, 
                  make.rate=c(3600/40, 3600/90), 
                  arrv.rate=50, 
                  N.fn=function() 1, p.ratio=0){
  # make time: (Type1) 40sec in hour (Type2) 90sec in hour
  arr.time = rep(0, nbuyer+1)
  serv.time = rep(0, nbuyer+1)
  type=matrix(0, nrow=2, ncol=nbuyer+1)
  
  for(i in 2:(nbuyer+1)){
    # Arrival time
    arr.time[i] = arr.time[i-1] + rexp(1, arrv.rate)
    
    # Calculate service time
    # drinks type
    N = N.fn()
    t = sample(c(1, 2), N , T, prob=prob.type)
    type[1, i] = length(which(t==1))
    type[2, i] = length(which(t==2))
    
    # sum up all services
    serv.time[i] = rexp(1, pay.rate) * 
      ifelse(p.ratio>0, sample(c(0, 1), T, prob=c(p.ratio, 1-p.ratio)), 1) +  # Payment skip
      type[1, i] * rexp(1, make.rate[1]) + 
      type[2, i] * rexp(1, make.rate[2])
  }
  out=list(ArrivalTime=arr.time[-1], 
           ServiceTime=serv.time[-1], 
           Type=type[, -1])
}

sim.time.norm=function(nbuyer=100, 
                       prob.type=c(0.4, 0.6), 
                       pay.rate=3600/40, 
                       pay.std=40,
                       make.rate=c(3600/40, 3600/90), 
                       make.std=c(10, 10),
                       arrv.rate=50, 
                       N.fn=function() 1, p.ratio=0){
  # make time: (Type1) 40sec in hour (Type2) 90sec in hour
  arr.time = rep(0, nbuyer+1)
  serv.time = rep(0, nbuyer+1)
  type=matrix(0, nrow=2, ncol=nbuyer+1)
  
  for(i in 2:(nbuyer+1)){
    # Arrival time
    arr.time[i] = arr.time[i-1] + rexp(1, arrv.rate)
    
    # Calculate service time
    # drinks type
    N = N.fn()
    t = sample(c(1, 2), N , T, prob=prob.type)
    type[1, i] = length(which(t==1))
    type[2, i] = length(which(t==2))
    
    # sum up all services
    serv.time[i] = rtruncnorm(1, 0, Inf, pay.rate, pay.std) * 
      ifelse(p.ratio>0, sample(c(0, 1), T, prob=c(p.ratio, 1-p.ratio)), 1) +  # Payment skip
      type[1, i] * rtruncnorm(1, 0, Inf, make.rate[1], make.std[1]) + 
      type[2, i] * rtruncnorm(1, 0, Inf, make.rate[2], make.std[2])
  }
  out=list(ArrivalTime=arr.time[-1], 
           ServiceTime=serv.time[-1], 
           Type=type[, -1])
}
# Case function
Q.fn=function(qseq=c(1:5), nbuyer=100){
  q = sample(c(qseq, Inf), nbuyer, 
             prob=c(rep(1/length(qseq), length(qseq)),0.5), replace=T)
  return(q)
}

# 2) Drink amount simulate (sampling)
N.fn=function(maxN){ # No argument
  other.p = 0.5/(maxN-1)
  n = sample(c(1:maxN), 1, F, prob=c(0.5, rep(other.p, (maxN-1))))
  return(n)
}

# Simulation with scenario
sim.original=function(sim=sim.time(100), nserver=2){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  serverlog = rep(1, nbuyer)
  
  # caculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }
  
  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){
      start.time[i] = min(end.time.sofar)
      end.time[i] = start.time[i] + serv.time[i]
    } else {
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }
  
  # Count the quening size in Queue
  for (i in 1:nbuyer){
    if (start.time[i] > arr.time[i]){
      waiting = which(start.time[1:(i-1)] > arr.time[i])
      Qsofar[i]=length(waiting)
    }
  }
  
  # Check the server from where
  server.total = c(1:nserver)
  for (i in 2:nbuyer){
    busy = serverlog[which(end.time[1:i-1] > start.time[i])] #1 to nserver
    serverlog[i] <- min(server.total[!server.total %in% busy])
  }
  
  sim.table = cbind(#sapply(sim$ArrivalTime, clock),
                    sim$ArrivalTime,
                    matrix(serverlog, ncol=1),
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(start.time, ncol=1), 
                    sim$ServiceTime, 
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#Server', '#Type1', '#Type2', 'StartTime', 'ServiceTime', 'EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem')
  return(sim.table)
}
sim.1=function(sim=sim.time(100), nserver=2, balk.fn=Q.fn){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  Q.result=rep(1, nbuyer) # 1 means remaining
  serverlog = rep(1, nbuyer)
  
  
  # Pre-requisites: 
  ## 1)calculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }
  ## 2)calculate the giveup thresholds
  Q = balk.fn(nbuyer=nbuyer)
  
  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    # Check waiting length
    queueingNow = length(which(start.time[1:(i-1)] > arr.time[i]))
    Qsofar[i] = queueingNow
    
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){ # Waiting
      if(queueingNow > Q[i]){ # Balking
        start.time[i] = arr.time[i]
        serv.time[i] = 0
        end.time[i] = arr.time[i]
        Q.result[i] = 0 
      } else { # Accept waiting
        start.time[i] = min(end.time.sofar)
        end.time[i] = start.time[i] + serv.time[i]
      }
    } else { # Servers available
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }
  
  server.total = c(1:nserver)
  for (i in 2:nbuyer){
    busy = serverlog[which(end.time[1:i-1] > start.time[i])] #1 to nserver
    serverlog[i] <- min(server.total[!server.total %in% busy])
  }
  
  sim.table = cbind(sim$ArrivalTime, 
                    matrix(serverlog, ncol=1),
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(start.time, ncol=1), 
                    matrix(serv.time, ncol=1),
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1), 
                    matrix(Q.result, ncol=1),
                    matrix(Q, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#server', '#Type1', '#Type2', 'StartTime','ServiceTime','EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem', 'Balk', 'BalkThres')
  return(sim.table)
}
sim.2=function(sim=sim.time(100), nserver=2, forbid.thres=Inf){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  K.result=rep(1, nbuyer)
  serverlog = rep(1, nbuyer)
  
  # Pre-requisites: 
  ## 1)calculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }
  
  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    # Check waiting length
    queueingNow = length(which(start.time[1:(i-1)] > arr.time[i]))
    Qsofar[i] = queueingNow
    
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){ # Waiting
      if(queueingNow > forbid.thres){ # Forbiden, out of waiting space
        start.time[i] = arr.time[i]
        serv.time[i] = 0
        end.time[i] = arr.time[i]
        K.result[i] = 0 
      } else { # Accept waiting
        start.time[i] = min(end.time.sofar)
        end.time[i] = start.time[i] + serv.time[i]
      }
    } else { # Servers available
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }

  server.total = c(1:nserver)
  for (i in 2:nbuyer){
    busy = serverlog[which(end.time[1:i-1] > start.time[i])] #1 to nserver
    serverlog[i] <- min(server.total[!server.total %in% busy])
  }
  
  sim.table = cbind(sim$ArrivalTime,
                    matrix(serverlog, ncol=1),
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(start.time, ncol=1), 
                    matrix(serv.time, ncol=1),
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1),
                    matrix(K.result, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#server', '#Type1', '#Type2', 'StartTime','ServiceTime','EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem', 'SpaceAvail')
  return(sim.table)
}
sim.3=function(sim=sim.time(100), nserver=2, balk.fn=Q.fn, forbid.thres=Inf){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  QK.result=rep(1, nbuyer)
  serverlog = rep(1, nbuyer)
  
  # 1 for (Available Space & Accept Waiting)
  # 0 for (Reject Waiting)
  # -1 for (No avaiable Space)...Not sure the waiting decision
  
  # Pre-requisites: 
  ## 1)calculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }
  ## 2)calculate the giveup thresholds
  Q = balk.fn(nbuyer=nbuyer)
  
  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    # Check waiting length
    queueingNow = length(which(start.time[1:(i-1)] > arr.time[i]))
    Qsofar[i] = queueingNow
    
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){ # Waiting
      if(queueingNow > forbid.thres){ # Forbidding
        start.time[i] = arr.time[i]
        serv.time[i] = 0
        end.time[i] = arr.time[i]
        QK.result[i] = -1 
      } else if(queueingNow > Q[i]){ # Balking
        start.time[i] = arr.time[i]
        serv.time[i] = 0
        end.time[i] = arr.time[i]
        QK.result[i] = 0 
      } else { # Accept waiting
        start.time[i] = min(end.time.sofar)
        end.time[i] = start.time[i] + serv.time[i]
      }
    } else { # Servers available
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }
  
  server.total = c(1:nserver)
  for (i in 2:nbuyer){
    busy = serverlog[which(end.time[1:i-1] > start.time[i])] #1 to nserver
    serverlog[i] <- min(server.total[!server.total %in% busy])
  }
  
  sim.table = cbind(sim$ArrivalTime, 
                    matirx(serverlog, ncol=1),
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(start.time, ncol=1), 
                    matrix(serv.time, ncol=1),
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1),
                    matrix(QK.result, ncol=1),
                    matrix(Q, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#server', '#Type1', '#Type2', 'StartTime','ServiceTime','EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem', 'Entering', 'BalkThres')
  return(sim.table)
}
sim.4=function(sim=sim.time(100, N.fn=function() N.fn(MAXN)), nserver=2){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  serverlog = rep(1, nbuyer)

  # caculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }
  
  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){
      start.time[i] = min(end.time.sofar)
      end.time[i] = start.time[i] + serv.time[i]
    } else {
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }
  
  # Count the quening size in Queue
  for (i in 1:nbuyer){
    if (start.time[i] > arr.time[i]){
      waiting = which(start.time[1:(i-1)] > arr.time[i])
      Qsofar[i]=length(waiting)
    }
  }
  
  server.total = c(1:nserver)
  for (i in 2:nbuyer){
    busy = serverlog[which(end.time[1:i-1] > start.time[i])] #1 to nserver
    serverlog[i] <- min(server.total[!server.total %in% busy])
  }
  
  
  sim.table = cbind(sim$ArrivalTime, 
                    matrix(serverlog, ncol=1),
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(start.time, ncol=1), 
                    sim$ServiceTime, 
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#server','#Type1', '#Type2', 'StartTime', 'ServiceTime', 'EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem')
  return(sim.table)
}





# Clock display
clock=function(hours){
  min=60*(hours-floor(hours))
  sec=round(60*(min-floor(min)), 2)
  return(paste(floor(hours), floor(min), round(60*(min-floor(min))), sep=':'))
}

