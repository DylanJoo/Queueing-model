# Simulation
source('sim_model.R')

shinyServer(
  function(input, output, session){
    waits <- reactiveValues()
    waits$resetindicator<-0
  
    # Plot the RI & RP
    output$ri <- renderPlot({
      S <- input$SampleSize
      dist.ri <- input$ri
      lambda.ri<-input$lambda.ri
      randomVec <- rpois(S, lambda.ri)
      
      hist(randomVec, col='blue')
    })
    output$rp <- renderPlot({
      S <- input$SampleSize
      
      # Paycheck service time
      dist.rp.pay <- input$rp_pay
      mu.rp.pay <- input$mu.rp.pay
      mean.rp.pay <- input$mean.rp.pay
      std.rp.pay <- input$std.rp.pay
      
      # Making service time
      dist.rp.make <- input$rp_make
      mean.rp.make <- c(input$mean.rp.make1, input$mean.rp.make2)
      std.rp.make <- c(input$std.rp.make1, input$std.rp.make2)
      mu.rp.make <- c(input$mu.rp.make1, input$mu.rp.make2)

      # Service time of paycheck
      if(dist.rp.pay == "Normal"){randomVec.pay <- rnorm(S, mean.rp.pay, std.rp.pay)}
      else if (dist.rp.pay == "Exponential"){randomVec.pay <- rexp(S, mu.rp.pay)}
      
      # Service time of making 40% buying #1, 60% for buying #2
      if(dist.rp.make == "Normal"){
        randomVec.make <- 
          sample(c(rnorm(0.4*S, mean.rp.make[1], std.rp.make[1]), rnorm(0.6*S, mean.rp.make[2], std.rp.make[2])))}
      else if (dist.rp.make == "Exponential"){
        randomVec.make <- 
          sample(c(rexp(0.4*S, mu.rp.make[1]), rexp(0.6*S, mu.rp.make[2])))}
      
      hist((randomVec.pay+randomVec.make),  col='blue', breaks=S/100)
    })
    
    
    # Start with case
    table <- eventReactive(input$generate, {
      if(input$rp_make == 'Normal' && input$rp_pay =='Normal'){
        SIM=sim.time.norm(input$SampleSize, prob.type=c(0.4, 0.6), 
                      arrv.rate=input$lambda.ri,
                      pay.rate=input$mean.rp.pay, 
                      pay.std=input$std.rp.pay,
                      make.rate=c(input$mean.rp.make1, input$mean.rp.make2),
                      make.std=c(input$std.rp.make1, input$std.rp.make2), 
                      N.fn=function() 1, p.ratio=0)
      } else {
        SIM=sim.time(input$SampleSize, prob.type=c(0.4, 0.6), 
                 arrv.rate=input$lambda.ri,
                 pay.rate=input$mu.rp.pay, make.rate=c(input$mu.rp.make1, input$mu.rp.make2), 
                 N.fn=function() 1, p.ratio=0)
      }
      if(input$case == "Original"){
        sim.original(sim=SIM, nserver=input$nserver)
      } else if(input$case == 'Q: Customer balking'){
        sim.1(sim=SIM, nserver=input$nserver, balk.fn=Q.fn)
      } else if (input$case == 'K: Limited waiting space'){
        sim.2(sim=SIM, nserver=input$nserver, forbid.thres=input$kspace)
      } else if (input$case == 'N: Drinks per customer would buy'){
        SIM=sim.time(input$SampleSize, prob.type=c(0.4, 0.6), 
                     arrv.rate=input$lambda.ri,
                     pay.rate=input$mu.rp.pay, make.rate=c(input$mu.rp.make1, input$mu.rp.make2), 
                     N.fn=function() N.fn(input$ndrink), p.ratio=0)
        sim.4(sim=SIM, nserver=input$nserver)
      }

    })
    output$data <- renderTable({table()})

    # Timing
    timer<-reactiveValues(inc=0, timer=reactiveTimer(1000), started=FALSE)
    
    observeEvent(input$jump, {timer$inc<-timer$inc+1})
    observeEvent(input$go, {timer$started<-TRUE})
    observeEvent(input$pause, {timer$started<-FALSE})
    observeEvent(input$stop, {
      timer$started<-FALSE 
      timer$inc<-0})
      
    observe({
      timer$timer()
      sim.table = table()
      if(timer$started){
        
        ## WAIT
        output$time <- renderText(timer$inc/3600)
        
        arrCustomer = sum(isolate(timer$inc/3600) > sim.table[, 1])
        startCustomer = sum(isolate(timer$inc/3600) > sim.table[, 5])
        endCustomer = sum(isolate(timer$inc/3600) > sim.table[, 7])
        inqueue = arrCustomer - startCustomer
        inprocess = startCustomer - endCustomer 
        # get waiting customer: default=10

        lapply(1:10, function(i) {
            output[[paste0('wait', i)]] <- renderUI({img(src=ifelse(i<=inqueue, 'wait.png', 'white.png'), height=50, width=50)})
        })
        
        process = rep(0, 2)
        ## WIP
        for (i in 1:2){
          if(inprocess < 1){
            process = rep(0, 2)
          } else if (inprocess < 2){
            process[i] = sim.table[startCustomer, 2] == i
          } else {
            process = rep(1, 2)
          }
        }
        lapply(1:2, function(i) {
          output[[paste0('wip', i)]] <- renderUI({img(src=ifelse(process[i], 'cook.png', 'white.png'), height=50, width=50)})
        })
        
        ## PRODUCT
        lapply(1:10, function(i) {
          output[[paste0('done', i)]] <- renderUI({img(src=ifelse(i<=round(endCustomer %% 10), 'bubble.png', 'white.png'), height=50, width=50)})
        })

        # inc + unit
        timer$inc<-isolate(timer$inc)+input$speed
      }
    })

    
}
)
