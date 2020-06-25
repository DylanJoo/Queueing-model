shinyServer(
  fluidPage(
    # header
    headerPanel('Queueing Simulation'),
    
    # sidebar
    sidebarPanel(
      # Setting simulation scope
      sliderInput('SampleSize', 'Sample size',
                  min=100, max=1000, value=1000, step=100),
      sliderInput('nserver', 'Number of server',
                  min=1, max=5, value=2, step=1),
      
      hr(style="border-color: grey;"),
      
      # Pick the available cases
      radioButtons("case", "Select a Real case",
                         choices=c("Original", "Q: Customer balking", "K: Limited waiting space", "N: Drinks per customer would buy")),
      
      conditionalPanel(condition="input.case == 'Q: Customer balking'", 
                       numericInput('qbalk', 'Waiting threshold', 5),
                       numericInput('prob_inf', 'Probability of balking anyway', 0.5)),
      conditionalPanel(condition="input.case == 'K: Limited waiting space'", 
                       numericInput('kspace', 'K', 10)),
      conditionalPanel(condition="input.case == 'N: Drinks per customer would buy'", 
                       numericInput('ndrink', 'N', 1)),
      
      hr(style="border-color: grey;"),
      
      # For arrival
      selectInput("ri", 'Arrival Distribution', choices=c('Poission')), 
      conditionalPanel(condition="input.ri == 'Poission'", 
                       numericInput('lambda.ri', 'Customer Arrival Rate', 50)),
      
      hr(style="border-color: grey;"),
      
      # For service time of pay
      selectInput("rp_pay", 'Serive Time of Payment Distribution', choices=c('Exponential', 'Normal')), 
      conditionalPanel(condition="input.rp_pay == 'Normal'", 
                       numericInput('mean.rp.pay', 'Mean', 10),
                       numericInput('std.rp.pay', 'Standard Deviation', 3600/40)),
      conditionalPanel(condition="input.rp_pay == 'Exponential'", 
                       numericInput('mu.rp.pay', 'Service Rate of payment', 3600/40)),
    
      hr(style="border-color: grey;"),
      
      # For service time of making type 1 drink
      selectInput("rp_make", 'Serive Time of making Distribution', choices=c('Exponential', 'Normal')), 
      conditionalPanel(condition="input.rp_make == 'Normal'", 
                       numericInput('mean.rp.make1', 'Mean#1', 10),
                       numericInput('mean.rp.make2', 'Mean#2', 10),
                       numericInput('std.rp.make1', 'Standard Deviation#1', 3),
                       numericInput('std.rp.make2', 'Standard Deviation#2', 3)),
      
      conditionalPanel(condition="input.rp_make == 'Exponential'", 
                       numericInput('mu.rp.make1', 'Service Rate of making #1', 3600/40),
                       numericInput('mu.rp.make2', 'Service Rate of making #2', 3600/90)),
      
      hr(style="border-color: grey;"),
      
      # Adjustment buttons
      actionButton("generate", "Generate Report"),
      helpText("\t First generate a random result",
                 "\t then you could see the distributions on page1, and report on page3"),
      hr(style="border-color: grey;"),
      sliderInput('speed', 'SpeedUP(60X )',
                    min=1, max=1000, value=60, step=10),
      splitLayout(
        actionButton('jump', 'NEXT'),
        actionButton('go', 'START'),
        actionButton('pause', 'PAUSE'),
        actionButton('stop', 'STOP')),
      
      helpText("Next button will popup the next time step.",
                 "Go button will automatically visualize the queue."),
      
      hr(style="border-color: grey;"),
      helpText("Author: jhjoo @ 2020\n",
               "You could check full codes on github! Enjoy it!\n",
               "dylanjootw[at]gmail.com")
        
      
      
    ),
    
    # main
    mainPanel(
      tabsetPanel(
        tabPanel("Distribution", 
                 splitLayout(plotOutput('ri'), plotOutput('rp'))),
  
        tabPanel("Animation", 
                 verticalLayout(
                   HTML('<left><h2>Time in hours:</h2></left>'),
                   textOutput('time'),
                   HTML('<center><h1>Waiting in queue: </h1></center>'),
                   hr(style="border-color: grey;"),
                   splitLayout(
                     lapply(1:10, function(i) {column(uiOutput(paste0('wait', i), align="center"), width=2)})
                     ), 
                   HTML('<center><h1>Preparing: </h1></center>'),
                   hr(style="border-color: grey;"),
                   splitLayout(
                     lapply(1:2, function(i) {column(uiOutput(paste0('wip', i), align="center"), width=6)})
                     ), 
                   HTML('<center><h1>Finished: </h1></center>'),
                   hr(style="border-color: grey;"),
                   splitLayout(
                     lapply(1:10, function(i) {column(uiOutput(paste0('done', i), align="center"), width=2)})
                   )

                 )

                 ),
        tabPanel("Time Table", tableOutput('data'))
      )
      
    )
  )
)
