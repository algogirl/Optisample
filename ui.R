library(shiny)

# Define UI for slider demo application
shinyUI(fluidPage(
  
  #  Application title
  titlePanel("OPTISAMPLE: To design strategies of sampling for active surveillance"),
  

  # Sidebar with sliders that demonstrate various available
  # options
  
    # Show a table summarizing the values entered
    navbarPage("",
            tabPanel("DESCRIPTION",                      
                     
                    #strong("Optisample"),p("is designed to optimize the strategy of sampling   for each farm in order to substantiate the freedom of infection considering also the costs of testing.  
                              
                             #Apart from the herd size, the estimated prevalence and the test sensitivity, the model takes into account the risk of being infected at arrival and during the production cycle, the structure of the herd and how the samples are selected over time.
                              
                             #This model is an expanded approach of the models proposed by Cannon (2002) and Martin (2007, 2008) to substantiate freedom of disease at herd and population level." ),
                                                                                                              
                     textOutput("desc00"),
                     #strong("WORKING EXAMPLE: ACTIVE SURVEILLANCE FOR PORCINE RESPIRATORY REPRODUCTIVE SYNDROME"),
                     imageOutput("pic1", inline = TRUE),
                     #img(src="fig.PNG", height = 400, width = 800,align = "center"),
                     downloadButton('downloadData', 'Examples'),
                     textOutput("desc1"),
                     downloadButton('downloadData1', 'Manuscript'),
                     p("Please send any comments, questions or suggestions to Ana Alba. Copyright 2016 University of Minnesota"),
                     imageOutput("pic2", inline = TRUE),
                     #img(src="logo.png", height = 100, width = 180),
                     imageOutput("pic3", inline = TRUE)),
                     #img(src="stemma.png", height = 150, width = 80)),
                     
                    
            tabPanel("INPUTS: VALUES OF THE PARAMETERS", fluidRow( 
              column (6, 
              h3(textOutput("textI1"), align="left"),
                    # 1. Simple integer interval
                    # sliderInput("time", "TIME",
                     #            min=0, max=11, value=11),
                     
                     # 2. Decimal interval with step value
                   #  sliderInput("nb", "HERD SIZE",
                   #             min = 0, max = 10000, value = 3000),
                    numericInput("nb", "HERD SIZE", value=1000),
                    
                    # 15. Specification of range within an interval
                    sliderInput("Pdesign", "PREVALENCE TO DETECT",
                                min =0, max = 1, value = 0.05, step = 0.01),
                     
                    # 18. Probability of introduction at arrival
                    #strong("PROBABILITY OF BEING INFECTED AT ARRIVAL:"),
                    sliderInput("Pr", "         Range",
                                min = 0, max = 1, value = c(0.2, 0.5), step = 0.01),
                    numericInput("Pr0","         Most likely value", min = 0, max = 1, value=0.4),
                    
                    #sliderInput("Pr_min", "Prob infected at arrival minimum",
                    #           min =0.01, max = 0.50, value = 0.01, step = 0.01),
                    # 21. Probability of introduction at arrival
                   #strong("RISK OF INCURSION BETWEEN CONSECUTIVE SAMPLINGS:"),
                   sliderInput("PriorBS","        Range",
                                min = 0, max = 1, value = c(0.2, 0.5), step = 0.01),
                   numericInput("PriorBS0", "         Most likely value", min = 0, max = 1, value=0.4),
                    # sliderInput("PriorBS", "Prob infection between consecutive samplings",
                    #             min =0, max = 1, value = 0.05, step = 0.01),
                    
                    # 22. Correlation
                   #strong("DEGREE OF REPRESENTATIVENESS AND CONTIGUITY AMONG SAMPLED GROUPS"),
                   sliderInput("COR",  "           Range",
                                min = 0, max = 1, value = c(0.2, 0.5), step = 0.01),
                   numericInput("COR0","           Most likely value", min = 0, max = 1, value=0.4)
              ),
            
                    column(6,
                     h3(textOutput("textI2"), align = "left"),
                     # 16. Sensitivity of test
                     #strong("TEST SENSITIVITY:"),
                     sliderInput("se", "           Range",
                                 min =0, max = 1, value = c(0.8, 0.9), step = 0.01),
                     numericInput("se0","           Most likely value", min = 0, max = 1, value=0.84),
                     numericInput("cost", "PRICE OF TEST", value=0),
                     
                     
                     #Frequency of testing
                     
                     selectInput("time", "FREQUENCY OF TESTING (T):",
                                 c("Daily" = "DAILY",
                                   "Weekly" = "WEEKLY",
                                   "Monthly" = "MONTHLY",
                                   "Yearly" = "YEARLY")),
                     
                     
                     # 3. Number of tests in each sampling 
                     sliderInput("n1","Number of samples at T1",
                                 min = 0, max = 100,value = 30),
                     
                     # 4. Number of tests in each sampling
                     sliderInput("n2", "Number of samples at T2",
                                 min = 0, max = 100,value = 30),
              
                     # 5. Number of tests in each sampling
                     sliderInput("n3", "Number of samples at T3",
                                 min = 0, max = 100,value = 30),
                     
                     # 6. Number of tests in each sampling
                     sliderInput("n4", "Number of samples at T4",
                                 min = 0, max = 100,value = 30),
                     
                     # 7. Number of tests in each sampling
                     sliderInput("n5", "Number of samples at T5",
                                 min = 0, max = 100,value = 30),
                     
                     # 8. Number of tests in each sampling
                     sliderInput("n6", "Number of samples at T6",
                                 min = 0, max = 100,value = 30),
                     
                     # 9. Number of tests in each sampling
                     sliderInput("n7", "Number of samples at T7",
                                 min = 0, max = 100,value = 30),
                     
                     # 10. Number of tests in each sampling
                     sliderInput("n8", "Number of samples at T8",
                                 min = 0, max = 100,value = 30),
                     
                     # 11. Number of tests in each sampling
                     sliderInput("n9", "Number of samples at T9",
                                 min = 0, max = 100,value = 30),
                     
                     
                     # 12. Number of tests in each sampling
                     sliderInput("n10", "Number of samples at T10",
                                 min = 0, max = 100,value = 30),
                     
                     # 13. Number of tests in each sampling
                     sliderInput("n11", "Number of samples at T11",
                                 min = 0, max = 100,value = 30),
                     
                     
                     # 14. Number of tests in each sampling
                     sliderInput("n12", "Number of samples at T12",
                                 min = 0, max = 100,value = 30)
                     
                     ))),
                     
                     # 17. Specificity of test
                     # sliderInput("sp", "Specificity of test",
                     #            min =0, max = 1, value = 0.99, step = 0.01),
                     
                     
                     
                     
                     # 19. Probability of introduction at arrival
                     #sliderInput("Pr_max", "Prob infected at arrival maximum",
                     #           min =0.50, max = 0.99, value = 0.99, step = 0.01),
                     
                     
                     # 20. Simulations
                     #sliderInput("sim", "Nb simulations",
                      #           min =1000, max = 1000, value = 1000),
                     
                     
            tabPanel("OUTCOMES", tableOutput("values"),
                                #strong("Frequency of sampling"),
                                textOutput("freq"),
                                #strong("Maximum number of animals would be infected"),
                                textOutput("Di"),
                                #strong("Total cost of laboratory testing"),
                                textOutput("cost"),
                            
                                
      #plotOutput("A1",width = "100%", height = "400px"),
      #plotOutput("A2",width = "100%", height = "400px"), 
                            plotOutput("A3",width = "100%", height = "400px"),
                            plotOutput("A4",width = "100%", height = "400px"))
            )
))
