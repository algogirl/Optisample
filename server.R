library(shiny)
library(lattice)
library(tkrplot)
library(utils)
library(spc)
library(vcd)
library(Matrix)
library(pastecs)
library(markdown)
library(knitr)
library(gdata)
library(FFD)
library(LearnBayes)
library(reshape)
library(mc2d)
library(zoo)
library(xtable)


# Define server logic for slider examples
shinyServer(function(input, output){
  
  # Reactive expression to compose a data frame containing all of
  # the values
  
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Time", 
               "Number of pigs",
               "First sampling",
               "Second sampling",
               "Third sampling",
               "Fourth sampling",
               "Fifth sampling",
               "Sixth sampling",
               "Seventh sampling",
               "Eighth sampling",
               "Ninth sampling",
               "Tenth sampling",
               "Eleventh sampling",
               "Twelth sampling",
               "Prevalence to detect",
               "Sensitivity of test",
               #"Specificity of test",
               "Prob infected at arrival",
               "sim",
               "Prob infection between consecutive samplings",
               "Correlation between groups"),
      
      Value = as.character(c( 
                           input$nb,
                           input$n1,
                           input$n2,
                           input$n3,
                           input$n4,
                           input$n5,
                           input$n6,
                           input$n7,
                           input$n8,
                           input$n9,
                           input$n10,
                           input$n11,
                           input$n12,
                           input$Pdesign,
                           input$se,
                           input$time,
                           input$Pr0,
                           input$PriorBS0,
                           input$COR0,
                           input$se0,
                           #input$sp,
                           paste(input$Pr ,collapse =' '),
                           paste(input$PriorBS ,collapse =' '),
                           paste(input$COR ,collapse =' '))),
      stringsAsFactors=FALSE)
  })
  
  # Show the values using an HTML table
  
  #output$values <- renderTable({
   # sliderValues()
  #})
  
  output$desc00 <- renderText({""})
  
  output$desc1 <- renderText({"Suggested citation: Alba A., Morrison E., Cheeran, A., Alvarez, J., Rovira, A., Perez, A. (2016). 'Optisample': Open web-based application to optimize sampling strategies for active surveillance at herd level.Porcine Respiratory Reproductive Syndrome as a working
example"})
 
  output$pic1 <- renderImage({
    filename <- normalizePath(file.path('.',paste('fig', '.PNG', sep='')))                        #     # Return a list containing the filename
                                list(src = filename,
                                     width = 800,
                                     height = 400,
                                    align = "center")
                                        }, deleteFile = FALSE)
  
  output$downloadData <- downloadHandler(
    filename = 'sampleFiles.pdf',  
    content <- function(file) {
    file.copy('sampleFiles.pdf', file)
    },
    contentType = "application/pdf"
   )
  
  output$pic2 <- renderImage({
    filename <- normalizePath(file.path('.',paste('logo', '.png', sep='')))                        #     # Return a list containing the filename
    list(src = filename,
         width = 180,
         height = 100)
  }, deleteFile = FALSE)
  
  output$pic3 <- renderImage({
    filename <- normalizePath(file.path('.',paste('stemma', '.png', sep='')))                        #     # Return a list containing the filename
    list(src = filename,
         width = 80,
         height = 150)
  }, deleteFile = FALSE)
  
  
  output$downloadData1 <- downloadHandler(
    filename = 'Optisample2.pdf',  
    content <- function(file) {
      file.copy('Optisample2.pdf', file)
    },
    contentType = "application/pdf"
  )
  
  output$textI1 <- renderText({
    "HERD CONTEXT"
  })
  
  output$textI2 <- renderText({
    "SAMPLING"
  })
  
  output$freq <-renderText({
    freq<-input$time
    freq
  })
  
  output$Total <- renderText({ 
    Total<-input$n1+input$n2+input$n3+input$n4+input$n5+input$n6+input$n7+input$n8+input$n9+input$n10+input$n11+input$n12
    Total
  })
  
  
  output$Di <- renderText({ 
    Di<-round(input$Pdesign*input$nb,0)
    Di
  })
  

  output$cost <- renderText({
    TotalA <- input$n1+input$n2+input$n3+input$n4+input$n5+input$n6+input$n7+input$n8+input$n9+input$n10+input$n11+input$n12
    cost <- input$cost * TotalA
    cost
  })
  #### A partir de aqui
  

    # Herd size 
  
  # Function Probability of being free after sampling X in different groups
  
  
  # Table 1 summarizes the parameters considered in each one of four scenarios.
    
  #SCENARIO<-c("A")
  #N<-c(Na)
  #Pdesign<-c(PdesignA)
  #TOTAL<-c(TOTALA)
  #se<-c(seA)
  #sp<-c(spA)
  #Prior <-c(PriorA)
  #PriorBS<-c(PriorBSA) 
  #COR<-c(CORA)
  #COST<-c(COSTA) 
  # Npiglet<-c(NpigletA,NpigletB,,NpigletC,NpigletD)
  # Costpiglet<-c(CostpigletA,CostpigletB,CostpigletC,CostpigletD)
  # Impact<-c(ImpactA,  ImpactB, ImpactC,  ImpactD )
  #PARAM <- data.frame(SCENARIO,N, Pdesign,TOTAL, se, sp, Prior, PriorBS, COR, COST)
  #print(xtable(PARAM,floating=FALSE))
  
  # summarizes the sampling strategies used in each scenario.
  
  
  #SP<-data.frame(A)
  #SP1 <-t(SP)
  #SP2<-as.data.frame(SP)
  #print(xtable(SP,floating=FALSE))
  
  
  
  
  # the number of total samples, the sampling costs and the probability of being free over all a 12-month period expressed as AUC for each one of the scenarios considered
  
  
  #SCENARIO<-c("A")
  #TOTAL <- c(TOTALA)
  #COSTSAMP<-c(COSTsamplingA)
  #AUCu <- c(AUC1A)
  #COSTEFu<-c(median(CEA),median(CEB), median(CEC), median(CED))
  #AUCg<-c(AUC2A)
  #COSTEFg<-c(median(CECA),median(CECB), median(CECC), median(CECD))
  #C <- data.frame(SCENARIO,TOTAL, COSTSAMP, AUCu,AUCg)
  #print(xtable(C,floating=FALSE))
  
  
  # probability of being free in the different periods of time (T1-T12)
  
  
  #SCENARIO<-c("A")
  #TOTAL <-c(TOTALA)
  
  #TEST <- data.frame(SCENARIO,TOTAL,sumtA)
  #TEST1 <- t(TEST)
  #TEST2 <- as.data.frame(TEST1)
  #print(xtable(TEST2,floating=FALSE))
  
  
  
  
  #the probability of being free in the different periods of time (T1-T12) considerinng correlated groups
  
  
  #SCENARIO<-c("A")
  #TOTAL <-c(TOTALA)
  
  #TEST <- data.frame(SCENARIO,TOTAL,sumtcA)
  #TEST1 <- t(TEST)
  #TEST2 <- as.data.frame(TEST1)
  
  #print(xtable(TEST2,floating=FALSE))
  
  
  #Time=c(1:12)
  #par(mfrow=c(2,1))
  output$A3<-renderPlot({
    N<-input$nb 
    Na<-N
    
    # Sample size (n) over time 
    # n1<- 30; n2<- 30; n3<- 30; n4<- 0; n5<-0; n6<-0; n7<- 0; n8<- 0; n9<- 0; n10<- 0; n11<-0; n12<-0; 
    
    # Prevalence (Design)
    
    Pdesign<-input$Pdesign; PdesignA<-Pdesign
    
    # Total number of samples
    TOTAL<-input$n1+input$n2+input$n3+input$n4+input$n5+input$n6+input$n7+input$n8+input$n9+input$n10+input$n11+input$n12
    A<-c(input$n1,input$n2,input$n3,input$n4,input$n5,input$n6,input$n7,input$n8,input$n9,input$n10,input$n11,input$n12)
    TOTALA<-TOTAL 
    
    # Sensitivity of the lab test;
    se_min<-input$se[1];se_max<-input$se[2]; se_mode<-input$se0;
    se<-rpert(100, min = se_min, mode = se_mode, max = se_max,shape = 4)
    seA<-median(se)
    
    # Especificity of the lab test (for the moment forget about it)
    #sp<-input$sp
    sp<- 1
    spA<-sp
    
    # Probability of being infected at arrival
    Pr_min<-input$Pr[1]; Pr_max<-input$Pr[2]; Pr_mode<-input$Pr0;
    Prior<- rpert(100, min = Pr_min, mode = Pr_mode, max = Pr_max,shape = 4)
    PriorA <-median(Prior)

    
    # Probability of being infected between consecutive samplings (BS)
    ntimesmin<-1; ntimesmax<-1
    ntimesminA<-ntimesmin
    ntimesmaxA<-ntimesmax
    year<-10
    yearA<-year
    PrBS_min<-((ntimesmin)/(year*12));PrBS_max<-((ntimesmax)/(year*12));simBS<-10
    
    PriorBS_min<-input$PriorBS[1]; PriorBS_max <- input$PriorBS[2];PriorBS0<-input$PriorBS0; #Prob between consecutive months
    PriorBS<-rpert(100, min = PriorBS_min, mode = PriorBS0, max = PriorBS_max,shape = 4)
    PriorBSA <- PriorBS
    
    # Degree of agreement between groups
    Cmax <- input$COR[2]
    Cmin <- input$COR[1]
    Cmode<- input$COR0
    COR<-rpert(10, min=Cmin, mode=Cmode, max=Cmax, shape=4) 
    CORA<-median(COR)
    
    #Diseased animals (included for future modifications)
    
    Di<-round(Pdesign*N,0); Di2<-round(Pdesign*N,0);Di3<-round(Pdesign*N,0);Di4<-round(Pdesign*N,0); Di5<-round(Pdesign*N,0); Di6<-round(Pdesign*N,0)
    
    
    #Cost RT-PCR by sample
    
    #COST<-30/6
    #COSTA<-COST 
    
    # Below, this part should not be included in the model of shiny
    
    # Npiglet<- 33 #number of piglets expected by sow 
    # NpigletA<-Npiglet
    # Costpiglet<- 36 #coste piglet 
    # CostpigletA<-Costpiglet
    
    # Imin<-0.2
    # Imode<-0.4
    # Imax<-0.5
    
    # Impact<- rpert(10, min=Imin, mode=Imode, max=Imax, shape=4)
    # ImpactA<-median(Impact)
    
    
    
    
    
    # Function Probability of being free after sampling X in the same group
    
    PFREEho <- function (Prior, i, N, n1, Di, seA, sp, PriorBS) {
      R<-as.vector(rep(0,length(Prior))) #  p-value of detecting a minimum P*
      PREV<-as.vector(rep(0,length(Prior))) #  P*
      SE<-as.vector(rep(0,length(Prior))) # sensitivity of the sampling
      PostFree<-as.vector(rep(0,length(Prior))) # Prob of being free with the negatives outcomes and the initial probability of being infected
      PriorF<-as.vector(rep(0,length(Prior))) # Prob of being infected and the prob of new incursion between consecutive samplings
      
      for (i in 1:length(Prior))
      {    # Using package FFD (freedom of disease) computes the p-value of being infected (R) if we sample n from a total population of N and we expect hypothetically "Di"" diseased animals with a sensitivity se and a specificity sp
        
        R[i]<- computePValue(N, n1, Di,seA,sp)
        PREV[i]<-Di/N
        SE[i]<- 1-R[i]
        
        # Prior: probability of being infected at arrival
        PostFree[i]<- (1-Prior[i])/(1-Prior[i]*SE[i]) 
        PriorF<-(1-PostFree)+PriorBS-((1-PostFree)*PriorBS)
      }
      minPostFree <- min(PostFree)
      medianPostFree <- median(PostFree)
      maxPostFree <- max(PostFree)
      PostFree <- rpert(10, min=minPostFree, mode=medianPostFree, max=maxPostFree, shape=4)
      PriorF<-(1-PostFree)+PriorBS-((1-PostFree)*PriorBS)
      return(list(PostFree=PostFree,PriorF=PriorF,minPostFree=minPostFree,medianPostFree=medianPostFree,maxPostFree=maxPostFree))
    }
    
    # Probability of being free after consecutive samplings considering a unique population.We are testing the same group.
    
    # SCENARIO A
    t <- matrix (nrow = 12, ncol = 3)
    colnames(t) <- c ("minPostFree", "medianPostFree", "maxPostFree")
    sumt <- data.frame (1:12,2,3)
    sumtA <- data.frame (1:12)
    for( j in 1: 12)
    {
      if(j == 1)
      {
        tee <- PFREEho(Prior, i, N, A[j], Di, seA, sp, PriorBS)
        t[j,"minPostFree"] <- tee$minPostFree
        t[j,"medianPostFree"] <- tee$medianPostFree
        t[j,"maxPostFree"] <- tee$maxPostFree
        sumt[j]<-c(m1<-round(t[j,"minPostFree"],3),md1<- round(t[j,"medianPostFree"],3),mx1<- round(t[j,"maxPostFree"],3))
        sumtA[j]<-sumt[j,2]
      }
      else
      {
        tee <- PFREEho(tee$PriorF, length(tee$PriorF), N, A[j], Di, seA, sp, PriorBS)
        t[j,"minPostFree"] <- tee$minPostFree
        t[j,"medianPostFree"] <- tee$medianPostFree
        t[j,"maxPostFree"] <- tee$maxPostFree
        sumt[j]<-c(m1<-round(t[j,"minPostFree"],3),md1<- round(t[j,"medianPostFree"],3),mx1<- round(t[j,"maxPostFree"],3))
        sumtA[j]<-sumt[j,2]
        
      }
    }
    Time<- c(0:11)
    # OUTPUTS OF SAMPLING THE SAME GROUP
    MINA <- t[,"minPostFree"]
    MEDIANA <- t[,"medianPostFree"]
    MAXA <- t[,"maxPostFree"]
    id <- order(Time)
    
    
    # OUTPUT : AREA UNDER THE CURVE SAMPLING THE SAME GROUP
    
    id <- order(Time)
    
    AUCmin1<-round((sum(diff(Time[id])*rollmean(MINA[id],2)))/11,2)
    AUC1 <- round((sum(diff(Time[id])*rollmean(MEDIANA[id],2)))/11,2)
    AUCmax1<-round((sum(diff(Time[id])*rollmean(MAXA[id],2)))/11,2)
    
    AUCmin1
    AUC1A<-AUC1
    AUCmax1
    
    Time<- c(1:12)
    A3 <- plot(Time,MEDIANA, type=c("b"),ylim =c(0,1), ylab=c("Probability of freedom"), main="Samplings conducted over time in a unique group selected at random", xlab=c(paste("Total number of samples:",TOTAL, "      Overall probability of being free of infection:",AUC1A,"   Range:",AUCmin1,"-",AUCmax1)), col="blue",lwd=3,las=1, cex.lab=1.6,cex.axis=1.3, cex=2, cex.main=2, cex.sub=2)
    lines(Time,MINA,col="darkgrey")
    lines(Time,MAXA,col="darkgrey")
    #legend(10,0.3, legend=c("AUC:",AUC1A), cex=1.7, border=FALSE)
    A3
    
  })
  
  output$A4 <- renderPlot({
    N<-input$nb 
    Na<-N
    
    # Sample size (n) over time 
    # n1<- 30; n2<- 30; n3<- 30; n4<- 0; n5<-0; n6<-0; n7<- 0; n8<- 0; n9<- 0; n10<- 0; n11<-0; n12<-0; 
    
    # Prevalence (Design)
    
    Pdesign<-input$Pdesign; PdesignA<-Pdesign
    
    # Total number of samples
    TOTAL<-input$n1+input$n2+input$n3+input$n4+input$n5+input$n6+input$n7+input$n8+input$n9+input$n10+input$n11+input$n12
    A<-c(input$n1,input$n2,input$n3,input$n4,input$n5,input$n6,input$n7,input$n8,input$n9,input$n10,input$n11,input$n12)
    TOTALA<-TOTAL 
    
    # Sensitivity of the lab test;
    se_min<-input$se[1]; se_max<-input$se[2]; se_mode<-input$se0; 
    se<-rpert(100, min = se_min, mode = se_mode, max = se_max,shape = 4)
    seA<-median(se)
    
    # Especificity of the lab test (for the moment forget about it)
    #sp<-input$sp
    sp <-1
    spA<-sp
    
    # Probability of being infected at arrival
    Pr_min<-input$Pr[1]; Pr_max<-input$Pr[2]; Pr_mode<-input$Pr0;
    Prior<-rpert(100, min = Pr_min, mode = Pr_mode, max = Pr_max,shape = 4)
    PriorA <-median(Prior)
    
    # Probability of being infected between consecutive samplings (BS)
   # ntimesmin<-1; ntimesmax<-1
  #  ntimesminA<-ntimesmin
  #  ntimesmaxA<-ntimesmax
   # year<-10
  #  yearA<-year
  #  PrBS_min<-((ntimesmin)/(year*12));PrBS_max<-((ntimesmax)/(year*12));simBS<-10
  
    PriorBS_min<-input$PriorBS[1]; PriorBS_max <- input$PriorBS[2] ; PriorBS_mode <- input$PriorBS0#Prob between consecutive months
    PriorBS<-rpert(100, min = PriorBS_min, mode = PriorBS_mode, max = PriorBS_max,shape = 4)
    PriorBSA <- PriorBS
    
    # Degree of agreement between groups
    Cmax <- input$COR[2]
    Cmin <- input$COR[1]
    Cmode<- input$COR0 
    COR<-rpert(10, min=Cmin, mode=Cmode, max=Cmax, shape=4) 
    CORA<-median(COR)
    
    #Diseased animals (included for future modifications)
    
    Di<-round(Pdesign*N,0); Di2<-round(Pdesign*N,0);Di3<-round(Pdesign*N,0);Di4<-round(Pdesign*N,0); Di5<-round(Pdesign*N,0); Di6<-round(Pdesign*N,0)
    
    
    #Cost RT-PCR by sample
    
    #COST<-30/6
    #COSTA<-COST 
    
    # Below, this part should not be included in the model of shiny
    
    # Npiglet<- 33 #number of piglets expected by sow 
    # NpigletA<-Npiglet
    # Costpiglet<- 36 #coste piglet 
    # CostpigletA<-Costpiglet
    
    # Imin<-0.2
    # Imode<-0.4
    # Imax<-0.5
    
    # Impact<- rpert(10, min=Imin, mode=Imode, max=Imax, shape=4)
    # ImpactA<-median(Impact)
    
    
    
    
    
    # Function Probability of being free after sampling X in the same group
    
    PFREEho <- function (Prior, i, N, n1, Di, seA, sp, PriorBS) {
      R<-as.vector(rep(0,length(Prior))) #  p-value of detecting a minimum P*
      PREV<-as.vector(rep(0,length(Prior))) #  P*
      SE<-as.vector(rep(0,length(Prior))) # sensitivity of the sampling
      PostFree<-as.vector(rep(0,length(Prior))) # Prob of being free with the negatives outcomes and the initial probability of being infected
      PriorF<-as.vector(rep(0,length(Prior))) # Prob of being infected and the prob of new incursion between consecutive samplings
      
      for (i in 1:length(Prior))
      {    # Using package FFD (freedom of disease) computes the p-value of being infected (R) if we sample n from a total population of N and we expect hypothetically "Di"" diseased animals with a sensitivity se and a specificity sp
        
        R[i]<- computePValue(N, n1, Di,seA,sp)
        PREV[i]<-Di/N
        SE[i]<- 1-R[i]
        
        # Prior: probability of being infected at arrival
        PostFree[i]<- (1-Prior[i])/(1-Prior[i]*SE[i])   
        PriorF<-(1-PostFree)+PriorBS-((1-PostFree)*PriorBS)
      }
      minPostFree <- min(PostFree)
      medianPostFree <- median(PostFree)
      maxPostFree <- max(PostFree)
      PostFree <- rpert(10, min=minPostFree, mode=medianPostFree, max=maxPostFree, shape=4)
      PriorF<-(1-PostFree)+PriorBS-((1-PostFree)*PriorBS)
      return(list(PostFree=PostFree,PriorF=PriorF,minPostFree=minPostFree,medianPostFree=medianPostFree,maxPostFree=maxPostFree))
    }
    
    # Probability of being free after consecutive samplings considering a unique population.We are testing the same group.
    
    # SCENARIO A
    t <- matrix (nrow = 12, ncol = 3)
    colnames(t) <- c ("minPostFree", "medianPostFree", "maxPostFree")
    sumt <- data.frame (1:12,2,3)
    sumtA <- data.frame (1:12)
    for( j in 1: 12)
    {
      if(j == 1)
      {
        tee <- PFREEho(Prior, i, N, A[j], Di, seA, sp, PriorBS)
        t[j,"minPostFree"] <- tee$minPostFree
        t[j,"medianPostFree"] <- tee$medianPostFree
        t[j,"maxPostFree"] <- tee$maxPostFree
        sumt[j]<-c(m1<-round(t[j,"minPostFree"],3),md1<- round(t[j,"medianPostFree"],3),mx1<- round(t[j,"maxPostFree"],3))
        sumtA[j]<-sumt[j,2]
      }
      else
      {
        tee <- PFREEho(tee$PriorF, length(tee$PriorF), N, A[j], Di, seA, sp, PriorBS)
        t[j,"minPostFree"] <- tee$minPostFree
        t[j,"medianPostFree"] <- tee$medianPostFree
        t[j,"maxPostFree"] <- tee$maxPostFree
        sumt[j]<-c(m1<-round(t[j,"minPostFree"],3),md1<- round(t[j,"medianPostFree"],3),mx1<- round(t[j,"maxPostFree"],3))
        sumtA[j]<-sumt[j,2]
      }
    }
    PFREEc <- function (PF1, COR, PriorBS, i, N, n2, Di2, seA, sp) {
      
      PF2<-as.matrix(PF1)
      M<-COR%*%t(PF2) # Prior ~ previous PFree * Correlation
      Mn<-as.vector(M)
      PInfC<-(1-(Mn))  # Prior of being infected according to the information received from the previous sampling
      PriorC<-PInfC+PriorBS-(PInfC*PriorBS)  # Prior of being infected according to the previous sampling + Prob of incursion between sampling
      Prior2<-as.vector(PriorC)
      
      ### Vectors to save the results
      
      R2<-as.vector(rep(0,length(Prior2))) # vector to save the results
      PREV2<-as.vector(rep(0,length(Prior2))) # vector to save the results
      SE2<-as.vector(rep(0,length(Prior2))) # vector to save the results
      PostFree2<-as.vector(rep(0,length(Prior2))) # vector to save the results
      PriorF2<-as.vector(rep(0,length(Prior2))) # vector to save the results
      
      ### CALCULATIONS
      
      for (i in 1:length(Prior2))
      {
        R2[i]<- computePValue(N, n2, Di2,seA,sp)
        PREV2[i]<-Di2/N
        SE2[i]<- 1-R2[i]
        
        # Prior: probability of being infected at arrival
        PostFree2[i]<- (1-Prior2[i])/(1-Prior2[i]*SE2[i])
        PriorF2<-(1-PostFree2)+PriorBS-((1-PostFree2)*PriorBS)
      }  
      minPostFree2 <- min(PostFree2)
      medianPostFree2 <- median(PostFree2)
      maxPostFree2 <- max(PostFree2)
      return(list(PostFree2=PostFree2,PriorF2=PriorF2,minPostFree2=minPostFree2,medianPostFree2=medianPostFree2,maxPostFree2=maxPostFree2))
    }
    
        
    # SCENARIO A CONSIDERING THAT WE ARE SAMPLING DIFFERENT GROUPS
    
    tc <- matrix (nrow = 12, ncol = 3)
    colnames(tc) <- c ("minPostFree", "medianPostFree", "maxPostFree")
    sumtc <- data.frame (1:12,2,3)
    sumtcA <- data.frame (1:12)
    for( j in 1: 12)
    {
      if (j == 1)
      {
        teeC <- PFREEho(Prior, i, N, A[j], Di, seA, sp, PriorBS)
        tc[j,"minPostFree"] <- teeC$minPostFree
        tc[j,"medianPostFree"] <- teeC$medianPostFree
        tc[j,"maxPostFree"] <- teeC$maxPostFree
        sumtc[j]<-c(m1<-round(tc[j,"minPostFree"],3),md1<- round(tc[j,"medianPostFree"],3),mx1<- round(tc[j,"maxPostFree"],3))
        sumtcA[j]<-sumtc[j,2]
        TeeC <- rpert(10, min=tc[j,"minPostFree"], mode=tc[j,"medianPostFree"], max=tc[j,"maxPostFree"], shape=4)
      }
      else
      {
        teeC <- PFREEc(TeeC, COR, PriorBS, length(TeeC),N, A[j], Di3, seA, sp)
        tc[j,"minPostFree"] <- teeC$minPostFree2
        tc[j,"medianPostFree"] <- teeC$medianPostFree2
        tc[j,"maxPostFree"] <- teeC$maxPostFree2
        sumtc[j]<-c(m1<-round(tc[j,"minPostFree"],3),md1<- round(tc[j,"medianPostFree"],3),mx1<- round(tc[j,"maxPostFree"],3))
        sumtcA[j]<-sumtc[j,2]
        TeeC<-rpert(10, min=tc[j,"minPostFree"], mode=tc[j,"medianPostFree"], max=tc[j,"maxPostFree"], shape=4)
      }
    }
    
    
    
    
    
    # OUTPUTS OF SAMPLING DIFFERENT GROUPS
    
    MINcA <- tc[,"minPostFree"]
    MEDIANcA <- tc[,"medianPostFree"]
    MAXcA <- tc[,"maxPostFree"]
    
    # Cost of sampling
    
    
    COSTsampling<-input$cost*TOTALA
    COSTsamplingA<-COSTsampling
    
    
    # PLOTS AS LAYOUTS (OUTPUTS)
    #par(mfrow=c(2,1))
    #output$A1 <- renderPlot({
        
    # plot(Time,MEDIANA, type=c("l"),ylim =c(0,1), ylab=c("ProbFREE at Prev:",Pdesign), xlab="Follow up in months", main=c("samples:",n1+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12), col="blue",lwd=2)
    #lines(Time,MINA,col="darkgrey")
    #lines(Time,MAXA,col="darkgrey")
    #})
    
    #output$A2 <- renderPlot({
    # A2<-plot(Time,MEDIANcA, type=c("l"),ylim =c(0,1), ylab=c("ProbFREE at Prev:",Pdesign), xlab="Follow up in months", main=c("samples:", n1+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12), col="darkgreen",lwd=2)
    #  lines(Time,MINcA,col="darkgrey")
    # lines(Time,MAXcA,col="darkgrey")
    #  A2
    #})
    
    # IMPACT OF DISEASE FOR THE FUTURE (???? Don't do anything with this part)
    # CE <- COSTsamplingA + ((N*Npiglet*Impact*Costpiglet)*(1-AUC1A))
    # CEA<-CE 
    # mCE <-min(round(CE,0))
    # MCE <-max(round(CE,0))
    # COST-EFFECTIVENESS
    # npiglets   
    # CostPIGLET
    #IMPACTO DE LA ENFERMEDAD: % DE PERDIDAS ATRIBUIBLES A LA PRESENCIA DE ENFERMEDAD 
    # COSTOT<-COSTsampling + ((1- AUC)*N* npiglets*Time)*IMPACTO*CostPIGLET 
    
    Time<-c(1:12)
    
    
    # OUTPUT : AREA UNDER THE CURVE SAMPLING DIFFERENT GROUPS
    
    
    id <- order(Time)
  
    AUCmin2<-round((sum(diff(Time[id])*rollmean(MINcA[id],2)))/11,2)
                 
    AUC2 <- round((sum(diff(Time[id])*rollmean(MEDIANcA[id],2)))/11,2)
   
    AUCmax2<-round((sum(diff(Time[id])*rollmean(MAXcA[id],2)))/11,2)
    
    AUC2A<-AUC2

    
    
    # IMPACT OF DISEASE FOR THE FUTURE (???? Don't do anything with this part)
    
    # CE <- COSTsamplingA + ((N*Npiglet*Impact*Costpiglet)*(1-AUC2A))
    # CECA<-CE
    # mCE <-min(round(CE,0))
    # MCE <-max(round(CE,0))
    
    # COST-EFFECTIVENESS
    # npiglets   
    # CostPIGLET
    # IMPACTO DE LA ENFERMEDAD: % DE PERDIDAS ATRIBUIBLES A LA PRESENCIA DE ENFERMEDAD 
    # COSTOT<-COSTsampling + ((1- AUC)*N* npiglets*Time)*IMPACTO*CostPIGLET 
    
    
    
    
    
    # COMPARISON OF SCENARIOS 
    
    
    Time<- c(1:12)
    
    A4<-plot(Time,MEDIANcA, type=c("b"),ylim =c(0,1), ylab=c("Probability of freedom"), main="Samplings conducted in different groups of the same herd", xlab=c(paste("Total number of samples:", TOTAL,"       Overall probability of being free of infection:",AUC2A,"   Range:",AUCmin2,"-",AUCmax2)),col="darkgreen",lwd=3,las=1,cex.lab=1.6,cex.axis=1.3, cex=2,cex.main=2, cex.sub=2)
    lines(Time,MINcA,col="darkgrey")
    lines(Time,MAXcA,col="darkgrey")
    # legend(10,0.3, legend=c("AUC:",AUC2A), cex=1.7, border=FALSE)
    A4
  })
  
  #SCENARIO<-c("A")
  #N<-c(Na)
  #Pdesign<-c(PdesignA)
  #TOTAL<-c(TOTALA)
  #se<-c(seA)
  #sp<-c(spA)
  #Prior <-c(PriorA)
  #PriorBS<-c(PriorBS) 
  #COR<-c(CORA)
  #COSTSAMP<-c(COSTA) 
  # Npiglet<-c(NpigletA,NpigletB,,NpigletC,NpigletD)
  # Costpiglet<-c(CostpigletA,CostpigletB,CostpigletC,CostpigletD)
  # Impact<-c(ImpactA,  ImpactB, ImpactC,  ImpactD )
  #COSTSAMP<-c(COSTsamplingA)
  #AUCu <- c(AUC1A)
  #AUCg<-c(AUC2A)
  #PARAM <- data.frame(SCENARIO,N, Pdesign,TOTAL, se, sp, Prior, PriorBS, COR, COSTSAMP,AUCu, AUCg)
  #d<-t(PARAM)
  #print(xtable(d,floating=FALSE))
  
    
  
})
