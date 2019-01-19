server <- function(input, output, session) {
  
  ################################First part :::date de debut:::###################### 
  
  
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'directory'))
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  )
  
  path_entry = reactive({readDirectoryInput(session, 'directory')})
  
  decomp_path_entry<-reactive({basename(path_entry())})
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$sortie
    },
    handlerExpr = {
      if (input$sortie > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'sortie'))
        updateDirectoryInput(session, 'sortie', value = path)
      }
    }
  )
  
  path_out = reactive({readDirectoryInput(session, 'sortie')})
  
  
  
  #Date transformation  
  plt <- reactive({as.POSIXlt(input$date1)})
  mois <- reactive({plt()[["mon"]] + 1 })
  jour <- reactive({as.numeric(plt()[["mday"]])})
  an   <- reactive({as.numeric(plt()[["year"]] + 1900)})
  julian.dd <- reactive({jour()-32+((275*mois())%/%9)+2*(3%/%(mois()+1))+((4*mois()-100*(an()%%4))+390)%/%400})
  
  
  message<- c("Veuillez patienter quelques minutes apres validation")
  
  Answer1 <- reactive({switch(input$seqq,
                              nonconsec = T,
                              consec = F)})
  
  datededebut<-eventReactive(input$clic,{
    
    if(req(nchar(path_entry())>0)){
      date.de.debut(path_entry(),Answer1(),julian.dd(),input$hauteur,input$nbrjrseq,input$sequencesec,input$nbrjr)
    }
  }
  
  )
  
  
  
  observe({write.xlsx(datededebut(),file=paste(path_out(),paste(decomp_path_entry(),"date_de_debut.xlsx",sep="_"),sep = "/"),row.names = F) })
  
  datededebut.mat<-reactive({as.matrix(datededebut())})     
  dim.onset<-reactive({dim(datededebut())})
  
  moyy<-reactive({colMeans(datededebut.mat()[1:dim.onset()[1],2:dim.onset()[2]],na.rm = T)})
  
  output$plotbox<- renderPlot({
    
    boxplot.matrix(datededebut.mat()[1:dim.onset()[1],2:dim.onset()[2]], range=0, boxwex=0.8, cex=1, cex.axis=0.8, oma=c(1.2,0.3,0.1,0.1),varwidth = F,las=2,ylab = "Jour Julien",col = "skyblue")
    points(moyy(),pch=8,col="red")
    legend("topleft", legend = c("date moyenne"), col = c("red"), pch = 8, bty = "n", pt.cex = 0.5, cex = 0.8)
    title(main = paste("Serie des dates de debut des stations de:", datededebut.mat()[1,1], datededebut.mat()[dim.onset()[1],1], sep="-" ))
  })
  
  
  #???output$results <- renderText({decomp_path_entry()})
  
  output$msg <- renderText({message})
  
  output$down <- downloadHandler(
    filename = function(){
      paste("Date_de_debut",input$var3,sep=".")
    },
    content = function(file){
      
      if(input$var3=="png")
        png(file)
      else
        pdf(file)
      boxplot.matrix(datededebut.mat()[1:dim.onset()[1],2:dim.onset()[2]], range=0, boxwex=0.8, cex=1, cex.axis=0.8, oma=c(1.2,0.3,0.1,0.1),varwidth = F,las=2,ylab = "Jour Julien",col = "skyblue")
      points(moyy(),pch=8,col="red")
      legend("topleft", legend = c("date moyenne"), col = c("red"), pch = 8, bty = "n", pt.cex = 0.5, cex = 0.8)
      title(main = paste("Serie des dates de debut des stations de:", datededebut.mat()[1,1], datededebut.mat()[dim.onset()[1],1], sep="-" ))
      dev.off()
      
    }
  )
  
  ################################Second part :::date de fin::::###################### 
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory1
    },
    handlerExpr = {
      if (input$directory1 > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'directory1'))
        updateDirectoryInput(session, 'directory1', value = path)
      }
    }
  )
  
  path_entry1 = reactive({readDirectoryInput(session, 'directory1')})
  decomp_path_entry1<-reactive({basename(path_entry1())})
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$sortie1
    },
    handlerExpr = {
      if (input$sortie1 > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'sortie1'))
        updateDirectoryInput(session, 'sortie1', value = path)
      }
    }
  )
  
  path_out1 = reactive({readDirectoryInput(session, 'sortie1')})
  
  
  
  
  
  #Date transformation  
  plt2 <- reactive({as.POSIXlt(input$date2)})
  mois1 <- reactive({plt2()[["mon"]] + 1 })
  jour1 <- reactive({as.numeric(plt2()[["mday"]])})
  an1   <- reactive({as.numeric(plt2()[["year"]] + 1900)})
  julian.dd1 <- reactive({jour1()-32+((275*mois1())%/%9)+2*(3%/%(mois1()+1))+((4*mois1()-100*(an1()%%4))+390)%/%400})
  
  plt3 <- reactive({as.POSIXlt(input$date3)})
  mois2 <- reactive({plt3()[["mon"]] + 1 })
  jour2 <- reactive({as.numeric(plt3()[["mday"]])})
  an2   <- reactive({as.numeric(plt3()[["year"]] + 1900)})
  julian.dd2 <- reactive({jour2()-32+((275*mois2())%/%9)+2*(3%/%(mois2()+1))+((4*mois2()-100*(an2()%%4))+390)%/%400})
  
  Answer <- reactive({switch(input$dist,
                             norm = F,
                             unif = T)})
  
  
  
  message1<- c("Veuillez patienter quelques minutes apres validation")
  
  
  datedefin<-eventReactive(input$clic1,{
    
    if(req(nchar(path_entry1())>0)){
      date.de.fin(path_entry1(),julian.dd1(),input$ETP,input$capretmax,Answer(),julian.dd2())
    }
  }
  
  )
  
  observe({write.xlsx(datedefin(),file=paste(path_out1(),paste(decomp_path_entry1(),"date_de_fin.xlsx",sep="_"),sep = "/"),row.names = F) })
  
  
  datedefin.mat<-reactive({as.matrix(datedefin())})     
  dim.onset1<-reactive({dim(datedefin())})
  
  moyy1<-reactive({colMeans(datedefin.mat()[1:dim.onset1()[1],2:dim.onset1()[2]],na.rm = T)})
  
  output$plotbox1<- renderPlot({
    
    boxplot.matrix(datedefin.mat()[1:dim.onset1()[1],2:dim.onset1()[2]], range=0, boxwex=0.8, cex=1.2, cex.axis=0.8, oma=c(1.2,0.3,0.1,0.1),varwidth = F,las=2,ylab = "Jour Julien",col = "skyblue")
    points(moyy1(),pch=8,col="red")
    legend("topleft", legend = c("date moyenne"), col = c("red"), pch = 8, bty = "n", pt.cex = 0.5, cex = 0.8)
    title(main = paste("Serie des dates de fin des stations de:", datedefin.mat()[1,1], datedefin.mat()[dim.onset1()[1],1], sep="-" ))
  })
  
  
  #output$results <- renderTable({datededebut()})
  
  output$msg1 <- renderText({message2})
  
  output$down1 <- downloadHandler(
    filename = function(){
      paste("Date_de_fin",input$var4,sep=".")
    },
    content = function(file){
      
      if(input$var4=="png")
        png(file)
      else
        pdf(file)
      boxplot.matrix(datedefin.mat()[1:dim.onset1()[1],2:dim.onset1()[2]], range=0, boxwex=0.8, cex=1.2, cex.axis=0.8, oma=c(1.2,0.3,0.1,0.1),varwidth = F,las=2,ylab = "Jour Julien",col = "skyblue")
      points(moyy1(),pch=8,col="red")
      legend("topleft", legend = c("date moyenne"), col = c("red"), pch = 8, bty = "n", pt.cex = 0.5, cex = 0.8)
      title(main = paste("Serie des dates de fin des stations de:", datedefin.mat()[1,1], datedefin.mat()[dim.onset1()[1],1], sep="-" ))
      dev.off()
      
    }
  )
  
  
  
  #################################Sequences seches######################################
  
  message2<- c("Veuillez patienter quelques minutes apres validation")
  
  output$msg2<- renderText({message2})
  
  dataset1<-reactive({ 
    inFile1 <- input$uploadFile1 
    dat<-read.xlsx(inFile1$datapath, 1)
    return(dat)
  })
  
  dataset2<-reactive({ 
    inFile2 <- input$uploadFile2 
    dat<-read.xlsx(inFile2$datapath, 1)
    return(dat)
  })
  
  
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory2
    },
    handlerExpr = {
      if (input$directory2 > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'directory2'))
        updateDirectoryInput(session, 'directory2', value = path)
      }
    }
  )
  
  path_entry2 = reactive({readDirectoryInput(session, 'directory2')})
  decomp_path_entry2<-reactive({basename(path_entry2())})
  
  
  
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$sortie2
    },
    handlerExpr = {
      if (input$sortie2 > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'sortie2'))
        updateDirectoryInput(session, 'sortie2', value = path)
      }
    }
  )
  
  path_out2 = reactive({readDirectoryInput(session, 'sortie2')})
  
  Answer3 <- reactive({switch(input$typeseq,
                              SeqDD = 1,
                              SeqDF = 2,
                              SeqDDDF=3
  )})  
  
  Answer4 <- reactive({switch(input$typeseqfin,
                              Seqfdf = 1,
                              Seqfdfmoy = 2,
                              Seqflmoy=3
  )})
  
  
  sequenceseche<-eventReactive(input$clic2,{
    
    if(req(nchar(path_entry2())>0)){
      sequence.seche(dataset1(),dataset2(),path_entry2(),Answer3(),Answer4(),input$longperiod)
    }
  }
  
  )
  
  
  observe({
    
    if(Answer3()==1){
      write.xlsx(sequenceseche(),file=paste(path_out2(),paste(decomp_path_entry2(),"Sequence_sec_en_debut_saison.xlsx",sep = "_"),sep = "/"),row.names = F) 
    }else{
      if(Answer3()==3){
        write.xlsx(sequenceseche(),file=paste(path_out2(),paste(decomp_path_entry2(),"Sequence_sec_Maxi_debut_et_Fin_saison.xlsx",sep = "_"),sep = "/"),row.names = F) 
      }else{
        if((Answer3()==2)&&(Answer4()==1)){
          write.xlsx(sequenceseche(),file=paste(path_out2(),paste(decomp_path_entry2(),"Sequence_sec_en_Fin_saison_DDX_DF.xlsx",sep = "_"),sep = "/"),row.names = F) 
        }
        else{
          if((Answer3()==2)&&(Answer4()==2)){
            write.xlsx(sequenceseche(),file=paste(path_out2(),paste(decomp_path_entry2(),"Sequence_sec_en_Fin_saison_DDX_DFMoy.xlsx",sep = "_"),sep = "/"),row.names = F) 
          }else{
            if((Answer3()==2)&&(Answer4()==3)){
              write.xlsx(sequenceseche(),file=paste(path_out2(),paste(decomp_path_entry2(),"Sequence_sec_en_Fin_saison_DDX_LONGMoyX.xlsx",sep = "_"),sep = "/"),row.names = F) 
            } 
            
          }
        }
        
      }
      
      
    }
  })
  
  sequenceseche.mat<-reactive({as.matrix(sequenceseche())})     
  dim.onset2<-reactive({dim(sequenceseche())})
  
  moyy2<-reactive({colMeans(sequenceseche.mat()[1:dim.onset2()[1],2:dim.onset2()[2]],na.rm = T)})
  
  output$plotbox3<- renderPlot({
    
    boxplot.matrix(sequenceseche.mat()[1:dim.onset2()[1],2:dim.onset2()[2]], range=0, boxwex=0.8, cex=1, cex.axis=0.8, oma=c(1.2,0.3,0.1,0.1),varwidth = F,las=2,ylab = "Nombre de Jours",col = "skyblue")
    points(moyy2(),pch=8,col="red")
    legend("topleft", legend = c("Nombre moyen"), col = c("red"), pch = 8, bty = "n", pt.cex = 0.5, cex = 0.8)
    title(main = paste("Serie des pauses pluviometriques des stations de:", sequenceseche.mat()[1,1], sequenceseche.mat()[dim.onset2()[1],1], sep="-" ))
  })
  
  
  
  output$down3 <- downloadHandler(
    filename = function(){
      paste("Sequence_seche",input$var6,sep=".")
    },
    content = function(file){
      
      if(input$var6=="png")
        png(file)
      else
        pdf(file)
      boxplot.matrix(sequenceseche.mat()[1:dim.onset2()[1],2:dim.onset2()[2]], range=0, boxwex=0.8, cex=1, cex.axis=0.8, oma=c(1.2,0.3,0.1,0.1),varwidth = F,las=2,ylab = "Nombre de Jours",col = "skyblue")
      points(moyy2(),pch=8,col="red")
      legend("topleft", legend = c("Nombre moyen"), col = c("red"), pch = 8, bty = "n", pt.cex = 0.5, cex = 0.8)
      title(main = paste("Serie des pauses pluviometriques des stations de:", sequenceseche.mat()[1,1], sequenceseche.mat()[dim.onset2()[1],1], sep="-" ))
      dev.off()
      
    }
  )
  
  
  
  #################################Longeur des saisons######################
  message3<- c("Veuillez patienter quelques minutes apres validation")
  
  output$msg3<- renderText({message3})
  
  dataset3<-reactive({ 
    inFile3 <- input$uploadFile3 
    dat<-read.xlsx(inFile3$datapath, 1)
    return(dat)
  })
  
  dataset4<-reactive({ 
    inFile4 <- input$uploadFile4 
    dat<-read.xlsx(inFile4$datapath, 1)
    return(dat)
  }) 
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$sortie3
    },
    handlerExpr = {
      if (input$sortie3 > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'sortie3'))
        updateDirectoryInput(session, 'sortie3', value = path)
      }
    }
  )
  
  path_out3 = reactive({readDirectoryInput(session, 'sortie3')}) 
  
  long_saison<-eventReactive(input$clic3,{
    
    if(req(nchar(path_out3())>0)){
      long.saison(dataset3(),dataset4())
    }
  }
  
  )
  
  observe({
    write.xlsx(long_saison(),file=paste(path_out3(),"longueur_saison.xlsx",sep = "/"),row.names = F) 
  })
  
  
  long_saison.mat<-reactive({as.matrix(long_saison())})     
  dim.onset3<-reactive({dim(long_saison())})
  
  moyy3<-reactive({colMeans(long_saison.mat()[1:dim.onset3()[1],2:dim.onset3()[2]],na.rm = T)})
  
  output$plotbox4<- renderPlot({
    
    boxplot.matrix(long_saison.mat()[1:dim.onset3()[1],2:dim.onset3()[2]], range=0, boxwex=0.8, cex=1, cex.axis=0.8, oma=c(1.2,0.3,0.1,0.1),varwidth = F,las=2,ylab = "Nombre de Jours",col = "skyblue")
    points(moyy3(),pch=8,col="red")
    legend("topleft", legend = c("Nombre moyen de Jours"), col = c("red"), pch = 8, bty = "n", pt.cex = 0.5, cex = 0.8)
    title(main = paste("Serie des longeurs de saisons des stations de:", long_saison.mat()[1,1], long_saison.mat()[dim.onset3()[1],1], sep="-" ))
  })
  
  
  output$down2 <- downloadHandler(
    filename = function(){
      paste("Longeur",input$var5,sep=".")
    },
    content = function(file){
      
      if(input$var5=="png")
        png(file)
      else
        pdf(file)
      boxplot.matrix(sequenceseche.mat()[1:dim.onset2()[1],2:dim.onset2()[2]], range=0, boxwex=0.8, cex=1, cex.axis=0.8, oma=c(1.2,0.3,0.1,0.1),varwidth = F,las=2,ylab = "Nombre de Jours",col = "skyblue")
      points(moyy2(),pch=8,col="red")
      legend("topleft", legend = c("Nombre moyen"), col = c("red"), pch = 8, bty = "n", pt.cex = 0.5, cex = 0.8)
      title(main = paste("Serie des pauses pluviometriques des stations de:", sequenceseche.mat()[1,1], sequenceseche.mat()[dim.onset2()[1],1], sep="-" ))
      dev.off()
      
    }
  )  
  
  #################################var trends##############
  
  
  dataset5<-reactive({ 
    inFile5 <- input$uploadFile5 
    dat<-read.xlsx(inFile5$datapath, 1)
    return(dat)
  })
  
  
  
  
  
  ####################################PPPPPPPPP
  
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    switch(input$input_type,
           "Variabilite et tendances" = sliderInput("dynamic1", "Choisir la normale:",
                                                    min = 1951, max = 2020, value = c(1981,2010)),
           "Analyse de risque" = radioButtons("dynamic2", "Choisir:",
                                              choices = c("1er et 4eme quintile" = "option1",
                                                          "1er et dernier decile" = "option2"),
                                              selected = "option1"
           )
    )
  })
  ################
  
  var<-eventReactive(input$clic4,{
    if(input$input_type=="Variabilite et tendances"){
      varibilite(dataset5(),input$dynamic1[1],input$dynamic1[2])
    }})
  
  risk<-eventReactive(input$clic4,{
    if((input$input_type=="Analyse de risque")&(input$dynamic2=="option1")){
      risque(dataset5(),0.2,0.8) 
    }else{
      if((input$input_type=="Analyse de risque")&(input$dynamic2=="option2")){
        risque(dataset5(),0.1,0.9)
      }
    }     
  })
  
  output$variable <- renderUI({ 
    obj<-switch(input$dataset,
                "iris" = var()[,-c(1)],
                "mtcars" = risk()[,-c(1)])	 
    var.opts<-namel(colnames(obj))
    selectInput("variable","Station ou quantile:", var.opts) # uddate UI 				 
  })
  
  t<-reactive(input$variable) 
  v<-reactive(var()[,t()])
  v2<-reactive(risk()[,t()])
  an11<-reactive(var()[,1])
  station<-reactive(risk()[,1])
  riskcal<-reactive(as.data.frame(cbind(station(),v2())))
  length1<-reactive(length(an11()))
  mm<-reactive(as.vector(filter(v(), rep(1/3,3),side=2)))
  
  s1 <- reactive(ts(data = v(), start = an11()[1], end =an11()[length1()], frequency = 1))
  mannkend<-reactive(MannKendall(s1()))
  tau=reactive(as.vector(mannkend()[[1]]))
  pvalue=reactive(as.vector(mannkend()[[2]]))
  
  pos_debut=reactive(regexpr('date_de_debut',input$uploadFile5$name))
  pos_fin=reactive(regexpr('date_de_fin',input$uploadFile5$name))
  pos_seq=reactive(regexpr('Sequence',input$uploadFile5$name))
  pos_long=reactive(regexpr('longueur_saison',input$uploadFile5$name))
  
  ###############################plot
  output$plot1<- renderPlot({
    if(input$input_type=="Variabilite et tendances"){
      if(pos_debut()!=-1){
        yu<-barplot(v(), col=ifelse(v()>0,"red","blue"), names.arg = an11(), cex.names = 0.75, cex.axis = 0.7, xlab = "Annees", ylab = "anomalies standardisees", axes = TRUE, las=2)
        lines(x=yu,y=mm(),lwd=2)
        mtext(paste("tau de Mannkendall:",tau(),"  P_value:", pvalue(), sep =" " ), side=3)
        legend("topleft",lty = c("solid"), legend = c("3-pt moy. mob."),col = c("black"),cex = 0.7)
        title(main = paste("Tendances des dates de debut de:", input$variable,  sep=" " ))
      }else{
        if(pos_fin() !=-1){
          yu<-barplot(v(), col=ifelse(v()>0,"blue","red"), names.arg = an11(), cex.names = 0.75, cex.axis = 0.7, xlab = "Annees", ylab = "anomalies standardisees", axes = TRUE, las=2)
          lines(x=yu,y=mm(),lwd=2)
          mtext(paste("tau de Mannkendall:",tau(),"  P_value:", pvalue(), sep =" " ), side=3)
          legend("topleft",lty = c("solid"), legend = c("5-pt moy. mob."),col = c("black"),cex = 0.7)
          title(main = paste("Tendances des dates de fin de:", input$variable,  sep=" " ))
        }else{
          
          if(pos_seq() !=-1){
            yu<-barplot(v(), col=ifelse(v()>0,"red","blue"), names.arg = an11(), cex.names = 0.75, cex.axis = 0.7, xlab = "Annees", ylab = "anomalies standardisees", axes = TRUE, las=2)
            lines(x=yu,y=mm(),lwd=2)
            mtext(paste("tau de Mannkendall:",tau(),"  P_value:", pvalue(), sep =" " ), side=3)
            legend("topleft",lty = c("solid"), legend = c("5-pt moy. mob."),col = c("black"),cex = 0.7)
            title(main = paste("Tendances des sequences seches de :", input$variable,  sep=" " ))
          }else{
            
            if(pos_long() !=-1){
              yu<-barplot(v(), col=ifelse(v()>0,"blue","red"), names.arg = an11(), cex.names = 0.75, cex.axis = 0.7, xlab = "Annees", ylab = "anomalies standardisees", axes = TRUE, las=2)
              lines(x=yu,y=mm(),lwd=2)
              mtext(paste("tau de Mannkendall:",tau(),"  P_value:", pvalue(), sep =" " ), side=3)
              legend("topleft",lty = c("solid"), legend = c("5-pt moy. mob."),col = c("black"),cex = 0.7)
              title(main = paste("Tendances des longeurs de saison de :", input$variable,  sep=" " ))
            }
          }
          
        }             
        
      }
      
    }else{
      if(input$input_type=="Analyse de risque"){
        if((input$variable== "20%")){
          ggplot(data=riskcal(), aes(x=station(), y=v2() )) +
            geom_bar(stat="identity", color="blue", fill="yellow")+
            xlab("Station") +ylab("Premier quintile")+ggtitle("Premier quintile")
        }else{
          if((input$variable=="10%")){
            ggplot(data=riskcal(), aes(x=station(), y=v2() )) +
              geom_bar(stat="identity", color="blue", fill="yellow")+
              xlab("Station")+ ylab("Premier decile")+ggtitle("Premier decile") 
            
          }else{
            if((input$variable=="80%")){
              ggplot(data=riskcal(), aes(x=station(), y=v2() )) +
                geom_bar(stat="identity", color="blue", fill="green")+
                xlab("Station")+ ylab("4eme quintile")+ggtitle("4eme quintile")
            }else{
              if((input$variable=="90%")){
                ggplot(data=riskcal(), aes(x=station(), y=v2() )) +
                  geom_bar(stat="identity", color="blue", fill="green")+
                  xlab("Station")+ylab("dernier decile")+ggtitle("Dernier decile")
                
              }
              
            }
            
          }
          
          
          
        }
      }
    }
    
  })
  
  
  output$down4 <- downloadHandler(
    filename = function(){
      paste("tendances",input$var7,sep=".")
    },
    content = function(file){
      
      if(input$var7=="png")
        png(file)
      else
        pdf(file)
      
      if(input$input_type=="Variabilite et tendances"){
        if(pos_debut()!=-1){
          yu<-barplot(v(), col=ifelse(v()>0,"red","blue"), names.arg = an11(), cex.names = 0.75, cex.axis = 0.7, xlab = "Annees", ylab = "anomalies standardisees", axes = TRUE, las=2)
          lines(x=yu,y=mm(),lwd=2)
          mtext(paste("tau de Mannkendall:",tau(),"  P_value:", pvalue(), sep =" " ), side=3)
          legend("topleft",lty = c("solid"), legend = c("3-pt moy. mob."),col = c("black"),cex = 0.7)
          title(main = paste("Tendances des dates de debut de:", input$variable,  sep=" " ))
        }else{
          if(pos_fin() !=-1){
            yu<-barplot(v(), col=ifelse(v()>0,"blue","red"), names.arg = an11(), cex.names = 0.75, cex.axis = 0.7, xlab = "Annees", ylab = "anomalies standardisees", axes = TRUE, las=2)
            lines(x=yu,y=mm(),lwd=2)
            mtext(paste("tau de Mannkendall:",tau(),"  P_value:", pvalue(), sep =" " ), side=3)
            legend("topleft",lty = c("solid"), legend = c("5-pt moy. mob."),col = c("black"),cex = 0.7)
            title(main = paste("Tendances des dates de fin de:", input$variable,  sep=" " ))
          }else{
            
            if(pos_seq() !=-1){
              yu<-barplot(v(), col=ifelse(v()>0,"red","blue"), names.arg = an11(), cex.names = 0.75, cex.axis = 0.7, xlab = "Annees", ylab = "anomalies standardisees", axes = TRUE, las=2)
              lines(x=yu,y=mm(),lwd=2)
              mtext(paste("tau de Mannkendall:",tau(),"  P_value:", pvalue(), sep =" " ), side=3)
              legend("topleft",lty = c("solid"), legend = c("5-pt moy. mob."),col = c("black"),cex = 0.7)
              title(main = paste("Tendances des sequences seches de :", input$variable,  sep=" " ))
            }else{
              
              if(pos_long() !=-1){
                yu<-barplot(v(), col=ifelse(v()>0,"blue","red"), names.arg = an11(), cex.names = 0.75, cex.axis = 0.7, xlab = "Annees", ylab = "anomalies standardisees", axes = TRUE, las=2)
                lines(x=yu,y=mm(),lwd=2)
                mtext(paste("tau de Mannkendall:",tau(),"  P_value:", pvalue(), sep =" " ), side=3)
                legend("topleft",lty = c("solid"), legend = c("5-pt moy. mob."),col = c("black"),cex = 0.7)
                title(main = paste("Tendances des longeurs de saison de :", input$variable,  sep=" " ))
              }
            }
            
          }             
          
        }
        
      }else{
        if(input$input_type=="Analyse de risque"){
          if((input$variable== "20%")){
            ggplot(data=riskcal(), aes(x=station(), y=v2() )) +
              geom_bar(stat="identity", color="blue", fill="yellow")+
              xlab("Station") +ylab("Premier quintile")+ggtitle("Premier quintile")
          }else{
            if((input$variable=="10%")){
              ggplot(data=riskcal(), aes(x=station(), y=v2() )) +
                geom_bar(stat="identity", color="blue", fill="yellow")+
                xlab("Station")+ ylab("Premier decile")+ggtitle("Premier decile") 
              
            }else{
              if((input$variable=="80%")){
                ggplot(data=riskcal(), aes(x=station(), y=v2() )) +
                  geom_bar(stat="identity", color="blue", fill="green")+
                  xlab("Station")+ ylab("4eme quintile")+ggtitle("4eme quintile")
              }else{
                if((input$variable=="90%")){
                  ggplot(data=riskcal(), aes(x=station(), y=v2() )) +
                    geom_bar(stat="identity", color="blue", fill="green")+
                    xlab("Station")+ylab("dernier decile")+ggtitle("Dernier decile")
                  
                }
                
              }
              
            }
            
            
            
          }
        }
      }
      
      dev.off()
      
    }
  )
  
  #####################################Evaluation################################################
  dataset6<-reactive({ 
    inFile5 <- input$uploadFile6 
    dat<-read.xlsx(inFile5$datapath, 1)
    return(dat)
  })
  
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$sortie4
    },
    handlerExpr = {
      if (input$sortie4 > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'sortie4'))
        updateDirectoryInput(session, 'sortie4', value = path)
      }
    }
  )
  
  path_out4 = reactive({readDirectoryInput(session, 'sortie4')})
  
  
  output$variable11 <- renderUI({ 
    obj1<-dataset6()[,1]	 
    selectInput("variable11","selectionner l'annee:", obj1) # update UI 				 
  })
  
  evaluations<-eventReactive(input$clic5,{
    #if(req(nchar(path_out4())>0)){
    evaluation(dataset6(),input$variable11,input$moyennne[1],input$moyennne[2],0.3333333,0.66666666)
    #}
  })
  
  
  pos_debut1=reactive(regexpr('date_de_debut',input$uploadFile6$name))
  pos_fin1=reactive(regexpr('date_de_fin',input$uploadFile6$name))
  pos_seq1=reactive(regexpr('Sequence',input$uploadFile6$name))
  pos_long1=reactive(regexpr('longueur_saison',input$uploadFile6$name))
  
  DF=eventReactive(input$clic5,{tt(pos_debut1(),pos_fin1(),pos_seq1(),pos_long1(),evaluations())})
  ############################//////////////////////////////////////#############
  #DF=reactive(as.data.frame(DF(), strstr))
  numberofrows <- reactive({nrow(DF())})
  previous <- reactive(DF())
  
  MyChanges <-reactive(
    if(is.null(input$hotable1)){return(previous())}
    else if(!identical(previous(),input$hotable1)){
      # hot.to.df function will convert your updated table into the dataframe
      mytable <- hot_to_r(input$hotable1)
      mytable <- as.data.frame(hot_to_r(input$hotable1), stringsAsFactors = FALSE)
      # here the second column is a function of the first and it will be multipled by 100 given the values in the first column
      mytable <- mytable[1:numberofrows(),]
      
      dim1=dim(mytable)
      for(i in 1 : dim1[1]){
        if((is.na(mytable[i,2])==T)|(is.na(mytable[i,3])==T)){mytable[i,4]<-NA_character_} else{
          if(regexpr(mytable[i,2],mytable[i,3])!=-1){mytable[i,4]<-"V"}else{mytable[i,4]<-"F"}
        }
      }
      
      
      
      #mytable<-tt2(mytable)
      mytable
    }
    
  )
  output$hotable1 <- renderRHandsontable({rhandsontable(MyChanges())%>%
      hot_col("station", readOnly = TRUE) %>%
      hot_col("Observation", readOnly = TRUE)%>%
      hot_col("Verification", readOnly = T)
  })
  
  
  nombre_vrai=reactive(sum(MyChanges()[,4]=="V",na.rm = T))
  nombre_na=reactive(sum(is.na(MyChanges()[,4]),na.rm = T))
  Pourcentvrai=reactive(100*nombre_vrai()/(numberofrows()-nombre_na()))
  Pourcentfaux=reactive(100-(Pourcentvrai()))
  
  output$pourvrai= renderText({paste("Pourcentage de vrai pour l'annee",input$variable11, "est ",Pourcentvrai(),"%",sep=" ")})
  output$pourfaux= renderText({paste("Pourcentage de faux pour l'annee",input$variable11, "est ",Pourcentfaux(),"%",sep=" ")})
  
  observeEvent(input$save,{write.xlsx(MyChanges(),file=paste(path_out4(),paste("Evaluation",input$uploadFile6$name,sep="_"),sep = "/"),row.names = F)})
  
  #observe({write.xlsx(evaluations(),file=paste(path_out4(),paste("Evaluation",input$uploadFile6$name,sep="_"),sep = "/"),row.names = F) })
  
 ##############################################data check####################################
  dataset7<-reactive({ 
    inFile6 <- input$uploadFile7 
    dat<-read.xlsx(inFile6$datapath, 1)
    dat[,c(-1)][dat[,c(-1)]<input$valmini]<-"1"
    dat[,c(-1)][dat[,c(-1)]>input$valmaxi]<-"2"
    return(dat)
  })
  
  
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$sortie5
    },
    handlerExpr = {
      if (input$sortie5 > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'sortie5'))
        updateDirectoryInput(session, 'sortie5', value = path)
      }
    }
  )
  
  path_out5 = reactive({readDirectoryInput(session, 'sortie5')})
  
  checkdata<-eventReactive(input$clic6,{dataset7()})
  

  output$hotable2 <- renderRHandsontable({rhandsontable(checkdata(),readOnly = TRUE)%>%
      hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.NumericRenderer.apply(this, arguments);
                 if (value == 1) {
               td.style.background = 'pink';
               } else if (value == 2) {
               td.style.background = 'lightgreen';
               }
  }")
  })
#################################################Cumul###################################################
  message10<- c("CUMUL GLISSANT")
  output$message10<- renderText({message10})
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory10
    },
    handlerExpr = {
      if (input$directory10 > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'directory10'))
        updateDirectoryInput(session, 'directory10', value = path)
      }
    }
  )
  
  path_out10 = reactive({readDirectoryInput(session, 'directory10')})
  
  
  
  
    Answer10 <- reactive({switch(input$input_type10,
                              "MAM"= 1,
                              "AMJ"= 2,
                              "MJJ"=3,
                              "JJA"=4,
                              "JAS"=5,
                              "ASO"=6,
                              "SON"=7
  )})
  
    
cumu<-eventReactive(input$clic10,{cumul(path_out10(),Answer10())}) 

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$sortie11
  },
  handlerExpr = {
    if (input$sortie11 > 0) {
      # condition prevents handler execution on initial app launch
      
      path = choose.dir(default = readDirectoryInput(session, 'sortie11'))
      updateDirectoryInput(session, 'sortie11', value = path)
    }
  }
)

path_out11 = reactive({readDirectoryInput(session, 'sortie11')})

observe({
  if(input$input_type10=="MAM"){write.xlsx(cumu(),file=paste(path_out11(),"MAM.xlsx",sep = "/"),row.names = F)}
  if(input$input_type10=="AMJ"){write.xlsx(cumu(),file=paste(path_out11(),"AMJ.xlsx",sep = "/"),row.names = F)}
  if(input$input_type10=="MJJ"){write.xlsx(cumu(),file=paste(path_out11(),"MJJ.xlsx",sep = "/"),row.names = F)}
  if(input$input_type10=="JJA"){write.xlsx(cumu(),file=paste(path_out11(),"JJA.xlsx",sep = "/"),row.names = F)}
  if(input$input_type10=="JAS"){write.xlsx(cumu(),file=paste(path_out11(),"JAS.xlsx",sep = "/"),row.names = F)}
  if(input$input_type10=="ASO"){write.xlsx(cumu(),file=paste(path_out11(),"ASO.xlsx",sep = "/"),row.names = F)}
  if(input$input_type10=="SON"){write.xlsx(cumu(),file=paste(path_out11(),"SON.xlsx",sep = "/"),row.names = F)}
  })
##########################MAXI PLUVIO######################
message11<- c("MAXIMUM PLUVIOMETRIQUE")
output$message11<- renderText({message11})
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$directory12
  },
  handlerExpr = {
    if (input$directory12 > 0) {
      # condition prevents handler execution on initial app launch
      
      path = choose.dir(default = readDirectoryInput(session, 'directory12'))
      updateDirectoryInput(session, 'directory12', value = path)
    }
  }
)
path_out12 = reactive({readDirectoryInput(session, 'directory12')})

Answer20 <- reactive({switch(input$dist20,
                           nor = 1,
                           un = 2)})

dataset20<-reactive({ 
  inFile20 <- input$uploadFile20 
  dat<-read.xlsx(inFile20$datapath, 1)
  return(dat)
})

dataset21<-reactive({ 
  inFile21 <- input$uploadFile21 
  dat<-read.xlsx(inFile21$datapath, 1)
  return(dat)
})

maxxx<-eventReactive(input$clic20,{Maxi(path_out12(), Answer20(),dataset20(),dataset21())})

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$sortie20
  },
  handlerExpr = {
    if (input$sortie20 > 0) {
      # condition prevents handler execution on initial app launch
      
      path = choose.dir(default = readDirectoryInput(session, 'sortie20'))
      updateDirectoryInput(session, 'sortie20', value = path)
    }
  }
)

path_out20 = reactive({readDirectoryInput(session, 'sortie20')})

observe({
  if(Answer20()==1){write.xlsx(maxxx(),file=paste(path_out20(),"Maxi_annuel.xlsx",sep = "/"),row.names = F)}
  if(Answer20()==2){write.xlsx(maxxx(),file=paste(path_out20(),"Maxi_debut_fin.xlsx",sep = "/"),row.names = F)}

})

#########################NOMBRE DE JOUR##################
message12<- c("NOMBRE DE JOUR DE PLUIE")
output$message12<- renderText({message12})

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$directory13
  },
  handlerExpr = {
    if (input$directory13 > 0) {
      # condition prevents handler execution on initial app launch
      
      path = choose.dir(default = readDirectoryInput(session, 'directory13'))
      updateDirectoryInput(session, 'directory13', value = path)
    }
  }
)
path_out13 = reactive({readDirectoryInput(session, 'directory13')})

Answer30 <- reactive({switch(input$dist30,
                             nbr_an = 1,
                             nbr_D = 2)})

dataset30<-reactive({ 
  inFile30 <- input$uploadFile30 
  dat<-read.xlsx(inFile30$datapath, 1)
  return(dat)
})

dataset31<-reactive({ 
  inFile31 <- input$uploadFile31 
  dat<-read.xlsx(inFile31$datapath, 1)
  return(dat)
})

nbrjj<-eventReactive(input$clic30,{nbjour(path_out13(), Answer30(),dataset30(),dataset31(),input$haut)})
 

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$sortie30
  },
  handlerExpr = {
    if (input$sortie30 > 0) {
      # condition prevents handler execution on initial app launch
      
      path = choose.dir(default = readDirectoryInput(session, 'sortie30'))
      updateDirectoryInput(session, 'sortie30', value = path)
    }
  }
)

path_out30 = reactive({readDirectoryInput(session, 'sortie30')})

observe({
  if(Answer30()==1){write.xlsx(nbrjj(),file=paste(path_out30(),"Nombre_de_jour_annuel.xlsx",sep = "/"),row.names = F)}
  if(Answer30()==2){write.xlsx(nbrjj(),file=paste(path_out30(),"Nombre_de_jour_debut_fin.xlsx",sep = "/"),row.names = F)}
  
})

#########################CUMUL PLUVIOMETRIQUE##################
message13<- c("CUMUL")
output$message13<- renderText({message13})

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$directory14
  },
  handlerExpr = {
    if (input$directory14 > 0) {
      # condition prevents handler execution on initial app launch
      
      path = choose.dir(default = readDirectoryInput(session, 'directory14'))
      updateDirectoryInput(session, 'directory14', value = path)
    }
  }
)
path_out14 = reactive({readDirectoryInput(session, 'directory14')})

Answer40 <- reactive({switch(input$dist40,
                             nor4 = 1,
                             un4 = 2)})

dataset40<-reactive({ 
  inFile40 <- input$uploadFile40 
  dat<-read.xlsx(inFile40$datapath, 1)
  return(dat)
})

dataset41<-reactive({ 
  inFile41 <- input$uploadFile41 
  dat<-read.xlsx(inFile41$datapath, 1)
  return(dat)
})

cummm<-eventReactive(input$clic40,{cm(path_out14(), Answer40(),dataset40(),dataset41())})

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$sortie40
  },
  handlerExpr = {
    if (input$sortie40 > 0) {
      # condition prevents handler execution on initial app launch
      
      path = choose.dir(default = readDirectoryInput(session, 'sortie40'))
      updateDirectoryInput(session, 'sortie40', value = path)
    }
  }
)

path_out40 = reactive({readDirectoryInput(session, 'sortie40')})

observe({
  if(Answer40()==1){write.xlsx(cummm(),file=paste(path_out40(),"cumul_annuel.xlsx",sep = "/"),row.names = F)}
  if(Answer40()==2){write.xlsx(cummm(),file=paste(path_out40(),"cumul_debut_fin.xlsx",sep = "/"),row.names = F)}
  
})
}