########################################FONCTIONS DE L'APPLICATION#################################################################
#######################################HOUNGNIBO C. M. MANDELA########################################

                                    #############################

#des fonctions ecrits pour le calcul des différents parametres H_Mandela
#function date de debut

date.de.debut<-function(chemin.in,GSSN,date.dem.cal,qte,day_J,seq.max,nbjr_30){
  
  nom.station<-list.files(path =chemin.in) # lecture des stations dans le dossier
  
  lecture.extract.anne<-read.xlsx(paste(chemin.in,nom.station[1],sep = "/"),1)#lire pour extraire les années au niveau de la première station
  nom.ligne<-sub("X", "", colnames(lecture.extract.anne))#année extraite
  #nom.ligne<-colnames(lecture.extract.anne)#année extraite
  nbre.station<-length(nom.station)
  date.debut.out<-as.numeric(nom.ligne)
  
  if(GSSN==T){
    
    for(i in 1:nbre.station){
      #Lecture des stations
      nom.propre.station<-sub(".xlsx", "", nom.station[i])
      nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
      nom.mm[nom.mm==9999.0]<-NA_real_
      nom.mm[nom.mm==9988.0]<-0
      nom.mm[nom.mm==8888.0]<-0
      # nom.mm[,-1]
      dimens<-dim(nom.mm)
      
      #debut calcul date de début à transformer en fonction
      
      date.debut<-matrix(NA_real_,nrow = dimens[2],ncol = 1)
      colnames(date.debut)<-c(nom.propre.station)
      
      for(k in 1:dimens[2]){     #par colonne sur une station
        j<-date.dem.cal #initilisation date 
        
        test.NAN<-all(!is.na(nom.mm[j:(j+100),k]))
        #test.cond=F
        
        if(test.NAN==F) {
          date.debut[k,1]<-NA_real_
        }else{
          repeat{
            p<-is.na(nom.mm[j,k])
            q<-is.na(nom.mm[j+1,k]) 
            r<-is.na(nom.mm[j+2,k])
            test11<-p||q||r
            if (test11==T){
              date.debut[k,1]<-NA_real_
              break()
            }
            
            #test sur les 20 mm en 1 jour ou 03 jours consecutifs
            A<-nom.mm[j,k]>=qte
            B<-nom.mm[j+1,k]+nom.mm[j,k]>=qte
            C<-nom.mm[j+2,k]+nom.mm[j+1,k]+nom.mm[j,k]>=qte
            
            #test sur 1, 1 ou 2j consecutifs, 1 ou 2 ou 3j consecutifs.        
            
            if(day_J==1){
              test.20<-A
            }else{
              if(day_J==2){
                test.20<-A||B    
              }else{
                test.20<-A||B||C     
              }
            }
            
            #test sur les sequences seches
            count.seq<-0
            max.count<-0
            max.counter<-0
            u<-j
            while(u<=(j+nbjr_30)&& (is.na(nom.mm[u,k]))!=TRUE){
              if(nom.mm[u,k]<0.85){
                count.seq<-count.seq+1
                max.counter<-max.counter+1
              }else{
                if(max.count<count.seq){max.count<-count.seq}
                count.seq<-0
                max.counter<-0
              }
              if(max.count<max.counter){max.count<-max.counter}
              u<-u+1
            }
            
            test.seq<-max.count<=seq.max
            test.cond<-test.seq&&test.20
            j<-j+1
            
            if (test.cond==T){
              date.debut[k,1]<-(j+1)
              break()
            } 
          } 
          #   test.NAN<-all(!is.na(nom.mm[j:(j+60),k]))
          
        }
      }
      date.debut.out<-cbind(date.debut.out,date.debut)
    }
    names.1<-colnames(date.debut.out)
    names.1[1]<-"Annee"
    colnames(date.debut.out)<-names.1 
    
  }else{ 
    for(i in 1:nbre.station){
      #Lecture des stations
      nom.propre.station<-sub(".xlsx", "", nom.station[i])
      nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
      nom.mm[nom.mm==9999.0]<-NA_real_
      nom.mm[nom.mm==9988.0]<-0
      nom.mm[nom.mm==8888.0]<-0
      # nom.mm[,-1]
      dimens<-dim(nom.mm)
      
      #debut calcul date de début à transformer en fonction
      
      date.debut<-matrix(NA_real_,nrow = dimens[2],ncol = 1)
      colnames(date.debut)<-c(nom.propre.station)
      
      for(k in 1:dimens[2]){     #par colonne sur une station
        j<-date.dem.cal #initilisation date 15 mars
        
        test.NAN<-all(!is.na(nom.mm[j:(j+100),k]))
        #test.cond=F
        
        if(test.NAN==F) {
          date.debut[k,1]<-NA_real_
        }else{
          repeat{
            p<-is.na(nom.mm[j,k])
            q<-is.na(nom.mm[j+1,k]) 
            r<-is.na(nom.mm[j+2,k])
            test11<-p||q||r
            if (test11==T){
              date.debut[k,1]<-NA_real_
              break()
            }
            
            #test sur les 20 mm en 1 jour ou 03 jours consecutifs
            A<-nom.mm[j,k]>=qte
            B<-nom.mm[j+1,k]+nom.mm[j,k]>=qte
            C<-nom.mm[j+2,k]+nom.mm[j+1,k]+nom.mm[j,k]>=qte
            
            #test sur 1, 2j, 3j consecutifs.        
            
            if(day_J==1){
              test.20<-A
            }else{
              if(day_J==2){
                test.20<-B    
              }else{
                test.20<-C     
              }
            }
            
            #test sur les sequences seches
            count.seq<-0
            max.count<-0
            max.counter<-0
            u<-j
            while(u<=(j+nbjr_30)&& (is.na(nom.mm[u,k]))!=TRUE){
              if(nom.mm[u,k]<0.85){
                count.seq<-count.seq+1
                max.counter<-max.counter+1
              }else{
                if(max.count<count.seq){max.count<-count.seq}
                count.seq<-0
                max.counter<-0
              }
              if(max.count<max.counter){max.count<-max.counter}
              u<-u+1
            }
            
            test.seq<-max.count<seq.max
            test.cond<-test.seq&&test.20
            j<-j+1
            
            if (test.cond==T){
              date.debut[k,1]<-(j+1)
              break()
            } 
          } 
          #   test.NAN<-all(!is.na(nom.mm[j:(j+60),k]))
          
        }
      }
      date.debut.out<-cbind(date.debut.out,date.debut)
    }
    
    names.1<-colnames(date.debut.out)
    names.1[1]<-"Annee"
    colnames(date.debut.out)<-names.1 
    
  }
  return(date.debut.out)
}

#date de fin

date.de.fin<-function(chemin.in,date.dem.cal,ETP,Cap_ret_maxi,GSSN,jour_fix){
  
  nom.station<-list.files(path =chemin.in)# lecture des stations dans le dossier
  
  lecture.extract.anne<-read.xlsx(paste(chemin.in,nom.station[1],sep = "/"),1)#lire pour extraire les années au niveau de la première station
  nom.ligne<-sub("X", "", colnames(lecture.extract.anne))#année extraite
  nbre.station<-length(nom.station)
  date.fin.out<-as.numeric(nom.ligne)
  #as.data.frame(date.fin.out)
  
  if(GSSN==T){
    for(i in 1:nbre.station){
      #Lecture des stations
      nom.propre.station<-sub(".xlsx", "", nom.station[i])
      nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
      nom.mm[nom.mm==9999.0]<-NA_real_
      nom.mm[nom.mm==9988.0]<-0.0
      nom.mm[nom.mm==8888.0]<-0.0
      dimens<-dim(nom.mm)
      date.fin<-matrix(NA_real_,nrow = dimens[2],ncol = 1)
      colnames(date.fin)<-c(nom.propre.station)
      for(k in 1:dimens[2]){     #par colonne sur une station
        j<-date.dem.cal #réserve utile au ....
        ru<-0
        
        test.NAN<-all(!is.na(nom.mm[(j-120):j,k]))
        if(test.NAN==F) {
          date.fin[k,1]<-NA_real_
        }else{ 
          
          for(h in 1:j){
            if(is.na(nom.mm[h,k])){
              next()
            }
            ru<-nom.mm[h,k]+ru-ETP
            if(ru>Cap_ret_maxi){
              ru<-Cap_ret_maxi
            }
            if(ru<0){
              ru<-0
            }
          }
          v<-j+1
          while(ru>0&&is.na(nom.mm[v,k])==F&&v<=jour_fix){
            ru<-nom.mm[v,k]+ru-ETP
            if(ru>Cap_ret_maxi){
              ru<-Cap_ret_maxi
            }
            v<-v+1
          }
          date.fin[k,1]<-(v-1)
        }
      }
      date.fin.out<-cbind(date.fin.out,date.fin)  
    }
    
    names.1<-colnames(date.fin.out)
    names.1[1]<-"Annee"
    colnames(date.fin.out)<-names.1 
    
  }else{
    for(i in 1:nbre.station){
      #Lecture des stations
      nom.propre.station<-sub(".xlsx", "", nom.station[i])
      nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
      nom.mm[nom.mm==9999.0]<-NA_real_
      nom.mm[nom.mm==9988.0]<-0.0
      nom.mm[nom.mm==8888.0]<-0.0
      dimens<-dim(nom.mm)
      date.fin<-matrix(NA_real_,nrow = dimens[2],ncol = 1)
      colnames(date.fin)<-c(nom.propre.station)
      for(k in 1:dimens[2]){     #par colonne sur une station
        j<-date.dem.cal #réserve utile au ....
        ru<-0
        
        test.NAN<-all(!is.na(nom.mm[(j-120):j,k]))
        if(test.NAN==F) {
          date.fin[k,1]<-NA_real_
        }else{ 
          
          for(h in 1:j){
            if(is.na(nom.mm[h,k])){
              next()
            }
            ru<-nom.mm[h,k]+ru-ETP
            if(ru>Cap_ret_maxi){
              ru<-Cap_ret_maxi
            }
            if(ru<0){
              ru<-0
            }
          }
          v<-j+1
          while(ru>0&&is.na(nom.mm[v,k])==F){
            ru<-nom.mm[v,k]+ru-ETP
            if(ru>Cap_ret_maxi){
              ru<-Cap_ret_maxi
            }
            v<-v+1
          }
          date.fin[k,1]<-(v-1)
        }
      }
      date.fin.out<-cbind(date.fin.out,date.fin)  
    }
    names.1<-colnames(date.fin.out)
    names.1[1]<-"Annee"
    colnames(date.fin.out)<-names.1 
  }
  
  return(date.fin.out)
} 


sequence.seche<-function(date_debut,date_fin,chemin.in,type.de.seq,type.de.seq.fin,duration){
  
  date.debut<-date_debut
  date.fin<-date_fin
  long_saison<-date.debut
  long_saison[,-c(1)]<-date.fin[,-c(1)]-date.debut[,-c(1)]
  Long_moy<-colMeans(long_saison, na.rm = T)
  Fin_moy<-colMeans(date.fin, na.rm = T)
  
  nom.station<-list.files(path =chemin.in)
  lecture.extract.anne<-read.xlsx(paste(chemin.in,nom.station[1],sep = "/"),1)
  nom.ligne<-sub("X", "", names(lecture.extract.anne))
  nbre.station<-as.numeric(length(nom.station))
  seq.sec.out<-as.numeric(nom.ligne)
  
  type.seq<-type.de.seq
  type.seq.fin <-type.de.seq.fin                     
  j<-duration
  Long_moy_j<-Long_moy-j
  
  if(type.seq==1){#sequence en debut de saison
    for(i in 1:nbre.station){
      st.rain<-date.debut[,i+1]
      on.rain<-date.fin[,i+1]
      
      #Lecture des stations
      nom.propre.station<-sub(".xlsx", "", nom.station[i])
      nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
      nom.mm[nom.mm==9999.0]<-NA_real_
      nom.mm[nom.mm==9988.0]<-0
      nom.mm[nom.mm==8888.0]<-0
      dimens<-dim(nom.mm)
      
      seq.sec<-matrix(NA_real_,nrow = dimens[2],ncol = 1)
      colnames(seq.sec)<-c(nom.propre.station)
      
      
      for(k in 1:dimens[2]){     #par colonne sur une station
        
        p<-is.na(st.rain[k])
        
        if (p==T||(all(!is.na(nom.mm[st.rain[k]:(st.rain[k]+j),k])))==F){
          seq.sec[k,1]<-NA_real_
          next()
        }else{
          #test sur les séquences sèches
          count.seq<-0
          max.count<-0
          max.counter<-0
          u<-st.rain[k]
          m<-u+j
          while(u<=m &&(is.na(nom.mm[u,k]))!=TRUE){
            if(nom.mm[u,k]<0.85){
              count.seq<-count.seq+1
              max.counter<-max.counter+1
            }else{
              if(max.count<count.seq){max.count<-count.seq}
              count.seq<-0
              max.counter<-0
            }
            if(max.count<max.counter){max.count<-max.counter}
            u<-u+1
          }
          seq.sec[k,1]<-max.count
        } 
      }
      seq.sec.out<-cbind(seq.sec.out,seq.sec)
    }
    names.1<-colnames(seq.sec.out)
    names.1[1]<-"Annee"
    colnames(seq.sec.out)<-names.1
  }else{
    if(type.seq==2){
      #########################################################      
      if(type.seq.fin==1){ #sequence seche fin de saison(DD+j-DF)
        for(i in 1:nbre.station){
          st.rain<-date.debut[,i+1]
          on.rain<-date.fin[,i+1]
          #Lecture des stations
          nom.propre.station<-sub(".xlsx", "", nom.station[i])
          nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
          nom.mm[nom.mm==9999.0]<-NA_real_
          nom.mm[nom.mm==9988.0]<-0
          nom.mm[nom.mm==8888.0]<-0
          dimens<-dim(nom.mm)
          
          #début calcul date de début à transformer en fonction
          
          seq.sec<-matrix(NA_real_,nrow = dimens[2],ncol = 1)
          colnames(seq.sec)<-c(nom.propre.station)
          
          for(k in 1:dimens[2]){     #par colonne sur une station
            p<-is.na(st.rain[k])
            q<-is.na(on.rain[k])
            if (p==T||q==T||(all(!is.na(nom.mm[(st.rain[k]+j):on.rain[k],k])))==F){
              seq.sec[k,1]<-NA_real_
              next()
            }else{
              #test sur les séquences sèches
              count.seq<-0
              max.count<-0
              max.counter<-0
              u<-st.rain[k]+j
              
              while(u<=on.rain[k] &&(is.na(nom.mm[u,k]))!=TRUE){
                if(nom.mm[u,k]<0.85){
                  count.seq<-count.seq+1
                  max.counter<-max.counter+1
                }else{
                  if(max.count<count.seq){max.count<-count.seq}
                  count.seq<-0
                  max.counter<-0
                }
                if(max.count<max.counter){max.count<-max.counter}
                u<-u+1
              }
              seq.sec[k,1]<-max.count
            } 
          }
          seq.sec.out<-cbind(seq.sec.out,seq.sec)
        }
        names.1<-colnames(seq.sec.out)
        names.1[1]<-"Annee"
        colnames(seq.sec.out)<-names.1 
      }else{
        if(type.seq.fin==2){#sequence seche fin de saison(DD+j-DFMoyenne)
          for(i in 1:nbre.station){
            st.rain<-date.debut[,i+1]
            
            #Lecture des stations
            nom.propre.station<-sub(".xlsx", "", nom.station[i])
            nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
            nom.mm[nom.mm==9999.0]<-NA_real_
            nom.mm[nom.mm==9988.0]<-0
            nom.mm[nom.mm==8888.0]<-0
            
            dimens<-dim(nom.mm)
            finmoy<-rep(Fin_moy[i+1],dimens[2])
            #début calcul date de début à transformer en fonction
            
            seq.sec<-matrix(NA_real_,nrow = dimens[2],ncol = 1)
            colnames(seq.sec)<-c(nom.propre.station)
            
            for(k in 1:dimens[2]){     #par colonne sur une station
              p<-is.na(st.rain[k])
              q<-is.na(finmoy[k])
              if (p==T||q==T||(all(!is.na(nom.mm[(st.rain[k]+j):finmoy[k],k])))==F){
                seq.sec[k,1]<-NA_real_
                next()
              }else{
                #test sur les séquences sèches
                count.seq<-0
                max.count<-0
                max.counter<-0
                
                u<-st.rain[k]+j
                m<-finmoy[k]
                
                while(u<=m&&(is.na(nom.mm[u,k]))!=TRUE){
                  if(nom.mm[u,k]<0.85){
                    count.seq<-count.seq+1
                    max.counter<-max.counter+1
                  }else{
                    if(max.count<count.seq){max.count<-count.seq}
                    count.seq<-0
                    max.counter<-0
                  }
                  if(max.count<max.counter){max.count<-max.counter}
                  u<-u+1
                }
                seq.sec[k,1]<-max.count
              } 
            }
            seq.sec.out<-cbind(seq.sec.out,seq.sec)
          }
          names.1<-colnames(seq.sec.out)
          names.1[1]<-"Annee"
          colnames(seq.sec.out)<-names.1      
          
        }else{##################################
          
          if(type.seq.fin==3){####equence seche fin de saison(DD+j-LMoy-j)
            for(i in 1:nbre.station){
              st.rain<-date.debut[,i+1]
              
              #Lecture des stations
              nom.propre.station<-sub(".xlsx", "", nom.station[i])
              nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
              nom.mm[nom.mm==9999.0]<-NA_real_
              nom.mm[nom.mm==9988.0]<-0
              nom.mm[nom.mm==8888.0]<-0
              
              dimens<-dim(nom.mm)
              longmoyj<-rep(Long_moy_j[i+1],dimens[2])
              #début calcul date de début à transformer en fonction
              
              seq.sec<-matrix(NA_real_,nrow = dimens[2],ncol = 1)
              colnames(seq.sec)<-c(nom.propre.station)
              
              for(k in 1:dimens[2]){     #par colonne sur une station
                p<-is.na(st.rain[k])
                q<-is.na(longmoyj[k])
                if (p==T||q==T||(all(!is.na(nom.mm[(st.rain[k]+j):((st.rain[k]+j)+longmoyj[k]),k])))==F){
                  seq.sec[k,1]<-NA_real_
                  next()
                }else{
                  #test sur les séquences sèches
                  count.seq<-0
                  max.count<-0
                  max.counter<-0
                  u<-st.rain[k]+j
                  m<-((st.rain[k]+j)+longmoyj[k])
                  while(u<=m &&(is.na(nom.mm[u,k]))!=TRUE){
                    if(nom.mm[u,k]<0.85){
                      count.seq<-count.seq+1
                      max.counter<-max.counter+1
                    }else{
                      if(max.count<count.seq){max.count<-count.seq}
                      count.seq<-0
                      max.counter<-0
                    }
                    if(max.count<max.counter){max.count<-max.counter}
                    u<-u+1
                  }
                  seq.sec[k,1]<-max.count
                } 
              }
              seq.sec.out<-cbind(seq.sec.out,seq.sec)
            }
            names.1<-colnames(seq.sec.out)
            names.1[1]<-"Annee"
            colnames(seq.sec.out)<-names.1 
          }#########################
          
        }
      }
      
    }else{
      ############ Sequence maxi entre debut et fin
      
      for(i in 1:nbre.station){
        st.rain<-date.debut[,i+1]
        on.rain<-date.fin[,i+1]
        #Lecture des stations
        nom.propre.station<-sub(".xlsx", "", nom.station[i])
        nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
        nom.mm[nom.mm==9999.0]<-NA_real_
        nom.mm[nom.mm==9988.0]<-0
        nom.mm[nom.mm==8888.0]<-0
        dimens<-dim(nom.mm)
        
        #début calcul date de début à transformer en fonction
        
        seq.sec<-matrix(NA_real_,nrow = dimens[2],ncol = 1)
        colnames(seq.sec)<-c(nom.propre.station)
        
        for(k in 1:dimens[2]){     #par colonne sur une station
          
          p<-is.na(st.rain[k])
          q<-is.na(on.rain[k])
          if (p==T||q==T||(all(!is.na(nom.mm[st.rain[k]:on.rain[k],k])))==F){
            seq.sec[k,1]<-NA_real_
            next()
          }else{
            #test sur les séquences sèches
            count.seq<-0
            max.count<-0
            max.counter<-0
            u<-st.rain[k]
            m<-on.rain[k]
            while(u<=m&&(is.na(nom.mm[u,k]))!=TRUE){
              if(nom.mm[u,k]<0.85){
                count.seq<-count.seq+1
                max.counter<-max.counter+1
              }else{
                if(max.count<count.seq){max.count<-count.seq}
                count.seq<-0
                max.counter<-0
              }
              if(max.count<max.counter){max.count<-max.counter}
              u<-u+1
            }
            seq.sec[k,1]<-max.count
          } 
        }
        seq.sec.out<-cbind(seq.sec.out,seq.sec)
      }
      names.1<-colnames(seq.sec.out)
      names.1[1]<-"Annee"
      colnames(seq.sec.out)<-names.1
    }
  }
  return(seq.sec.out)
}


long.saison<-function(date_debut,date_fin){
  
  date.debut<-date_debut
  date.fin<-date_fin
  long_saison<-date.debut
  long_saison[,-c(1)]<-date.fin[,-c(1)]-date.debut[,-c(1)]
  
  names.1<-colnames(long_saison)
  names.1[1]<-"Annee"
  colnames(long_saison)<-names.1
  return(long_saison)
}

varibilite= function(nom.m,normale1,normale2){
  tampon1<-nom.m
  dimen<-dim(tampon1)
  tampon2<-tampon1[,1]
  norm1=normale1
  norm2=normale2
  if (norm1 %in% tampon2){ i1<-as.numeric(which(tampon2==norm1))}else{i1<-NA_integer_}
  if (norm2 %in% tampon2){ i2<-as.numeric(which(tampon2==norm2))}else{i2<-NA_integer_}
  if ((is.na(i1)==T)&&(!is.na(i2)==T)){ i1<-1 }
  if ((is.na(i2)==T)&&(!is.na(i1)==T)){ i2<-length(tampon2) }
  std1<-tampon1[i1:i2,2:dimen[2]]
  MOY2<-as.matrix(colMeans(std1, na.rm=TRUE))
  
  for(j in 1:dimen[1]){
    for(k in 2:dimen[2]){
      tampon1[j,k]<- (nom.m[j,k]- MOY2[k-1,1])/sd(std1[,k-1], na.rm=TRUE)
    }
  }
  tampon3<-cbind(tampon1,as.matrix(rowMeans(tampon1[,2:dimen[2]],na.rm = T)))
  name3=colnames(tampon3)
  name3[dimen[2]+1]="Zone_variabilite"
  colnames(tampon3)=name3 
  return(tampon3) 
  
}


risque= function(nom.m,prob1,prob2){
  tampon=nom.m
  dimension=dim(tampon)
  tampon=as.matrix(tampon)
  p1=prob1
  p2=prob2
  qant=colQuantiles(tampon[,2:dimension[2]], probs=c(p1,p2),na.rm=T)
  name4=colnames(tampon[,2:dimension[2]])
  qant=cbind(name4,qant)
  return(qant)
}

evaluation= function(nom.m,annee,normale1,normale2,prob1,prob2){
  tampon=nom.m
  dimension=dim(tampon)
  tampon2<-tampon[,1]
  tampon1=rep(NA_character_, (dimension[2]-1))
  tampon4=rep(NA_character_, (dimension[2]-1))
  tampon5=rep("V", (dimension[2]-1))
  
  norm1=normale1
  norm2=normale2
  
  if (norm1 %in% tampon2){ i1<-as.numeric(which(tampon2==norm1))}else{i1<-NA_integer_}
  if (norm2 %in% tampon2){ i2<-as.numeric(which(tampon2==norm2))}else{i2<-NA_integer_}
  if ((is.na(i1)==T)&&(!is.na(i2)==T)){ i1<-1 }
  if ((is.na(i2)==T)&&(!is.na(i1)==T)){ i2<-length(tampon2) }
  
  tampon=as.matrix(tampon)
  p1= prob1
  p2= prob2
  qant=colQuantiles(tampon[i1:i2,2:dimension[2]], probs=c(p1,p2),na.rm=T)
  i3<-as.numeric(which(tampon2==annee))
  tampon3=tampon[i3,2:dimension[2]]
  
  for(i in 1:(dimension[2]-1)){
    if(is.na(tampon3[i])==T){
      tampon1[i]<-NA_character_
    }else{
      if(tampon3[i]<qant[i,1]){
        tampon1[i]<-"below"
      }else{
        if(tampon3[i]>qant[i,2]){
          tampon1[i]<-"above"
        }else{
          tampon1[i]<-"moyenne" 
        }
      }
    }
  }
  
  station=colnames(tampon[,2:dimension[2]])
  
  tampon1=cbind(station, tampon1,tampon4,tampon5)
  names.1<-colnames(tampon1)
  names.1[2]<-"Observation"
  names.1[3]<-"Prevision"
  names.1[4]<-"Verification"
  
  colnames(tampon1)<-names.1
  return(tampon1)
}

tt<-function(posdebut1,posfin1,posseq1,poslong1,eval){
  if((posdebut1!=-1)|(posfin1!=-1)){eval[eval=="above"]<-"T"
  eval[eval=="below"]<-"P" 
  eval[eval=="moyenne"]<-"N" 
  }else{
    if((posseq1!=-1)|(poslong1!=-1)){eval[eval=="above"]<-"L"
    eval[eval=="below"]<-"C" 
    eval[eval=="moyenne"]<-"N"
    }
  } 
  return(eval)
}

tt2<-function(table){
#  table1=as.data.frame.character(table)
  dim1=dim(table1)
for(i in 1 : dim1[1]){
  if((is.na(table1[i,2])==T)|(is.na(table1[i,3])==T)){table1[i,4]<-NA_character_}else{
  if(regexpr(table1[i,2],table1[i,3])!=-1){table1[i,4]<-"V"}else{table1[i,4]<-"F"}
  }
}
return(table1)
}

namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}


###########################cumul############################"
cumul<-function (chemin.in,saison){
  
  nom.station<-list.files(path =chemin.in) # lecture des stations dans le dossier
  lecture.extract.anne<-read.xlsx(paste(chemin.in,nom.station[1],sep = "/"),1)#lire pour extraire les années au niveau de la première station
  nom.ligne<-sub("X","", colnames(lecture.extract.anne))#année extraite
  nbre.station<-length(nom.station)
  cumul.out<-as.numeric(nom.ligne)
  #colnames(cumul.out)<-"Annee"
  
  for(i in 1: nbre.station){
    nom.propre.station<-sub(".xlsx", "", nom.station[i])
    nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
    nom.mm[nom.mm==9999.0]<-NA_real_
    nom.mm[nom.mm==9988.0]<-0
    nom.mm[nom.mm==8888.0]<-0
    if(saison==1){
      trans1<-nom.mm[61:152,]
      cum<-as.matrix(colSums(trans1, na.rm=FALSE))
      colnames(cum)<-c(nom.propre.station)
      cumul.out<-cbind(cumul.out,cum)
    }
    
    if(saison==2){
      trans1<-nom.mm[92:182,]
      cum<-as.matrix(colSums(trans1, na.rm=FALSE))
      colnames(cum)<-c(nom.propre.station)
      cumul.out<-cbind(cumul.out,cum)
    }
    if(saison==3){
      trans1<-nom.mm[122:213,]
      cum<-as.matrix(colSums(trans1, na.rm=FALSE))
      colnames(cum)<-c(nom.propre.station)
      cumul.out<-cbind(cumul.out,cum)
    }
    
    if(saison==4){
      trans1<-nom.mm[153:244,]
      cum<-as.matrix(colSums(trans1, na.rm=FALSE))
      colnames(cum)<-c(nom.propre.station)
      cumul.out<-cbind(cumul.out,cum)
    }
    if(saison==5){
      trans1<-nom.mm[183:274,]
      cum<-as.matrix(colSums(trans1, na.rm=FALSE))
      colnames(cum)<-c(nom.propre.station)
      cumul.out<-cbind(cumul.out,cum)
    }
    if(saison==6){
      trans1<-nom.mm[214:305,]
      cum<-as.matrix(colSums(trans1, na.rm=FALSE))
      colnames(cum)<-c(nom.propre.station)
      cumul.out<-cbind(cumul.out,cum)
    }
    if(saison==7){
      trans1<-nom.mm[245:334,]
      cum<-as.matrix(colSums(trans1, na.rm=FALSE))
      colnames(cum)<-c(nom.propre.station)
      cumul.out<-cbind(cumul.out,cum)
    }
  
  }
  
  names.1<-colnames(cumul.out)
  names.1[1]<-"Annee"
  colnames(cumul.out)<-names.1
  
  return(cumul.out)
}

###################################Maxi pluviométrique#########################
Maxi<-function (chemin.in,choix,date_debut,date_fin){
  nom.station<-list.files(path = chemin.in) # lecture des stations dans le dossier
  lecture.extract.anne<-read.xlsx(paste(chemin.in,nom.station[1],sep = "/"),1)
  nom.ligne<-sub("X","", colnames(lecture.extract.anne))#année extraite
  nbre.station<-length(nom.station)
  maxi.out<-as.numeric(nom.ligne)
  
  
  for(i in 1: nbre.station){
    nom.propre.station<-sub(".xlsx", "", nom.station[i])
    nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
    nom.mm[nom.mm==9999.0]<-NA_real_
    nom.mm[nom.mm==9988.0]<-0
    nom.mm[nom.mm==8888.0]<-0

    if(choix==1){
      maxx<-as.matrix(colMaxs(as.matrix(nom.mm), na.rm=T))
      colnames(maxx)<-c(nom.propre.station)
      maxi.out<-cbind(maxi.out,maxx)
    }
    if(choix==2){
      date.debut<-date_debut
      date.fin<-date_fin
      st.rain<-as.data.frame(date.debut[,i+1])
      on.rain<-as.data.frame(date.fin[,i+1])
      maxx=NULL
      for(j in 1:dim(st.rain)[1]){
        p<-is.na(st.rain[j,1])
        t<-is.na(on.rain[j,1])
        if(p==T|t==T){
          AB= NA_real_
          maxx= rbind(maxx,AB)
          next()
        }else{ 
          A=st.rain[j,1]
          B=on.rain[j,1]
          AB= max(nom.mm[A:B,j],na.rm = T)
          maxx=rbind(maxx,AB)
             }
      }
      colnames(maxx)<-c(nom.propre.station)
      maxi.out<-cbind(maxi.out,maxx)
      
    
    
  }
  

  }
  names.1<-colnames(maxi.out)
  names.1[1]<-"Annee"
  colnames(maxi.out)<-names.1
  return(maxi.out)
}


###################################nombre de jour##########################
nbjour<-function(chemin.in,choix,date_debut,date_fin,hauteur){
  nom.station<-list.files(path = chemin.in) # lecture des stations dans le dossier
  lecture.extract.anne<-read.xlsx(paste(chemin.in,nom.station[1],sep = "/"),1)
  nom.ligne<-sub("X","", colnames(lecture.extract.anne))#année extraite
  nbre.station<-length(nom.station)
  nbjr.out<-as.numeric(nom.ligne)
 
  
  for(i in 1: nbre.station){
    nom.propre.station<-sub(".xlsx", "", nom.station[i])
    nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
    nom.mm[nom.mm==9999.0]<-NA_real_
    nom.mm[nom.mm==9988.0]<-0
    nom.mm[nom.mm==8888.0]<-0
    
    if(choix==1){
      nbjr=NULL
      for (j in 1:dim(nom.mm)[2]) {
        nbjr[j]<-length(which(nom.mm[,j]>hauteur))
      }
      nbjr=as.matrix(nbjr)
      colnames(nbjr)<-c(nom.propre.station)
      nbjr.out<-cbind(nbjr.out,nbjr) 
    }
    if(choix==2){
      date.debut<-date_debut
      date.fin<-date_fin
      st.rain<-as.data.frame(date.debut[,i+1])
      on.rain<-as.data.frame(date.fin[,i+1])
      nbjr=NULL
      for(j in 1:dim(st.rain)[1]){
        p<-is.na(st.rain[j,1])
        t<-is.na(on.rain[j,1])
        if(p==T|t==T){
          AB= NA_real_
          nbjr= rbind(nbjr,AB)
          next()
        }else{ 
          A=st.rain[j,1]
          B=on.rain[j,1]
          AB=length(which(nom.mm[A:B,j]>hauteur))
          nbjr=rbind(nbjr,AB)
        }
      }
      colnames(nbjr)<-c(nom.propre.station)
      nbjr.out<-cbind(nbjr.out,nbjr)
      
    }
    
    
  }
  names.1<-colnames(nbjr.out)
  names.1[1]<-"Annee"
  colnames(nbjr.out)<-names.1
  return(nbjr.out)
}


###################################Cumul pluviométrique#########################
cm<-function (chemin.in,choix,date_debut,date_fin){
  nom.station<-list.files(path = chemin.in) # lecture des stations dans le dossier
  lecture.extract.anne<-read.xlsx(paste(chemin.in,nom.station[1],sep = "/"),1)
  nom.ligne<-sub("X","", colnames(lecture.extract.anne))#année extraite
  nbre.station<-length(nom.station)
  cum.out<-as.numeric(nom.ligne)
  
  
  for(i in 1: nbre.station){
    nom.propre.station<-sub(".xlsx", "", nom.station[i])
    nom.mm<-read.xlsx(paste(chemin.in,nom.station[i],sep = "/"),1)
    nom.mm[nom.mm==9999.0]<-NA_real_
    nom.mm[nom.mm==9988.0]<-0
    nom.mm[nom.mm==8888.0]<-0
    
    if(choix==1){
      cum<-as.matrix(colSums(as.matrix(nom.mm), na.rm=T))
      colnames(cum)<-c(nom.propre.station)
      cum.out<-cbind(cum.out,cum)
    }
    if(choix==2){
      date.debut<-date_debut
      date.fin<-date_fin
      st.rain<-as.data.frame(date.debut[,i+1])
      on.rain<-as.data.frame(date.fin[,i+1])
      cum=NULL
      for(j in 1:dim(st.rain)[1]){
        p<-is.na(st.rain[j,1])
        t<-is.na(on.rain[j,1])
        if(p==T|t==T){
          AB= NA_real_
          cum= rbind(cum,AB)
          next()
        }else{ 
          A=st.rain[j,1]
          B=on.rain[j,1]
          AB= sum(nom.mm[A:B,j],na.rm = T)
          cum=rbind(cum,AB)
        }
      }
      colnames(cum)<-c(nom.propre.station)
      cum.out<-cbind(cum.out,cum)
      
      
      
    }
    
    
  }
  names.1<-colnames(cum.out)
  names.1[1]<-"Annee"
  colnames(cum.out)<-names.1
  return(cum.out)
}


############################################################################################################################################################







#' Choose a Folder Interactively (Mac OS X)
#'
#' Display a folder selection dialog under Mac OS X
#'
#' @param default which folder to show initially
#' @param caption the caption on the selection dialog
#'
#' @details
#' Uses an Apple Script to display a folder selection dialog.  With \code{default = NA},
#' the initial folder selection is determined by default behavior of the
#' "choose folder" Apple Script command.  Otherwise, paths are expanded with
#' \link{path.expand}.
#'
#' @return
#' A length one character vector, character NA if 'Cancel' was selected.
#'
if (Sys.info()['sysname'] == 'Darwin') {
  choose.dir = function(default = NA, caption = NA) {
    command = 'osascript'
    args = '-e "POSIX path of (choose folder{{prompt}}{{default}})"'
    
    if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
      prompt = sprintf(' with prompt \\"%s\\"', caption)
    } else {
      prompt = ''
    }
    args = sub('{{prompt}}', prompt, args, fixed = T)
    
    if (!is.null(default) && !is.na(default) && nzchar(default)) {
      default = sprintf(' default location \\"%s\\"', path.expand(default))
    } else {
      default = ''
    }
    args = sub('{{default}}', default, args, fixed = T)
    
    suppressWarnings({
      path = system2(command, args = args, stderr = TRUE)
    })
    if (!is.null(attr(path, 'status')) && attr(path, 'status')) {
      # user canceled
      path = NA
    }
    
    return(path)
  }
} else if (Sys.info()['sysname'] == 'Linux') {
  choose.dir = function(default = NA, caption = NA) {
    command = 'zenity'
    args = '--file-selection --directory --title="Choose a folder"'
    
    suppressWarnings({
      path = system2(command, args = args, stderr = TRUE)
    })
    
    #Return NA if user hits cancel
    if (!is.null(attr(path, 'status')) && attr(path, 'status')) {
      # user canceled
      return(default)
    }
    
    #Error: Gtk-Message: GtkDialog mapped without a transient parent
    if(length(path) == 2){
      path = path[2]
    }
    
    return(path)
  }
}

#' Directory Selection Control
#'
#' Create a directory selection control to select a directory on the server
#'
#' @param inputId The \code{input} slot that will be used to access the value
#' @param label Display label for the control, or NULL for no label
#' @param value Initial value.  Paths are exapnded via \code{\link{path.expand}}.
#'
#' @details
#' This widget relies on \link{\code{choose.dir}} to present an interactive
#' dialog to users for selecting a directory on the local filesystem.  Therefore,
#' this widget is intended for shiny apps that are run locally - i.e. on the
#' same system that files/directories are to be accessed - and not from hosted
#' applications (e.g. from shinyapps.io).
#'
#' @return
#' A directory input control that can be added to a UI definition.
#'
#' @seealso
#' \link{updateDirectoryInput}, \link{readDirectoryInput}, \link[utils]{choose.dir}
directoryInput = function(inputId, label, value = NULL) {
  if (!is.null(value) && !is.na(value)) {
    value = path.expand(value)
  }
  
  tagList(
    singleton(
      tags$head(
        tags$script(src = 'js/directory_input_binding.js')
      )
    ),
    
    div(
      class = 'form-group directory-input-container',
      shiny:::`%AND%`(label, tags$label(label)),
      div(
        span(
          class = 'col-xs-9 col-md-11',
          style = 'padding-left: 0; padding-right: 5px;',
          div(
            class = 'input-group shiny-input-container',
            style = 'width:100%;',
            div(class = 'input-group-addon', icon('folder-o')),
            tags$input(
              id = sprintf('%s__chosen_dir', inputId),
              value = value,
              type = 'text',
              class = 'form-control directory-input-chosen-dir',
              readonly = 'readonly'
            )
          )
        ),
        span(
          class = 'shiny-input-container',
          tags$button(
            id = inputId,
            class = 'btn btn-default directory-input',
            '...'
          )
        )
      )
    )
    
  )
  
}

#' Change the value of a directoryInput on the client
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param inputId The id of the input object.
#' @param value A directory path to set
#' @param ... Additional arguments passed to \link{\code{choose.dir}}.  Only used
#'    if \code{value} is \code{NULL}.
#'
#' @details
#' Sends a message to the client, telling it to change the value of the input
#' object.  For \code{directoryInput} objects, this changes the value displayed
#' in the text-field and triggers a client-side change event.  A directory
#' selection dialog is not displayed.
#'
updateDirectoryInput = function(session, inputId, value = NULL, ...) {
  if (is.null(value)) {
    value = choose.dir(...)
  }
  session$sendInputMessage(inputId, list(chosen_dir = value))
}

#' Read the value of a directoryInput
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param inputId The id of the input object
#'
#' @details
#' Reads the value of the text field associated with a \code{directoryInput}
#' object that stores the user selected directory path.
#'
readDirectoryInput = function(session, inputId) {
  session$input[[sprintf('%s__chosen_dir', inputId)]]
}




