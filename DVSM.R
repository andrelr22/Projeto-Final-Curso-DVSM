
rm(list=ls())
library(discretization)
library(prodlim)
library(abind) ## realiza o bind entre 2 matrizes 
library(triebeard)
library(stringr) ## obtem contagem de '-' na string

convertTrieLabel <-function(old_label, new_label){
  
  old_label_vector<- unlist(strsplit(old_label,'-'))
  
  instances<-NULL
  labels<-NULL
  for(i in 1:length(old_label_vector)){
    
    aux<-as.numeric(strsplit(old_label_vector[i], "@")[[1]])
    instances<-c(instances,aux[2])
    labels<-c(labels,aux[1])
  }
  
  return_label<-NULL
  if(new_label%in%labels){
    
    for(i in 1:length(labels)){
      
      if(labels[i]==new_label) {
        
        new_instance<-as.numeric(instances[i])+1

        aux<-paste(labels[i],new_instance, sep = '@')
        
        return_label <-paste(return_label,aux, sep = '-')
      } else {
        
        aux<-paste(labels[i],instances[i], sep='@')
        
        return_label <-paste(return_label,aux, sep = '-')
        
      }
      
      
    }
    return_label<-substring(return_label, 2)
    return(return_label)
    
  }else{
    
    new <- paste(new_label,1,sep = '@')
    ret_label<-paste(old_label,new, sep = '-')
    return(ret_label)
  }
}

getLabel<-function(pattern_begins, pattern_ends,conversion_table,end_act){
  
  
  converted_begins<- conversion_table[pattern_begins]
  converted_ends <- conversion_table[pattern_ends]
  
  end_act_index<-end_act[,2]
  


  minimum<-min( end_act_index[ end_act_index>=converted_begins])
  first<- which(end_act_index == minimum) # gets which activity the pattern's first sensor is in
  

  
  minimum<-min( end_act_index[ end_act_index>=converted_ends])
  last<- which(end_act_index == minimum)
  
  

  
  if(first==last){
    
    return(end_act[first,1])
    
  }else {
    
    return(as.numeric(paste(end_act[first,1],end_act[last,1], sep = "")))
    
  }
  
}

getAvrgStartTime<-function(instances, prev_start_time, start_time){
  
  instances<-instances-1
  midnight<-'00:00:00'
  
  midnight<-strptime(midnight, format ='%H:%M:%OS')
  prev_start_time<-strptime(prev_start_time, format ='%H:%M:%OS')
  start_time<-strptime(start_time, format ='%H:%M:%OS')
  
  start_time = as.numeric((difftime(start_time, midnight, units = "secs")))
  prev_start_time = as.numeric((difftime(prev_start_time, midnight, units = "secs")))
  
  AvrgTime<-((prev_start_time*instances)+start_time)/(instances+1)
  
  return(AvrgTime)
}

getDuration<-function(firstSensor, size, data, conversion_table){
  
  conversion_table_index<-firstSensor:(firstSensor + size -1)
  sequence_sensors_index<-conversion_table[conversion_table_index]
  
  sequence_sensors<-as.character(data[sequence_sensors_index,3])
  j=1
  count = size

  while(j<=length(sequence_sensors)){

      if(startsWith(as.character(sequence_sensors[j]),'AD1') || startsWith(as.character(sequence_sensors[j]),'D') || startsWith(as.character(sequence_sensors[j]),'I') ) {

        sequence_sensors<-sequence_sensors[-j]  #sensores começados em AD1 e D não possuem fim, sendo instantâneos, dessa forma o algoritmo nao procurará por seu fim
        count=count-1
        j=j-1
      }
    
    j=j+1
  }
  

  i=sequence_sensors_index[1]+1
  mark=0
  while(count!=0 && i <= nrow(data)){
    
    if((i %in% sequence_sensors_index) == FALSE){ #confere se i não é igual a uma das ativações de sensores da sequencia
       aux <- as.character(data[i,3])
        if(aux %in%  sequence_sensors  ){
          count=count - 1
          sequence_sensors[match(aux,sequence_sensors)] <- 'DONE'
          mark = i 
        }
      
    }
    
    i=i+1  
  }
  
    
    
  
  
  start = sequence_sensors_index[1]
  end = max(sequence_sensors_index[length(sequence_sensors_index)], mark ) #caso um sensor instantâneo tenha ação posterior ao termino do ultimo sensor não instantãneo, o fim é dado na ativação do instantaneo

  
  start_time=paste(as.character(data[start,1]),as.character(data[start,2]), sep = ' ')
  end_time=paste(as.character(data[end,1]),as.character(data[end,2]), sep = ' ') 
  
  POSIX_start = strptime(start_time, format ='%Y-%m-%d %H:%M:%OS'  )
  POSIX_end = strptime(end_time, format ='%Y-%m-%d %H:%M:%OS'  )
  delta_t = (difftime(POSIX_end, POSIX_start, units = "secs"))
  
  retlist<-list(as.numeric(delta_t),as.character(data[start,2]))
  #print(as.numeric(delta_t))
  return(retlist)
}

filteredData<-function(data){
  
  sensors<-NULL
  conversion_table<-NULL
  
  for(i in 1:nrow(data)){
    
    if(startsWith(as.character(data[i,3]),'M') && as.character(data[i,4]) =='ON' ){
      sensors<-c(sensors,data[i,3])
      conversion_table<-c(conversion_table,i)
        
    } else if(startsWith(as.character(data[i,3]),'I') && as.character(data[i,4]) =='PRESENT'){
      
      
      sensors<-c(sensors,data[i,3])
      conversion_table<-c(conversion_table,i)
      
    } else if(as.character(data[i,3]) == 'asterisk' && as.character(data[i,4]) =='START'){
      
      
      sensors<-c(sensors,data[i,3])
      conversion_table<-c(conversion_table,i)
      
    } else if(startsWith(as.character(data[i,3]),'AD1') ){
      
      
      sensors<-c(sensors,data[i,3])
      conversion_table<-c(conversion_table,i)
      
    } else if(startsWith(as.character(data[i,3]),'D') ){
      
      
      sensors<-c(sensors,data[i,3])
      conversion_table<-c(conversion_table,i)
      
    }
    
  } 
  

  retlist<-list(sensors,conversion_table)
  return(retlist)
  
}

getData<-function(){
  setwd("C:/Users/andre/Desktop/UFMG/PFC/Datasets/adlnormal")
  data<-NULL
  
 
  aux<-read.table("p01.t1")
  data<-rbind(data,aux)
  act_end<-c(1,nrow(data))
  
  
  aux<-read.table("p01.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))

  
  aux<-read.table("p01.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p01.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  

  aux<-read.table("p01.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))

  aux<-read.table("p02.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p02.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p02.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p02.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p02.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p03.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p03.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p03.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p03.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p03.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p04.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p04.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p04.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p04.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p04.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p05.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p05.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p05.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p05.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p05.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p06.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p06.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p06.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p06.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p06.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p07.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p07.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p07.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p07.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p07.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p08.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p08.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p08.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p08.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p08.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p09.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p09.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p09.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p09.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p09.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p10.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p10.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p10.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p10.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p10.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p11.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p11.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p11.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p11.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p11.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p12.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p12.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p12.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p12.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p12.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p13.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p13.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p13.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p13.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p13.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p14.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p14.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p14.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p14.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p14.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p15.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p15.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p15.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p15.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p15.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p16.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p16.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p16.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p16.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p16.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p32.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p32.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p32.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p32.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p32.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p40.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p40.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p40.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p40.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p40.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p41.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p41.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p41.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p41.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p41.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p42.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p42.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p42.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p42.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p42.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p43.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p43.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p43.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p43.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p43.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p49.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p49.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p49.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p49.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p49.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p50.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p50.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p50.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p50.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p50.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  aux<-read.table("p51.t1")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(1,nrow(data)))
  
  
  aux<-read.table("p51.t2")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(2,nrow(data)))
  
  
  aux<-read.table("p51.t3")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(3,nrow(data)))
  
  
  aux<-read.table("p51.t4")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(4,nrow(data)))
  
  
  aux<-read.table("p51.t5")
  data<-rbind(data,aux)
  act_end<-rbind(act_end,c(5,nrow(data)))
  
  
   
  

  retlist<-list(data, act_end)
  return(retlist)
}


isInside<-function(A,B){
  j=1
  count=0
  disc=0
  
  if(A[1]!=B[1]){
    return(0)
  }
  
  lenA<-length(A)
  lenB<-length(B)
  if(A[lenA]!=B[lenB]){
    return(0)
  }
  
  for(i in 1:lenA){
    
    if(is.na(B[j])){

      return (1/(1 + (lenA-lenB)/lenB))
    }
    
    if(A[i]==B[j]){
      j=j+1
    }
  }
 
  if(j==(length(B)+1)){
    
    return (1/(1 + (lenA-lenB)/lenB))
  } else {
    
    return (0)
  }
   
}

Continuity<-function(){
  return(1)
}

softMax<-function(c){''
  ret<-1/(1+exp(c))
  return(ret)
}

checkDisc<-function(S,trie,trie_var){
  
  sequence<-as.numeric(unlist(strsplit(S,'-')))
  keys<-get_keys(trie)
  
  return_patterns<-NULL
  for(i in 1:length(trie)){
    pattern<-as.numeric(unlist(strsplit(keys[i],'-')))
    
    aux<-NULL
    disc<-isInside(sequence,pattern) # INVERTER 
 
     if(disc != 0 ){

      
      for(j in 1:length(pattern)){
        
        aux<-paste(aux, pattern[j], sep = '-')
        
        
      }
      aux<-paste(aux,disc, sep = '!')
      aux<-substring(aux, 2)
      return_patterns<-c(return_patterns,aux)
      
      
    }
      
      
  }
  
  return_patterns<-c(return_patterns,'var')
  
  keys<-get_keys(trie_var)
  for(i in 1:length(trie_var)){ ## compara a sequencia com todas as variaçoes
    pattern<-as.numeric(unlist(strsplit(keys[i],'-'))) #transforma padrão em vetor numérico
    
    if(keys[i]!='root'){ ## apenas realiza a comparaçao se n for a root
      
      aux<-NULL
      disc<-isInside(sequence,pattern) # INVERTER 
      if(disc != 0 ){ ## caso haja descontinuidades, adiciona o padrão a sequência de retorno, junto com a descontinuidade para aquela instância
  
        
        for(j in 1:length(pattern)){
          
          aux<-paste(aux, pattern[j], sep = '-')
          
          
        }
        aux<-paste(aux,disc, sep = '!')
        aux<-substring(aux, 2)

        return_patterns<-c(return_patterns,aux)
        
        
      }
      
      
    }
    

    
    
  }
  
    return(return_patterns)
  }
  
  
compressionPattern<-function(D, pattern_size, instances_numb, continuity){
  
  C=length(D)*continuity/(pattern_size + descriptionLength(D,pattern_size,instances_numb))
  
  
  return(softMax(C))
}

compressionVariation<-function(D,pattern_size,variation_size,pattern_instances_numb, variation_instances_numb, continuity){
  
  Num=(descriptionLength(D,pattern_size,pattern_instances_numb) + pattern_size)*continuity
  Den=(descriptionLength(D,variation_size,variation_instances_numb) + variation_instances_numb)
  
  C=Num/Den
  
  return(softMax(C))
  
  
}

## TODO PENSAR DEPOIS -> É POSSIVEL QUE VÁRIOS PADRÕES PASSEM DO THRESHOLD, DEVEMOS CONSIDERAR O PRIMEIRO COMO PADRÃO "PAI" DA VARIAÇÃO?
checkVar<-function(threshold,trie,possible_pattern,trie_var){

  debug_variable=0 ##TODO deletar depois
  selected_pattern=-1
  var_values<-get_values(trie_var)
  keys<-get_keys(trie) ##OBTEM TODAS AS CHAVES
  possible_pattern<-unlist(strsplit(possible_pattern,'-'))
  possible_pattern_size<-length(possible_pattern)

  for(i in 1:length(keys)){

    pattern<-strsplit(keys[i],'-') ## COLOCA A CHAVE EM FORMATO ADEQUADO PARA A FUNÇÃO LEVENSTEIN
    leven<-levenshtein(as.numeric(unlist(pattern)), (as.numeric(unlist(possible_pattern))))
    
    var_count=0
    avr_leven<-leven 
    for(j in 1:length(var_values)){

      father_pattern<-unlist(strsplit( var_values[j],'!'))[2]
      compared_variation <- unlist(strsplit( var_values[j],'!'))[1]
      compared_variation <- unlist(strsplit(compared_variation,'-'))
      compared_size<-length(compared_variation)

      if(keys[i]==father_pattern && compared_size<possible_pattern_size){


        father_pattern<-strsplit(keys[i],'-')
        leven_var<-levenshtein(as.numeric(unlist(father_pattern)),(as.numeric(unlist(possible_pattern))))
        var_count = var_count+1
        avr_leven = avr_leven + leven_var
      }
      
      
    }

    leven<-avr_leven/(var_count+1)

    if(1 > leven && leven > threshold){ ##CASO A SIMILARIDADE SEJA SUPERIOR AO THRESHOLD, É UMA VARIAÇÃO
      
      

      selected_pattern<-i
      threshold<-leven
      debug_variable<-debug_variable+1

      
    }
    

      
  }
  if(selected_pattern == -1){

    return(-1)
    
  }else {

    return(keys[selected_pattern])
      
  }
 
  
} 

addTrieVar<-function(trie_var,pattern, father_pattern, duration, start_time,label){
  
  if(as.numeric(duration)> 1000 || as.numeric(duration < 0)){
    
    return(trie_var)
    
  }

  match<-longest_match(trie_var,pattern)
  
  if(is.na(match)){
    match<-'#@9$#$Af#=+#NA'
  }else{
    match<-unlist(strsplit(match,'!'))[1]
  }

  if( pattern == match){ ##testa se a variação ja está na trie
    

    
    keys <- get_keys(trie_var) 
    position <- which(keys==pattern)
    
    values <- get_values(trie_var)
    pattern_value<-values[position]
    instance_count<- as.numeric(unlist(strsplit(pattern_value,'!'))[3]) + 1  
    new_disc<-( (as.numeric(unlist(strsplit(pattern_value,'!'))[4]) * (instance_count - 1) + 1 )/instance_count) 
    
    
    new_duration <-(( (as.numeric(unlist(strsplit(pattern_value,'!'))[5]) * (instance_count - 1)) + duration )/instance_count)
    
    new_start_time<-(( (as.numeric(unlist(strsplit(pattern_value,'!'))[6]) * (instance_count - 1)) + start_time )/instance_count)
    new_label<-convertTrieLabel(unlist(strsplit(pattern_value,'!'))[7],label)
    
    new_value<-paste(pattern,father_pattern,instance_count,new_disc,new_duration,new_start_time,new_label,sep = '!')
    
    

    trie_add(trie_var, pattern, new_value) ## adiciona 1 ao número de instâncias daquele padrão
  }
  else{
    #print(duration)
    new_value<-paste(pattern,father_pattern,'1','1',duration,  start_time , paste(label,'1', sep = '@') , sep = '!')

    trie_add(trie_var, pattern, new_value) ##adiciona padrão novo na trie
  }
  
  return (trie_var)
  
}

addTrie <-function(trie,pattern,duration,start_time,label){
  
  if(as.numeric(duration)> 1000 || as.numeric(duration < 0)){
    
    return(trie)
    
  }
  match<-longest_match(trie,pattern)
  
  if(is.na(match)){
    match<-'#@9$#$Af#=+#NA'
  }else{
    match<-unlist(strsplit(match,'!'))[1]
  }
  
  if( pattern == match){ ##testa se o padrão ja está na trie

    
    keys <- get_keys(trie) 
    position <- which(keys==pattern)
    
    values <- get_values(trie)
    pattern_value<-values[position]
    instance_count<- as.numeric(unlist(strsplit(pattern_value,'!'))[2]) + 1  
    new_disc<-( (as.numeric(unlist(strsplit(pattern_value,'!'))[3]) * (instance_count - 1) + 1 )/instance_count)
    
    new_duration <-(( (as.numeric(unlist(strsplit(pattern_value,'!'))[4]) * (instance_count - 1)) + duration )/instance_count)
    
    new_start_time<-(( (as.numeric(unlist(strsplit(pattern_value,'!'))[5]) * (instance_count - 1)) + start_time )/instance_count)
    new_label<-convertTrieLabel(unlist(strsplit(pattern_value,'!'))[6],label)

    new_value<-paste(pattern,instance_count,new_disc,new_duration,new_start_time, new_label, sep = '!')

    trie_add(trie, pattern, new_value) ## adiciona 1 ao número de instâncias daquele padrão
  }
  else{
   #print(duration)
    new_value<-paste(pattern,'1', '1', duration,  start_time , paste(label,'1', sep = '@'), sep = '!')

    trie_add(trie, pattern, new_value) ##adiciona padrão novo na trie
  }
  
  return (trie)
}


addDisc<-function(trie,trie_var,disc_vector,duration,start_time, label){
  
  if(as.numeric(duration)> 1000 || as.numeric(duration < 0)){
    return(0)
    
  }
  
  cut<-which(disc_vector=='var')

  
  if(cut>1){

    patterns<-disc_vector[1:(cut-1)]

    
    for(i in 1:length(patterns)){
      
      pattern_sequence<-unlist(strsplit(patterns[i],'!'))[1]
      

      disc_value<-as.numeric(unlist(strsplit(patterns[i],'!'))[2])
      trie_value<-longest_match(trie,pattern_sequence)


 
      instances <-as.numeric(unlist(strsplit(trie_value,'!'))[2])

      prev_disc <-as.numeric(unlist(strsplit(trie_value,'!'))[3])

      new_disc<-(disc_value + (prev_disc*instances))/(instances + 1)
      instances<-instances+1

      
      new_duration <-(( (as.numeric(unlist(strsplit(trie_value,'!'))[4]) * (instances - 1)) + duration )/instances)

      new_start_time<-(( (as.numeric(unlist(strsplit(trie_value,'!'))[5]) * (instances - 1)) + start_time )/instances)
      
      new_label<-convertTrieLabel(unlist(strsplit(trie_value,'!'))[6],label)
      
      pattern_value<- paste(pattern_sequence,instances,new_disc,new_duration,new_start_time,new_label,sep = '!')

      trie_add(trie,pattern_sequence,pattern_value)
      
    }
      
  }
     
  

  if(length(disc_vector)>cut){
    
    variations<-disc_vector[(cut+1):length(disc_vector)]
    
    
    for(i in 1:length(variations)){
      
      variation_sequence<-unlist(strsplit(variations[i],'!'))[1]
      disc_value<-as.numeric(unlist(strsplit(variations[i],'!'))[2])
      trie_value<-longest_match(trie_var,variation_sequence)
      
      if(is.na(trie_value)){
        
        print('padrão não encontrado na trie')
        
      }
      father_pat<-unlist(strsplit(trie_value,'!'))[2]
      instances <-as.numeric(unlist(strsplit(trie_value,'!'))[3])
      prev_disc <-as.numeric(unlist(strsplit(trie_value,'!'))[4])
      
      new_disc<-(disc_value + (prev_disc*instances))/(instances + 1)
      instances<-instances+1
      
      
      new_duration <-(( (as.numeric(unlist(strsplit(trie_value,'!'))[5]) * (instances - 1)) + duration )/instances)
      
      new_start_time<-(( (as.numeric(unlist(strsplit(trie_value,'!'))[6]) * (instances - 1)) + start_time )/instances)
      
      new_label<-convertTrieLabel(unlist(strsplit(trie_value,'!'))[7],label)
      
      variation_value<- paste(variation_sequence,father_pat,instances,new_disc,new_duration,new_start_time, new_label, sep = '!')

      trie_add(trie_var,variation_sequence,variation_value)
      
    }
    
  }
    
  }
  

PruningVar<-function(trie,trie_var,threshold,sensors){
  pattern_keys<-get_keys(trie)
  pattern_values<-get_values(trie)
  
  var_values<- get_values(trie_var)

  
  var_keys<-get_keys(trie_var)
  
  for (i in 1:length(var_values)){
    
    father_pattern<-unlist(strsplit(var_values[i],'!'))[2]

    
    if(father_pattern %in% pattern_keys){
      
      index<-which(pattern_keys==father_pattern)
      
      
      variation_instances_numb <- as.numeric(unlist(strsplit(var_values[i],'!'))[3]) 
      variation_size<-length(unlist(str_split(var_keys[i], "-")))
      
      pattern_instances_numb <- as.numeric(unlist(strsplit(pattern_values[index],'!'))[2]) 
      pattern_size<-length(unlist(str_split(pattern_keys[index], "-")))
      variation_continuity <- as.numeric(unlist(strsplit(var_values[i],'!'))[4]) 
      
      compressed_value<- compressionVariation(sensors, pattern_size, variation_size, pattern_instances_numb, variation_instances_numb, variation_continuity )
      
      
      if(compressed_value>threshold){
        if(var_keys[i] != 'root'){
          
          trie_remove(trie_var,var_keys[i])
          
        }

      }
      
    } else {
      
      if(var_keys[i] != 'root'){
        
        trie_remove(trie_var,var_keys[i])
        
      }
    }

  }
  return(trie_var)
  
  
}


VariationsInstancesNumb<- function(trie_var, pattern){ #obtem o número de instâncias de todas as variações de um padrão
  
  values<-get_values(trie_var)
  
  var_count<-0
  for(i in 1:length(values)){
  
    father_pattern<-unlist(strsplit(values[i],'!'))[2]  #obtem o padrão pai da variação i

    if(pattern == father_pattern) { ## confere se o padrão pai é o mesmo padrão do qual queremos saber o número de variações

    var_count <- var_count + as.numeric(unlist(strsplit(values[i],'!'))[3])  
    }
  }
  
  return(var_count)
}

#TODO -> Conta de bits removidos está parcialmente errada, pq variações não possuem o mesmo tamanho do padrão

Pruning<-function(trie, threshold, sensors, trie_var){

  values<- get_values(trie)
  numb_patterns<-length(values)
  
  keys<-get_keys(trie)
  var_values<-get_values(trie_var)
  var_keys<-get_keys(trie_var)
  
  for (i in 1:length(values)){
    disc=1
    tot_instances=0
    var_count=0
    avr_var_length=0
    for(j in 1:length(var_values)){ # obtem todas as variações daquele padrão, e a continuidade média das variações
      
      aux<-unlist(strsplit( var_values[j],'!'))[2]

      if(aux==keys[i]){
        

        
        var_disc<-as.numeric(unlist(strsplit(var_values[j],'!'))[4])  
        var_instances<-as.numeric(unlist(strsplit(var_values[j],'!'))[3])  
        disc = (((disc*tot_instances) + (var_disc*var_instances))/(tot_instances + var_instances))
        
        var_length<- length(unlist(str_split(var_keys[j], "-")))
        avr_var_length <- weighted.mean(c(avr_var_length,var_length),c(tot_instances,var_instances))
        tot_instances = tot_instances + var_instances
        var_count=var_count+1 #contador do número de variações
        
   
        
      }
    }
    
    disc_pattern = var_disc<-as.numeric(unlist(strsplit(values[i],'!'))[3])  
    instances<-as.numeric(unlist(strsplit(values[i],'!'))[2]) 


    continuity = (((disc*tot_instances) + ( disc_pattern*instances))/(tot_instances +instances))
    
    pattern_instances<- as.numeric(unlist(strsplit(values[i],'!'))[2])
    instances_numb <- pattern_instances + tot_instances
    pattern_size<-length(unlist(str_split(keys[i], "-")))
    
    avr_size<-weighted.mean(c(avr_var_length, pattern_size), c(tot_instances,pattern_instances))
    
    compressed_value<- compressionPattern(sensors, instances_numb, avr_size,continuity )
    

    if(compressed_value>threshold){
      trie_remove(trie,keys[i])

    }
  }
  return(trie)
} 



descriptionLength<-function(D, pattern_size, instances_numb){
  
  removed_bytes<-instances_numb*(pattern_size-1)
  
  DL<-length(D)-removed_bytes

  return(DL)
  }



addPrefixSulfix<-function(sensors,size,trie,trie_var,threshold,data,conversion_table, end_act){
  samples<-length(sensors)
  
  
  
  for (i in 1:samples){
    print(size)
    print((i/samples*100))
    aux<-sensors[i]
    first_sensor_index <- i
    
    #if(size == 3  && ((i/samples*100)>1)){ ## USADO PARA TESTES -> DELETAR DEPOIS
     # readline(prompt="Press [enter] to continue")
      
    #}
    
    
    for(j in 2:size){ ##obtem sequencias de size sensores
      aux<-(c(aux, sensors[i+j-1]))

      
    }
    potencial_pattern<-aux[1]
    for(j in 2:size){ 
      potencial_pattern<-paste(potencial_pattern, aux[j], sep = '-') ##coloca os sensores em formato para a trie, da forma 'X-Y-Z'.
    }
    
    
    
    match_var<-longest_match(trie_var, potencial_pattern)
    
    match<-longest_match(trie,potencial_pattern) ##confere se o padrão está contido na arvore, visando não obter prefixo e sufixos de padrões já prunados
    ## ADD tbm da match com a arvore de variações
    
    if(is.na(match_var)){
      match_var<-'#@9$#$Af#=+#NA'
    }else{
      match_var<-unlist(strsplit(match_var,'!'))[1]
    }
    
    if(is.na(match)){
      match<-'#@9$#$Af#=+#NA'
    }else{
      match<-unlist(strsplit(match,'!'))[1]
    }
    
    # TODO -> CONFERIR SE PADRÕES EXCLUIDOS POR BAIXA FREQUENCIA NÃO TRIGGAM O IF ABAIXO
    
    
    ## ADD adicionar outro if para a trie de variações
    if(match == potencial_pattern || match_var==potencial_pattern) { ### se TRUE, padrão está na arvore
      
      
      if((i+size)<samples) {
        
        retlist<-getDuration(first_sensor_index,(size + 1), data, conversion_table)
        duration<-unlist(retlist[1])
        start_time<-unlist(retlist[2])
        start_time<-getAvrgStartTime(1,'00:00:00',start_time )
        
        aux<-(c(aux, sensors[i+size])) ### ADICIONA SUFIXO AO PADRÃO
        
        pattern<-aux[1]
        
        
        for(j in 2:(size+1)){  ##FORMATA PARA A TRIE
          pattern<-paste(pattern, aux[j], sep = '-')
        }
        
        ret<-checkDisc(pattern,trie,trie_var)


        
        label <- getLabel(i,i+size,conversion_table,end_act)
        
        addDisc(trie,trie_var,ret,duration,start_time,label)
        
        father_pattern <- checkVar(threshold, trie, pattern, trie_var)
        
        if(father_pattern == -1){
          
          trie<-addTrie(trie,pattern,duration,start_time,label)
          
        }else{
          
          trie_var<-addTrieVar(trie_var,pattern, father_pattern,duration,start_time, label)
          
        }
        
        
      }
      
     
      #ADD A TRIE
      
      if(i!= 1){ ## CASO NÃO SEJA A PRIMEIRA INTERAÇÃO, ADICIONA PREFIXO
        
        retlist<-getDuration((first_sensor_index-1),(size + 1), data, conversion_table)
        duration<-unlist(retlist[1])
        start_time<-unlist(retlist[2])
        start_time<-getAvrgStartTime(1,'00:00:00',start_time )
        
        aux<-(c(aux, sensors[i-1]))
        
        pattern<-aux[1]
        
        label <- getLabel(i-1,i+size-1,conversion_table,end_act)
        
        for(j in 2:(size+1)){
          pattern<-paste(pattern, aux[j], sep = '-')
        }
        
        ret<-checkDisc(pattern,trie,trie_var)
        addDisc(trie,trie_var,ret,duration,start_time,label)

        father_pattern <- checkVar(threshold, trie, pattern, trie_var)
        
        if(father_pattern == -1){
          
          trie<-addTrie(trie,pattern,duration,start_time,label)
          
        }else{
          

         
           trie_var<-addTrieVar(trie_var,pattern,father_pattern,duration,start_time,label)
          
        }
      }
    }
  }
  retlist<-list(trie,trie_var)
  return(trie)
}


#add_prefix 
#Ideia base -> utilizando a tabela doubles, procurar todas as instâncias de doubles 
## em complete_list. 



levenshtein <-function(a,b){
  leven <- adist(intToUtf8(a),intToUtf8(b))
  module<-max(length(a),length(b))
  
  return(1 - (leven/module))
  
}

slidingWindow<-function(sensors, data, conversion_table, end_act){
  
  samples<-length(sensors)
  aux<-(c(sensors[1], sensors[2]))
  key<-paste(aux[1], aux[2], sep = '-')
  
  retlist<-getDuration(conversion_table[1],2,data,conversion_table)
  duration<-as.numeric(unlist(retlist[1]))
  start_time<-unlist(retlist[2])
  start_time<-getAvrgStartTime(1,'00:00:00',start_time )
  
  value<-paste(key,1,1,duration,start_time, sep = '!')
  trie<-trie(key,value)

  
  for (i in 2:(samples-1)){

    retlist<-getDuration(i,2,data,conversion_table)
    duration<-as.numeric(unlist(retlist[1]))
    if(duration > 100){
      

    }
    start_time<-unlist(retlist[2])
    start_time<-getAvrgStartTime(1,'00:00:00',start_time ) #converting it to seconds after midnight

    aux<-(c(sensors[i], sensors[i+1]))
    aux<-paste(aux[1], aux[2], sep = '-')
    
    label<-getLabel(i,i+1,conversion_table,end_act)
    
    trie<-addTrie(trie,aux,duration,start_time,label)
    
    
 
  }

  return (trie)
 
  
}

#setwd("C:/Users/andre/Desktop/UFMG/PFC/Datasets/twor.summer.2009/")
#data<-read.csv("data1.csv")

# 40 instancias de um padrão de tamanho 2 com continuidade 1
#compressionPattern((sensors), 80,2,1)
prunning_threshold <-0.263967

#prunning_threshold <-0.263967,0.264967,
#compressionVariation(sensors, pattern_size, variation_size, pattern_instances_numb, variation_instances_numb, variation_continuity )
#compressionVariation(sensors, 3, 4, 100, 2, 1)
# 5 das 50 instâncias são da variação X
prunning_var_threshold<-0.2779242
variance_threshold<-0.85
#prunning_var_threshold<-0.2779242
############################################# DATASET NOVO

retlist<-getData()
data<-retlist[[1]]
end_act<-matrix(as.numeric(retlist[[2]]),ncol = 2)
filtered<-filteredData(data)

sensors<-unlist(filtered[1])
conversion_table<-unlist(filtered[2])

############################################# DATASET ANTIGO 

#data<-read.delim("annotated")
#sensors<-as.numeric(data[,3])

trie_var<-trie('root','root!root!1') 

trie<-slidingWindow(sensors,data,conversion_table, end_act)
trie<-Pruning(trie,prunning_threshold,sensors, trie_var)

old_length<-0
new_length<-length(trie)



size<-2
u<-1
#while(old_length != new_length || u==15){
while(u<25){ 
  print(11)
  trie<-addPrefixSulfix(sensors,size,trie,trie_var,variance_threshold,data,conversion_table, end_act)
  print(12)
  trie<-Pruning(trie,prunning_threshold,sensors, trie_var)
  print(13)
  trie_var<-PruningVar(trie,trie_var,prunning_var_threshold,sensors)
  print(14)
  size<-size+1
  old_length<-new_length
  print('size')
  print(u)
  print('length')

  new_length<-length(trie)

  u<-u+1
}

trie_values <- get_values(trie)
trie_keys <- get_keys(trie)
var_values <-get_values(trie_var)
var_keys <- get_keys(trie_var)
setwd("C:/Users/andre/Desktop/UFMG/PFC/Codigos/Cluster/testes0206 - alterando threshold similaridade/0.65/")
write.csv(trie_values, file ='trie_values.csv')
write.csv(trie_keys, file ='trie_keys.csv')
write.csv(var_values, file ='var_values.csv')
write.csv(var_keys, file ='var_keys.csv')

levels = sapply(data[1,3],levels)
write.csv(levels, file = 'factor_levels.csv')

###TESTBENCH PART, DELETE IT LATTER 
S<-'46-41-42-42-42-42-42-42-36'





##Compression threshold -> 0.3 
##Compression Variation threshold -> 0.1 
##frequent events threshhold -> 0.6

###TODO 
## 1- TESTAR MUDANÇA DE 3 PARA 4 PADRÕES  OK
## 2- CRIAR PODA DE AMOSTRAS SEM DESCONTINUIDADE(DEIXAR UMA CONSTANTE NO LUGAR)
## 3- ELABORAR AS FUNÇÕES DE CONTINUIDADE 
## 4- ATRIBUIR PADRÕES A VARIAÇÕES(UMA VARIAÇÃO É MAIOR EM TAMANHO QUE SEU PADRÃO ORIGINAL, MAS E PARA 2 PADRÕES DE MESMO TAMANHO??)  RELER A PROCURA DE MAIS ITENS FALTANTES
##


####DUVIDAS 

## 1- ONDE ENCONTRO O A SIMILARIDADE MÍNIMA PARA 2 PADRÕES SEREM VARIAÇÕES -> MENOR QUE 0.4?