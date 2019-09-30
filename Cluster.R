
#----------------------------------------------------------------------------------------------


### ALGORITMO de cluster hierárquico, parte do projeto de fim de curso :
## RECONHECIMENTO DE ATIVIDADES HUMANAS VIA APRENDIZADO DE MAQUINA EM AMBIENTE RESIDENCIAL
## Engenharia de COntrole e Automação - UFMG
## Autor : André Lage 
## Email : andrelagerocha@gmail.com
## github : andrelr22

# ----------------------------------------------------------------------------------------------



rm(list=ls())

#creates a matrix of distances for the house rooms. If both patterns happens in the same room, the distance is 0, otherwise the distance is 1
checkDistRoom<-function(room){
  
  
  patterns_numb<-length(room)
  dif<-matrix(nrow = patterns_numb, ncol = patterns_numb)
   for(i in 1:patterns_numb){
    for(j in 1:patterns_numb){
      
      if(room[i]==room[j]){
        
        dif[i,j]=0
        
      }else{
        
        dif[i,j]=1
        
      }
      
    }
    
    
  }
  return(dif)
  
}

#get mode of a vector
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#  read all instances labels from a pattern (including labels from its variations) and determine an label for it.
getLabel<-function(pattern, labels_instances, variation_table, threshold){
  
  variation_index<-getPatternVar(pattern, variation_table)
  
  pattern_labels <- unlist(strsplit(labels_instances, '-'))
  

  instances<-NULL
  labels<-NULL
  for(i in 1:length(pattern_labels)){
    
    aux<-as.numeric(strsplit(pattern_labels[i], "@")[[1]])
    instances<-c(instances,aux[2])
    labels<-c(labels,aux[1])
  }
  
  selected_variations<-variation_table[variation_index,7]
  if(is.null(variation_index) == 0){
    
    for(i in 1:length( selected_variations)){
      variation_labels<-unlist(strsplit(selected_variations[i], '-'))
      for(j in 1:length(variation_labels)){
 
        aux<-as.numeric(strsplit( variation_labels[j], "@")[[1]])
        
        if(aux[1]%in%labels){
          
          instances[which(labels == aux[1])] = instances + aux[2]
          
        } else {
          
          instances<-c(instances,aux[2])
          labels<-c(labels,aux[1])
          
        }
        
      }
      
    }
    
  }

  
  
  all_instances <- sum(instances)
  
  for(i in 1:length(instances)){
    
    if(instances[i]/all_instances > threshold){
      
      return (labels[i])
    }
    
  }
  
  return ('X')
}

# gets the room that a pattern happens, using mode .

getRoom <-function(pattern1,instances1, variation_table,factors) {
  
  variation_index1<-getPatternVar(pattern1,variation_table)
  
  
  pattern_room<-houseRooms(pattern1,factors)
  if(is.null(variation_index1)){
    
    
    return(houseRooms(pattern1,factors))
    
  }else {
    
    variation_room<-NULL
    for(i in 1:length(variation_index1)){
      
      variation_room<-c(variation_room, houseRooms(variation_table[variation_index1[i],2],factors))
      
    }
    

    variation_instances<-variation_table[variation_index1,3]
    
    
    variation_instances<-c(variation_instances,instances1)
    variation_room<-c(variation_room,pattern_room)
    
    
    for(i in 1:length(variation_room)){
      
      mode_vector<-rep(variation_room[i],variation_instances[i])
      
    }
    
    md<-Mode(mode_vector)
    return(md)
    
  }
  
}

# reads a pattern and, based on its sensors, determines which room this pattern happend

houseRooms<-function(pattern,factors){
  
  
  
  LivingRoom<-c('M01','M02','M03','M04','M05','M06','M07','M08','M09','M10','M11','M12','M13','M14','M15','I08','I09')
  Kitchen <-c('M16','M17','T02','M18','AD1-B','AD1-C','AD1-A','D01','I01','I02','I03','I04','I05','I06','I07')
  Storage<-c('M19','M20')
  LeftLower<-c('M23','M21','M22','M24','M25','M26')
  

  aux<-as.numeric(unlist(strsplit(pattern,'-')))
  location<-NULL 
  
    for(j in 1:length(aux)) {

      if(factors[aux[1]]%in%LivingRoom){
        
        location<-c(location,1)
        
      }else if(factors[aux[j]]%in%Kitchen){
        
        location<-c(location,2)
        
      }else if(factors[aux[j]]%in%Storage){
        
        location<-c(location,3)
        
      }else if(factors[aux[j]]%in%LeftLower){  
        
        location<-c(location,4)
        
      }else{
      
        print('sensor não cadastrado em houseRooms')

    }
    
  }

  return(Mode(location) )
}

# calculates the levenshtein distance

levenshtein <-function(a,b){
 
  leven <- adist(intToUtf8(a),intToUtf8(b))
  module<-max(length(a),length(b))

  return(1 - (leven/module))
  
}

# gets a similarity distance matrix, comparing all patterns
SimilarityDist<-function(patterns, variations){
  num_patterns<-nrow(patterns)
  similarity<-matrix(nrow = num_patterns , ncol = num_patterns)
  for(i in 1:num_patterns){
    for(j in 1:num_patterns){
      

      similarity[i,j]<-getSimilarity(patterns[i,1],patterns[j,1],as.numeric(patterns[i,2]),as.numeric(patterns[j,2]), variations)
      
    }
    
    
    
  }
  return(similarity)
  
}

# gets the average duration of a pattern

getDuration <-function(pattern1,instances1, duration, variation_table) {
  
  variation_index1<-getPatternVar(pattern1,variation_table)
  
  
  
  if(is.null(variation_index1)){
    
    
    return(duration)
    
  }else {
    
    variation_duration<-variation_table[variation_index1,5]
    variation_instances<-variation_table[variation_index1,3]
    
    
    variation_instances<-c(variation_instances,instances1)
    variation_duration<-c(variation_duration,duration)

    avr<-weighted.mean(as.numeric(variation_duration),as.numeric(variation_instances))
    return(avr)
    
  }
  
}
#gets the average start time of a pattern

getStartTime <-function(pattern1,instances1, start_time, variation_table) {
  
  
  variation_index1<-getPatternVar(pattern1,variation_table)
 
  

  if(is.null(variation_index1)){
    
    
    return(start_time)
    
  }else {
    
    variation_start_time<-variation_table[variation_index1,6]
    variation_instances<-variation_table[variation_index1,3]
    

    variation_instances<-c(variation_instances,instances1)
    variation_start_time<-c(variation_start_time,start_time)
    avr<-weighted.mean(as.numeric(variation_start_time),as.numeric(variation_instances))
    return(avr)
    
  }
  
}
#caclulates the average similarity between 2 patterns (including all variations)

getSimilarity <-function(pattern1, pattern2,instances1, instances2, variation_table) {
  
  variation_index1<-getPatternVar(pattern1,variation_table)
  variation_index2<-getPatternVar(pattern2,variation_table)
  
  pattern1<-strsplit(pattern1,'-')
  pattern2<-strsplit(pattern2,'-')
  leven_patterns<-levenshtein(as.numeric(unlist(pattern1)),as.numeric(unlist(pattern2)))
  leven<-NULL
  if(is.null(variation_index1) && is.null(variation_index2)){

    
    return(leven_patterns)
    
  }else if(is.null(variation_index1)){

    variation<-variation_table[variation_index2,1]
    variation_instances<-variation_table[variation_index2,3]
    
    for(i in 1:length(variation_index2)){
      
      var_splt<-strsplit(variation[i],'-')
      leven<-c(leven,levenshtein(as.numeric(unlist(pattern1)),as.numeric(unlist(var_splt))))
      
    }
    leven<-c(leven,leven_patterns)
    variation_instances<-c(variation_instances,instances2)
    avr_leven<-weighted.mean(leven,variation_instances)
    return(avr_leven)
    
    
  }else if(is.null(variation_index2)){

    variation<-variation_table[variation_index1,1]
    variation_instances<-variation_table[variation_index1,3]
    
    for(i in 1:length(variation_index1)){
      
      var_splt<-strsplit(variation[i],'-')
      leven<-c(leven,levenshtein(as.numeric(unlist(pattern2)),as.numeric(unlist(var_splt))))
      
    }
    leven<-c(leven,leven_patterns)
    variation_instances<-c(variation_instances,instances1)
    avr_leven<-weighted.mean(leven,variation_instances)
    return(avr_leven)
    
  }else{

    return(leven_patterns)
  }

  
}

# get all variations from a pattern 
getPatternVar<-function (pattern, variation_table){
  
  variations_index<-NULL
  for(i in 1:nrow(variation_table)){
    
    if(pattern == variation_table[i,2]){
      
      variations_index<-c(variations_index,i)
      
    }
    
  }
  
  return (variations_index)
  
}


### INPUT STARTS --------------------------
## csv's folder, change it to suit your local machine
setwd("C:/Users/andre/Desktop/UFMG/PFC/Codigos/Cluster/teste 1 - 0705/")

#more info about those inputs in DVSM.R
patterns <-read.csv('trie_values.csv')
variations<-read.csv('var_values.csv')
factors<-read.csv('factor_levels.csv')

## INPUT ENDS -----------------------------------------

#pre process csv data

raw_patterns <- patterns[2]
raw_patterns <-raw_patterns$x
raw_patterns <-as.character(raw_patterns)


patterns<-NULL
for(i in 1:length(raw_patterns)){
  
  patterns<-rbind(patterns,unlist(strsplit(raw_patterns[i],'!')))
  
}


#pre process csv data


raw_variations <- variations[2]
raw_variations <-raw_variations$x
raw_variations <-as.character(raw_variations)

# pre process csv data

variations<-NULL
for(i in 1:length(raw_variations)){
  
  variations<-rbind(variations,unlist(strsplit(raw_variations[i],'!')))
  
}



factors<-factors$V1
factors<-as.character(factors)

start_time<-NULL
duration<-NULL
similarity<-NULL
roomy<-NULL
labels<-NULL

#label threshold. If more than 45% of all instances from a pattern X are labeled as activity A1, this pattern label becames A1. 
# If none of the activities get over 45% instances of that pattern, it will not have a label.

label_threshold<-0.45

#separate the pattern table into vectors
for(i in 1:nrow(patterns)){
  
  start_time<-c(start_time,getStartTime(patterns[i,1], patterns[i,2], patterns[i,5], variations))
  duration<-c(duration,getDuration(patterns[i,1], patterns[i,2], patterns[i,4], variations))
  roomy <-c(roomy,getRoom(patterns[i,1],patterns[i,2],variations,factors))
  labels<-c(labels,getLabel(patterns[i,1],patterns[i,6],variations, label_threshold))
}

bk_duration <-duration
bk_roomy <-roomy
bk_start_time <-start_time

start_time<-as.numeric(start_time)
duration<-as.numeric(duration)
dist_room<-checkDistRoom(roomy)


room<-as.dist(dist_room)
start_time<-dist(start_time)
duration <- dist(duration)

sim_matrix<-SimilarityDist(patterns,variations)
sim_matrix<-abs(1-sim_matrix)
similarity<-as.dist(sim_matrix)

A=0.013* 0 #START TIME
B=1 * 0.25 ## DURATION
C=41 * 4### SIMILARITY 
D=98 * 0.5 ### ROOM

# calculates the distance matrix
dist_matrix<-start_time*A + duration*B + similarity*C + room*D
dist_matrix<-dist_matrix/mean(dist_matrix)

# cluster algorithm 
hc.complete =hclust(dist_matrix, method="average", members=NULL)

# plots the resulting dendogram
plot(hc.complete ,main="Dendograma", xlab="", sub="",
     cex=.9)

classes<-cutree(hc.complete,5)



####################### DATA ANALISE 



medias<-NULL
desvio<-NULL
resulting_clusters<-matrix(0,nrow = 5, ncol = 5)
for( i in 1:5){
  
  aux<-bk_duration[which(labels == i)]
  medias<-c(medias,mean(as.numeric(aux)))
  desvio <-c(desvio,sd(as.numeric(aux)))        
   a<-which(classes == i)
   cluster_labs<-labels[a]
   clust_table<-as.matrix(table(cluster_labs))
   
    rname <- rownames(clust_table)
    aux <- which(rname=='X') 
    if(length(aux) !=0){
      
      clust_table<-clust_table[-aux]
      rname <-rname[-aux] 
      
    }

    resulting_clusters[i,as.numeric(rname)]<-clust_table
    
   
            
}

write.csv(resulting_clusters, file = 'resultadosClustertest1.csv')

### OUTPUTS - 

#resulting_clusters - a table with the results. 
#Each row contains an activity and each collumn a cluster. In a way that [1,1] for example contains the number of patterns
#labeled as activity 1 that were grouped as part of cluster 1

resulting_clusters


