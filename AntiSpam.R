library("magrittr")

z = NULL

setwd("H:/Spam/")
file_list <- list.files()


for ( f in file_list){
  url = f

s = paste("H:/Spam/", f,sep="")



corpus = paste(readLines(s), collapse=" ") %>% 
  gsub(s, " ", .)


z = c(z,corpus %>% 
  gsub(" +", " ", .) %>% 
  strsplit(split = " ") )



}


esiste= function(stri) {
  
  x=1
  size=length(emails$Word)

  while(x<=size)
  {
    
    if(emails$Word[x]==stri)
    { 
      emails$Count[x]=as.numeric(emails$Count[x])+1

      return(TRUE) 
    }
    x=x+1
  }
  
return(FALSE)
  
}

increm=function(strit){
  y=1
  siz=length(emails$Word)
  
  while(y<=siz)
  {
    
    if(emails$Word[y]==strit)
    { 
      return(y) 
    }
    y=y+1
  }
  
  
  }


emails = data.frame(Word=character(1), Spam=numeric(1), Count=numeric(1),stringsAsFactors=FALSE)
#emails=rbind(emails,c(z[[1]][1],1,1))

colnames(emails)[1]="Word"

colnames(emails)[2]="Spam"

colnames(emails)[3]="Count"

index=0

for( i in 1:length(z) ){
  for (j in 1:length(z[[i]])){
    #print(j)
    
    if ( esiste(z[[i]][j]) ){
      asd=increm(z[[i]][j])
      emails$Count[asd]=as.numeric(emails$Count[asd])+1
   
    }
      
    else{
      emails=rbind(emails,c(z[[i]][j],1,1))
    }
      

  }
 
}


#########################################################################################################################

z = NULL

setwd("H:/Ham/")
file_list <- list.files()


for ( f in file_list){
  url = f
  
  s = paste("H:/Ham/", f,sep="")
  
  
  
  corpus = paste(readLines(s), collapse=" ") %>% 
    gsub(s, " ", .)
  
  
  z = c(z,corpus %>% 
          gsub(" +", " ", .) %>% 
          strsplit(split = " ") )
  
  
  
}


esisteh= function(stri) {
  
  x=1
  size=length(hams$Word)
  
  while(x<=size)
  {
    
    if(hams$Word[x]==stri)
    { 
      hams$Count[x]=as.numeric(hams$Count[x])+1
      
      return(TRUE) 
    }
    x=x+1
  }
  
  return(FALSE)
  
}

incremh=function(strit){
  y=1
  siz=length(hams$Word)
  
  while(y<=siz)
  {
    
    if(hams$Word[y]==strit)
    { 
      return(y) 
    }
    y=y+1
  }
  
  
}


hams = data.frame(Word=character(1), Spam=numeric(1), Count=numeric(1),stringsAsFactors=FALSE)
#emails=rbind(emails,c(z[[1]][1],1,1))

colnames(hams)[1]="Word"

colnames(hams)[2]="Spam"

colnames(hams)[3]="Count"

index=0

for( i in 1:length(z) ){
  for (j in 1:length(z[[i]])){
    #print(j)
    
    if ( esisteh(z[[i]][j]) ){
      asd=incremh(z[[i]][j])
      hams$Count[asd]=as.numeric(hams$Count[asd])+1
      
    }
    
    else{
      hams=rbind(hams,c(z[[i]][j],1,1))
    }
    
    
  }
  
}




pres= function(wordz,df) {
  
  x=1
  size=length(df$Word)
  
  while(x<=size)
  {
    
    if(df$Word[x]==wordz)
      return(TRUE) 

    x=x+1
  }
  
  return(FALSE)
  
}

getInd= function(wordz,df) {
  
  x=1
  size=length(df$Word)
  
  while(x<=size)
  {
    if(df$Word[x]==wordz)
      return(x) 
    
    x=x+1
  }
  
  
}

#########################################################################################################################

spamicity = data.frame(Word=character(1), Spam=numeric(1), stringsAsFactors=FALSE)
z=NULL

setwd("H:/Inbox/")
file_list <- list.files()


for ( f in file_list){
  print("Valore di f")
  print(f)
  print("Valore di file_list length")
  print(length(file_list))
  url = f
  
  s = paste("H:/Inbox/", f,sep="")
  
  
  
  corpus = paste(readLines(s), collapse=" ") %>% 
    gsub(s, " ", .)
  
  
  z = c(z,corpus %>% 
          gsub(" +", " ", .) %>% 
          strsplit(split = " ") )
  
  
  
}


tmp=0
i=0
j=0
for( i in 1:length(z) ){

 for (j in 1:length(z[[i]])){

     if(pres(z[[i]][j],emails) & pres(z[[i]][j],hams) )
     {
       
       hind=getInd(z[[i]][j],hams)
       sind=getInd(z[[i]][j],emails)
       print(hind)
       print(sind)
       
       tmp=tmp+as.integer(emails$Count[sind])/(as.integer(emails$Count[sind])+as.integer(hams$Count[hind]))
       
     }
   else if (pres(z[[i]][j],emails))
   {
    tmp=tmp+0.9 
    
   }
   else if(pres(z[[i]][j],hams))
   {
     tmp=tmp+0.1
   }
   else
     tmp=tmp+0.5
    
    
 }


  print("Valore tmp")
  print(tmp)
  perc = tmp/( length(z[[i]]) )
  spamicity = rbind(spamicity,c( file_list[i], perc ) ) 
  tmp=0
  
        }

  