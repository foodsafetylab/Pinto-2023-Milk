# FUNCTIONS ---------------------------------------------------------------



#--Function for searching data frame--
# a=Data Frame looking b= Colum in data frame c= Keywrd using "", d=number of selections
#WARNING, has errors, not used. 
Func_Search_Data<-function(a,b,c,d){
  a[ sample( which(b==c),d),]  
}


Func_seach_Data4<-function(a,b,c,d){
  subset<-a[which(b==c),]
  subset<-head(subset,n=d)
  sample_n(subset,1)
}

#Items touched during selection: 
#a = data frame, b=#touched c#"contamination" col name
Func_Index_DF<-function(a,b,c){
  as.numeric(a[b,colnames(a)==c])
}


# Log Reduction -----------------------------------------------------------

Func_Logred<-function(a,b){
  #a ,data frame column, b log reduction
    a*(10^b)
  }


# Adding Time -------------------------------------------------------------

Func_Adding_Time<-function(Column, Time){
  #column of data frame
  #Time, time parameter that is being added
  (Column + Time)
}

#Converting to CFU/g

Func_Convert_Log<-function(DF,Column){
  for (i in 1:nrow(DF)){
    N<-DF[i,colnames(DF)== Column]
    if(N>0){
    N<-log10(DF[i,colnames(DF)== Column])
    DF[i,colnames(DF)== Column]<-N
    }
  }
  return(DF)
}

Func_Convert_pergram<-function(DF){
  for(i in 1:nrow(DF)){
    N<-DF[i,colnames(DF)== "Contamination"]
    if(N>0){
      N<-(DF[i,colnames(DF)== "Contamination"])/Fr_Mean_weight
      DF[i,colnames(DF)== "Contamination"]<-N
    }
  }
  return(DF)
}



#Function for spoilage

Func_Spoilage_YN<-function(DF){
  for (i in 1:nrow(DF)){
    N<-as.numeric(DF[i,colnames(DF)== "SpoilageCon"])
    if (N>Spoilage_Treshold){
      DF[i,colnames(DF)== "SpoiledYN"]<-TRUE
    }
  }
  return(DF)
}
  

#Allergen Function: 

Func_Allergen_CC<-function(DF, PickedVar){
  if(DF[PickedVar,colnames(DF)== "ExposedAllergen"] == TRUE ){
    Cont_Student_Allergen_YN<-1
  } else if(DF[PickedVar,colnames(DF)== "ExposedAllergen"] == FALSE && Cont_Student_Allergen_YN == 1){
    DF[PickedVar,colnames(DF)== "ExposedAllergen"]<- TRUE
  }
  return(DF)
}


#Appending Data Frame Function

Func_Append_Column_Final<-function(DF = AFr_Summary_DF ){
  sapply(DF, as.numeric)
  Total<-DF[1:5]
  Total$Type<-"Total"
  Selection<-DF[c(1,6:9)]
  names(Selection)<-c("Iteration.N", "MeanCont", "MedianCont", "Cont5th", "Cont95th")
  Selection$Type<-"Service Line"
  ST<-DF[c(1,10:13)]
  names(ST)<-c("Iteration.N", "MeanCont", "MedianCont", "Cont5th", "Cont95th")
  ST$Type<-"Share Table"
  Total<-rbind(Total,Selection, ST)
  return(Total)
}




#Functions for the Transfer probability. 
inv.logit<-function(x){
  exp(x)/(1+exp(x))
}




#Dose Response. Adding the Dose Response to the Items
Func_Rep_DR<-function(DF_DR_Analysis){
  DF_DR_Analysis$Infection<-apply(DF_DR_Analysis,1,Func_DR_Infection)
  Number_Inf_Fr<-sum(DF_DR_Analysis$Infection==TRUE)
  #
  DF_DR_Analysis$Illness<-apply(DF_DR_Analysis,1,Func_DR_Illness)
  Number_Ill_Fr<-sum(DF_DR_Analysis$Illness==TRUE)
  l<-list("infection"=Number_Inf_Fr, "Illness"=Number_Ill_Fr )
  return(l)
}

#Function to extract list into a Data Frame
Func_List2DF<-function(list){
  df<-data.frame(matrix(unlist(list), nrow=length(list), byrow=T),stringsAsFactors=FALSE)
  return(df)
}


