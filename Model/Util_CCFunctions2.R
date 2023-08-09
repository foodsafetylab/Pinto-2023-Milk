
#Searching for the Touched Fruit Function

Func_Touched<-function(DF,Item,RowSizeVar,Item_Picked,SearchLocation){
  #DF= Data Frame
  #RowSizeVar= Variable Row Size from inputs
  #Item = "Fruit" , "PSS" or "PRE"
  #Item_Picked: Fr_Tocuhed or Pre_Toched, Pss_Touched
  
  Search.df.item_touched<-Func_seach_Data4(DF,DF$Location,SearchLocation ,RowSizeVar) #Searching for fruit to touch
  Item_Picked<-as.numeric(Search.df.item_touched$Item.No.) #ITem touched
  DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "Touched") #Adding History to History
  #Cross Contamination from Touching Fruit @Touch
  OutputsCCF<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked, Item=Item)
  Cont_Student<-OutputsCCF$Cont_Student
  DF<-OutputsCCF$Data.Frame
  #Cross Contamination from Allergens
  #DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
  OutputsFT<-list(DF=DF, Cont_Student=Cont_Student)
  return(OutputsFT)
}

#Function for Picked Items
Func_Picked<-function(DF, Item_Picked, Location){
  #DF = Data Frame
  #ITem Picked
  #Location = "Selection Table" or Shared
  Search.df.fr<-Func_seach_Data4(DF,DF$Location,Location,Row_size_Fr)
  #Fruit Selected #
  Item_Picked<-as.numeric(Search.df.fr$Item.No.)
  DF[Item_Picked,colnames(DF)== "Location"]<-"Tray"
  DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "Tray")
  DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "Touched")
  DF<-DF
  Item_Picked<-Item_Picked
  OutputFP<-list(DF=DF, Item_Picked=Item_Picked)
}


Func_Picked_ST<-function(DF,Item_Picked_ST){
  Search.df.fr_ST<-Func_seach_Data4(DF,DF$Location,"Shared",1)
  #Fruit from share table selected #
  Item_Picked_ST<-as.numeric(Search.df.fr_ST$Item.No.)
  DF[Item_Picked_ST,colnames(Search.df.fr_ST)== "Location"]<-"Tray"
  DF[Item_Picked_ST,colnames(DF)=="History"]<-paste(DF[Item_Picked_ST,colnames(DF)=="History"], "Tray")
  DF[Item_Picked_ST,colnames(DF)=="History"]<-paste(DF[Item_Picked_ST,colnames(DF)=="History"], "Touched")
  DF<-DF
  Item_Picked_ST<-Item_Picked_ST
  OutputFP_ST<-list(DF=DF, Item_Picked_ST=Item_Picked_ST)
}


Func_Eat_Fr<-function(Eat_YN_Item, DF, Item_Picked,Item, Location){
  #Eat_YN_ITem:Eat_YN_Fr
  #DF: Fr_Data.Frame
  #Item_Picked: Fr_Picked
  #Item= "Friut"
  if(Eat_YN_Item==1){
    DF[Item_Picked,colnames(DF)== "Location"]<-"Consumed"
    DF[Item_Picked,colnames(DF)== "ConsumedBy"]<-(paste(l,"",k,j,z))
    DF[Item_Picked,colnames(DF)== "ConsumedAt"]<-Location
    DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "Consumed")
    #Contamination
    if (Wrapping_Apples == 1){
      #Cross Contamination at consumption if apples wrapped
      OutputsCCFW<-Func_Cross_Contamination_Consumption_Wrapped(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked)
      Cont_Student<-OutputsCCFW$Cont_Student
      DF<-OutputsCCFW$Data.Frame
      #DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
    } else if (Wrapping_Apples == 0){
      #Cross Contamination @ Consumption apples not wrapped. 
      OutputsCCF<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked, Item=Item)
      Cont_Student<-OutputsCCF$Cont_Student
      DF<-OutputsCCF$Data.Frame
    } #end of if wrapp 
  }else if(Eat_YN_Item==0){
    #Updating Location and History for consumption
    DF[Item_Picked,colnames(DF)== "Location"]<-"Not Consumed"
    DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "NotConsumed")
    #Cross Crontamination from apples not being Consumed touch to ST/ Trash
    OutputsCCF<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked, Item=Item)
    Cont_Student<-OutputsCCF$Cont_Student
    DF<-OutputsCCF$Data.Frame
    #DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
  } #end of eat if statement
  OutputsFEFr<-list(DF=DF, Cont_Student=Cont_Student)
  return(OutputsFEFr)
}


Func_Eat_Pss<-function(Eat_YN_Item,DF,Item_Picked, Item,Location){
  #Eat_YN_Item = Eat_YN_Pss
  #DF=Pss.Data.Frame
  #Item_Picked= Pss_Picked
  #Item = "PSS
  if(Eat_YN_Item==1){
    DF[Item_Picked,colnames(DF)== "Location"]<-"Consumed" 
    DF[Item_Picked,colnames(DF)== "ConsumedBy"]<-(paste(l,"",k,j,z))
    DF[Item_Picked,colnames(DF)== "ConsumedAt"]<-Location
    DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "Consumed")
    #Contamination Insdide Pss @Consumption
    OutputsCCFW<-Func_Cross_Contamination_Consumption_Wrapped(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked)
    Cont_Student<-OutputsCCFW$Cont_Student
    DF<-OutputsCCFW$Data.Frame
    #DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
  }else if(Eat_YN_Item== 0){
    #Updating Data frame Location and History
    DF[Item_Picked,colnames(DF)== "Location"]<-"Not Consumed" 
    DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "NotConsumed")
    #Contamination from Touch @ Consumption
    OutputsCCF<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked, Item=Item)
    Cont_Student<-OutputsCCF$Cont_Student
    DF<-OutputsCCF$Data.Frame
    #DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
  } #end of Else statement for Eat
  OutputsFEPss<-list(DF=DF, Cont_Student=Cont_Student)
  return(OutputsFEPss)
}



Func_Eat_Pre<-function(Eat_YN_Item,DF, Item_Picked, Item,Location){
  if(Eat_YN_Item==1){
    DF[Item_Picked,colnames(DF)== "Location"]<-"Consumed" 
    DF[Item_Picked,colnames(DF)== "ConsumedBy"]<-(paste(l,"",k,j,z))
    DF[Item_Picked,colnames(DF)== "ConsumedAt"]<-Location
    DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "Consumed")
    #Contamination Container Pre to Mouth @ Consumption
    OutputFCCPre<-Func_Cross_Contamination_Pre_Consumption(Cont_Student=Cont_Student,DF = DF, Item_Picked = Item_Picked)
    Cont_Student<-OutputFCCPre$Cont_Student
    DF<-OutputFCCPre$DF
    #DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
  }else if(Eat_YN_Item==0){
    DF[Item_Picked,colnames(DF)== "Location"]<-"Not Consumed"  
    DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "NotConsumed")
    
    #Contamination at Pre Container
    OutputsCCF<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked, Item=Item)
    Cont_Student<-OutputsCCF$Cont_Student
    DF<-OutputsCCF$Data.Frame
    #DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
  } 
  OutputFEPre<-list(DF=DF,Cont_Student=Cont_Student)
  return(OutputFEPre)
}

Func_Shared<-function(DF, Item_Picked,Share_YN_Food, Item){
  #DF = Main Data Frame of Item
  #Item Picked = Item picked from Selection
  if(DF[Item_Picked,colnames(DF)=="Location"]== "Not Consumed"){
    if(Share_YN_Food==1){
      if(ST_Aside==1){
        DF[Item_Picked,colnames(DF)== "Location"]<-"SharedAside" 
      } else if(ST_Aside==0){
        DF[Item_Picked,colnames(DF)== "Location"]<-"Shared"
      }
      DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "Shared")
      DF[Item_Picked,colnames(DF)=="STtimes"]<-Func_Index_DF(DF,Item_Picked,"STtimes")+1
      #V_Shared_Fr<-(V_Shared_Fr+1)
      if(Wash_Bucket==1 && Item == "Fruit"){
        Reduction_wash<-Func_Randomize_Wash(Wash_Method = Wash_Method)
        Cont_Item<-as.numeric(DF[Item_Picked,colnames(DF)=="Contamination"])
        DF[Item_Picked,colnames(DF)=="Contamination"]<-round(Cont_Item*10^Reduction_wash,0)
        DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "WashedB")
      }
    }else if(Share_YN_Food==0){
      DF[Item_Picked,colnames(DF)== "Location"]<-"Not Shared" 
      DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "NotShared")
    }
  }
  return(DF)
}

#======================================================================================================================
#Cross_Contamination Fruit. 

Func_Cross_Contamination<-function(Cont_Student,Data.Frame, Item_Picked, Item){
  Conta<-Func_Index_DF(Data.Frame,Item_Picked,"Contamination")
  if(salmonella==1){
      #update the Fr Contamination in Data frame
      Tr_H_F<-Cont_Student*TE_H_F #Transfer from Hand to Fruit
      Tr_F_H<-(Conta* TE_F_H) #Tranfer from fruit to hand
  }else if (norovirus ==1 && Wrapping_Apples==0 && Item == "Fruit"){
    Conta<-round(Conta,digits = 0)
    Cont_Student<-round(Cont_Student,digits = 0)
    Tr_H_F<-rbinom(n=1,size = Cont_Student, prob = TrP_H_F)
    Tr_F_H<-rbinom(n=1,size = Conta,prob = TrP_F_H)
  }else if (norovirus ==1 && Wrapping_Apples==0 && (Item=="PSS" || Item=="PRE")){
    Conta<-round(Conta,digits = 0)
    Cont_Student<-round(Cont_Student,digits = 0)
    Tr_H_F<-rbinom(n=1,size = Cont_Student, prob = TrP_H_S)
    Tr_F_H<-rbinom(n=1,size = Conta,prob = TrP_S_H)
  }else if (norovirus ==1 && Wrapping_Apples==1){
    Conta<-round(Conta,digits = 0)
    Cont_Student<-round(Cont_Student,digits = 0)
    Tr_H_F<-rbinom(n=1,size = Cont_Student, prob = TrP_H_S)
    Tr_F_H<-rbinom(n=1,size = Conta,prob = TrP_S_H)
  }
      #Contamination Tranfered History. 
      Overall_Tr<-(Tr_H_F-Tr_F_H)
      Data.Frame[Item_Picked,colnames(Data.Frame)== "TouchesContHist"]<-paste(Data.Frame[Item_Picked,colnames(Data.Frame)=="TouchesContHist"], as.numeric(Overall_Tr),sep = ",") #Adding Contamination to
      #Continuing Contamination
      Cont_Updated<- Conta + Overall_Tr #New Contamination of Fruit
      Cont_Difference<-Conta-(Cont_Updated) #Difference in contamination to update student contamination
      Data.Frame[Item_Picked,colnames(Data.Frame)== "Contamination"]<-Cont_Updated #update the Fr Contamination in Data frame
      Cont_Student<-ifelse(Cont_Student +(Cont_Difference)<0,0,Cont_Student +(Cont_Difference)) #Updating Contamination in Student's hands
      #Adding Variables to Global Environment
      OutputsFCC<-list(Data.Frame=Data.Frame, Cont_Student=Cont_Student)
      return(OutputsFCC)

  } 

Func_Cross_Contamination_Consumption_Wrapped<-function(Cont_Student, Data.Frame, Item_Picked){
  #Contamination of Fruit
  Conta<-Func_Index_DF(Data.Frame,Item_Picked,"Contamination")
  
  if(salmonella==1){
    #update the Fr Contamination in Data frame
    Tr_H_F<-Cont_Student*TE_H_F #Transfer from Hand to Fruit
    Tr_F_H<-(Conta* TE_F_H) #Tranfer from fruit to hand
  }else if(norovirus ==1 ){
    Conta<-round(Conta,digits = 0)
    Cont_Student<-round(Cont_Student,digits = 0)
    Tr_H_F<-rbinom(n=1,size = Cont_Student, prob = TrP_H_S)
    Tr_F_H<-rbinom(n=1,size = Conta,prob = TrP_S_H)
  }
  
  Overall_Tr<-(Tr_H_F-Tr_F_H)
  Data.Frame[Item_Picked,colnames(Data.Frame)== "TouchesContHist"]<-paste(Data.Frame[Item_Picked,colnames(Data.Frame)=="TouchesContHist"], as.numeric(Overall_Tr),sep = ",") #Adding Contamination to
  #Continuing Cross Contamination
  Cont_Updated<- Conta + Overall_Tr #New Contamination of Fruit
  Cont_Difference<-Conta-(Cont_Updated) #Difference in contamination to update student contamination
  Data.Frame[Item_Picked,colnames(Data.Frame)== "Contamination"]<-Cont_Updated #update the Fr Contamination in Data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Difference)<0,0,Cont_Student +(Cont_Difference)) #Updating Contamination in Student's hands
  #Adding Contamination to Inside of Fruit
  if(salmonella==1){
    #update the Fr Contamination in Data frame
    Tr_H_F_Inside<-Cont_Student*TE_H_F
  }else if (norovirus ==1){
    Cont_Student<-round(Cont_Student,digits = 0)
    Tr_H_F_Inside<-rbinom(n=1,size = Cont_Student, prob = TrP_H_F)
  }
  Overall_Tr<-Tr_H_F_Inside
  Data.Frame[Item_Picked,colnames(Data.Frame)== "TouchesContHist"]<-paste(Data.Frame[Item_Picked,colnames(Data.Frame)=="TouchesContHist"], as.numeric(Overall_Tr),sep = ",") #Adding Contamination to
  
  Cont_Consumed<-Tr_H_F_Inside
  Data.Frame[Item_Picked,colnames(Data.Frame)== "Contamination"]<- Cont_Updated
  Data.Frame[Item_Picked,colnames(Data.Frame)== "ContConsumed"]<- Cont_Consumed
#Outputs
  OutputsFCCW<-list(Data.Frame=Data.Frame, Cont_Student=Cont_Student)
  return(OutputsFCCW)
}



Func_Cross_Contamination_Pre_Consumption<-function(Cont_Student, DF, Item_Picked){
  Tr_H_Pre<-Cont_Student*TE_H_S #Transfer from Hand to Pre
  Tr_Pre_H<-(Func_Index_DF(DF,Item_Picked,"Contamination")* TE_S_H) #Tranfer from Pre to hand
  Cont_Pre_Updated<- Func_Index_DF(DF,Item_Picked,"Contamination") + Tr_H_Pre - (Tr_Pre_H) #New Contamination of Pre
  Cont_Pre_Difference<-Func_Index_DF(DF,Item_Picked,"Contamination")-(Cont_Pre_Updated) #Difference in contamination to update student contamination
  DF[Item_Picked,colnames(DF)== "Contamination"]<-Cont_Pre_Updated #update the Pre Contamination in Data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Pre_Difference)<0,0,Cont_Student +(Cont_Pre_Difference)) #Updating Contamination in Student's hands
  Tr_Pre_Mouth<-(Func_Index_DF(DF,Item_Picked,"Contamination")* TE_Pre_Mouth)
  Cont_Pre_Consumed<-Tr_Pre_Mouth
  DF[Item_Picked,colnames(DF)== "Contamination"]<-Cont_Pre_Consumed
  OutputFCCPre<-list(DF=DF, Cont_Student=Cont_Student)
  return(OutputFCCPre)
  
}

