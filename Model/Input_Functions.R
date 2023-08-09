
#INPUT FUNCTIONS ============================================================================= 

# Function Contamination in Student Hands ------------------------------------------
#Inputs for this function #Inputs_ICont_Student


Func_ICont_Student<-function(IC_salmonella,mass_feces_hands,HU_NV_in_Feces,Pr_WashingHand,LogRed,... ){
  #Salmonella
  if(salmonella ==1){
    IC_Student<-IC_salmonella  #CFU/Hand
    return(IC_Student)
  } 
  if(norovirus ==1){
    Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces)) #GEC/g
    IC_Student<- Personal_Contamination #GEC/Hand
    HawashingYN<-ifelse(runif(1)<Pr_WashingHand,1,0)
      if(HawashingYN==1){
        IC_Student<-IC_Student*(10^-LogRed)
      }
    IC_Student<-round(IC_Student,digits = 0)
    return(IC_Student)
  }
}


# Initial Contamination Functions --------------------------------------------


#Function that adds contminations to Data frames and converts from CFU/cm^2 to CFU/Apple or item


func_Cont_cm2<-function(DF, Prevalence, logContamination, Fr_Mean_area ){
  #Df= Data frame
  #Prevalence = parameter Prevalence of pathogen
  # Contamination = Initial Contamination of the pathogen. 
  # parameter for log contamination
  for(i in 1:nrow(DF)){
    Fr_Cont_YN<- ifelse(runif(1)<Prevalence,1,0) 
    Contamination<-10^(logContamination)* Fr_Mean_area
    if(Fr_Cont_YN==1){
      DF[i,colnames(DF)== "Contamination"]<-Contamination
      DF[i,colnames(DF)== "InContamination"]<-Contamination
    } else if (Fr_Cont_YN==0){
      DF[i,colnames(DF)== "Contamination"]<-as.numeric(0)
      DF[i,colnames(DF)== "InContamination"]<-as.numeric(0)
    }
  }
  return(DF)
}



#Special Function that adds norovirus to fruit items
#Inputs For Function: Inputs_Cont_HuNov_Fr

func_Cont_HuNoV_Fr<-function(DF){
  for (i in 1:nrow(DF)){
    Prevalence <-0#rbetagen(1,0.79,1.03,0.0,0.2) #0.0865215
    HuNoV_ContFruit<-rlnormTrunc(1,2.38,3.52, 0,6.97) #add 1.578
    Fr_Cont_YN<- ifelse(runif(1)<Prevalence,1,0)
    Contamination<-(10^HuNoV_ContFruit)*Fr_Mean_weight #GEC/Apple
    Contamination<-round(Contamination,digits = 0)
    if(Fr_Cont_YN==1){
      DF[i,colnames(DF)== "Contamination"]<-Contamination
      DF[i,colnames(DF)== "InContamination"]<-Contamination
    } else if (Fr_Cont_YN==0){
      DF[i,colnames(DF)== "Contamination"]<-as.numeric(0)
      DF[i,colnames(DF)== "InContamination"]<-as.numeric(0)
    }
  }
  return(DF)
}
#Functions for Growth Models

# Growth Model for Enteric -------------------------------------------------------------


#Inputs for function: Inputs_Growth_Sto_Ecoli
#Inputs for function: Inputs_Growth_Sto_Salmonella

Func_Growth_Enteric<-function(Condition,DF,TimeVar,b,k,Tmin){
  if(Condition== "refrigerated"){
    if(Temp_Ref<5){
      for (i in 1:nrow(DF)){
        Die_off<-((-k)*TimeVar)
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Die_off)
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    } else if (Temp_Ref>=5){
      rate<-(b*(Temp_Ref-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
        Con_Change<-rate*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Con_Change )
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    }
  } else if (Condition=="room temp"){
    if(Temp_RT<5){
      for (i in 1:nrow(DF)){
        Die_off<-(-k)*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Die_off)
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    } else if (Temp_RT>=5){
      rate<-(b*(Temp_RT-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
        Con_Change<-rate*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Con_Change )
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    }  
  }
}





# Growth Model For Norovirus ----------------------------------------------

#growht norovirus in plastic
#no input necesary, no variability in inputs
Func_Growth_Sto_Norovirus_Plastic<-function(Condition,DF,TimeVar){
  b<-137.74
  n<-.50
  N0<-0
  if(Condition== "room temp"){
    for (i in 1:nrow(DF)){
      Growth<- -(TimeVar/b)^n 
      N<-(DF[i,colnames(DF)== "Contamination"])
      Con_Final<-ifelse(N==0,N,N*10^Growth)
      Con_Final<-round(Con_Final,digits = 0) #rounding for the binomial
      DF[i,colnames(DF)== "Contamination"]<-Con_Final
    }
  } 
  return(DF)
}  

#Die Off norovirus in fruit
#no need for inputs, no variability
Func_Growth_Sto_Norovirus<-function(Condition,DF,TimeVar){
  if(Condition== "room temp"){
    f<-0
    b1<-(-416)
    b2<-47.33
    TimeVar<-TimeVar/24
    for (i in 1:nrow(DF)){
      Growth<-log10((2*f/(1+exp(b1*TimeVar)))+(2*(1-f)/(1+exp(b2*TimeVar))))
      N<-(DF[i,colnames(DF)== "Contamination"])
      Con_Final<-ifelse(N==0,N,N*10^Growth)
      Con_Final<-round(Con_Final,digits = 0) #rounding for the binomial
      DF[i,colnames(DF)== "Contamination"]<-Con_Final
    }
    return(DF)
  } else if (Condition == "refrigerated"){
    f<-0
    b1<-(.08)
    b2<-4.63
    TimeVar<-TimeVar/24
    for (i in 1:nrow(DF)){
      Growth<-log10((2*f/(1+exp(b1*TimeVar)))+(2*(1-f)/(1+exp(b2*TimeVar))))
      N<-(DF[i,colnames(DF)== "Contamination"])
      Con_Final<-ifelse(N==0,N,N*10^Growth)
      Con_Final<-round(Con_Final,digits = 0) #rounding for the binomial
      DF[i,colnames(DF)== "Contamination"]<-Con_Final
    }
    return(DF)
  }
} 


# Spoilage of Organisms. ----------------------------------------------
#No variability

#this is for applying it to the whole dataframe
Func_Growth_Milk_Spoilage<-function(Temp,DF,TimeVar, GrowthVar){
  b<-0.03578
  Tmin<-(-1.19)
  Tmax<-(41.2)
  c<-.1719
  k<-(((Temp-Tmin)*b)^2)+GrowthVar
  k_ad = k*log10(2)
  for (i in 1:nrow(DF)){
    Growth<-TimeVar/24*k_ad
    N<-DF[i,colnames(DF)== "SpoilageCon"]
    Con_Final<-N + Growth
    DF[i,colnames(DF)== "SpoilageCon"]<-as.numeric(Con_Final)
  }
  return(DF)
} 



#Function for adding time and Adding Growth

Func_Time_Temp<-function(DF, Item_Picked, Temp, Time){
  #Parameters for milk spoilage
  b<-.03772
  Tmin<-(-6.1)
  Tmax<-(41.2)
  c<-.1719
  k<-(b*(Temp-Tmin)*(1-exp(c*(Temp-Tmax))))^2
  k_ad = k*log10(2)
  #How much growth
  Growth<-Time*k_ad
  #N current contamination
  N<-DF[Item_Picked,colnames(DF)== "SpoilageCon"]
  Con_Final<-N + Growth #Growth in log. 
  #Refreshing the contamination in the RoW
  DF[Item_Picked,colnames(DF)== "SpoilageCon"]<-as.numeric(Con_Final)
  #Adding time to the Data frame
  Current_Time = DF[Item_Picked,colnames(DF)== "TotTime"]
  DF[Item_Picked,colnames(DF)== "TotTime"]<-Current_Time+Time
  DF[Item_Picked,colnames(DF)== "PickTS"]<-TRUE
  return(DF)
}

#Adding time to consumed milk milks. 
Func_Adding_Time_ConItem<-function(DF, Item_Picked, Time){
  #Adding time to the Data frame
  Current_Time = DF[Item_Picked,colnames(DF)== "TotTime"]
  DF[Item_Picked,colnames(DF)== "TotTime"]<-Current_Time+Time
  #DF[Item_Picked,colnames(DF)== "PickTS"]<-TRUE
  return(DF)
}

Func_Adding_Time_alldf<-function(DF, Time){
  #Adding time to the Data frame
  Current_Time = DF[,colnames(DF)== "TotTime"]
  DF[,colnames(DF)== "TotTime"]<-Current_Time+Time
  #DF[,colnames(DF)== "PickTS"]<-TRUE
  return(DF)
}


#Dose Response Function----------------------------------------

Func_DR_Infection<-function(x){
  alpha<-0.04 
  betar<-0.055
  hunov<-as.numeric(x[["Contamination"]])
  Probinf<-(1-hyperg_1F1(a = alpha,b = alpha+betar,x = -hunov))
  Infected_YN<-ifelse(runif(1)<Probinf,1,0)
  if(Infected_YN==1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


Func_DR_Illness<-function(x){
  nw<-2.55E-3
  r<-0.086
  hunov<-as.numeric(x[["Contamination"]])
  if(x[["Infection"]] == TRUE){
    Probill<-(1-(1+nw*hunov)^(-r))
    Ill_YN<-ifelse(runif(1)<Probill,1,0)
    if(Ill_YN==1){
      return(TRUE)
    }else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
} 



#Function that randomizes the selection of Fruit wahing reduction
if(NSA_Analysis_Other_Wash==1){
  Func_Randomize_Wash<-function(Wash_Method){
    if (Wash_Method==1){
      Logred<-5#1.919069
    } else if (Wash_Method==2){
      Logred<-0.87
    } else if (Wash_Method==3){
      Logred<- 0#0.1991058
    }
    return(-Logred)
  }
} else {
  Func_Randomize_Wash<-function(Wash_Method){
    if (Wash_Method==1){
      Logred<-runif(1,0.26,1.06)
    } else if (Wash_Method==2){
      Logred<-rnorm(1,0.667,0.33)
    } else if (Wash_Method==3){
      Logred<-runif(1,1,2)
    }
    return(-Logred)
  }
}



#Function that randomizes hand sanitizer reduction

if(NSA_Analysis_Other_Sanitize==1){
  Func_Randomize_Sanitizer<-function(Wash_Method){
    if (Wash_Method==10){
      Logred<-3.375144
    } else if (Wash_Method==11){
      Logred<-1.386432
    } else if (Wash_Method==12){
      Logred<-0.1622854 
    } 
    return(Logred)
  }
}else{
  Func_Randomize_Sanitizer<-function(Wash_Method){
    if (Wash_Method==1){
      #Wilson, non residual hand sanitizer 30s. 
      Logred<-rnormTrunc(n = 1,mean = 1.06,sd = 0.54,min = 0.15,max = 1.89)
      #Ecudero Abarca 30s high soil load, conservative. 
    } else if (Wash_Method==2){
      Logred<-rnorm(1,2.2,0.07)
      #VF447 70% ethanol Manciaga in vivo 30s
    } else if (Wash_Method==3){
      Logred<-rnorm(1,2.48,0.45)
      #Kampf sterillum virguard 95% ethanol
    } else if (Wash_Method==4){
      Logred<-rnorm(1,2.17,1.065)
      #Liu hand sanitizers in general 62% alcoh
    }else if (Wash_Method==5){
      Logred<-runif(1,0.14,0.34)
      #LAgues 62% ethanol purell
    }else if (Wash_Method==6){
      Logred<-0.5
    }
    return(Logred)
  }
}






#Extras Not Used:----------------------------------------------------------------------------------------------------------------------

#Special Function that adds norovirus to fruit items
#Inputs For Function: Inputs_Cont_HuNov_Fr
#Note Note bein Used as Backup.
#Change name remove_PFU

func_Cont_HuNoV_Fr_PFU<-function(DF, Prevalence,Genomic_copies_per_PFU,HuNoV_ContFruit){
  for (i in 1:nrow(DF)){
    Fr_Cont_YN<- ifelse(runif(1)<Prevalence,1,0)
    Contamination<-(10^HuNoV_ContFruit)/(10^Genomic_copies_per_PFU) *Fr_Mean_weight #PFU/Apple
    if(Fr_Cont_YN==1){
      DF[i,colnames(DF)== "Contamination"]<-Contamination
      DF[i,colnames(DF)== "InContamination"]<-Contamination
    } else if (Fr_Cont_YN==0){
      DF[i,colnames(DF)== "Contamination"]<-as.numeric(0)
      DF[i,colnames(DF)== "InContamination"]<-as.numeric(0)
    }
  }
  return(DF)
}







#this function changes contamination from CFU CM^2 to CFU/g
#NOTE: Not being USED Right now.Here just in case 
func_Cont_Fr<-function(DF, Prevalence, area_av , area_sd, logContamination, weight_av, weight_sd ){
  #Df= Data frame
  #Prevalence = parameter Prevalence of pathogen
  # Contamination = Initial Contamination of the pathogen. 
  # parameter for volume_av
  # parameter for volume_sd
  # parameter for log contamination
  for(i in 1:nrow(DF)){
    Fr_Cont_YN<- ifelse(runif(1)<Prevalence,1,0) 
    Fr_Area<-rnorm(1,area_av,area_sd)
    Fr_Weight<-rnorm(1,weight_av,weight_sd)
    Contamination<-(10^(logContamination)* Fr_Area)/(Fr_Weight)
    if(Fr_Cont_YN==1){
      DF[i,colnames(DF)== "Contamination"]<-Contamination
    } else if (Fr_Cont_YN==0){
      DF[i,colnames(DF)== "Contamination"]<-0
    }
  }
  return(DF)
}


#Note Note bein Used as Backup.
#Change name remove_PFU
#Initial Contamination of Student
Func_ICont_Student_PFU<-function(IC_salmonella,mass_feces_hands,HU_NV_in_Feces,Genomic_copies_per_PFU,... ){
  #Salmonella
  if(salmonella ==1){
    IC_Student<-IC_salmonella  #CFU/Hand
    return(IC_Student)
  } 
  if(norovirus ==1){
    Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU) #PFU/Hand
    IC_Student<- Personal_Contamination #PFU/Hand
    return(IC_Student)
  }
}
