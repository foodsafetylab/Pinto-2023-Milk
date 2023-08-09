
# Outputs creation on Individual service data frames -----------------------------------------------------------------

  #Fr_DF <- paste("Fr_Data.Frame_D", k,"_S",j, sep = "")
  #assign(Fr_DF, Fr_Data.Frame)
  
  #Pss_DF <- paste("Pss_Data.Frame_D", k,"_S",j, sep = "")
  #assign(Pss_DF, Pss_Data.Frame)
  
  #Pre_DF <- paste("Pre_Data.Frame_D", k,"_S",j, sep = "")
  #assign(Pre_DF, Pre_Data.Frame)


# VECTORS -----------------------------------------------------------------

if (Sim_Fruit==1){
  Fr_Vector_Sh <- paste("V_Shared_Fr_M", j, sep = "")
  assign(Fr_Vector_Sh, V_Shared_Fr)
}

if (Sim_PSS==1) {
    Pss_Vector_Sh <- paste("V_Shared_Pss_M", j, sep = "")
  assign(Pss_Vector_Sh, V_Shared_Pss)
}

if (Sim_PRE ==1){
  Pre_Vector_Sh <- paste("V_Shared_Pre_M", j, sep = "")
  assign(Pre_Vector_Sh, V_Shared_Pre)
}  




# =========================================================OUTPUTS FOR MEALS==================================================

# FRUIT -------------------------------------------------------------------  
  
if (Sim_Fruit ==1){
  
  #Fruit that stayed in share table.
  Left_ST_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Shared"),]
  Left_ST_Aside_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "SharedAside"),]
  if(ST_Aside==1){
    Left_ST_Fr<-bind_rows(Left_ST_Fr, Left_ST_Aside_Fr)
  }else if (ST_Aside==0){
    Left_ST_Fr<-Left_ST_Fr  
  }
  #Fruit that stayed in Selection table
  Left_Selection_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Selection Table"),]
  #Consumed Fr day 1, for exposure assesment
  Consumed_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Consumed"),]
  #Amounts left of fruit. 
  No_Left_ST_Fr<-nrow(Left_ST_Fr)
  No_Left_Selection_Fr<-nrow(Left_Selection_Fr)
  
  
  # Adding Time Between Services Fruit --------------------------------------------
  
  if(j>0 && j<=(Service_No-1)){
    #selection
    if(No_Left_Selection_Fr>0){
      Left_Selection_Fr$TotTime<-Func_Adding_Time(Left_Selection_Fr$TotTime, Time_Service) 
    }
    
    #Share Table
    if(No_Left_ST_Fr>0){
      Left_ST_Fr$TotTime<-Func_Adding_Time(Left_ST_Fr$TotTime, Time_Service) 
    }
  }
  
  
  # Adding Growth Between Every Service  Fruit-------------------------------------
  
  if(j>0 && j<=(Service_No-1)){
    if(E_coli==1 && Growth ==1){
      #Selection Table Items
      if(No_Left_Selection_Fr>0){
        Left_Selection_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Fr,TimeVar=Time_Service),Inputs_Growth_Sto_Ecoli)) #Using function on left over items 
      }
      #Share Table Items
      if(No_Left_ST_Fr>0){
        Left_ST_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Fr,TimeVar=Time_Service),Inputs_Growth_Sto_Ecoli)) #using function on left over St items 
      }
    }
    
    if(salmonella==1 && Growth ==1){
      #Selection Table Items
      if(No_Left_Selection_Fr>0){
        Left_Selection_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Fr,TimeVar=Time_Service),Inputs_Growth_Sto_Salmonella)) #Using function on left over items 
      }
      #Share Table Items
      if(No_Left_ST_Fr>0){
        Left_ST_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Fr,TimeVar=Time_Service),Inputs_Growth_Sto_Salmonella)) #using function on left over St items 
      }
    }
    
    if(norovirus==1 && Growth ==1){
      #selection table items
      if(No_Left_Selection_Fr>0){
        Left_Selection_Fr<- Func_Growth_Sto_Norovirus("room temp", Left_Selection_Fr, Time_Service) #using function on left over items 
      }
      #Share table items
      if(No_Left_ST_Fr>0){
        Left_ST_Fr<-Func_Growth_Sto_Norovirus("room temp", Left_ST_Fr, Time_Service) #Share table left over items
      }
    }
  }
  
  # Adding Turnaround Growth Fruit ------------------------------------------------
  
  if(j>0 && j<=(Service_No-1)){
    if(E_coli==1 && Growth ==1){
      #Selection Table Items
      if(No_Left_Selection_Fr>0){
        Left_Selection_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Fr,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Ecoli)) #Using function on left over items #Using function on left over items 
      }
      
      #Share Table Items
      if(No_Left_ST_Fr>0){
        Left_ST_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Fr,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Ecoli)) #using function on left over St items
      }
      
    }
    if(salmonella==1 && Growth ==1){
      #Selection Table Items
      if(No_Left_Selection_Fr>0){
        Left_Selection_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Fr,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Salmonella)) #Using function on left over items #Using function on left over items
      }
      
      #Share Table Items
      if(No_Left_ST_Fr>0){
        Left_ST_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Fr,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Salmonella)) #using function on left over St items
      }
      
    }
    if(norovirus==1 && Growth ==1){
      #selection table items
      if(No_Left_Selection_Fr>0){
        Left_Selection_Fr<- Func_Growth_Sto_Norovirus("room temp", Left_Selection_Fr, Time_Turnaround) #using function on left over items
      }
      #Share table items
      if(No_Left_ST_Fr>0){
        Left_ST_Fr<-Func_Growth_Sto_Norovirus("room temp", Left_ST_Fr, Time_Turnaround) #Share table left over items 
      }
    }
  }
  
  
  # Washing Log Reduction ---------------------------------------------------
  
  if(Wash_Between_Services ==1){
    
    #Washing selection Items
    if(Wash_Selection_YN_Fr==1){
      Reduction_wash<-Func_Randomize_Wash(Wash_Method = Wash_Method)
      Left_Selection_Fr$Contamination<-Func_Logred(Left_Selection_Fr$Contamination,Reduction_wash)
      Left_Selection_Fr$WashHistory<-(Left_Selection_Fr$WashHistory+1)
    }
    
    #washing share table items
    if(Wash_ST_YN_Fr==1){
      Reduction_wash<-Func_Randomize_Wash(Wash_Method = Wash_Method)
      Left_ST_Fr$Contamination<-Func_Logred(Left_ST_Fr$Contamination,Reduction_wash)
      Left_ST_Fr$WashHistory<-(Left_ST_Fr$WashHistory+1)
    }
  }
  
} #End of Fruit
  
  

  



# PSS ---------------------------------------------------------------------

if (Sim_PSS==1){
  
  #Pss that stayed in share table. 
  Left_ST_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Shared"),]
  Left_ST_Aside_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "SharedAside"),]
  if(ST_Aside==1){
    Left_ST_Pss<-bind_rows(Left_ST_Pss, Left_ST_Aside_Pss)
  }else if (ST_Aside==0){
    Left_ST_Pss<-Left_ST_Pss  
  }
  
  #Pss that stayed in Selection table
  Left_Selection_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Selection Table"),]
  #Consumed Pss day 1, for exposure assesment
  Consumed_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Consumed"),]
  #Amounts left of Pss. 
  No_Left_ST_Pss<-nrow(Left_ST_Pss)
  No_Left_Selection_Pss<-nrow(Left_Selection_Pss)
  
  
  # Adding time between services Pss ----------------------------------------
  
  if(j>0 && j<=(Service_No-1)){
    #selection
    Left_Selection_Pss$TotTime<-Func_Adding_Time(Left_Selection_Pss$TotTime, Time_Service)
    #Share Table
    Left_ST_Pss$TotTime<-Func_Adding_Time(Left_ST_Pss$TotTime, Time_Service)
  }
  
  
  # Adding Growth Between Every Service  Pss-------------------------------------
  
  if(j>0 && j<=(Service_No-1)){
    if(E_coli==1 && Growth_Pss ==1){
      #Selection Table Items
      if (No_Left_Selection_Pss>0){
        Left_Selection_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pss,TimeVar=Time_Service),Inputs_Growth_Sto_Ecoli)) #Using function on left over items
      }
      
      #Share Table Items
      if(No_Left_ST_Pss>0){
        Left_ST_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pss,TimeVar=Time_Service),Inputs_Growth_Sto_Ecoli)) #using function on left over St items
      }
      
    }
    if(salmonella==1 && Growth_Pss ==1){
      if (No_Left_Selection_Pss>0){
        Left_Selection_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pss,TimeVar=Time_Service),Inputs_Growth_Sto_Salmonella)) #Using function on left over items
      }
      #Share Table Items
      if(No_Left_ST_Pss>0){
        Left_ST_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pss,TimeVar=Time_Service),Inputs_Growth_Sto_Salmonella)) #using function on left over St items 
      }
      
    }
    if(norovirus==1 && Growth_Pss ==1){
      #selection table items
      if (No_Left_Selection_Pss>0){
        Left_Selection_Pss<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_Selection_Pss, Time_Service) #using function on left over items 
      }
      
      #Share table items
      if(No_Left_ST_Pss>0){
        Left_ST_Pss<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_ST_Pss, Time_Service) #Share table left over items 
      }
      
    }
  }
  
  # Adding Turnaround Growth Pss ------------------------------------------------
  
  if(j>0 && j<=(Service_No-1)){
    if(E_coli==1 && Growth_Pss ==1){
      #Selection Table Items
      if (No_Left_Selection_Pss>0){
        Left_Selection_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pss,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Ecoli)) #Using function on left over items
      }
      
      #Share Table Items
      if(No_Left_ST_Pss>0){
        Left_ST_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pss,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Ecoli)) #using function on left over St items 
      }
      
    }
    if(salmonella==1 && Growth_Pss ==1){
      #Selection Table Items
      if (No_Left_Selection_Pss>0){
        Left_Selection_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pss,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Salmonella)) #Using function on left over items 
      }
      
      #Share Table Items
      if(No_Left_ST_Pss>0){
        Left_ST_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pss,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Salmonella)) #using function on left over St items 
      }
      
    }
    if(norovirus==1 && Growth_Pss ==1){
      #selection table items
      if (No_Left_Selection_Pss>0){
        Left_Selection_Pss<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_Selection_Pss, Time_Turnaround) #using function on left over items 
      }
      
      #Share table items
      if(No_Left_ST_Pss>0){
        Left_ST_Pss<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_ST_Pss, Time_Turnaround) #Share table left over items 
      }
      
    }
  }
  
} #End of PSS
  


# PRE ---------------------------------------------------------------------
  
if(Sim_PRE==1){
  
  #Pre that stayed in share table. 
  Left_ST_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Shared"),]
  Left_ST_Aside_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "SharedAside"),]
  if(ST_Aside==1){
    Left_ST_Pre<-bind_rows(Left_ST_Pre, Left_ST_Aside_Pre)
  }else if (ST_Aside==0){
    Left_ST_Pre<-Left_ST_Pre  
  }
  #Pre that stayed in Selection table
  Left_Selection_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Selection Table"),]
  #Consumed Pre day 1, for exposure assessment
  Consumed_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Consumed"),]
  #Amounts left of Pre 
  No_Left_ST_Pre<-nrow(Left_ST_Pre)
  No_Left_Selection_Pre<-nrow(Left_Selection_Pre)
  
  # Adding time between services Pre ----------------------------------------
  if(j>0 && j<=(Service_No-1)){
    #selection
    #Left_Selection_Pre$TotTime<-Func_Adding_Time(Left_Selection_Pre$TotTime, Time_Service)
    #Share Table
    #Left_ST_Pre$TotTime<-Func_Adding_Time(Left_ST_Pre$TotTime, Time_Service)
  }
  
  
  # Adding Growth Between Every Service  Pre-------------------------------------
  
  if(j>0 && j<=(Service_No-1)){
    if(E_coli==1 && Growth_Pre ==1){
      #Selection Table Items
      if (No_Left_Selection_Pre>0){
        Left_Selection_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pre,TimeVar=Time_Service),Inputs_Growth_Sto_Ecoli)) #Using function on left over items
      }
      
      #Share Table Items
      if(No_Left_ST_Pre>0){
        Left_ST_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pre,TimeVar=Time_Service),Inputs_Growth_Sto_Ecoli)) #using function on left over St items
      }
      
    }
    if(salmonella==1 && Growth_Pre ==1){
      #Selection Table Items
      if (No_Left_Selection_Pre>0){
        Left_Selection_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pre,TimeVar=Time_Service),Inputs_Growth_Sto_Salmonella)) #Using function on left over items  
      }
      
      #Share Table Items
      if(No_Left_ST_Pre>0){
        Left_ST_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pre,TimeVar=Time_Service),Inputs_Growth_Sto_Salmonella)) #using function on left over St items 
      }
      
    }
    if(norovirus==1 && Growth_Pre ==1){
      #selection table items
      if (No_Left_Selection_Pre>0){
        Left_Selection_Pre<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_Selection_Pre, Time_Service) #using function on left over items 
      }
      
      #Share table items
      if(No_Left_ST_Pre>0){
        Left_ST_Pre<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_ST_Pre, Time_Service) #Share table left over items 
      }
      
    }
    
    # Adding Spoilage Growth for Milk ------------------------------------------
    if (Milk_Spoilage_YN==TRUE){
      #Adding time or Milk spoilage to items that are leftover in service line.
      if(No_Left_Selection_Pre>0){
        #Left_Selection_Pre<-Func_Growth_Milk_Spoilage(Temp_SL, Left_Selection_Pre, Time_Service,Growth_variability)
        #Left_Selection_Pre<-Func_Spoilage_YN(Left_Selection_Pre)
        Left_Selection_Pre= Func_Adding_Time_alldf(DF = Left_Selection_Pre, 
                                                   Time = Time_Service_Length)
      }
      
      #Here commented out the Share table items because it is added through the process. 
      if(No_Left_ST_Pre>0){
        if(Share_Table_YN==1){
          #Left_ST_Pre<-Func_Growth_Milk_Spoilage(Temp_RT, Left_ST_Pre, Time_Service) #comment out if accounted in service
          #Left_ST_Pre<-Func_Spoilage_YN(Left_ST_Pre)
          #Adding time to items that were left in ST
          Left_ST_Pre= Func_Adding_Time_alldf(DF = Left_ST_Pre, 
                                              Time = Time_Service_Length)
        }
      }
    }
  }
  
  # Adding Turnaround Growth Pre ------------------------------------------------
  
  if(j>0 && j<=(Service_No-1)){
    
    if(E_coli==1 && Growth_Pre ==1){
      #Selection Table Items
      if (No_Left_Selection_Pre>0){
        Left_Selection_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pre,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Ecoli)) #Using function on left over items
      }
      
      #Share Table Items
      if(No_Left_ST_Pre>0){
        Left_ST_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pre,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Ecoli)) #using function on left over St items
      }
      
    }
    if(salmonella==1 && Growth_Pre ==1){
      #Selection Table Items
      if (No_Left_Selection_Pre>0){
        Left_Selection_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pre,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Salmonella)) #Using function on left over items 
      }
      
      #Share Table Items
      if(No_Left_ST_Pre>0){
        Left_ST_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pre,TimeVar=Time_Turnaround),Inputs_Growth_Sto_Salmonella)) #using function on left over St items 
      }
      
    }
    if(norovirus==1 && Growth_Pre ==1){
      #selection table items
      if (No_Left_Selection_Pre>0){
        Left_Selection_Pre<-Func_Growth_Sto_Norovirus_Plastic("room temp", Left_Selection_Pre, Time_Turnaround) #using function on left over items 
      }
      
      #Share table items
      if(No_Left_ST_Pre>0){
        Left_ST_Pre<-Func_Growth_Sto_Norovirus_Plastic("room temp", Left_ST_Pre, Time_Turnaround) #Share table left over items
      }
      
    }
    
    if (Milk_Spoilage_YN==TRUE){ 
      #after Turnaround time
      if(No_Left_Selection_Pre>0){
        #Left_Selection_Pre<-Func_Growth_Milk_Spoilage(Temp_RT, Left_Selection_Pre, Time_Turnaround,Growth_variability)
        #Left_Selection_Pre<-Func_Spoilage_YN(Left_Selection_Pre)
        Left_Selection_Pre= Func_Adding_Time_alldf(DF = Left_Selection_Pre, 
                                                   Time = Time_Turnaround_Length)
      }
      
      if(No_Left_ST_Pre>0){
        if(Share_Table_YN==1){
          #Left_ST_Pre<-Func_Growth_Milk_Spoilage(Temp_RT, Left_ST_Pre, Time_Turnaround,Growth_variability)
          #Left_ST_Pre<-Func_Spoilage_YN(Left_ST_Pre)
          Left_ST_Pre= Func_Adding_Time_alldf(DF = Left_ST_Pre, 
                                              Time = Time_Turnaround_Length)
        }
      }
    }
  }
  

  
} #End of PRE


  






  
