# FOOD POOL DATAFRAMES ----------------------------------------------------


#SERVICE 1 DAY 1 CREATION OF DATAFRAMES ================================================================

if(j==1 && k== 1){
  #Fruit-------------------------------------------------------------
  if (Sim_Fruit==1){
    Fr_Data.Frame<-Fuct_DF_Initial(FoodType = "Fruit")
    #Adding Initial Contaminations of fruit Data frame based on initial cont and prevalence CFU/Fruit
    
    if(salmonella==1 && Calculated_Cont_Fr==1){
      #Adding Contamination to the Dataframe
      Fr_Data.Frame<-func_Cont_cm2(Fr_Data.Frame,Prevalence_Salmonella_Fr,Fr_Contamination_salmonella,Fr_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      #Adding Contamination to Data Fra
      Fr_Data.Frame<-func_Cont_HuNoV_Fr(Fr_Data.Frame)
    }
  }
  
  # Pss ---------------------------------------------------------------------
  if(Sim_PSS==1){
    Pss_Data.Frame<-Fuct_DF_Initial(FoodType = "Pss")
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pss==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Salmonella_Pss,Pss_Contamination_salmonella,Pss_Mean_area)
      #Adding items to know Contaminated Items
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Norovirus_Pss,Pss_Contamination_norovirus,Pss_Mean_area)
      #Adding items to know Contaminated Items
    }
  }
  
  # Pre ---------------------------------------------------------------------
  if(Sim_PRE==1){
    Pre_Data.Frame<-Fuct_DF_Initial(FoodType = "Pre")
    
    #Adding initial spoilge contamination to the Dataframe
    Pre_Data.Frame<-Func_Adding_Initial_MilkCont(df = Pre_Data.Frame, mean = Milk_Con_Mean, sd = Milk_Con_SD)
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pss==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Salmonella_Pre,Pre_Contamination_salmonella,Pre_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pss ==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Norovirus_Pre,Pre_Contamination_norovirus,Pre_Mean_area)
    }
  }
  
} #end of J1



#SERVICE >1 DAY 1 ===============================================================


if(j>1 && k ==1 ){
  
  # Fruit -------------------------------------------------------------------
  if (Sim_Fruit==1){
    Fr_Data.Frame<-Fuct_DF_Reservice(FoodType = "Fruit")
    
    #Adding Initial Contaminations of fruit
    if(salmonella==1 && Calculated_Cont_Fr==1){
      #Adding Contaminations to the Data Frames
      Fr_Data.Frame<-func_Cont_cm2(Fr_Data.Frame,Prevalence_Salmonella_Fr,Fr_Contamination_salmonella,Fr_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      #Adding Contamination to the Data Frames
      Fr_Data.Frame<-func_Cont_HuNoV_Fr(Fr_Data.Frame)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Fr[,colnames(Left_ST_Fr)=="STtimes"]<-(Left_ST_Fr[,colnames(Left_ST_Fr)=="STtimes"])+1
    #Binding all the stuff. for next service
    Fr_Data.Frame<-rbind(Left_Selection_Fr,Fr_Data.Frame,Left_ST_Fr)
    #Reseting Numbers in Data Frames
    Fr_Data.Frame$Item.No.<- 1:nrow(Fr_Data.Frame)
    row.names(Fr_Data.Frame)<-1:nrow(Fr_Data.Frame)
    Fr_Data.Frame$Service<-j
    Fr_Data.Frame$Day<-k
  }
  
  # Pss ---------------------------------------------------------------------
  if(Sim_PSS==1){
    Pss_Data.Frame<-Fuct_DF_Reservice(FoodType = "Pss")
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pss==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Salmonella_Pss,Pss_Contamination_salmonella,Pss_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pss ==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Norovirus_Pss,Pss_Contamination_norovirus,Pss_Mean_area)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"]<-(Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"])+1
    #Binding all the stuff. for next service
    Pss_Data.Frame<-rbind(Left_Selection_Pss,Pss_Data.Frame,Left_ST_Pss)
    #Reseting Numbers in Data Frames
    Pss_Data.Frame$Item.No.<- 1:nrow(Pss_Data.Frame)
    row.names(Pss_Data.Frame)<-1:nrow(Pss_Data.Frame)
    Pss_Data.Frame$Service<-j
    Pss_Data.Frame$Day<-k
  }
  
  # Pre ---------------------------------------------------------------------
  if(Sim_PRE==1){
    Pre_Data.Frame<-Fuct_DF_Reservice(FoodType = "Pre")
    
    #Adding initial spoilge contamination to the Datafra,e
    Pre_Data.Frame<-Func_Adding_Initial_MilkCont(df = Pre_Data.Frame, mean = Milk_Con_Mean, sd = Milk_Con_SD)
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pre==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Salmonella_Pre,Pre_Contamination_salmonella,Pre_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pre ==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Norovirus_Pre,Pre_Contamination_norovirus,Pre_Mean_area)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"]<-(Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"])+1
    #Binding all the stuff. for next service
    Pre_Data.Frame<-rbind(Left_Selection_Pre,Pre_Data.Frame,Left_ST_Pre)
    #Reseting Numbers in Data Frames
    Pre_Data.Frame$Item.No.<- 1:nrow(Pre_Data.Frame)
    row.names(Pre_Data.Frame)<-1:nrow(Pre_Data.Frame)
    Pre_Data.Frame$Service<-j
    Pre_Data.Frame$Day<-k
  }
  
}#end of j loop




# SERVICES MULTIPLE AND MULTIPLE DAYS ===============================================================

if(j>0 && k>1 ){
  
  # Fruit ----------------------------------------------------------------------
  if (Sim_Fruit==1){
    Fr_Data.Frame<-Fuct_DF_Reservice(FoodType = "Fruit")
    
    #Adding Initial Contaminations of fruit 
    if(salmonella==1 && Calculated_Cont_Fr==1){
      #Adding Contamination to the Data Frames
      Fr_Data.Frame<-func_Cont_cm2(Fr_Data.Frame,Prevalence_Salmonella_Fr,Fr_Contamination_salmonella,Fr_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      #Adding Contamination to the Data Frames
      Fr_Data.Frame<-func_Cont_HuNoV_Fr(Fr_Data.Frame)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Fr[,colnames(Left_ST_Fr)=="STtimes"]<-(Left_ST_Fr[,colnames(Left_ST_Fr)=="STtimes"])+1
    #Binding all the stuff. for next service
    Fr_Data.Frame<-rbind(Left_Selection_Fr,Fr_Data.Frame,Left_ST_Fr)
    #Reseting Numbers in Data Frames
    Fr_Data.Frame$Item.No.<- 1:nrow(Fr_Data.Frame)
    row.names(Fr_Data.Frame)<-1:nrow(Fr_Data.Frame)
    Fr_Data.Frame$Service<-j
    Fr_Data.Frame$Day<-k
  }
  
  # Pss ---------------------------------------------------------------------
  if(Sim_PSS==1){
    Pss_Data.Frame<-Fuct_DF_Reservice(FoodType = "Pss")
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pss==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Salmonella_Pss,Pss_Contamination_salmonella,Pss_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pss ==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Norovirus_Pss,Pss_Contamination_norovirus,Pss_Mean_area)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"]<-(Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"])+1
    #Binding all the stuff. for next service
    Pss_Data.Frame<-rbind(Left_Selection_Pss,Pss_Data.Frame,Left_ST_Pss)
    #Reseting Numbers in Data Frames
    Pss_Data.Frame$Item.No.<- 1:nrow(Pss_Data.Frame)
    row.names(Pss_Data.Frame)<-1:nrow(Pss_Data.Frame)
    Pss_Data.Frame$Service<-j
    Pss_Data.Frame$Day<-k
  }
  
  # Pre ---------------------------------------------------------------------
  if(Sim_PRE==1){
    Pre_Data.Frame<-Fuct_DF_Reservice(FoodType = "Pre")
    #Adding initial spoilge contamination to the Datafra,e
    Pre_Data.Frame<-Func_Adding_Initial_MilkCont(df = Pre_Data.Frame, mean = Milk_Con_Mean, sd = Milk_Con_SD)
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pre==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Salmonella_Pre,Pre_Contamination_salmonella,Pre_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pre ==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Norovirus_Pre,Pre_Contamination_norovirus,Pre_Mean_area)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"]<-(Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"])+1
    #Binding all the stuff. for next service
    Pre_Data.Frame<-rbind(Left_Selection_Pre,Pre_Data.Frame,Left_ST_Pre)
    #Reseting Numbers in Data Frames
    Pre_Data.Frame$Item.No.<- 1:nrow(Pre_Data.Frame)
    row.names(Pre_Data.Frame)<-1:nrow(Pre_Data.Frame)
    Pre_Data.Frame$Service<-j
    Pre_Data.Frame$Day<-k
  }
  
}#end of j loop


# Vectors -----------------------------------------------------------------


if (Sim_Fruit==1){
 V_Shared_Fr<-c(0) 
}
if(Sim_PSS==1){
 V_Shared_Pss<-c(0) 
}
if(Sim_PRE==1){
  V_Shared_Pre<-c(0)
}


