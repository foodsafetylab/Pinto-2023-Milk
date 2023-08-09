#Functions for outputs


#Updating not consumed items. 
func_update_notcons<-function(df){
  if (Donation_End_Week == FALSE){
  df$Location[df$Location=="Not Shared"]<-"Discarded"
  df$Location[df$Location=="Not Consumed"]<-"Discarded"
  if(k==Food_Days && j==Service_No){
    df$Location[df$Location=="Selection Table"]<-"Discarded"
    df$Location[df$Location=="Shared"]<-"Discarded"
    df$Location[df$Location=="SharedAside"]<-"Discarded"
    }
  }else if (Donation_End_Week == TRUE){
    df$Location[df$Location=="Not Shared"]<-"Discarded"
    df$Location[df$Location=="Not Consumed"]<-"Discarded"
    if(k==Food_Days && j==Service_No){
      df$Location[df$Location=="Selection Table"]<-"Donated"
      df$Location[df$Location=="Shared"]<-"Donated"
      df$Location[df$Location=="SharedAside"]<-"Donated"
    }
  }
  return(df)
}


# Code To add Services to Total 

func_Add_Services<-function(DF){
  for(i in 1:nrow(DF)){
    DF[i,colnames(DF)=="TotServices"]<-Func_Index_DF(DF,i,"TotServices")+1
  }
  return(DF)
}


###Function for Output Days: 

#Function to get leftover ST food. 
Func_Left_ST<-function(df, ST_Aside){
  #Fruit that stayed in share table. 
  Left_ST<-df[which(df$Location == "Shared"),]
  if(ST_Aside==1){
    Left_ST_Aside<-df[which(df$Location == "SharedAside"),]
    Left_ST<-bind_rows(Left_ST, Left_ST_Aside)
  }
  return (Left_ST)
}

Func_Left_Sel<-function(df){
  #Fruit that stayed in Selection table 
  Left_Selection<-df[which(df$Location == "Selection Table"),]
  No_Left_ST_Fr<-nrow(Left_ST_Fr)
  No_Left_Selection_Fr<-nrow(Left_Selection_Fr)
}






































##Removed: 


# #Updated items from not consumed, not shared, etc to wasted. 
# Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Not Shared"]<-"Discarded"
# Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Not Consumed"]<-"Discarded"
# #####  Discard Items from the final service. 
# if(k==Food_Days && j==Service_No){
#   Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Selection Table"]<-"Discarded"
#   Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Shared"]<-"Discarded"
#   Fr_Data.Frame$Location[Fr_Data.Frame$Location=="SharedAside"]<-"Discarded"
# }
# 
# #Updated items from not consumed, not shared, etc to wasted.
# Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Not Shared"]<-"Discarded"
# Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Not Consumed"]<-"Discarded"
# ##### Discard Items from the final service.
# if(k==Food_Days && j==Service_No ){
#   Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Selection Table"]<-"Discarded"
#   Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Shared"]<-"Discarded"
#   Pss_Data.Frame$Location[Pss_Data.Frame$Location=="SharedAside"]<-"Discarded"
# }
# 
# #Updated items from not consumed, not shared, etc to wasted.
# Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Not Shared"]<-"Discarded"
# Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Not Consumed"]<-"Discarded"
# ##### Discard Items from the final service.
# if(k==Food_Days && j==Service_No ){
#   Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Selection Table"]<-"Discarded"
#   Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Shared"]<-"Discarded"
#   Pre_Data.Frame$Location[Pre_Data.Frame$Location=="SharedAside"]<-"Discarded"
#   
# }

