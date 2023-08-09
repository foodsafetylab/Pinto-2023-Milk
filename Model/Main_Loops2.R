
#Week Iteration =============================================================
for (l in 1:Sens_Iterations){
  source("Input_RandomWeeks.R") #To change Methods every Week. eh Hand washing, Hand sanitizer.
  #Day Iteration=============================================================
  for (k in 1:Food_Days){
    #Service Iteration ======================================================
    for (j in 1:Service_No){
      #Creation of Data Frame for new Service. 
      source("Util_DataFrames.R")  
      #Beginning of Student Iteration========================================
      for (z in 1:N_Iterations){
        Outputs_Student_Loop<-Main_Loop() #Main Model Loop. Outputs come out of here
        
        #Outputs based on the outputs selected from the simulation
        if (Sim_Fruit == 1){ 
          #Only saving dataframe as output if fruit is one/on
          Fr_Data.Frame<-Outputs_Student_Loop$Fr_Data.Frame
        }
        if (Sim_PSS==1)
        {
          #Only saving PSS Dataframe is PSS is one/on
          Pss_Data.Frame<-Outputs_Student_Loop$Pss_Data.Frame
        }
        #Only Saving PRE Dataframe is PRE is one/on
        if (Sim_PRE==1){
          Pre_Data.Frame<-Outputs_Student_Loop$Pre_Data.Frame
        }

      } #End Student Loop
      
      
      #Processes that occur at the end of a service.
      
      if (Sim_Fruit == 1){
        #Getting Rid of left over items. End of week.
        Fr_Data.Frame<-func_update_notcons(Fr_Data.Frame)
        #Adding Services
        Fr_Data.Frame<-func_Add_Services(Fr_Data.Frame)
        #Adding the data to the datalist
        datalistFr[[j]]<-Fr_Data.Frame
        #Sensitivity List
        
      }
      if (Sim_PSS == 1){
        #Getting Rid of left over items. End of week.
        Pss_Data.Frame<-func_update_notcons(Pss_Data.Frame)
        #Adding Services
        Pss_Data.Frame<-func_Add_Services(Pss_Data.Frame)
        #Adding the data to the datalist
        datalistPss[[j]]<-Pss_Data.Frame
      }
      if (Sim_PRE == 1){
        #Getting Rid of left over items. End of week.
        Pre_Data.Frame<-func_update_notcons(Pre_Data.Frame)
        #Adding Services
        Pre_Data.Frame<-func_Add_Services(Pre_Data.Frame)
        #Adding the data to the datalist
        datalistPre[[j]]<-Pre_Data.Frame
      }
      
      
      source("Output_Services.R") 
      
      if (Sim_Fruit == 1){
        List_Sens_Fr[[paste(l,k,j)]]<-Fr_Data.Frame
      }
      if (Sim_PSS == 1){
        List_Sens_Pss[[paste(l,k,j)]]<-Pss_Data.Frame
      }
      if (Sim_PRE == 1){
        List_Sens_Pre[[paste(l,k,j)]]<-Pre_Data.Frame
      }

      #message("Service #", j)
      
    } #end of service loop j
    
    if (Sim_Fruit==1){
      #Creation of the Services Data Frames
      Fr_Data = do.call(rbind,datalistFr)
      #Adding Data into the Datalists
      datalistFr_days[[k]]<-Fr_Data
    }
    if(Sim_PSS==1){
      #Creation of the Services Data Frames
      Pss_Data = do.call(rbind,datalistPss)
      #Adding Data into the Datalists
      datalistPss_days[[k]]<-Pss_Data
      
    }
    if(Sim_PRE==1){
      #Creation of the Services Data Frames
      Pre_Data = do.call(rbind,datalistPre)
      #Adding Data into the Datalists
      datalistPre_days[[k]]<-Pre_Data
    }
    
    source("Output_Days.R") 
    
    message("Day #", k)
    
  }#end of day loop k

  
  message("Done Gathering Week ", l)
  #end_time<-Sys.time()
  
  #Total_time<-end_time-start_time
  #print(Total_time)
} #end of l loop for iterations. 





                                      





