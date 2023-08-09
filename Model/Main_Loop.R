Main_Loop<-function(){
  #Loading in Dataframes into the initial function.
  #Sim Fruit = Fruit Simulation ON
  #SIm PSS = PSS Simulations ON
  #Sim PRE = PRE Simulation ON
  
  if (Sim_Fruit ==1){
    Fr_Data.Frame<-Fr_Data.Frame #Global Variables
  }

  if (Sim_PSS ==1){
    Pss_Data.Frame<-Pss_Data.Frame #Global Variables
  }
  
  if (Sim_PRE ==1){
    Pre_Data.Frame<-Pre_Data.Frame #Global Variables
  }

  #Random Inputs for Sensitivity Analysis 
  if(NSA_Analysis==1){
    source("NSA_Options_For_Inputs.R") # Fixed: Nominal Range Sens A
  } else if (NSA_FW_Analysis ==1){
    source("NSA_Options_For_FW.R") #Fixed: Nominal Range Food Waste Inputs
  } else {
    source("Input_Random.R") #Normal Random Inputs
  }  
  
  
  #Calculations for initial contamination of the student. 
  source("Calc_StudentContamination.R")
  
  #Feeding Items into the system if any items ran out. 
  source("Calc_FeedingItems.R")
  
  #STUDENT SELECTION =============================================================================================================
  
  if (Sim_Fruit == 1){ #Fruit---------------------------------------------------------------
    
    #Student Touching Fruit in line
    
    #Did Student touch other fruit based on probability? 
    Touch_YN_Fr<-ifelse(runif(1)<Pr_touch_Food,1,0) 
    
    #If touched what is the contamination and adding it to data frame?
    if(Touch_YN_Fr==1){
      #Making sure there is enough fruit in selection table
      Fr_Available<-Fr_Data.Frame$Location == "Selection Table" 
      Sum_Fr_Available<-sum(Fr_Available, na.rm = TRUE)
      #Proceed if there is enough fruit in selection table. 
      if(Sum_Fr_Available>ntouched_Fr){
        #Looping through touched fruit. 
        for (i in 1:ntouched_Fr){
          OutputsFT<-Func_Touched(DF = Fr_Data.Frame, 
                                  RowSizeVar = Row_size_Fr, 
                                  Item = "Fruit", 
                                  Item_Picked = Fr_Touched, 
                                  SearchLocation = "Selection Table")
          
          Cont_Student<-OutputsFT$Cont_Student
          Fr_Data.Frame<-OutputsFT$DF
        } #End of touching loop.
      } # End of Sum Available
    } #End of Touch YN Fr
    
    
    #Student Picking Fruit from line
    
    #Did a student pick up fruit from the selection table? 
    Pick_YN_Fr<-ifelse(runif(1,0,1)<Pr_select_Fr,1,0)
    
    #Picking a Fruit from Fruit data frame 
    if(Pick_YN_Fr==1){
      #Making sure there is enough fruit in selection table
      Fr_Available<-Fr_Data.Frame$Location == "Selection Table" 
      Sum_Fr_Available<-as.numeric(sum(Fr_Available,na.rm = TRUE))
      #if there is more than one fruit select the fruit from the selection table
      if(Sum_Fr_Available>0){   
        OutputFP<-Func_Picked(DF = Fr_Data.Frame, Item_Picked = Fr_Picked, Location = "Selection Table")
        Fr_Data.Frame<-OutputFP$DF
        Fr_Picked<-OutputFP$Item_Picked
        
        #Contamination from Hand to fruit Going into Tray
        OutputFCC<-Func_Cross_Contamination(Cont_Student=Cont_Student,
                                            Data.Frame=Fr_Data.Frame, 
                                            Item_Picked= Fr_Picked, 
                                            Item="Fruit")
        Fr_Data.Frame<-OutputFCC$Data.Frame
        Cont_Student<-OutputFCC$Cont_Student
        #Fr_Data.Frame<-Func_Allergen_CC(Fr_Data.Frame,Fr_Picked) #Adding Allergen Contamination from touch.
      } #End of Sum Fruit Available
    } #End of Pick YN Fruit 
  }#End of Sim Fruit

  
  if (Sim_PSS ==1){ #Pss-----------------------------------------------------------------------
    
    
    #Did student touch other Pss based on probability 
    Touch_YN_Pss<-ifelse(runif(1)<Pr_touch_Food,1,0) 
    
    #If touched what is the contamination and adding it to data frame
    
    if(Touch_YN_Pss==1){
      #Making sure there is enough Pss in selection table
      Pss_Available<-Pss_Data.Frame$Location == "Selection Table" 
      Sum_Pss_Available<-sum(Pss_Available, na.rm = TRUE)
      
      if(Sum_Pss_Available>ntouched_Pss){
        for (i in 1:ntouched_Pss){
          OutputsFT<-Func_Touched(DF = Pss_Data.Frame, 
                                  RowSizeVar = Row_size_Pss, Item = "PSS", 
                                  Item_Picked = Pss_Touched,
                                  SearchLocation = "Selection Table")
          
          Cont_Student<-OutputsFT$Cont_Student
          Pss_Data.Frame<-OutputsFT$DF
        }
      }
    }
    
    
    #Student picking Pss from line
    
    Pick_YN_Pss<-ifelse(runif(1)<Pr_select_Pss,1,0) 
    #Picking a Pss from Pss Data Frame 
    if(Pick_YN_Pss==1){
      #Making sure there is enough PSS in Selection table
      Pss_Available<-Pss_Data.Frame$Location == "Selection Table" 
      Sum_Pss_Available<-sum(Pss_Available, na.rm = TRUE)
      if(Sum_Pss_Available>0){
        OutputFP<-Func_Picked(DF = Pss_Data.Frame,Item_Picked = Pss_Picked,Location = "Selection Table")
        Pss_Data.Frame<-OutputFP$DF
        Pss_Picked<-OutputFP$Item_Picked
        
        #Contamination from Hand to PSS Going into Tray
        OutputFCC<-Func_Cross_Contamination(Cont_Student=Cont_Student,
                                            Data.Frame=Pss_Data.Frame,
                                            Item_Picked= Pss_Picked, 
                                            Item="PSS")
        Pss_Data.Frame<-OutputFCC$Data.Frame
        Cont_Student<-OutputFCC$Cont_Student
        #Pss_Data.Frame<-Func_Allergen_CC(Pss_Data.Frame,Pss_Picked) #Adding Allergen Contamination from touch.
      } #End of  PSS Available
    } #End of Pick YN
  } #End of Sim PSS if

  

  if(Sim_PRE){ #Pre-------------------------------------------------------------------------------------

    
    #Did the Student touch any Pre during selection? 
    Touch_YN_Pre<-ifelse(runif(1)<Pr_touch_Food,1,0) 
    
    #If touched what is the contamination and adding it to data frame
    
    if(Touch_YN_Pre==1){
      #Making Sure there is enough PRE in Selection Table
      Pre_Available<-Pre_Data.Frame$Location == "Selection Table" 
      Sum_Pre_Available<-sum(Pre_Available, na.rm = TRUE)
      if(Sum_Pre_Available>ntouched_Pre){
        for (i in 1:ntouched_Pre){
          OutputsFT<-Func_Touched(DF = Pre_Data.Frame, 
                                  RowSizeVar = Row_size_Pre, 
                                  Item = "PRE", 
                                  Item_Picked = Pre_Touched,
                                  SearchLocation = "Selection Table")
          Cont_Student<-OutputsFT$Cont_Student
          Pre_Data.Frame<-OutputsFT$DF
        }
      }
    }
    
    
    #Picking Pre from line
    Pick_YN_Pre<-ifelse(runif(1)<Pr_select_Pre,1,0) 
    #Picking a Pre from Pre Data Frame 
    if(Pick_YN_Pre==1){
      Pre_Available<-Pre_Data.Frame$Location == "Selection Table" 
      Sum_Pre_Available<-sum(Pre_Available, na.rm = TRUE)
      if(Sum_Pre_Available>0){
        OutputFP<-Func_Picked(DF = Pre_Data.Frame,Item_Picked = Pre_Picked,Location = "Selection Table")
        Pre_Data.Frame<-OutputFP$DF
        Pre_Picked<-OutputFP$Item_Picked
        
        #Contamination from Hand to Pre Going into Tray
        OutputFCC<-Func_Cross_Contamination(Cont_Student=Cont_Student,
                                            Data.Frame=Pre_Data.Frame,
                                            Item_Picked= Pre_Picked, 
                                            Item="PRE")
        Pre_Data.Frame<-OutputFCC$Data.Frame
        Cont_Student<-OutputFCC$Cont_Student
        # #Milk Spoilage Sub model
        # if (Milk_Spoilage_YN==TRUE){
        #   Pre_Data.Frame<-Func_Time_Temp(DF =Pre_Data.Frame , 
        #                                  Item_Picked =Pre_Picked , 
        #                                  Temp = Temp_SL,
        #                                  Time = Time_SL)
        # }
      }
    }
  } #End of Sim Pre

  
  # CONSUMPTION===================================================================================================================
  
  
   if (Sim_Fruit==1){#Fruit -------------------------------------------------------------
    #Did the student consume the Fruit?
    Eat_YN_Fr<-ifelse(runif(1)<Pr_eat_Fr,1,0)
    
    #Changing Data Frame so it updates when student consumes fruit.
    
    if(Pick_YN_Fr==1 && Sum_Fr_Available>0){ 
      #Making sure there was enough fruit. 
      OutputsFEFr<-Func_Eat_Fr(Eat_YN_Item = Eat_YN_Fr, 
                               DF = Fr_Data.Frame,
                               Item_Picked = Fr_Picked,
                               Item = "Fruit", 
                               Location ="Selection Table")
      Cont_Student<-OutputsFEFr$Cont_Student
      Fr_Data.Frame<-OutputsFEFr$DF
    }#end of pick statement
  } #End of Sim Fruit Statement.
  
  if (Sim_PSS==1){ #Pss------------------------------------------------------------------

    #Did student consume the Pss
    Eat_YN_Pss<-ifelse(runif(1)<Pr_eat_Pss,1,0)
    
    #Changing Data Frame for consumption of Pss
    if(Pick_YN_Pss==1 && Sum_Pss_Available>0){
      OutputsFEPss<-Func_Eat_Pss(Eat_YN_Item = Eat_YN_Pss,
                                 DF = Pss_Data.Frame, 
                                 Item_Picked = Pss_Picked, 
                                 Item = "PSS",
                                 Location ="Selection Table")
      Cont_Student<-OutputsFEPss$Cont_Student
      Pss_Data.Frame<-OutputsFEPss$DF
    } #end of If
  } #End of Sim PSS

  
  
  if (Sim_PRE==1){ #Pre -----------------------------------------------------------------
    #Did student consume the Pre
    Eat_YN_Pre<-ifelse(runif(1)<Pr_eat_Pre,1,0)
    #Changing Data Frame for consumption of Pre
    if(Sum_Pre_Available>0){
      if(Pick_YN_Pre==1){
        
        #if student consumed pre, then adding consumption time #NEW
        if (Eat_YN_Pre==1){
          Pre_Data.Frame<-Func_Adding_Time_ConItem(DF = Pre_Data.Frame, 
                                                   Item_Picked = Pre_Picked, 
                                                   Time = round(runif(1,1,Time_Service_Length),0))
          
          
        }
        
        OutputFEPre<-Func_Eat_Pre(Eat_YN_Item = Eat_YN_Pre,
                                  DF = Pre_Data.Frame, 
                                  Item_Picked = Pre_Picked, 
                                  Item = "PRE",
                                  Location ="Selection Table")
        Cont_Student<-OutputFEPre$Cont_Student
        Pre_Data.Frame<-OutputFEPre$DF
        
        #Milk Spoilage Sub model for time during regardless of if item got consumer or not
        # if (Milk_Spoilage_YN==TRUE){
        #           Pre_Data.Frame<-Func_Time_Temp(DF =Pre_Data.Frame , 
        #                                Item_Picked =Pre_Picked , 
        #                                Temp = Temp_SD,
        #                                Time = Time_SD)
        # }
      }
    }#End of sum available Statement
  } #End of Sim_PRE
  

 
  #Avoid Sick Students from ST
  Student_Allowed<-1
  if(Ill_Avoid_ST==1){
    if(Is_Student_ill== 1){
      Student_Allowed<-ifelse(runif(1)<Pr_Ill_Avoid_ST,1,0)
    }
  }
  
  
  #NOT CONSUMED TO ST================================================================================================================
  
  if(Share_Table_YN==1){ 

    if(Student_Allowed==1){
      
      #Hand_Sanitizing or hand washing Station for allowed student. 
      if (Hawashing_Station_ST == 1){
        LogRed_Prior = rpert(1,0.17,0.45,6,shape = 4)
        Cont_Student<-round(Cont_Student*10^-LogRed_Prior,0)
      }
      
      if (Sanitizing_Station_ST == 1){
        LogRed_Prior<-Func_Randomize_Sanitizer(Wash_Method = Sanitizer_Method)
        Cont_Student<-round(Cont_Student*10^-LogRed_Prior,0)
      }

      #Probability of the student sharing their food. 
      Share_YN_Food<-ifelse(runif(1)<Pr_share_Food,1,0)
      
      #Fruit--------------------------------------------------------------------
      if (Sim_Fruit==1){
        if(Pick_YN_Fr==1 && Sum_Fr_Available>0){  
          Fr_Data.Frame<-Func_Shared(DF = Fr_Data.Frame, Item_Picked = Fr_Picked,Share_YN_Food=Share_YN_Food, Item ="Fruit")
        }
      }

      
      #Pss--------------------------------------------------------------------
      if (Sim_PSS==1){
        if(Pick_YN_Pss==1 && Sum_Pss_Available>0){  
          Pss_Data.Frame<-Func_Shared(DF = Pss_Data.Frame, Item_Picked = Pss_Picked,Share_YN_Food=Share_YN_Food, Item ="Pss")
          
        }
      }

      
      #Pre--------------------------------------------------------------------
      if(Sim_PRE==1){
        if(Pick_YN_Pre==1 && Sum_Pre_Available>0){    
          Pre_Data.Frame<-Func_Shared(DF = Pre_Data.Frame, Item_Picked = Pre_Picked,Share_YN_Food=Share_YN_Food, Item ="Pre")
          if ((Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Location"]=="Not Shared")|(Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Location"]=="Not Consumed")){
            Pre_Data.Frame<-Func_Adding_Time_ConItem(DF = Pre_Data.Frame, 
                                     Item_Picked = Pre_Picked, 
                                     Time = round(runif(1,1,Time_Service_Length),0))
          }
           
        } 
      }
    
      
      #PICKING ST ITEMS ===========================================================================================================
      
      #Fruit------------------------------------------------------------------
      if (Sim_Fruit==1){ 
        #Making sure there are items in the Share Table
        Items_Shared<-Fr_Data.Frame$Location == "Shared" 
        Sum_Shared<-sum(Items_Shared, na.rm = TRUE)
        
        if(Sum_Shared>0){
          #Did a student approach tio pick an item for the share table? 
          Pick_ST_YN_Fr<-ifelse(runif(1)<Pr_Pick_ST_Fr,1,0) 
          #Fruit picked from Share Table.
          
          if(Pick_ST_YN_Fr==1){
            
            #Did Student touch other fruit based on probability? 
            Touch_YN_Fr_ST<-ifelse(runif(1)<Pr_touch_Food_ST,1,0) 
            
            #If touched what is the contamination and adding it to data frame?
            if(Touch_YN_Fr_ST==1){
              Fr_Available<-Fr_Data.Frame$Location == "Shared" 
              Sum_Fr_Available<-sum(Fr_Available, na.rm = TRUE)
              if(Sum_Fr_Available>ntouched_Fr){
                for (i in 1:ntouched_Fr){
                  OutputsFT<-Func_Touched(DF = Fr_Data.Frame, 
                                          RowSizeVar = Row_size_Fr, 
                                          Item = "Fruit", 
                                          Item_Picked = Fr_Touched_ST,
                                          SearchLocation = "Shared")
                  Cont_Student<-OutputsFT$Cont_Student
                  Fr_Data.Frame<-OutputsFT$DF
                }
              }
            }
            
            OutputFP_ST<-Func_Picked_ST(DF = Fr_Data.Frame,Item_Picked_ST = Fr_ST_Picked)
            Fr_Data.Frame<-OutputFP_ST$DF
            Fr_ST_Picked<-OutputFP_ST$Item_Picked_ST
          }
          
          #Contamination from Hand to Fruit or from Fruit to Hand. 
          if(Pick_ST_YN_Fr==1){
            #Contamination from picking item share table
            OutputFCC_ST<-Func_Cross_Contamination(Cont_Student=Cont_Student,
                                                   Data.Frame=Fr_Data.Frame, 
                                                   Item_Picked= Fr_ST_Picked, 
                                                   Item="Fruit")
            Fr_Data.Frame<-OutputFCC_ST$Data.Frame
            Cont_Student<-OutputFCC_ST$Cont_Student
            #Fr_Data.Frame<-Func_Allergen_CC(Fr_Data.Frame,Fr_ST_Picked) #Adding Allergen Contamination
          }
        }  #end of if to make sure there are ST items
      } #End of if Sim Fruit. 
      

            
      #Pss--------------------------------------------------------------
      if (Sim_PSS==1){ 
        Items_Shared_Pss<-Pss_Data.Frame$Location == "Shared" 
        Sum_Shared_Pss<-sum(Items_Shared_Pss, na.rm = TRUE)
        if(Sum_Shared_Pss>0){
          
          
          #Did a student pick an item for the share table? 
          Pick_ST_YN_Pss<-ifelse(runif(1)<Pr_Pick_ST_Pss,1,0) 
          
          #Pss picked from Share Table. 
          if(Pick_ST_YN_Pss==1){
            
            #Did Student touch other fruit based on probability? 
            Touch_YN_Pss_ST<-ifelse(runif(1)<Pr_touch_Food_ST,1,0) 
            
            #If touched what is the contamination and adding it to data frame?
            if(Touch_YN_Pss_ST==1){
              Pss_Available<-Pss_Data.Frame$Location == "Shared"
              Sum_Pss_Available<-sum(Pss_Available, na.rm = TRUE)
              if(Sum_Pss_Available>ntouched_Pss){
                for (i in 1:ntouched_Pss){
                  OutputsFT<-Func_Touched(DF = Pss_Data.Frame, 
                                          RowSizeVar = Row_size_Pss, 
                                          Item = "PSS", 
                                          Item_Picked = Pss_Touched_ST,
                                          SearchLocation = "Shared")
                  Cont_Student<-OutputsFT$Cont_Student
                  Pss_Data.Frame<-OutputsFT$DF
                }
              }
            }
            
            OutputFP_ST<-Func_Picked_ST(DF = Pss_Data.Frame,Item_Picked_ST = Pss_ST_Picked)
            Pss_Data.Frame<-OutputFP_ST$DF
            Pss_ST_Picked<-OutputFP_ST$Item_Picked_ST
          }
          
          #Contamination from Hand to Pss or from Hand to Pss. 
          if(Pick_ST_YN_Pss==1){
            #Contamination from picking item share table
            OutputFCC_ST<-Func_Cross_Contamination(Cont_Student=Cont_Student,
                                                   Data.Frame=Pss_Data.Frame, 
                                                   Item_Picked=  Pss_ST_Picked, 
                                                   Item="PSS")
            Pss_Data.Frame<-OutputFCC_ST$Data.Frame
            Cont_Student<-OutputFCC_ST$Cont_Student
            #Pss_Data.Frame<-Func_Allergen_CC(Pss_Data.Frame,Pss_ST_Picked) #Adding Allergen Contamination
          }
        } #end of loop to check if there are enough shared items
      } #end of if Sim Pre if statement

      
      
      #Pre-----------------------------------------------------------------
      if (Sim_PRE==1){
        Items_Shared_Pre<-Pre_Data.Frame$Location == "Shared" 
        Sum_Shared_Pre<-sum(Items_Shared_Pre, na.rm = TRUE)
        if(Sum_Shared_Pre>0){
          
          #Did a student pick an item for the share table? 
          Pick_ST_YN_Pre<-ifelse(runif(1)<Pr_Pick_ST_Pre,1,0) 
          
          #Pre picked from Share Table. 
          if(Pick_ST_YN_Pre==1){
            
            #Did Student touch other fruit based on probability? 
            Touch_YN_Pre_ST<-ifelse(runif(1)<Pr_touch_Food_ST,1,0) 
            
            #If touched what is the contamination and adding it to data frame?
            if(Touch_YN_Pre_ST==1){
              Pre_Available<-Pre_Data.Frame$Location == "Shared"
              Sum_Pre_Available<-sum(Pre_Available, na.rm = TRUE)
              if(Sum_Pre_Available>ntouched_Pre){
                for (i in 1:ntouched_Pre){
                  OutputsFT<-Func_Touched(DF = Pre_Data.Frame, 
                                          RowSizeVar = Row_size_Pre, 
                                          Item = "PRE", 
                                          Item_Picked = Pre_Touched_ST,
                                          SearchLocation = "Shared")
                  Cont_Student<-OutputsFT$Cont_Student
                  Pre_Data.Frame<-OutputsFT$DF
                }
              }
            }
            OutputFP_ST<-Func_Picked_ST(DF = Pre_Data.Frame,Item_Picked_ST = Pre_ST_Picked)
            Pre_Data.Frame<-OutputFP_ST$DF
            Pre_ST_Picked<-OutputFP_ST$Item_Picked_ST
          }
          
          if(Pick_ST_YN_Pre==1){
            #Contamination from picking item share table
            OutputFCC_ST<-Func_Cross_Contamination(Cont_Student=Cont_Student,
                                                   Data.Frame=Pre_Data.Frame, 
                                                   Item_Picked= Pre_ST_Picked, 
                                                   Item="PRE")
            Pre_Data.Frame<-OutputFCC_ST$Data.Frame
            Cont_Student<-OutputFCC_ST$Cont_Student
            #Milk Spoilage Sub model
            # if (Milk_Spoilage_YN==TRUE){
            #   Pre_Data.Frame<-Func_Time_Temp(DF =Pre_Data.Frame , 
            #                                  Item_Picked =Pre_ST_Picked , 
            #                                  Temp = Temp_ST,
            #                                  Time = Time_ST)
            # }
            #Pre_Data.Frame<-Func_Allergen_CC(Pre_Data.Frame,Pre_ST_Picked) #Adding Allergen Contamination
          }
        }#End of if there is st items loop
      } #End of if Sim PRe statement

      
      #CONSUMPTION OF ST ITEMS ===================================================================================================== 
      
      #Consuming Fruit--------------------------------------------------------------------------
      if (Sim_Fruit==1){ 
        if(Sum_Shared>0){
          if (Pick_ST_YN_Fr==1){
            #Did the student consume the Fruit?
            Eat_YN_ST_Fr<-ifelse(runif(1)<Pr_eat_ST_Fr,1,0)
            #Changing Data Frame so it updates when student consumes fruit.
            OutputsFEFr<-Func_Eat_Fr(Eat_YN_Item = Eat_YN_ST_Fr, 
                                     DF = Fr_Data.Frame,
                                     Item_Picked = Fr_ST_Picked,
                                     Item = "Fruit",
                                     Location = "Share Table")
            Cont_Student<-OutputsFEFr$Cont_Student
            Fr_Data.Frame<-OutputsFEFr$DF
          }
        }
      } #End of Sim Fruit Statement.
      
      
      #Consuming Pss--------------------------------------------------------------------------
      if(Sim_PSS==1){
        if(Sum_Shared_Pss>0){
          if(Pick_ST_YN_Pss==1){
            #Did the student consume the Fruit?
            Eat_YN_ST_Pss<-ifelse(runif(1)<Pr_eat_ST_Pss,1,0)
            #Changing Data Frame so it updates when student consumes fruit.
            OutputsFEPss<-Func_Eat_Pss(Eat_YN_Item = Eat_YN_ST_Pss, 
                                       DF = Pss_Data.Frame,
                                       Item_Picked = Pss_ST_Picked,
                                       Item = "PSS",
                                       Location = "Share Table")
            Cont_Student<-OutputsFEPss$Cont_Student
            Pss_Data.Frame<-OutputsFEPss$DF
          }
        }
      } #End of the Sim PSS if

      
      
      #Consuming Pre--------------------------------------------------------------------------
      if(Sim_PRE==1){
        if(Sum_Shared_Pre>0){
          if(Pick_ST_YN_Pre==1){
            #Did the student consume the Fruit?
            Eat_YN_ST_Pre<-ifelse(runif(1)<Pr_eat_ST_Pre,1,0)
            
            #if student consumed pre from ST, then adding consumption time #NEW
            if (Eat_YN_ST_Pre==1){
              Pre_Data.Frame<-Func_Adding_Time_ConItem(DF = Pre_Data.Frame, 
                                                       Item_Picked = Pre_ST_Picked, 
                                                       Time = round(runif(1,1,Time_Service_Length),0)) #time in minutes. 
            } 
            
            #Changing Data Frame so it updates when student consumes milk.
            OutputsFEPre<-Func_Eat_Pre(Eat_YN_Item = Eat_YN_ST_Pre, 
                                       DF = Pre_Data.Frame,
                                       Item_Picked = Pre_ST_Picked,
                                       Item = "PRE",
                                       Location = "Share Table")
            Cont_Student<-OutputsFEPre$Cont_Student
            Pre_Data.Frame<-OutputsFEPre$DF
            
            if (Eat_YN_ST_Pre== 0){ 
              #Deciding to share to ST one more time if they decide not to eat, if not discard
              if(Pick_ST_YN_Pre==1){  
                #print("this happened")
                Pre_Data.Frame<-Func_Shared(DF = Pre_Data.Frame, Item_Picked = Pre_ST_Picked,Share_YN_Food=Share_YN_Food, Item ="Pre")
                #print(Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)== "Location"])
                if (Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)== "Location"]=="Not Shared"){
                  #print("happened")
                  Pre_Data.Frame<-Func_Adding_Time_ConItem(DF = Pre_Data.Frame, 
                                                           Item_Picked = Pre_ST_Picked, 
                                                           Time = round(runif(1,1,Time_Service_Length),0))
                }
                
              }
            }
            
          }
          
        }
      } #End of the Sim Pre if
      
      
   
    }#End of ill student exclusion loop  
  }#end of Share Table  toggle loop
  
  
  if (Sim_Fruit ==1 && Sim_PRE ==0 && Sim_PSS ==0){
    Outputs_Student_Loop<-list(Fr_Data.Frame=Fr_Data.Frame) #Fruit on
  } else if (Sim_Fruit ==0 && Sim_PRE ==1 && Sim_PSS ==0){
    Outputs_Student_Loop<-list(Pre_Data.Frame=Pre_Data.Frame) #PRE on
  } else if (Sim_Fruit ==0 && Sim_PRE ==0 && Sim_PSS ==1){
    Outputs_Student_Loop<-list(Pss_Data.Frame=Pss_Data.Frame) #PSS on
  } else if (Sim_Fruit ==1 && Sim_PRE ==1 && Sim_PSS ==0){
    Outputs_Student_Loop<-list(Fr_Data.Frame=Fr_Data.Frame,Pre_Data.Frame=Pre_Data.Frame) #Fruit and PRE on
  } else if (Sim_Fruit ==1 && Sim_PRE ==0 && Sim_PSS ==1){
    Outputs_Student_Loop<-list(Fr_Data.Frame=Fr_Data.Frame,Pss_Data.Frame=Pss_Data.Frame) #Fruit and PSS on
  } else if (Sim_Fruit ==0 && Sim_PRE ==1 && Sim_PSS ==1){
    Outputs_Student_Loop<-list(Fr_Data.Frame=Fr_Data.Frame,Pss_Data.Frame=Pss_Data.Frame) #PRE and PSS on
  } else if (Sim_Fruit ==1 && Sim_PRE ==1 && Sim_PSS ==1){
    Outputs_Student_Loop<-list(Fr_Data.Frame=Fr_Data.Frame,Pss_Data.Frame=Pss_Data.Frame, Pre_Data.Frame = Pre_Data.Frame) #Fruit, PRE and PSS on
  }
  
  
  return(Outputs_Student_Loop) #Final Return Statement of Main Loop
}

  