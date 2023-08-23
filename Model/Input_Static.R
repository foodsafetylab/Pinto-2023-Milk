# BASIC INPUTS ===========================================================================================================


# Selection of Hazard -----------------------------------------------------

  #Instructions: Select Hazard with 1, set non selected ones as 0
  salmonella<-0
  E_coli<-0
  norovirus<-1

  # Inputs for Iterations. Student, Services, Meal_Days ---------------------
  #Instructions: Calculated or simply input a number in N_Iterations and # the calculation
  
  #Students data: Initial Iterations i
  
  Students_p_grade<-89
  
  NSLP_rate<-.73
  
  N_Iterations<-round((Students_p_grade*NSLP_rate),0) #65
  
  #Services, number of days we are trying to iterate. Lunch periods per day
  Service_No<-2 #2 is baseline
  
  #Days we are trying to Iterate. Days
  Food_Days<-5 # 5 days of iterations

  #Weeks
  Sens_Iterations<-1000 #1000 weeks
  
  
#Lengths for milk model #need to make sense with no services all have to add up to 1440
  Time_Service_Length = 50 #50 is baseline
  Time_Turnaround_Length = 25 #25 is baseline
  Time_Overnight_Length = 1315 #1315 is baseline

# Sevice Line Information -------------------------------------------------
  
  #Initial Number of fruit
  Initial_Fr<-50 #Number of fruit 
  Initial_Pss<-50 #number of Packaged shelf stable
  Initial_Pre<-50 #number of packaged refrigerated
  
  #row size in selection table
  Row_size_Fr<-20
  Row_size_Pss<-20
  Row_size_Pre<-20  
  
  
# Inputs for Calculation if student is contaminated -------------------------
  

  
  
  Number_Student_Pathogens<-1 #Students that are contaminated that will enter the system every week. 
  
  Student_Pathogen_No<-c(1)  #No Student that is contaminated to enter iteration
  Student_Pathogen_Service<-c(1) #service in which contaminated kid enter the. 
  Student_Pathogen_Day<-c(1)#Day in which contaminated kids may enter system. 


  
  
#Inputs for allergen contamination ----------------------------------------
  
  Pr_Student_Allergen<-0 #probability of student bringing in Allergens
  Number_Student_Allergens<-1
  
  Student_Allergen_No<-c(1)  #No Student that is contaminated to enter iteration
  Student_Allergen_Service<-c(1) #service in which contaminated kid enter the. 
  Student_Allergen_Day<-c(1)#Day in which contaminated kids may enter system. 
  

# Inputs Behavioral Probabilities -----------------------------------------

  #Probability of Student touching other line items before picking their food. 
  Pr_touch_Food<-1
  
  #Pr
  Pr_touch_Food_ST<-1

  #Probability of student Picking up food from line-

  #Probability of Selecting Fruit
  Pr_select_Fr<-.56 #.23 other source
  #Probability of Selecting Pss
  Pr_select_Pss<-.59 #0.37 other source
  #Probability of selecting Pre
  Pr_select_Pre<-.96 #0.78 other source


  #Probability of consuming Food
  Pr_eat_Fr<-.63 #.48  
  Pr_eat_Pss<-.627 #.77
  Pr_eat_Pre<-.674 #.85



  
  # Inputs for Milk Spoilage -------------------------------------------------
  Milk_Con_Mean<- 0.38 # 0.38 #-1.77, q2.5 #2.52 q97.5, 0.38 q50 
  Milk_Con_SD<-1.1 #1.1
  Spoilage_Treshold<-7 #Considered spoiled milk. APCs log CFU/g
  
  #quantile(rnorm(100000,0.38,1.1),.50)
  
  
  #Inputs for E.coli Growth
  #Inputs for Func_Growth_Sto_Ecoli. Located in Input_Functions
  Inputs_Growth_Sto_Ecoli<-list(
    #Growth patameter
    b=0.023,
    #Growth parameter
    k=rnorm(1,.013,.001)/2.303,
    #Min Temp
    Tmin = (1.17)
  )
  
  #Inputs for Salmonella Growth
  
  Inputs_Growth_Sto_Salmonella<-list(
    #Growth patameter
    b=.020,
    #Growth parameter
    k=.0128/2.303,
    #Min Temp
    Tmin =(-0.571)
  )
  
  
#COMPLEX INPUTS =============================================================================  

# Inputs for Initial Food Contamination -----------------------------------
  
  
  #Toggle for calculated contamination, defaul is 0
  Calculated_Cont_Fr<-1
  Calculated_Cont_Pss<-1
  Calculated_Cont_Pre<-1
  
  #surface are of hand palm for when converting from cm^2 
  Student_PSA<-50.675 #cm^2
  
  #Fruit physical characteristics used for calculation
  Fr_Mean_area<-184.7 #cm2
  Fr_sd_area<- 8.6 #cm2
  Fr_Mean_weight<-171.1 #g
  Fr_sd_weight<-6.0 #g
  
  #Pss physical Characteristics
  Pss_Mean_area<-270 #cm2
  
  #Pre physical characteristics
  Pre_Mean_area<- 300 #cm2
  
#Salmonella
  
  #Fr Contamination per log CFU/ cm2 
  Fr_Contamination_salmonella<- (-4.16) #Salmonella
  #Prevalence of pathogens in Fruit
  Prevalence_Salmonella_Fr<-.04 #Probability of contamination.
  
  #Pss Contamination per log CFU/ cm2 
  Pss_Contamination_salmonella<- (0) #Salmonella
  #Prevalence of pathogens in Pss
  Prevalence_Salmonella_Pss<-0 #Probability of contamination.
  
  #Pre Contamination per log CFU/ cm2 
  Pre_Contamination_salmonella<- (0) #Salmonella
  #Prevalence of pathogens in Pre
  Prevalence_Salmonella_Pre<-0 #Probability of contamination.
  
  
#Norovirus  
  #Prevalence of Norovirus in Fruit
  Prevalence_Norovirus_Fr<-rbetagen(1,0.79,1.03,0,.2) #probability of food item being contaminated. 
  
  #Contamination levels of Norvirus in Pss
  Pss_Contamination_norovirus<-(0)  #norovirus
  Prevalence_Norovirus_Pss<-0 #probability of food item being contaminated. 
  

  Pre_Contamination_norovirus<-(0)  #norovirus
  Prevalence_Norovirus_Pre<- 0 #probability of food item being contaminated. 
  


  

# TOGGLES ==========================================================================
  
#Toggle to Change Which Foods to Simulate: 
  Sim_Fruit = 0
  Sim_PSS = 0
  Sim_PRE = 1 
  
#Toggle Cross-Contamination Steps. 
  Cross_Contamination =1

#Toggle for ill students avoiding share table
  Ill_Avoid_ST<-0
  Pr_Ill_Avoid_ST<-0.5
  
  
#All toggles 0 = no 1 == yes
  
#Growth, do we want to simulate growth of pathogens? 
  
  Growth<-0
  Growth_Pss<-0
  Growth_Pre<-0
  
# Washing Items Effect of washing items
  
  #Washing Between Services
  Wash_Between_Services<-0
  #Wash Selection Table Fruit
  Wash_Selection_YN_Fr<-0
  #Wash Share Table Items
  Wash_ST_YN_Fr<-0
  
#Share Table Toggle 
  
  #Include Share Table:
  Share_Table_YN<-1

  #Share Table to Service line after every Day # note turn in 1 if ST on
  STtoReservice_YN<-1 #this is reservice. 


#Visuals Toggles, if we want to change units in figures. 
  
  Units_Per_gram<-0
  
  
#Allergen Toggles
  Toggle_SelfAssigned_Allergens<-0
  
#Initial Contamination
  Toggle_SelfAssigned_Pathogens<-0
  
#Wrapping Apples
  Wrapping_Apples<-0
  
#Toggle for NSA_Analysis
  NSA_Analysis<-0
  #Other
  NSA_Analysis_Other<-0
  NSA_Analysis_Other_Sanitize<-0
  NSA_Analysis_Other_Wash<-0
  
  NSA_FW_Analysis <-0

#
  ST_Aside <-0

#Handwashing Station toggle. 

  Hawashing_Station <-0
  Sanitizing_Station<-0
  
  Hawashing_Station_ST <-0
  Sanitizing_Station_ST<-0
  
#Wash Bucket Station toggle. 
  
  Wash_Bucket<-0
  
#Milk Spoilage?
  
  Milk_Spoilage_YN<-TRUE
#Donation end of weeks
  Donation_End_Week <-TRUE
  
