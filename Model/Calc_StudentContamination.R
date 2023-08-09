
# Script for Initial Contaminations ---------------------------------------

#Running Student Contamination based on Probability.

Is_Student_ill<-0 #reseting student ill
Cont_Student<- ifelse(runif(1)<Pr_Student_iC,do.call(Func_ICont_Student,Inputs_ICont_Student),0) 
if (Cont_Student>0){
  #Student_Cont_Count<-(Student_Cont_Count+1) do not need
  Vector_Contaminations_In<-c(Vector_Contaminations_In, Cont_Student)
  if (Hawashing_Station == 1){
    LogRed_Prior = rpert(1,0.17,0.45,6,shape = 4) #Reduction due to Hanwashing Stration
    Cont_Student<-round(Cont_Student*10^-LogRed_Prior,0)
  }
  if (Sanitizing_Station == 1){ 
    LogRed_Prior<-Func_Randomize_Sanitizer(Wash_Method = Sanitizer_Method) #Reduction due to hand sanitizer
    Cont_Student<-round(Cont_Student*10^-LogRed_Prior,0)
  }
  Is_Student_ill<-1
  if(Is_Student_ill==1){
    Vector_Is_Student_ill<-c(Vector_Is_Student_ill,Is_Student_ill)
  }
}


#Runing Self assigned Student pathogen contamination
if(Toggle_SelfAssigned_Pathogens==1){
  if( k == Student_Pathogen_Day[Student_Pathogen_Count] 
      && j == Student_Pathogen_Service[Student_Pathogen_Count] 
      && z == Student_Pathogen_No[Student_Pathogen_Count]){
    Cont_Student<-do.call(Func_ICont_Student,Inputs_ICont_Student)
    print(Cont_Student)
    Student_Pathogen_Count<-(Student_Pathogen_Count+1)
    if(Student_Pathogen_Count>Number_Student_Pathogens){
      Student_Pathogen_Count<-Number_Student_Pathogens
    }
  }
}


#Running Student Allergen Contamination based on Probability
Cont_Student_Allergen_YN<-ifelse(runif(1)<Pr_Student_Allergen,1,0)

#Self Assigned Contaminations
if(Toggle_SelfAssigned_Allergens==1){
  if( k == Student_Allergen_Day[Student_Allergen_Count] 
      && j == Student_Allergen_Service[Student_Allergen_Count] 
      && z == Student_Allergen_No[Student_Allergen_Count]){
    Cont_Student_Allergen_YN <- 1 
    #print("A student is contaminated with Allergens")
    Student_Allergen_Count<-(Student_Allergen_Count+1)
    if(Student_Allergen_Count>Number_Student_Allergens){
      Student_Allergen_Count<-Number_Student_Allergens
    }
  }
}


