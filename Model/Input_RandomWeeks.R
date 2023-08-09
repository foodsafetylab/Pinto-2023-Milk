
#Weekly Random Input
if(NSA_Analysis_Other_Wash==1){
  Wash_Method<-TreatmentNo
} else {
  Wash_Method<-as.numeric(sample(1:3, 1)) 
}

Wash_Method_Tracker<-c(Wash_Method_Tracker,Wash_Method)


#Hand Sanitizer Method

if(NSA_Analysis_Other_Sanitize==1){
  Sanitizer_Method<-TreatmentNo
} else {
  Sanitizer_Method<-as.numeric(sample(1:6, 1))
}

Saniztizer_Method_Tracker<-c(Saniztizer_Method_Tracker,Sanitizer_Method)


