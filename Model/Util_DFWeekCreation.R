


#Data Frames for Sens Analysis
AFr_Summary_DF<-data.frame(
  "Iteration.N" = 1:Sens_Iterations,
  "MeanCont" = as.numeric(""),
  "MedianCont" =as.numeric(""),
  "Cont5th"=as.numeric(""),
  "Cont95th"=as.numeric(""),
  "MeanContSelection"= as.numeric(""),
  "MedianContSelection"= as.numeric(""),
  "ContSel5th"=as.numeric(""),
  "ContSel95th"=as.numeric(""),
  "MeanContST"= as.numeric(""),
  "MedianContST"= as.numeric(""),
  "ContST5th"=as.numeric(""),
  "ContST95th"=as.numeric(""),
  "AllergenConsumed"= as.numeric(""),
  stringsAsFactors = FALSE
)


APss_Summary_DF<-data.frame(
  "Iteration.N" = 1:Sens_Iterations,
  "MeanCont" = as.numeric(""),
  "MedianCont" =as.numeric(""),
  "Cont5th"=as.numeric(""),
  "Cont95th"=as.numeric(""),
  "MeanContSelection"= as.numeric(""),
  "MedianContSelection"= as.numeric(""),
  "ContSel5th"=as.numeric(""),
  "ContSel95th"=as.numeric(""),
  "MeanContST"= as.numeric(""),
  "MedianContST"= as.numeric(""),
  "ContST5th"=as.numeric(""),
  "ContST95th"=as.numeric(""),
  "AllergenConsumed"= as.numeric(""),
  stringsAsFactors = FALSE
)


APre_Summary_DF<-data.frame(
  "Iteration.N" = 1:Sens_Iterations,
  "MeanCont" = as.numeric(""),
  "MedianCont" = as.numeric(""),
  "Cont5th"=as.numeric(""),
  "Cont95th"=as.numeric(""),
  "MeanContSelection"= as.numeric(""),
  "MedianContSelection"= as.numeric(""),
  "ContSel5th"=as.numeric(""),
  "ContSel95th"=as.numeric(""),
  "MeanContST"= as.numeric(""),
  "MedianContST"= as.numeric(""),
  "ContST5th"=as.numeric(""),
  "ContST95th"=as.numeric(""),
  "SpoiledConsumed"= as.numeric(""),
  "AllergenConsumed"= as.numeric(""),
  stringsAsFactors = FALSE
)



#Dose Response Dataframe

AFr_DR_Summary_DF<-data.frame(
  "Week"= 1:Sens_Iterations,
  "NoInfected"=as.numeric(""),
  "NoIll"=as.numeric(""),
  stringsAsFactors = FALSE
)





