#Functions for Analysis


#1.creating main data frame.
Func_Asys_MainDF<-function(SummaryList, FoodType){
  #SummaryLisr = List_Sens_Fr
  #FoodType= = "Fruit"
  #1. Start from here
  Individual_Analysis_Fr<-rbind.fill(SummaryList)
  
  #Adding Type Column
  Individual_Analysis_Fr$Type<-FoodType
  
  #2. find the dupplicates
  #this step filters replicated based on the ID
  Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
    group_by(ID) %>% 
    filter(TotServices==max(TotServices))%>%
    filter(Location=="Consumed")%>%
    select(ID,Type,ConsumedBy,Contamination, ContConsumed, Infection,Illness,week)
  #3
  AnalysysDF<-Individual_Analysis_Fr
  return(AnalysysDF)
}


Func_Asys_MainDFDp<-function(SummaryList, FoodType){
  #SummaryLisr = List_Sens_Fr
  #FoodType= = "Fruit"
  #1. Start from here
  Individual_Analysis_Fr<-bind_rows(SummaryList)
  
  #Adding Type Column
  Individual_Analysis_Fr$Type<-FoodType
  
  #2. find the dupplicates
  #this step filters replicated based on the ID
  Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
    group_by(ID) %>% 
    filter(TotServices==max(TotServices))%>%
    filter(Location=="Consumed")%>%
    select(ID,Type,ConsumedBy,Contamination, ContConsumed, Infection,Illness,week)
  #3
  AnalysysDF<-Individual_Analysis_Fr
  return(AnalysysDF)
}




Func_Asys_ContbyStudent<-function(AnalysisDF){
  #Analysis DF = ST_ON_Analysis
  AnalysisDF<-AnalysisDF%>%
    group_by(ConsumedBy)%>%
    dplyr::summarise(Contamination = sum(Contamination))
  
  AnalysisDF$week <- substr(AnalysisDF$ConsumedBy, 1, 4) 
  AnalysisDF$Infection<-as.logical("")
  AnalysisDF$Illness<-as.logical("")
  return(AnalysisDF)
}



Func_DR_Main<-function(AnalysisDF, Reps_DR){
  #AnalysisDF = ST_ON_Analysis
  List_week_DR<-split(x=AnalysisDF,f=AnalysisDF$week)
  lista<-replicate(Reps_DR,lapply(List_week_DR,Func_Rep_DR),simplify = FALSE)
  list_Df_Rep<-lapply(lista, Func_List2DF)
  list_Df_Rep<-lapply(list_Df_Rep, setNames, c("Infections", "Illness"))
  list_Df_Rep<-lapply(list_Df_Rep,bind_rows)
  list_Df_Rep<-bind_cols(list_Df_Rep)
  
  df_inf_Week = list_Df_Rep[,seq(1, ncol(list_Df_Rep), 2) ]
  df_ill_Week = list_Df_Rep[,seq(2, ncol(list_Df_Rep), 2) ]
  
  df_inf_Week<-as.data.frame(t(df_inf_Week))
  df_ill_Week<-as.data.frame(t(df_ill_Week))
  
  rownames(df_inf_Week)<-paste("Rep",1:Reps_DR)
  rownames(df_ill_Week)<-paste("Rep",1:Reps_DR)
  colnames(df_inf_Week)<-paste("Week",1:Sens_Iterations)
  colnames(df_ill_Week)<-paste("Week",1:Sens_Iterations)
  
  OutputsDR<-list(df_inf_Week = df_inf_Week, df_ill_Week=df_ill_Week)
}


Func_DF_Prevalence<-function(AnalysisDFCop, df_Ill_Week, Intervention){
  #AnalysisDFCop= ST_ON_Analysis_Copy
  #df_Ill_Week = df_ill_Week_ON
  #Interventions = "Int"
  
  df1<-AnalysisDFCop %>% 
    group_by(week) %>% 
    summarise(count=n())
  df1$week<-paste("Week",df1$week)
  
  df_Ill_Week_Box <- melt(df_Ill_Week, measure.var = paste("Week",1:Sens_Iterations))
  
  df_Ill_Week_Box_Prev<-merge(df_Ill_Week_Box, df1, by.x='variable', by.y='week')
  df_Ill_Week_Box_Prev$Prev<-df_Ill_Week_Box_Prev$value/df_Ill_Week_Box_Prev$count
  
  df_Ill_Week_Box_Prev$Int<-Intervention
  
  return(df_Ill_Week_Box_Prev)
}


func_remove_repeats<-function(DF){
  DF %>% 
    group_by(ID) %>% 
    filter(TotServices==max(TotServices))->DF
  return(DF)
}

Func_DF_Locations_1<-function(){
  ST_OFF_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFF)
  ST_ON_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopON)
  ST_ONWash_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopONWash)
  ST_ONWr_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopONWr)
  ST_OFFWash_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFFWash)
  ST_OFFWr_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFFWr)
  ST_Exc_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopExc)
  ST_STClosed_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_STClosed)
  ST_STAside_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopSTAside)
  ST_Touch_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopTouch)
  ST_TouchST_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopTouchST)
  ST_WStation_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopWStation)
  ST_WBucket_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopWBucket)
  ST_Sanitizer_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopSanitizer)
  

  ST_OFF_Analysis_Con$Type<-"OFF"
  ST_ON_Analysis_Con$Type<-"ON"
  ST_ONWash_Analysis_Con$Type<-"ONWash"
  ST_ONWr_Analysis_Con$Type<-"ONWr"
  ST_OFFWash_Analysis_Con$Type<-"OFFWash"
  ST_OFFWr_Analysis_Con$Type<-"OFFWr"
  ST_Exc_Analysis_Con$Type<-"Exc"
  ST_STClosed_Analysis_Con$Type<-"XSTClosed"
  ST_STAside_Analysis_Con$Type<-"STAside"
  ST_Touch_Analysis_Con$Type<-"Touch"
  ST_TouchST_Analysis_Con$Type<-"TouchST"
  ST_WStation_Analysis_Con$Type<-"WStation"
  ST_WBucket_Analysis_Con$Type<-"WBucket"
  ST_Sanitizer_Analysis_Con$Type<-"Sanitizer"
  
  ST_Comb_Analysis_Con<-bind_rows(ST_OFF_Analysis_Con,ST_ON_Analysis_Con,ST_ONWash_Analysis_Con,
                                  ST_ONWr_Analysis_Con,ST_OFFWash_Analysis_Con,ST_OFFWr_Analysis_Con,
                                  ST_Exc_Analysis_Con,ST_STClosed_Analysis_Con,ST_STAside_Analysis_Con,ST_Touch_Analysis_Con,
                                  ST_TouchST_Analysis_Con,ST_WStation_Analysis_Con,ST_WBucket_Analysis_Con,ST_Sanitizer_Analysis_Con )
  
  return(ST_Comb_Analysis_Con)
}



Func_DF_Locations<-function(){
  ST_OFF_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFF)
  ST_ON_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopON)
  ST_ONWash_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopONWash)
  ST_ONWr_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopONWr)
  ST_OFFWash_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFFWash)
  ST_OFFWr_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFFWr)
  ST_Exc_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopExc)
  ST_STClosed_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_STClosed)
  ST_STAside_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopSTAside)
  ST_Touch_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopTouch)
  ST_TouchST_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopTouchST)
  ST_WStation_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopWStation)
  ST_WBucket_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopWBucket)
  ST_Sanitizer_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopSanitizer)
  
  
  T1<-nrow(ST_ON_Analysis_Con)
  T2<-nrow(ST_OFF_Analysis_Con)
  T3<-nrow(ST_ONWash_Analysis_Con)
  T4<-nrow(ST_ONWr_Analysis_Con)
  T5<-nrow(ST_OFFWash_Analysis_Con)
  T6<-nrow(ST_OFFWr_Analysis_Con)
  T7<-nrow(ST_Exc_Analysis_Con)
  T8<-nrow(ST_STClosed_Analysis_Con)
  T9<-nrow(ST_STClosed_Analysis_Con)
  T10<-nrow(ST_Touch_Analysis_Con)
  T11<-nrow(ST_TouchST_Analysis_Con)
  T12<-nrow(ST_WStation_Analysis_Con)
  T13<-nrow(ST_WBucket_Analysis_Con)
  T14<-nrow(ST_Sanitizer_Analysis_Con)
  
  T1C<-sum(ST_ON_Analysis_Con$Location=="Consumed")
  T2C<-sum(ST_OFF_Analysis_Con$Location=="Consumed")
  T3C<-sum(ST_ONWash_Analysis_Con$Location=="Consumed")
  T4C<-sum(ST_ONWr_Analysis_Con$Location=="Consumed")
  T5C<-sum(ST_OFFWash_Analysis_Con$Location=="Consumed")
  T6C<-sum(ST_OFFWr_Analysis_Con$Location=="Consumed")
  T7C<-sum(ST_Exc_Analysis_Con$Location=="Consumed")
  T8C<-sum(ST_STClosed_Analysis_Con$Location=="Consumed")
  T9C<-sum(ST_STAside_Analysis_Con$Location=="Consumed")
  T10C<-sum(ST_Touch_Analysis_Con$Location=="Consumed")
  T11C<-sum(ST_TouchST_Analysis_Con$Location=="Consumed")
  T12C<-sum(ST_WStation_Analysis_Con$Location=="Consumed")
  T13C<-sum(ST_WBucket_Analysis_Con$Location=="Consumed")
  T14C<-sum(ST_Sanitizer_Analysis_Con$Location=="Consumed")
  
  T1D<-sum(ST_ON_Analysis_Con$Location=="Discarded")
  T2D<-sum(ST_OFF_Analysis_Con$Location=="Discarded")
  T3D<-sum(ST_ONWash_Analysis_Con$Location=="Discarded")
  T4D<-sum(ST_ONWr_Analysis_Con$Location=="Discarded")
  T5D<-sum(ST_OFFWash_Analysis_Con$Location=="Discarded")
  T6D<-sum(ST_OFFWr_Analysis_Con$Location=="Discarded")
  T7D<-sum(ST_Exc_Analysis_Con$Location=="Discarded")
  T8D<-sum(ST_STClosed_Analysis_Con$Location=="Discarded")
  T9D<-sum(ST_STAside_Analysis_Con$Location=="Discarded")
  T10D<-sum(ST_Touch_Analysis_Con$Location=="Discarded")
  T11D<-sum(ST_TouchST_Analysis_Con$Location=="Discarded")
  T12D<-sum(ST_WStation_Analysis_Con$Location=="Discarded")
  T13D<-sum(ST_WBucket_Analysis_Con$Location=="Discarded")
  T14D<-sum(ST_Sanitizer_Analysis_Con$Location=="Discarded")
  
  
 Vector_Total_Items<-c(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)
 Vector_Total_Item_Con<-c(T1C,T2C,T3C,T4C,T5C,T6C,T7C,T8C,T9C,T10C,T11C,T12C,T13C,T14C)
 Vector_Total_Item_Dis<-c(T1D,T2D,T3D,T4D,T5D,T6D,T7D, T8D,T9D, T10D,T11D,T12D,T13D, T14D)
  
  outputsLocations<-list(Vector_Total_Items=Vector_Total_Items,
                         Vector_Total_Item_Con=Vector_Total_Item_Con,
                         Vector_Total_Item_Dis=Vector_Total_Item_Dis)
  
  return(outputsLocations)
}


Func_DF_Histogram_Log<-function(AnalysisDF,Intervention){
  #AnalysisDF = AnalysisDF
  #Intervention= "OnOFF
  ST_Analysis_Log<-AnalysisDF
  ST_Analysis_Log$Contamination[ST_Analysis_Log$Contamination==0]<-(.99)
  ST_Analysis_Log$Log<-log10(ST_Analysis_Log$Contamination)
  ST_Analysis_Log$Category<-""
  ST_Analysis_Log$OnOff<-Intervention
  
  
  for (i in 1:nrow(ST_Analysis_Log)){
    Conta<-as.numeric( ST_Analysis_Log$Contamination[i])
    if(Conta == .99 ){
      ST_Analysis_Log$Category[i] <-"0"
    } else if(Conta >0 && Conta< 100 ){
      ST_Analysis_Log$Category[i]  <-"<0 - 99"
    }else if(Conta >= 100 && Conta < 1000 ){
      ST_Analysis_Log$Category[i]  <-"100 - 999"
    }else if(Conta >=1000 ){
      ST_Analysis_Log$Category[i]  <-">=1000"
    }
  }
  return(ST_Analysis_Log)
}


Func_DF_Barplot_Log<-function(ST_Analysis_Log,Intervention){
  PlotOrder<-c("0","<0 - 99","100 - 999",">=1000")
  
  ST_Analysis_Log %>% 
    count(Category) %>% 
    mutate(perc = n / nrow(ST_Analysis_Log)) %>%
    slice(match(Category,PlotOrder))-> tips2
  
  tips2$OnOff<-Intervention
  
  return(tips2)
}



Func_NSA_Summary<-function(Trial){
  #Outlocation = "MaxOut
  #Trial = "T1"
  #Creating Data Frame of Consumed Items for all the products
  #Copy of Compiled Data Frame
  
  #Data Frame without repeats and Consumed Items
  Treatment1<-Func_Asys_MainDFDp(SummaryList = List_Sens_Fr,FoodType = "Fruit")
  
  #Treatment 1
  
  Treatment1_Copy<-Treatment1
  #ST_ON_Analysis<-ST_ON_Analysis_Copy
  Treatment1<-Func_Asys_ContbyStudent(Treatment1)
  #Drop NAs
  Treatment1<- Treatment1[!is.na(Treatment1$Contamination), ]
  
  #Analysis For weekly Dose Response ON
  
  OutputsDRT1<-Func_DR_Main(AnalysisDF = Treatment1, Reps_DR = 100)
  df_inf_Week_T1<-OutputsDRT1$df_inf_Week
  df_ill_Week_T1<-OutputsDRT1$df_ill_Week
  
  df_ill_Week_T1_Prev<-Func_DF_Prevalence(AnalysisDFCop = Treatment1_Copy, df_Ill_Week = df_ill_Week_T1,Intervention = Trial)
  
  meanT1<-mean(df_ill_Week_T1_Prev$Prev)
  
  
  return(meanT1)
}





Func_DF_Locations_2<-function(){
  ST_OFF_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFF)
  ST_ON_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopON)
  ST_STClosed_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_STClosed)
  ST_Exc_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopExc)
  ST_STAside_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopSTAside)

  
  Total_Items_ON<-nrow(ST_ON_Analysis_Con)
  Total_Items_OFF<-nrow(ST_OFF_Analysis_Con)
  
  ST_OFF_Analysis_Con$Type<-"OFF"
  ST_ON_Analysis_Con$Type<-"ON"
  ST_STClosed_Analysis_Con$Type<-"XClosed"
  ST_Exc_Analysis_Con$Type<-"XExc"
  ST_STAside_Analysis_Con$Type<-"XAside"
  
  ST_Comb_Analysis_Con<-bind_rows(ST_OFF_Analysis_Con,ST_ON_Analysis_Con,ST_STClosed_Analysis_Con,ST_Exc_Analysis_Con,ST_STAside_Analysis_Con)
  return(ST_Comb_Analysis_Con)
}



######
Function_RidExtra<-function(x){
  if (x[3]!=1000){
    x[3]<-str_sub(x[3],1,nchar(x[3])-1)
  }
  return(x[3])
}


Function_RidExtra2<-function(DF){
  WeekCol<-apply(X = DF,1,Function_RidExtra) 
  DF$week<-WeekCol
  return(DF)
}



