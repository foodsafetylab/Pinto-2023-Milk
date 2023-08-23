
DF_Milk_Dec<- data.frame("Time" = c(0,1440/60,2880/60,4320/60,5760/60),
                         "LogCFU" = c(2.44,3.25,4.31,5.39,6.19) )

#Model refrigerated control Gabby Data
model1<-lm(formula = LogCFU~Time, data = DF_Milk_Dec)

#Model experimental refrigerated control
#model2<-lm(formula = Ref_Control~min, data = Combined_DF)

summary(model1)
summary(model2)

Changes_Over_Time_NAF
(1:length(Time_Temp_Profile$tempM))/60

Data_Milk_NAF = data.frame("Time" = (1:length(Time_Temp_Profile_RC$tempM))/60,
                       "LogCFU" = Changes_Over_Time_RC_NAF )

model_NAF<-lm(formula = LogCFU~Time, data = Data_Milk_NAF )
summary(model_NAF)
  
#Slope is 0.40 log CFU/hr
0.040167/2.989e-02 #Slope Data/Slope with AF 1


##sUMMER AND WINTER COMBINED
All_Seasons_RC_Data_Comb<-data.frame("Time" = c(0,0,0,
                                               1440,1440,1440,
                                               2880,2880,2880,
                                               4320,4320,4320,
                                               5760,5760,5760), 
                                     "LogCFU" = c(2.44,2.31,2.31,
                                                  2.47,2.7,3.25,
                                                  3.31,3.64,4.31,
                                                  4.47,4.48,5.39,
                                                  5.31,5.31,6.19))

All_Seasons_RC_Data_Comb$Time<-All_Seasons_RC_Data_Comb$Time/60

model_Seasons<-lm(formula = LogCFU~Time, data = All_Seasons_RC_Data_Comb)
summary(model_Seasons)


0.035306/0.030408 #Slope Data/Slope with AF 1
