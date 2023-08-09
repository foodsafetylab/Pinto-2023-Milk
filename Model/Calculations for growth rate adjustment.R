0.08350774/2.303

exp(0.08350774)
log10(1.087094)


DF_Milk_Dec<- data.frame("Time" = c(0,1440/60,2880/60,4320/60,5760/60),
                         "LogCFU" = c(2.44, 3.25,4.31,5.39,6.19) )

#Model refrigerated control Gabby Data
model1<-lm(formula = LogCFU~Time, data = DF_Milk_Dec)

#Model experimental refrigerated control
model2<-lm(formula = Ref_Control~min, data = Combined_DF)

summary(model1)
summary(model2)
  
#Slope is 0.40 log CFU/hr
0.040167/0.030408
