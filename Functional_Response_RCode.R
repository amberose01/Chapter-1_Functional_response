#Functional response analysis for Rhyzobius lophanthae and Chrysoperla rufilabris


#### SDB (Rhyzobius lophanthae) Functional Response Curves ####

library(readr)
library('frair')

sdb <- read_csv("sdb.csv")


#subset data frames to specific CMBS life stages

sdbc<-sdb[sdb$cmbslifestage=='crawlers',]
sdbo<-sdb[sdb$cmbslifestage=='ovisacs',]
sdbe<-sdb[sdb$cmbslifestage=='eggs',]


###SDB Ovisacs###

#plot data
with(sdbo,
     plot(density, eaten, xlab=NA, ylab=NA, 
          cex.lab=1.1, pch=21, xlim=c(0,15), ylim=c(0,6), cex.axis=0.8 ,cex=1.0, bg='hot pink', col='black',
          text(x = 1.5, y = 5.7, "D) Ovisacs"), mgp=c(1.7,0.5,0)))

#add title
title('Scale destroyer beetles and CMBS ovisacs', cex.main=1.3)

#determine type of response
frair_test(eaten~density, sdbo)

#Check model by comparing AIC between type II and III models

b_flex <- frair_fit(eaten~density, data=sdbo, response='flexpnr',
                    start=list(b=0.0224056, q=0,h=10.85669), fixed=list(T=24))
b_fixed <- frair_fit(eaten~density, data=sdbo, response='flexpnr',
                     start=list(b=0.0224056,h=10.85669), fixed=list(T=24, q=0))
summary(b_flex$fit)
AIC(b_flex$fit, b_fixed$fit) #type II preferred as b_fixed has lower AIC value


#add best fit line
sdbo1<-frair_fit(eaten~density, data=sdbo, response='rogersII',
                 start=list(a=0.0208457, h=10.6775057), fixed=list(T=24))
lines(sdbo1, col='black', lwd=2)

# summarize stats
summary(sdbo1$fit)


###SDB Eggs###

with(sdbe,
     plot(density, eaten, xlab=NA, ylab=NA, 
          cex.lab=1.1, pch=21, cex.axis=0.8,cex=1.0, bg='hot pink',col='black',
          text(x = 24, y = 187, "E) Eggs"),mgp=c(1.7,0.5,0)))

#add title
title('Scale destroyer beetles and CMBS eggs', cex.main=1.3)

#determine type of response
frair_test(eaten~density, sdbe)

#Check model by comparing AIC values
b_flex <- frair_fit(eaten~density, data=sdbe, response='flexpnr',
                    start=list(b=0.1604320, q=0,h=0.1987681), fixed=list(T=24))
b_fixed <- frair_fit(eaten~density, data=sdbe, response='flexpnr',
                     start=list(b=0.1604320,h=0.1987681), fixed=list(T=24, q=0))
summary(b_flex$fit)
AIC(b_flex$fit, b_fixed$fit) #type III preferred as b_flex has lower AIC value

  #Plotting a type III response
with(sdbe,
     plot(density, eaten, xlab="Initial prey density", ylab="Number of prey attacked", 
          cex.lab=1.1, pch=21,cex=1.0, mgp = c(2, 0.5, 0), bg='hot pink',col='black'))

sdbe2<-frair_fit(eaten~density, data=sdbe, response='hassIIInr',
                 start=list(b=0.00686923, c=0.00960856, h=.24725677), fixed=list(T=24))
lines(sdbe2, lwd=2) #TYPE II OBSERVED

#Create Type II response
sdbe1<-frair_fit(eaten~density, data=sdbe, response='rogersII',
                 start=list(a=0.1604320, h=.1987681), fixed=list(T=24))
lines(sdbe1, lwd=2)

#summarize stats
summary(sdbe1$fit)


## SDB crawlers ##

#plot sdb crawler data

with(sdbc,
     plot(density, eaten, xlab='Initial prey density', ylab=NA, 
          cex.lab=1.1, pch=21, xlim=c(0,55), ylim=c(0,40), cex.axis=0.8, cex=1.0, bg='hot pink', col='black',
          text(x = 6.0, y = 37, "F) Crawlers"), mgp=c(1.7,0.5,0)))

#add title
title('Scale destroyer beetles and CMBS crawlers', cex.main=1.3)

#determine type of response
frair_test(eaten~density, sdbc)

#Check model by comparing AIC of type II and III models

b_flex <- frair_fit(eaten~density, data=sdbc, response='flexpnr',
                    start=list(b=0.0871856, q=0,h=0.4591673), fixed=list(T=24))
b_fixed <- frair_fit(eaten~density, data=sdbc, response='flexpnr',
                     start=list(b=0.0871856,h=0.4591673), fixed=list(T=24, q=0))
summary(b_flex$fit)
AIC(b_flex$fit, b_fixed$fit) #type II preferred as b_fixed has lower AIC value


#create best fit line
sdbc1<-frair_fit(eaten~density, data=sdbc, response='rogersII',
                 start=list(a=0.088893, h=0.469820), fixed=list(T=24))
lines(sdbc1, col='black', lwd=2)

#statistical summary
summary(sdbc1$fit)


####LW Functional Response Curves#### 

library(readr)
library(frair)
lw <- read_csv("lw.csv")

#subset data
lwe<-lw[lw$cmbslifestage=='eggs',]
lwc<-lw[lw$cmbslifestage=='crawlers',]
lwo<-lw[lw$cmbslifestage=='ovisacs',]

###LW Ovisacs###

#create plot
with(lwo,
     plot(density, eaten, xlab=NA, ylab='Number of prey attacked', 
          xlim=c(0,15), ylim=c(0,3), cex.lab=1.1, pch=21, cex.axis=0.8,cex=1.0, col='black', bg='hot pink',
          text(x = 1.7, y = 2.8, "A) Ovisacs"), mgp=c(1.7,0.5,0),
          yaxt = "n"))
axis(side = 2, at = c(0, 1, 2, 3),cex.axis=0.8)

#add title
title('Lacewing Ovisacs')

#determine response type
frair_test(eaten~density, lwo)

b_flex <- frair_fit(eaten~density, data=lwo, response='flexpnr',
                    start=list(b=0.21255, q=0,h=75.24959), fixed=list(T=24))
b_fixed <- frair_fit(eaten~density, data=lwo, response='flexpnr',
                     start=list(b=0.21255,h=75.24959), fixed=list(T=24, q=0))

summary(b_flex$fit) #error message to fit flex, couldn't invert Hessian
AIC(b_flex$fit, b_fixed$fit) 

#create best fit line
lwo1<-frair_fit(eaten~density, data=lwo, response='rogersII',
                start=list(a=0.21255, h=75.24959), fixed=list(T=24))
lines(lwo1, col='black', lwd=2, lty=2)

#summarize statistics
summary(lwo1$fit)


###LW Eggs###

#create plot
with(lwe,
     plot(density, eaten, xlab=NA, ylab='Number of prey attacked',
          cex.lab=1.1, pch=21, cex.axis=0.8,cex=1.0, bg='hot pink', col='black',
          text(x = 19, y = 110, "B) Eggs"), mgp=c(1.7,0.5,0)))

#add title
title('Lacewing and CMBS Eggs', cex.main=2)

#determine type of response
frair_test(eaten~density, lwe)

b_flex <- frair_fit(eaten~density, data=lwe, response='flexpnr',
                    start=list(b=0.0735951, q=0,h=.2923555), fixed=list(T=24))
b_fixed <- frair_fit(eaten~density, data=lwe, response='flexpnr',
                     start=list(b=0.0735951,h=.2923555), fixed=list(T=24, q=0))

summary(b_flex$fit) #error message to fit flex, couldn't invert Hessian
AIC(b_flex$fit, b_fixed$fit) #type III preferred
  
  #Plotting a Type III no replacement

with(lwe,
     plot(density, eaten, xlab="Initial prey density", ylab="Number of prey attacked", 
          cex.lab=1.1, pch=21,cex=1.0, mgp = c(2, 0.5, 0), bg='hot pink',col='black'))
lwe2<-frair_fit(eaten~density, data=lwe, response='hassIIInr',
                start=list(b=0.00686923, c=0.00960856, h=.24725677), fixed=list(T=24))
lines(lwe2, lwd=2) #TYPE II OBSERVED

#Plotting a Type II
lwe1<-frair_fit(eaten~density, data=lwe, response='rogersII',
                start=list(a=0.0735951, h=.2923555), fixed=list(T=24))
lines(lwe1, lwd=2)

#summarize stats
summary(lwe1$fit)



###LW Crawlers###

#create plot
with(lwc,
     plot(density, eaten, xlab='Initial prey density', ylab='Number of prey attacked',
          xlim=c(0,40),ylim=c(0,40),cex.lab=1.1, pch=21, cex.axis=0.8,cex=1.0, col='black', bg='hot pink',
          text(x = 4.5, y = 37, "C) Crawlers"), mgp=c(1.7,0.5,0)))

#add title
title('Lacewing and CMBS Crawlers', cex.main=2)

#determine response type
frair_test(eaten~density, lwc)

b_flex <- frair_fit(eaten~density, data=lwc, response='flexpnr',
                    start=list(b=0.0735951, q=0,h=.2923555), fixed=list(T=24))
b_fixed <- frair_fit(eaten~density, data=lwc, response='flexpnr',
                     start=list(b=0.0735951,h=.2923555), fixed=list(T=24, q=0)) #Error in fitting either response = type I

#perform linear regression for Type I
lwc_lm<-lm(eaten~density, data=lwc)
lm(eaten~density, lwc)
summary(lwc_lm)

#create best fit line
lwc1<-frair_fit(eaten~density, data=lwc, response='typeI',
                start=list(a=0.03641924), fixed=list(T=24))
lines(lwc1, col='black', lwd=2)

# summarize data
summary(lwc1$fit)








