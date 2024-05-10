setwd("C:/Users/qouthman/OneDrive - University of Arkansas System Division of Agriculture/Luke_Gatiboni")

#### Import data ####
dat <- read.csv("Soy_K_data.csv")

#### Structure of the data ####
str(dat)
names(dat)
unique(dat$Site_Year2)

#### Mehlich_I and Mehlich_III data cleaning ####
## Remove from each dataset the site-years where we have only two treatments ##

tapply(dat$Site_Year,list(dat$Source,dat$Site_Year),length)
min(tapply(dat$Site_Year,list(dat$Source,dat$Site_Year),length), na.rm = T)

## Site year 40, 41, 45, and 52 are the only two treatments site-years ##

rem <- c(40, 41, 45, 52)

dat1 <- dat[!dat$Site_Year %in% rem,]

## Confirm that the number of treatments plus control is 3 or more ##
min(tapply(dat1$Site_Year,list(dat1$Source,dat1$Site_Year),length), na.rm = T)

## Calculate relative yield ##
## %RY = Mean_Yield_Control/Mean_Yield_Max * 100

Mean_Yield_Max0 <- data.frame(tapply(dat1$Yield, dat1$Site_Year2,max,na.rm=T))
Mean_Yield_Max<-Mean_Yield_Max0$tapply.dat1.Yield..dat1.Site_Year2..max.
Mean_Yield_Control <- data.frame(dat1[dat1$Treatment_Number==1,])$Yield
Percent_RY_Control<-Mean_Yield_Control/Mean_Yield_Max*100

library(soiltestcorr)
ry95_Krate <- numeric()
pvalue_Krate <- numeric()
Krate_max_yield <- numeric()
Krate_max_yield1 <- numeric()
Soil_Test_Control <- numeric()

## Turn off (-1) or on (0) warnings 
options(warn = -1)

#### Krate vs. RY on cleaned dataset ####
## Remove result/output for rem <- c(40, 41, 45, 52) ## 

for (i in unique(dat1$Site_Year2)) {
  y0 <- dat1[dat1$Site_Year2 == i,]$Yield
  y <- y0[!is.na(y0)]
  x0 <- dat1[dat1$Site_Year2 == i,]$Krate
  x <-x0[!is.na(x0)]
  z <- x[which.max(y)]
  ry <- y / max(y, na.rm = T)*100
  Krate_max_yield[i] <- z
  Krate_max_yield1 <- c(Krate_max_yield1,rep(max(y0,na.rm = T),length(x0)))
  control <- dat1[dat1$Site_Year2 == i & dat1$Treatment_Number==1,]$Soil_Test_K
  Soil_Test_Control <- c(Soil_Test_Control,rep(control,length(x0)))
  
  # Use tryCatch to handle errors in quadratic_plateau function
  mod <- tryCatch({
    quadratic_plateau(stv = x, ry = ry, target = 95)
  }, error = function(e) {
    return(NA)
  })
  
  # Check if 'mod' is NA, and handle accordingly
  if (is.na(mod[1])) {
    ry95_Krate[i] = NA
    pvalue_Krate[i] = NA
  } else {
    ry95_Krate[i] = mod$STVt
    pvalue_Krate[i] = mod$pvalue
  }
}

ry95_Krate_clean <- ry95_Krate[-rem]
pvalue_Krate_clean <- pvalue_Krate[-rem]
Krate_max_yield_clean <- Krate_max_yield[-rem]
dat2 <- cbind.data.frame(dat1,Yield_Max= Krate_max_yield1, Soil_Test_Control)

STV2 <- numeric()

for (i in 1:791){
  if(dat2$Soil_Test_K[i] == dat2$Soil_Test_Control[i] | is.na(dat2$Soil_Test_K[i])){
    STV2[i] = dat2$Soil_Test_Control[i]
  } else { STV2[i] = dat2$Soil_Test_K[i]}
}

dat2$STV2 <- STV2
write.csv(dat2,"dat2.csv")
#### STV vs. RY ####
## Remove result/output for rem <- c(40, 41, 45, 52) ## 

ry95_stv <- numeric()
pvalue_stv <- numeric()
CSTV_stv <- numeric()
stv_yield_max <- numeric()
for (i in unique(dat1$Site_Year2)) {
  y0 <- dat1[dat1$Site_Year2 == i,]$Yield
  y <-y0[!is.na(y0)]
  x0 <- dat1[dat1$Site_Year2 == i,]$Soil_Test_K
  x<-x0[!is.na(x0)]
  z <- x[which.max(y)]
  stv_yield_max[i] <- z
  ry <- y / max(y, na.rm = T)*100
  
  # Use tryCatch to handle errors in quadratic_plateau function
  mod <- tryCatch({
    quadratic_plateau(stv = x, ry = ry, target = 95)
  }, error = function(e) {
    return(NA)
  })
  
  # Check if 'mod' is NA, and handle accordingly
  if (is.na(mod[1])) {
    ry95_stv[i] = NA
    pvalue_stv[i] = NA
    CSTV_stv[i] = NA
  } else {
    ry95_stv[i] = mod$STVt
    pvalue_stv[i] = mod$pvalue
    CSTV_stv[i] = mod$CSTV
  }
}

ry95_stv_clean <-ry95_stv[-rem]
pvalue_stv_clean <- pvalue_stv[-rem]
CSTV_stv_clean <- CSTV_stv[-rem]
stv_yield_max_clean <-stv_yield_max[-rem]
#### STV vs. RY ####
## Soil Method ##

dat1_stv_ry <- data.frame(dat1[dat1$Treatment_Number==1,],Mean_Yield_Control,
                         Mean_Yield_Max, Percent_RY_Control, ry95_stv_clean, 
                         CSTV_stv_clean, ry95_Krate_clean,stv_yield_max_clean,
                         Krate_max_yield_clean)

write.csv(dat1_stv_ry,"data_output.csv")

ry95_cstv_soil_method <- numeric()
pvalue_cstv_soil_method <- numeric()

for (i in unique(dat1_stv_ry$Soil_Method)) {
  y0 <- dat1_stv_ry[dat1_stv_ry$Soil_Method == i,]$Percent_RY_Control
  y<-y0[!is.na(y0)]
  x0 <- dat1_stv_ry[dat1_stv_ry$Soil_Method == i,]$Soil_Test_K
  x<-x0[!is.na(x0)]
 
  
  # Use tryCatch to handle errors in quadratic_plateau function #
  mod <- tryCatch({
    quadratic_plateau(stv = x, ry = y, target = 95)
  }, error = function(e) {
    return(NA)
  })
  
  # Check if 'mod' is NA, and handle accordingly
  if (is.na(mod[1])) {
    ry95_cstv_soil_method[i] = NA
    pvalue_cstv_soil_method[i] = NA
  } else {
    ry95_cstv_soil_method[i] = mod$CSTV
    pvalue_cstv_soil_method[i] = mod$pvalue
  }
}

ry95_cstv_soil_method
pvalue_cstv_soil_method

## Soil Method CSTV vs RY ##
y <- dat1_stv_ry[dat1_stv_ry$Soil_Method == "Mehlich_I",]$Percent_RY_Control
y0 <- y[!is.na(y)]
x <- dat1_stv_ry[dat1_stv_ry$Soil_Method == "Mehlich_I",]$Soil_Test_K
x0<-x[!is.na(x)]
tryCatch({
  quadratic_plateau(stv = x0, ry = y0, plot=T)
}, error = function(e) {
  return(NA)
})

#### Frequency Distribution Graph ####

## Call out each method ##

#### Mehlich 3 ####
dat1_stv_ry_M3 <- dat1_stv_ry[dat1_stv_ry$Soil_Method == 'Mehlich_III',]

## b) Soil test value in the control ##
max(dat1_stv_ry_M3$Soil_Test_K,na.rm = T)
hist(dat1_stv_ry_M3$Soil_Test_K,breaks = 10,labels = T,
     main="M3-Soil Test Value of K (ppm)", 
     ylab = "Frequency of Observations",
     xlab = "Soil Test K (ppm)")

## c) relative yield of the control treatment ##
max(dat1_stv_ry_M3$Percent_RY_Control, na.rm = T)
hist(dat1_stv_ry_M3$Percent_RY_Control,breaks = 10,labels = T,
     main="M3-Relative yield of the Control Treatment", 
     ylab = "Frequency of Observations",
     xlab = "Percent Relative Yield")

## d) rate of fertilizer where the maximum yield ##
max(dat1_stv_ry_M3$Krate_max_yield_clean, na.rm = T)
hist(dat1_stv_ry_M3$Krate_max_yield_clean,breaks = 10,labels = T,
     main="M3-Fertilizer Rate of Maximum Yield", 
     ylab = "Frequency of Observations",
     xlab = "K Rate kg/ha")

## e) soil test value (if available) at the treatment with 100% RY ##
max(dat1_stv_ry_M3$stv_yield_max_clean, na.rm = T)
hist(dat1_stv_ry_M3$stv_yield_max_clean,breaks = 10,labels = T,
     main="M3-Soil Test value (if available) at the Treatment with 100% RY", 
     ylab = "Frequency of Observations",
     xlab = "Soil Test K (ppm)")

## f) calculated rate of fertilizer to obtain 95% of the maximum yield ##
max(dat1_stv_ry_M3$ry95_Krate_clean, na.rm = T)
hist(dat1_stv_ry_M3$ry95_Krate_clean,breaks = 10,labels = T,
     main="M3-Calculated fertilizer rate to obtain 95% of the maximum yield (QP-Model)", 
     ylab = "Frequency of Observations",
     xlab = "K Rate kg/ha")

## g) STV @95% RY for each fertilizer rates trial ##
##(soil test (if stv is provided for all rates) vs yield) ##
max(dat1_stv_ry_M3$ry95_stv_clean, na.rm = T)
hist(dat1_stv_ry_M3$ry95_stv_clean,breaks = 10,labels = T,
     main="M3-STV @95% RY for each fertilizer rates trial", 
     ylab = "Frequency of Observations",
     xlab = "Soil Test K (ppm)")

#### Mehlich 1 ####
dat1_stv_ry_M1 <- dat1_stv_ry[dat1_stv_ry$Soil_Method == 'Mehlich_I',]

## b) Soil test value in the control ##
max(dat1_stv_ry_M1$Soil_Test_K,na.rm = T)
hist(dat1_stv_ry_M1$Soil_Test_K,breaks = 10,labels = T,
     main="M1-Soil Test Value of K (ppm)", 
     ylab = "Frequency of Observations",
     xlab = "Soil Test K (ppm)")

## c) relative yield of the control treatment ##
max(dat1_stv_ry_M1$Percent_RY_Control, na.rm = T)
hist(dat1_stv_ry_M1$Percent_RY_Control,breaks = 10,labels = T,
     main="M1-Relative yield of the Control Treatment", 
     ylab = "Frequency of Observations",
     xlab = "Percent Relative Yield")

## d) rate of fertilizer where the maximum yield ##
max(dat1_stv_ry_M1$Krate_max_yield_clean, na.rm = T)
hist(dat1_stv_ry_M1$Krate_max_yield_clean,breaks = 10,labels = T,
     main="M1-Fertilizer Rate of Maximum Yield", 
     ylab = "Frequency of Observations",
     xlab = "K Rate kg/ha")

## e) soil test value (if available) at the treatment with 100% RY ##
max(dat1_stv_ry_M1$stv_yield_max_clean, na.rm = T)
hist(dat1_stv_ry_M1$stv_yield_max_clean,breaks = 10,labels = T,
     main="M1-Soil Test value (if available) at the Treatment with 100% RY", 
     ylab = "Frequency of Observations",
     xlab = "Soil Test K (ppm)")

## f) calculated rate of fertilizer to obtain 95% of the maximum yield ##
max(dat1_stv_ry_M1$ry95_Krate_clean, na.rm = T)
hist(dat1_stv_ry_M1$ry95_Krate_clean,breaks = 10,labels = T,
     main="M1-Calculated fertilizer rate to obtain 95% of the maximum yield (QP-Model)", 
     ylab = "Frequency of Observations",
     xlab = "K Rate kg/ha")

## g) STV @95% RY for each fertilizer rates trial ##
##(soil test (if stv is provided for all rates) vs yield) ##
max(dat1_stv_ry_M1$ry95_stv_clean, na.rm = T)
hist(dat1_stv_ry_M1$ry95_stv_clean,breaks = 10,labels = T,
     main="M1-STV @95% RY for each fertilizer rates trial", 
     ylab = "Frequency of Observations",
     xlab = "Soil Test K (ppm)")


#### Olsen P ####
dat1_stv_ry_OlsenP <- dat1_stv_ry[dat1_stv_ry$Soil_Method == 'Olsen_P',]

## b) Soil test value in the control ##
max(dat1_stv_ry_OlsenP$Soil_Test_K,na.rm = T)
hist(dat1_stv_ry_OlsenP$Soil_Test_K,breaks = 10,labels = T,
     main="OlsenP-Soil Test Value of P (ppm)", 
     ylab = "Frequency of Observations",
     xlab = "Soil Test P (ppm)")

## c) relative yield of the control treatment ##
max(dat1_stv_ry_OlsenP$Percent_RY_Control, na.rm = T)
hist(dat1_stv_ry_OlsenP$Percent_RY_Control,breaks = 10,labels = T,
     main="OlsenP-Relative yield of the Control Treatment", 
     ylab = "Frequency of Observations",
     xlab = "Percent Relative Yield")

## d) rate of fertilizer where the maximum yield ##
max(dat1_stv_ry_OlsenP$Krate_max_yield_clean, na.rm = T)
hist(dat1_stv_ry_OlsenP$Krate_max_yield_clean,breaks = 10,labels = T,
     main="OlsenP-Fertilizer Rate of Maximum Yield", 
     ylab = "Frequency of Observations",
     xlab = "P Rate kg/ha")

## e) soil test value (if available) at the treatment with 100% RY ##
max(dat1_stv_ry_OlsenP$stv_yield_max_clean, na.rm = T)
hist(dat1_stv_ry_OlsenP$stv_yield_max_clean,breaks = 10,labels = T,
     main="OlsenP-Soil Test value (if available) at the Treatment with 100% RY", 
     ylab = "Frequency of Observations",
     xlab = "Soil Test P (ppm)")

## f) calculated rate of fertilizer to obtain 95% of the maximum yield ##
max(dat1_stv_ry_OlsenP$ry95_Krate_clean, na.rm = T)
hist(dat1_stv_ry_OlsenP$ry95_Krate_clean,breaks = 10,labels = T,
     main="OlsenP-Calculated fertilizer rate to obtain 95% of the maximum yield (QP-Model)", 
     ylab = "Frequency of Observations",
     xlab = "P Rate kg/ha")

## g) STV @95% RY for each fertilizer rates trial ##
##(soil test (if stv is provided for all rates) vs yield) ##
max(dat1_stv_ry_OlsenP$ry95_stv_clean, na.rm = T)
hist(dat1_stv_ry_OlsenP$ry95_stv_clean,breaks = 10,labels = T,
     main="OlsenP-STV @95% RY for each fertilizer rates trial", 
     ylab = "Frequency of Observations",
     xlab = "Soil Test K (ppm)")


#### QP Plots by Soil test class/categories and soil methods ####

#ry95_cstv_soil_method <- numeric()
#pvalue_cstv_soil_method <- numeric()
#plots<- list()
## Loop not working yet ##
library(ggplot2)
## Mehlich III ##
M3plot <- list()

for (i in 1:6) {
  
  dt <- subset(dat1_stv_ry, Soil_Method == "Mehlich_III")
  
  LL <- c(0,30.09,69.325,108.265,162.25,236)
  UL <- c(49.855,88.795,132.75,191.75,250.75,353)
  cat <-c("Very Low", "Low", "Medium", "High 1", "High 2", "Very High")
  
  ## Subset
  dts <-subset(dt, Soil_Test_K >= LL[i] & Soil_Test_K<=UL[i])
  
  y <- dts$Percent_RY_Control
  y0 <- y[!is.na(y)]
  x <- dts$Soil_Test_K
  x0<-x[!is.na(x)]
  
  # Use tryCatch to handle errors in quadratic_plateau function #
  mod <- tryCatch({
    quadratic_plateau(stv = x0, ry = y0)
  }, error = function(e) {
    return(NA)
  })
  
  # Check if 'mod' is NA, and handle accordingly
  if (is.na(mod[1])) {
    M3plot[[i]] <- NULL
  } else {
    
    ## CSTV
    target<-mod$plateau*0.95
    mod1 <- quadratic_plateau(stv = x0, ry = y0,target=target,plot = T)
    M3plot[[i]] <- mod1 + labs(subtitle = paste0("Avail. Class:", cat[i]),
                               x="Mehlich III Soil Test K (ppm)", 
                               y="Soybean Relative Yield (%)")
    
  }

}

## Mehlich I ##
M1plot <- list()
for (i in 1:6) {
  
  dt <- subset(dat1_stv_ry, Soil_Method == "Mehlich_I")
  
  LL <- c(0,11.73,27.025,42.205,63.25,92)
  UL <- c(19.435,34.615,51.75,74.75,97.75,196)
  cat <-c("Very Low", "Low", "Medium", "High 1", "High 2", "Very High")
  
  ## Subset
  dts <-subset(dt, Soil_Test_K >= LL[i] & Soil_Test_K<=UL[i])
   
  y <- dts$Percent_RY_Control
  y0 <- y[!is.na(y)]
  x <- dts$Soil_Test_K
  x0<-x[!is.na(x)]
  
  # Use tryCatch to handle errors in quadratic_plateau function #
  mod <- tryCatch({
    quadratic_plateau(stv = x0, ry = y0)
  }, error = function(e) {
    return(NA)
  })
  
  # Check if 'mod' is NA, and handle accordingly
  if (is.na(mod[1])) {
    M1plot[[i]] <- NULL
  } else {
    
    ## CSTV
    target<-mod$plateau*0.95
    mod1 <-quadratic_plateau(stv = x0, ry = y0,target=target,plot = T)
    M1plot[[i]] <- mod1 + labs(subtitle = paste0("Avail. Class:", cat[i]),
                               x="Mehlich I Soil Test K (ppm)", 
                               y="Soybean Relative Yield (%)")
    
  }
  
}

## OlsenP ##

OlsenPplot <- list()
for (i in 1:6) {
  
  dt <- subset(dat1_stv_ry, Soil_Method == "Olsen_P")

  LL <- c(0,1.2495,2.87875,4.49575,6.7375,9.8)
  UL <- c(2.07025,3.68725,5.5125,7.9625,10.4125,31)
  cat <-c("Very Low", "Low", "Medium", "High 1", "High 2", "Very High")
  
  ## Subset
  dts <-subset(dt, Soil_Test_K >= LL[i] & Soil_Test_K<=UL[i])
  
  y <- dts$Percent_RY_Control
  y0 <- y[!is.na(y)]
  x <- dts$Soil_Test_K
  x0<-x[!is.na(x)]
  
  # Use tryCatch to handle errors in quadratic_plateau function #
  mod <- tryCatch({
    quadratic_plateau(stv = x0, ry = y0)
  }, error = function(e) {
    return(NA)
  })
  
  # Check if 'mod' is NA, and handle accordingly
  if (is.na(mod[1])) {
    OlsenPplot[[i]] <- NULL
  } else {
    
    ## CSTV
    target<-mod$plateau*0.95
    mod1 <-quadratic_plateau(stv = x0, ry = y0,target=target,plot = T)
    OlsenPplot[[i]] <- mod1 + labs(subtitle = paste0("Avail. Class:", cat[i]),
                                   x="Olsen Soil Test P (ppm)",
                                   y="Corn Relative Yield (%)")
    
  }
  
}

#### Fertilizer Rate Vs Relative Yield ####

## Mehlich_III ##

M3Krate <- list()

for (i in 1:6) {
  dt <- subset(dat2, Soil_Method == "Mehlich_III")
  
  LL <- c(0,30.09,69.325,108.265,162.25,236)
  UL <- c(49.855,88.795,132.75,191.75,250.75,353)
  cat <-c("Very Low", "Low", "Medium", "High 1", "High 2", "Very High")
  
  ## Subset
  dts <-subset(dt, Soil_Test_Control >= LL[i] & Soil_Test_Control<=UL[i])
    
  ry <- dts$Yield / dts$Yield_Max *100
  ry0 <- ry[!is.na(ry)]
  x <- dts$Krate
  x0<-x[!is.na(x)]
  
  # Use tryCatch to handle errors in quadratic_plateau function #
  mod <- tryCatch({
    quadratic_plateau(stv = x0, ry = ry0)
  }, error = function(e) {
    return(NA)
  })
  
  # Check if 'mod' is NA, and handle accordingly
  if (is.na(mod[1])) {
    M3Krate[[i]] <- NULL
  } else {
    
    ## CSTV
    mod1 <- quadratic_plateau(stv = x0, ry = ry0, plot = T)
    M3Krate[[i]] <- mod1 + labs(subtitle = paste0("Mehlich III Avail. Class:", cat[i]),
                               x="Fertilizer K Rate (kg K / ha)", 
                               y="Soybean Relative Yield (%)")
    
  }
  
}

## Mehlich 3 Very High ##
dt2 <- subset(dat2, Soil_Method == "Mehlich_III")

## Subset
dts2 <-subset(dt2, Soil_Test_Control >= 236 & Soil_Test_Control<=353)

ry <- dts2$Yield / dts2$Yield_Max *100
ry0 <- ry[!is.na(ry)]
x <- dts2$Krate
x0<-x[!is.na(x)]

mod <- tryCatch({
  mod <- lm(ry0~x0)
}, error = function(e) {
  return(NA)
})

# Check if 'mod' is NA, and handle accordingly
if (is.na(mod[1])) {
  M3Krate_6 <- NULL
  } else {
  intercept <- round(coef(mod)[1],4)
  slope <- round(coef(mod)[2], 4)
  r2 <- round(summary(mod)$r.squared,4)
  equation <- paste("ry =", intercept, ifelse(slope>= 0, "+","-"), 
                    abs(slope),"Krate", "     ", "r.squared = ", r2)
  y2 <- predict(mod)
  d <- data.frame(cbind(x0,ry0,y2))
  ## CSTV
  M3Krate_6 <- ggplot(d, aes(x = x0, y = ry0))+geom_point()+
    geom_line(aes(x=x0,y2))+
    ylim(0,100)+
    labs(subtitle = paste0("Mehlich III Avail. Class: Very High"),
         x="Fertilizer K Rate (kg K / ha)", 
         y="Soybean Relative Yield (%)")+
    annotate("text", x=Inf,y=Inf, label=equation, hjust=1.1,vjust=50)+
    theme_bw()
}
 print(M3Krate_6)
 
## Mehlich_I ##

M1Krate <- list()

for (i in 1:6) {
  dt <- subset(dat2, Soil_Method == "Mehlich_I")
  
  LL <- c(0,11.73,27.025,42.205,63.25,92)
  UL <- c(19.435,34.615,51.75,74.75,97.75,196)
  cat <-c("Very Low", "Low", "Medium", "High 1", "High 2", "Very High")
  
  ## Subset
  dts <-subset(dt, Soil_Test_Control >= LL[i] & Soil_Test_Control<=UL[i])  
  
  ry <- dts$Yield / dts$Yield_Max *100
  ry0 <- ry[!is.na(ry)]
  x <- dts$Krate
  x0<-x[!is.na(x)]
  
  # Use tryCatch to handle errors in quadratic_plateau function #
  mod <- tryCatch({
    quadratic_plateau(stv = x0, ry = ry0)
  }, error = function(e) {
    return(NA)
  })
  
  # Check if 'mod' is NA, and handle accordingly
  if (is.na(mod[1])) {
    M1Krate[[i]] <- NULL
  } else {
    
  ## CSTV
  mod1 <- quadratic_plateau(stv = x0, ry = ry0, plot = T)
  M1Krate[[i]] <- mod1 + labs(subtitle = paste0("Mehlich I Avail. Class:", cat[i]),
								x="Fertilizer K Rate (kg K / ha)", 
								y="Soybean Relative Yield (%)")
    
  }
  
}

M1Krate2 <- list()

for (i in 1:3) {
  dt <- subset(dat2, Soil_Method == "Mehlich_I")
  
  LL <- c(42.205,63.25,92)
  UL <- c(74.75,97.75,196)
  cat <-c("High 1", "High 2", "Very High")
  
  ## Subset
  dts <-subset(dt, Soil_Test_Control >= LL[i] & Soil_Test_Control<=UL[i])
  
  
  ry <- dts$Yield / dts$Yield_Max *100
  ry0 <- ry[!is.na(ry)]
  x <- dts$Krate
  x0<-x[!is.na(x)]
  
  # Use tryCatch to handle errors in quadratic_plateau function #
  # polynomial is not better#
  mod <- tryCatch({
    mod <- lm(ry0~x0)
  }, error = function(e) {
    return(NA)
  })
  
  # Check if 'mod' is NA, and handle accordingly
  if (is.na(mod[1])) {
    M1Krate2[[i]] <- NULL
  } else {
    intercept <- round(coef(mod)[1],4)
    slope <- round(coef(mod)[2], 4)
    r2 <- round(summary(mod)$r.squared,4)
    equation <- paste("ry =", intercept, ifelse(slope>= 0, "+","-"), 
                      abs(slope),"Krate", "     ", "r.squared = ", r2)
    y2 <- predict(mod)
    d <- data.frame(cbind(x0,ry0,y2))
    ## CSTV
    mod1 <- ggplot(d, aes(x = x0, y = ry0))+geom_point()+
      geom_line(aes(x=x0,y2))+
      ylim(0,100)+
      labs(subtitle = paste0("Mehlich I Avail. Class:", cat[i]),
                                x="Fertilizer K Rate (kg K / ha)", 
                                y="Soybean Relative Yield (%)")+
      annotate("text", x=Inf,y=Inf, label=equation, hjust=1.1,vjust=50)+
      theme_bw()
    M1Krate2[[i]] <- mod1
    
  }
  
}

## Olsen_P ##

OPrate <- list()

for (i in 1:6) {
  dt <- subset(dat2, Soil_Method == "Olsen_P")
  
  LL <- c(0,1.2495,2.87875,4.49575,6.7375,9.8)
  UL <- c(2.07025,4.5,5.5125,7.9625,10.4125,31) ##3.68725
  cat <-c("Very Low", "Low", "Medium", "High 1", "High 2", "Very High")
  
  ## Subset
  dts <-subset(dt, Soil_Test_Control >= LL[i] & Soil_Test_Control<=UL[i])
  
  ry <- dts$Yield / dts$Yield_Max *100
  ry0 <- ry[!is.na(ry)]
  x <- dts$Krate
  x0<-x[!is.na(x)]
  
  # Use tryCatch to handle errors in quadratic_plateau function #
  mod <- tryCatch({
    quadratic_plateau(stv = x0, ry = ry0)
  }, error = function(e) {
    return(NA)
  })
  
  # Check if 'mod' is NA, and handle accordingly
  if (is.na(mod[1])) {
    OPrate[[i]] <- NULL
  } else {
    
  ## CSTV
  mod1 <- quadratic_plateau(stv = x0, ry = ry0, plot = T)
  OPrate[[i]] <- mod1 + labs(subtitle = paste0("Olsen P Avail. Class:", cat[i]),
                                x="Fertilizer P Rate (kg P / ha)", 
                                y="Corn Relative Yield (%)")
    
  }
  
}

OPrate2 <- list()

for (i in 1:3) {
  dt <- subset(dat2, Soil_Method == "Olsen_P")
  
  LL <- c(1.2495,2.87875,6.7375)
  UL <- c(4.5,5.5125,10.4125) ##3.68725
  cat <-c("Low", "Medium", "High 2")
  
  ## Subset
  dts <-subset(dt, Soil_Test_Control >= LL[i] & Soil_Test_Control<=UL[i])
  
  ry <- dts$Yield / dts$Yield_Max *100
  ry0 <- ry[!is.na(ry)]
  x <- dts$Krate
  x0<-x[!is.na(x)]
  
  # Use tryCatch to handle errors in quadratic_plateau function #
  # polynomial is not better#
  mod <- tryCatch({
    mod <- lm(ry0~x0)
  }, error = function(e) {
    return(NA)
  })
  
  # Check if 'mod' is NA, and handle accordingly
  if (is.na(mod[1])) {
    OPrate2[[i]] <- NULL
  } else {
    intercept <- round(coef(mod)[1],4)
    slope <- round(coef(mod)[2], 4)
    r2 <- round(summary(mod)$r.squared,4)
    equation <- paste("ry =", intercept, ifelse(slope>= 0, "+","-"), 
                      abs(slope),"Prate", "     ", "r.squared = ", r2)
    y2 <- predict(mod)
    d <- data.frame(cbind(x0,ry0,y2))
    ## CSTV
    mod1 <- ggplot(d, aes(x = x0, y = ry0))+geom_point()+
      geom_line(aes(x=x0,y2))+
      ylim(0,100)+
      labs(subtitle = paste0("Olsen P Avail. Class:", cat[i]),
           x="Fertilizer P Rate (kg P / ha)", 
           y="Corn Relative Yield (%)")+
      annotate("text", x=Inf,y=Inf, label=equation, hjust=1.1,vjust=50)+
      theme_bw()
    OPrate2[[i]] <- mod1
    
  }
  
}

#### Contour Plots ####
library(olsrr)
library(plotly)
dat2$Percent_RY <- dat2$Yield/dat2$Yield_Max*100
dat3 <- dat2[c("Soil_Method","STV2","Krate","Percent_RY")]

#### Mehlich III ####

dat_M3 <- dat3[dat3$Soil_Method=="Mehlich_III",]

modM3_1 <- lm(Percent_RY~(STV2+sqrt(STV2)+Krate+I(Krate^2))^2, data=dat_M3)
m31 <- ols_step_backward_p(modM3_1)# why remove K and K2 interaction
capture.output(m31,file = "MM3_1.txt")

modM3_2 <- lm(Percent_RY~(STV2+I(STV2^2)+Krate+I(Krate^2))^2, data=dat_M3)
m32 <- ols_step_backward_p(modM3_2)
capture.output(m32,file = "MM3_2.txt")

modM3_3 <- lm(Percent_RY~(STV2+sqrt(STV2)+Krate+sqrt(Krate))^2, data=dat_M3)
m33 <- ols_step_backward_p(modM3_3)
capture.output(m33,file = "MM3_3.txt")

modM3_4 <- lm(Percent_RY~(STV2+I(STV2^2)+Krate+sqrt(Krate))^2, data=dat_M3)
m34 <- ols_step_backward_p(modM3_4)
capture.output(m34,file = "MM3_4.txt")

newdata <- data.frame(STV2=seq(min(dat_M3$STV2),max(dat_M3$STV2),length.out=300),
                      Krate=seq(min(dat_M3$Krate),max(dat_M3$Krate),length.out=300))
#p <- predict(modM3,newdata)
p1 <- predict(m31$model, newdata)
p2 <- predict(m32$model, newdata)
p3 <- predict(m33$model, newdata)
p4 <- predict(m34$model, newdata)

p<- list(p1,p2,p3,p4)

pltm3 <- htmltools::tagList()

for (i in 1:4) {

pltm3[[i]] <- plot_ly(x=newdata$STV2,y=newdata$Krate,z=p[[i]], type="contour")%>%
  colorbar(title = "% Relative Yield of Soybean") %>%
  layout(title = paste0('Mehlich III - Model ', i),
         plot_bgcolor = "#FFFFFF",
         margin=list(l = 50,r = 50,b = 50,t = 50),
         font = list(size=16),
         xaxis = list(title = 'Soil Test K (ppm)'), 
         yaxis = list(title = 'Fertilizer K Rate (kg/ha)'))
htmlwidgets::saveWidget(pltm3[[i]],file = paste0("Mehlich III - Model", i, ".html"))
}

#### Mehlich I ####

dat_M1 <- dat3[dat3$Soil_Method=="Mehlich_I",]

modM1_1 <- lm(Percent_RY~(STV2+sqrt(STV2)+Krate+I(Krate^2))^2, data=dat_M1)
m11 <- ols_step_backward_p(modM1_1)# why remove K and K2 interaction
capture.output(m11,file = "MM1_1.txt")

modM1_2 <- lm(Percent_RY~(STV2+I(STV2^2)+Krate+I(Krate^2))^2, data=dat_M1)
m12 <- ols_step_backward_p(modM1_2)
capture.output(m12,file = "MM1_2.txt")

modM1_3 <- lm(Percent_RY~(STV2+sqrt(STV2)+Krate+sqrt(Krate))^2, data=dat_M1)
m13 <- ols_step_backward_p(modM1_3)
capture.output(m13,file = "MM1_3.txt")

modM1_4 <- lm(Percent_RY~(STV2+I(STV2^2)+Krate+sqrt(Krate))^2, data=dat_M1)
m14 <- ols_step_backward_p(modM1_4)
capture.output(m14,file = "MM1_4.txt")

newdata <- data.frame(STV2=seq(min(dat_M1$STV2),max(dat_M1$STV2),length.out=300),
                      Krate=seq(min(dat_M1$Krate),max(dat_M1$Krate),length.out=300))
#p <- predict(modM3,newdata)
p1 <- predict(m11$model, newdata)
p2 <- predict(m12$model, newdata)
p3 <- predict(m13$model, newdata)
p4 <- predict(m14$model, newdata)

p<- list(p1,p2,p3,p4)

pltm1 <- htmltools::tagList()

for (i in 1:4) {
  
  pltm1[[i]] <- plot_ly(x=newdata$STV2,y=newdata$Krate,z=p[[i]], type="contour")%>%
    colorbar(title = "% Relative Yield of Soybean") %>%
    
    layout(title = paste0('Mehlich I - Model ', i),
           plot_bgcolor = "#FFFFFF",
           margin=list(l = 50,r = 50,b = 50,t = 50),
           font = list(size=16),
           xaxis = list(title = 'Soil Test K (ppm)'), 
           yaxis = list(title = 'Fertilizer K Rate (kg/ha)'))
  htmlwidgets::saveWidget(pltm1[[i]],file = paste0("Mehlich I - Model", i, ".html"))
}

#### Olsen P ####
dat_OP <- dat3[dat3$Soil_Method=="Olsen_P",]

modOP_1 <- lm(Percent_RY~(STV2+sqrt(STV2)+Krate+I(Krate^2))^2, data=dat_OP)
p11 <- ols_step_backward_p(modOP_1)# why remove K and K2 interaction
capture.output(p11,file = "OP_1.txt")

modMOP_2 <- lm(Percent_RY~(STV2+I(STV2^2)+Krate+I(Krate^2))^2, data=dat_OP)
p12 <- ols_step_backward_p(modMOP_2)
capture.output(p12,file = "OP_2.txt")

modOP_3 <- lm(Percent_RY~(STV2+sqrt(STV2)+Krate+sqrt(Krate))^2, data=dat_OP)
p13 <- ols_step_backward_p(modOP_3)
capture.output(p13,file = "OP_3.txt")

modOP_4 <- lm(Percent_RY~(STV2+I(STV2^2)+Krate+sqrt(Krate))^2, data=dat_OP)
p14 <- ols_step_backward_p(modOP_4)
capture.output(p14,file = "OP_4.txt")

newdata <- data.frame(STV2=seq(min(dat_OP$STV2, na.rm=T),max(dat_OP$STV2, na.rm=T),length.out=300),
                      Krate=seq(min(dat_OP$Krate, na.rm=T),max(dat_OP$Krate, na.rm=T),length.out=300))
#p <- predict(modM3,newdata)
p1 <- predict(p11$model, newdata)
p2 <- predict(p12$model, newdata)
p3 <- predict(p13$model, newdata)
p4 <- predict(p14$model, newdata)

p<- list(p1,p2,p3,p4)

pltOP <- htmltools::tagList()

for (i in 1:4) {
  
  pltOP[[i]] <- plot_ly(x=newdata$STV2,y=newdata$Krate,z=p[[i]], type="contour")%>%
    colorbar(title = "% Relative Yield of Corn") %>%
    layout(title = paste0('Olsen P - Model ', i),
           plot_bgcolor = "#FFFFFF",
           margin=list(l = 50,r = 50,b = 50,t = 50),
           font = list(size=16),
           xaxis = list(title = 'Soil Test P (ppm)'), 
           yaxis = list(title = 'Fertilizer P Rate (kg/ha)'))
  
  htmlwidgets::saveWidget(pltOP[[i]],file = paste0("Olsen P - Model", i, ".html"))
}
