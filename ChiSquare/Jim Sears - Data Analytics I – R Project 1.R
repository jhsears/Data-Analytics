#---------------------------------------------------------------------------------------------------------------
#Data set creation and confirmation

#Import data from csv file
#Original Data (2017 tornado data):
tornado_data.orig <- read.csv("G:/My Drive/Bay Path/Courses/ADS522Z2 - Data Analytics I/R Project 1/Tornadoes_2017.csv",header = TRUE, sep = ",")
#Selected Data (modified 2017 tornado data, only includes entries with injuries and/or fatalities):
tornado_data <- read.csv("G:/My Drive/Bay Path/Courses/ADS522Z2 - Data Analytics I/R Project 1/Tornadoes_2017.01.csv",header = TRUE, sep = ",")

#confirm import by reviewing headers and first and last 6 entries
head(tornado_data)
# Month Day     Time State F.Scale PropLoss Length Width NumberStates F0 ln.Length. Injuries Fatalities
# 1     1  21  3:35:00    MS       3  9460000  31.06   900            1 No   3.435921       57          4
# 2     1  22  2:29:00    GA       3  2500000  24.66   700            1 No   3.205182       45         11
# 3     2   7 11:12:00    LA       3        0  10.09   600            1 No   2.311545       33          0
# 4     8   6  0:19:00    OK       2 50000000   6.90   550            1 No   1.931521       30          0
# 5     4  29 16:29:00    TX       4  1200000  21.42  1760            1 No   3.064325       25          2
# 6     4  29 17:08:00    TX       3   670000  39.71  1760            1 No   3.681603       24          2
tail(tornado_data)
# Month Day     Time State F.Scale PropLoss Length Width NumberStates F0 ln.Length. Injuries Fatalities
# 83    11   5 12:24:00    IN       2 11090000  38.72   600            2 No  3.6563563        1          0
# 84    11   5 12:24:00    IN       2    90000  30.67   600            2 No  3.4232850        1          0
# 85    11  18 15:20:00    KY       1   250000   1.99    60            1 No  0.6881346        1          0
# 86    11  18 15:32:00    KY       1   200000   1.82    50            1 No  0.5988365        1          0
# 87    12   4 17:13:00    MO       2   100000   7.97    50            2 No  2.0756845        1          0
# 88    12   4 17:13:00    MO       2   100000   6.90    50            2 No  1.9315214        1          0

#inspect the data frame
str(tornado_data)
# 'data.frame':	88 obs. of  13 variables:
#   $ Month       : int  1 1 2 8 4 4 5 2 2 2 ...
# $ Day         : int  21 22 7 6 29 29 16 28 28 28 ...
# $ Time        : chr  "3:35:00" "2:29:00" "11:12:00" "0:19:00" ...
# $ State       : chr  "MS" "GA" "LA" "OK" ...
# $ F.Scale     : int  3 3 3 2 4 3 3 3 4 4 ...
# $ PropLoss    : int  9460000 2500000 0 50000000 1200000 670000 10520000 0 14800000 8000000 ...
# $ Length      : num  31.1 24.7 10.1 6.9 21.4 ...
# $ Width       : int  900 700 600 550 1760 1760 1320 800 1100 1100 ...
# $ NumberStates: int  1 1 1 1 1 1 1 1 2 2 ...
# $ F0          : chr  "No" "No" "No" "No" ...
# $ ln.Length.  : num  3.44 3.21 2.31 1.93 3.06 ...
# $ Injuries    : int  57 45 33 30 25 24 25 14 12 12 ...
# $ Fatalities  : int  4 11 0 0 2 2 1 2 1 1 ...

#---------------------------------------------------------------------------------------------------------------
#Variable selection and declaration

#Qualitative data
#State where tornado occurred:
tor_state <- tornado_data$State

#Quantitative data
#Length of tornado path (miles)
tor_length <- tornado_data$Length
#Number of Injuries per tornado (people)
tor_injury <- tornado_data$Injuries
#Number of Fatalities per tornado (people)
tor_fatal <- tornado_data$Fatalities
#Injuries plus Fatalities per tornado (people)
tor_injfatal <- tornado_data$InjPlusFatal

#---------------------------------------------------------------------------------------------------------------
#Qualitative Variable Description adn Plots

#Frequency distribution
state.freq <- table(tor_state)
state.freq
# AL AR DC FL GA IA IL IN KS KY LA MA MD ME MO MS NC NE OH OK PA SC TN TX WI WY 
# 4  5  1  1  6  3  5  5  1  2 11  1  1  1 10  5  3  1  2  2  1  5  1  8  1  2 
state.relfreq <- round((state.freq/nrow(tornado_data)),3)
state.relfreq
# AL    AR    DC    FL    GA    IA    IL    IN    KS    KY    LA    MA    MD    ME    MO    MS    NC    NE    OH    OK 
# 0.045 0.057 0.011 0.011 0.068 0.034 0.057 0.057 0.011 0.023 0.125 0.011 0.011 0.011 0.114 0.057 0.034 0.011 0.023 0.023 
# PA    SC    TN    TX    WI    WY 
# 0.011 0.057 0.011 0.091 0.011 0.023 

#Qualitative Variable Plot
#Create a pie chart of a qualitative variable 
freq.lbls<-paste(names(state.freq),state.freq,sep=": ")
pie(state.freq, labels = freq.lbls, main = "Tornadoes with Injuries and/or Fatalities, by State")
relfreq.lbls<-paste(names(state.relfreq),state.relfreq,sep=": ")
pie(state.relfreq, labels = relfreq.lbls, main = "Tornadoes with Injuries and/or Fatalities by State, %")


#Frequency distribution, Sorted by Decreasing Frequency
state.freq.sort <- sort(table(tor_state), decreasing = TRUE)
state.freq.sort
# LA MO TX GA AR IL IN MS SC AL IA NC KY OH OK WY DC FL KS MA MD ME NE PA TN WI 
# 11 10  8  6  5  5  5  5  5  4  3  3  2  2  2  2  1  1  1  1  1  1  1  1  1  1 
state.relfreq.sort <- sort(round((state.freq/nrow(tornado_data)),3), decreasing = TRUE)
state.relfreq.sort
# LA    MO    TX    GA    AR    IL    IN    MS    SC    AL    IA    NC    KY    OH    OK    WY    DC    FL    KS    MA    MD    ME 
# 0.125 0.114 0.091 0.068 0.057 0.057 0.057 0.057 0.057 0.045 0.034 0.034 0.023 0.023 0.023 0.023 0.011 0.011 0.011 0.011 0.011 0.011 
# NE    PA    TN    WI 
# 0.011 0.011 0.011 0.011 

#Create a pie chart of a qualitative variable 
freq.sort.lbls<-paste(names(state.freq.sort),state.freq.sort,sep=": ")
pie(state.freq.sort, labels = freq.sort.lbls, main = "Tornadoes with Injuries and/or Fatalities, by State")

#Create a bar chart of a qualitative variable 
bp <- barplot(state.freq.sort, ylim=c(0,12), ylab = "Frequency", main = "Tornadoes with Injuries and/or Fatalities, by State")

#attempt to create pareto chart
bp <- barplot(state.freq.sort, ylim=c(0,12), ylab = "Frequency", main = "Tornadoes with Injuries and/or Fatalities, by State")
lines(bp, cumsum(state.freq.sort), ylim=c(0,1.05), col='red') 
axis(4)

bp <- barplot(state.freq.sort, ylim=c(0,100))
lines(bp, cumsum(state.freq.sort), ylim=c(0,1.05), col='red') 
axis(4)

bp <- barplot(state.relfreq.sort, ylim=c(0,1.05))
lines(bp, cumsum(state.relfreq.sort), ylim=c(0,1.05), col='red') 
axis(4)

#---------------------------------------------------------------------------------------------------------------
#Quantitative Variable Description

#Calculate the summary statistics

#Length
mean(tor_length)
# [1] 12.05045
median(tor_length)
# [1] 7.24
sd(tor_length)
# [1] 13.81552
IQR(tor_length)
# [1] 15.355
fivenum(tor_length)
# [1]  0.250  2.095  7.240 17.565 82.530

#Injuries
mean(tor_injury)
# [1] 5.102273
median(tor_injury)
# [1] 1
sd(tor_injury)
# [1] 9.675232
IQR(tor_injury)
# [1] 2.25
fivenum(tor_injury)
# [1]  0.0  1.0  1.0  3.5 57.0

#Fatalities
mean(tor_fatal)
# [1] 0.3636364
median(tor_fatal)
# [1] 0
sd(tor_fatal)
# [1] 1.314655
IQR(tor_fatal)
# [1] 0
fivenum(tor_fatal)
# [1]  0  0  0  0 11


#Injuries plus Fatalities
mean(tor_injfatal)
# [1] 5.465909
median(tor_injfatal)
# [1] 1
sd(tor_injfatal)
# [1] 10.6079
IQR(tor_injfatal)
# [1] 2.25
fivenum(tor_injfatal)
# [1]  1.0  1.0  1.0  3.5 61.0

#---------------------------------------------------------------------------------------------------------------
#Quantitative Variable Plots

#Length
#Frequency and Relative Frequency Tables
length.breaks = seq(0,85, by=5)
length.cut = cut(tor_length,length.breaks,right=FALSE)
length.freq = table(length.cut)
length.freq
# [0,5)  [5,10) [10,15) [15,20) [20,25) [25,30) [30,35) [35,40) [40,45) [45,50) [50,55) [55,60) [60,65) [65,70) [70,75) 
# 33      17      14       7       5       2       4       3       1       0       1       0       0       0       0 
# [75,80) [80,85) 
# 0       1 
length.relfreq <- round((length.freq/nrow(tornado_data)),3)
length.relfreq
# [0,5)  [5,10) [10,15) [15,20) [20,25) [25,30) [30,35) [35,40) [40,45) [45,50) [50,55) [55,60) [60,65) [65,70) [70,75) 
# 0.375   0.193   0.159   0.080   0.057   0.023   0.045   0.034   0.011   0.000   0.011   0.000   0.000   0.000   0.000 
# [75,80) [80,85) 
# 0.000   0.011
#Histogram
hist(tor_length, length.breaks, right=FALSE, labels = TRUE, xlab = "Miles", main = "Length of Tornado Path")
# length.x <- seq(min(tor_length),max(tor_length),length=80)
# length.f <- dnorm(length.x, mean(tor_length), sd=sd(tor_length))
# lines(length.x,length.f,col="red")
#Original Data
hist(tornado_data.orig$Length, breaks = seq(0,85, by=5), right=FALSE, 
     labels = TRUE, xlab = "Miles", main = "Length of Tornado Path (All Data)")

#Boxplot
boxplot(tor_length, horizontal=TRUE, xlab = "Miles", main = "Length of Tornado Path")
# text(x=fivenum(tor_length),labels=fivenum(tor_length),y=1.3)
# rug(tor_length)
boxplot.stats(tor_length)$out
# [1] 82.53 53.47 44.13
#Original Data:
boxplot(tornado_data.orig$Length, horizontal=TRUE, xlab = "Miles", main = "Length of Tornado Path (All Data)")



#Injuries
#Frequency and Relative Frequency Tables
injury.breaks = seq(0,60, by=5)
injury.cut = cut(tor_injury,injury.breaks,right=FALSE)
injury.freq = table(injury.cut)
injury.freq
# [0,5)  [5,10) [10,15) [15,20) [20,25) [25,30) [30,35) [35,40) [40,45) [45,50) [50,55) [55,60) 
# 68       6       7       0       1       2       2       0       0       1       0       1 
injury.relfreq <- round((injury.freq/nrow(tornado_data)),3)
injury.relfreq
# [0,5)  [5,10) [10,15) [15,20) [20,25) [25,30) [30,35) [35,40) [40,45) [45,50) [50,55) [55,60) 
# 0.773   0.068   0.080   0.000   0.011   0.023   0.023   0.000   0.000   0.011   0.000   0.011 
#Histogram
hist(tor_injury, injury.breaks, right=FALSE, labels = TRUE, xlab = "People", main = "Injuries per Tornado")
#Original Data
hist(tornado_data.orig$Injuries, breaks = seq(0,100, by=5), right=FALSE, 
     labels = TRUE, xlab = "People", main = "Injuries per Tornado (All Data)")

#Boxplot
boxplot(tor_injury, horizontal=TRUE, xlab = "People", main = "Injuries per Tornado")
text(x=fivenum(tor_injury),labels=fivenum(tor_injury),y=1.3)
rug(tor_injury)
#boxplot.stats(tor_injury)$out
# [1] 57 45 33 30 25 24 25 14 12 12 12 10 10 10  8
#Original Data:
boxplot(tornado_data.orig$Injuries, horizontal=TRUE, xlab = "People", main = "Injuries per Tornado (All Data)")



#Fatalities
#Frequency and Relative Frequency Tables
fatal.breaks = seq(0,12, by=1)
fatal.cut = cut(tor_fatal,fatal.breaks,right=FALSE)
fatal.freq = table(fatal.cut)
fatal.freq
# [0,1)   [1,2)   [2,3)   [3,4)   [4,5)   [5,6)   [6,7)   [7,8)   [8,9)  [9,10) [10,11) [11,12) 
# 73       9       4       0       1       0       0       0       0       0       0       1 
fatal.relfreq <- round((fatal.freq/nrow(tornado_data)),3)
fatal.relfreq
# [0,1)   [1,2)   [2,3)   [3,4)   [4,5)   [5,6)   [6,7)   [7,8)   [8,9)  [9,10) [10,11) [11,12) 
# 0.830   0.102   0.045   0.000   0.011   0.000   0.000   0.000   0.000   0.000   0.000   0.011 
#Histogram
hist(tor_fatal, fatal.breaks, right=FALSE, labels = TRUE, xlab = "People", main = "Fatalities per Tornado")
#Original Data
hist(tornado_data.orig$Fatalities, breaks = seq(0,12, by=1), right=FALSE, 
     labels = TRUE, xlab = "People", main = "Fatalities per Tornado (All Data)")

#Boxplot
boxplot(tor_fatal, horizontal=TRUE, xlab = "People", main = "Fatalities per Tornado")
text(x=fivenum(tor_fatal),labels=fivenum(tor_fatal),y=1.3)
rug(tor_fatal)
# boxplot.stats(tor_fatal)$out
# [1]  4 11  2  2  1  2  1  1  1  1  1  2  1  1  1
#Original Data:
boxplot(tornado_data.orig$Fatalities, horizontal=TRUE, xlab = "People", main = "Fatalities per Tornado (All Data)")



#Injuries plus Fatalities
#Frequency and Relative Frequency Tables
injfatal.breaks = seq(0,62, by=2)
injfatal.cut = cut(tor_injfatal,injfatal.breaks,right=FALSE)
injfatal.freq = table(injfatal.cut)
injfatal.freq
# [0,2)   [2,4)   [4,6)   [6,8)  [8,10) [10,12) [12,14) [14,16) [16,18) [18,20) [20,22) [22,24) [24,26) [26,28) [28,30) 
# 48      18       3       4       1       3       3       0       1       0       0       0       0       3       0 
# [30,32) [32,34) [34,36) [36,38) [38,40) [40,42) [42,44) [44,46) [46,48) [48,50) [50,52) [52,54) [54,56) [56,58) [58,60) 
# 1       1       0       0       0       0       0       0       0       0       0       0       0       1       0 
# [60,62) 
# 1 
injfatal.relfreq <- round((injfatal.freq/nrow(tornado_data)),3)
injfatal.relfreq
# [0,2)   [2,4)   [4,6)   [6,8)  [8,10) [10,12) [12,14) [14,16) [16,18) [18,20) [20,22) [22,24) [24,26) [26,28) [28,30) 
# 0.545   0.205   0.034   0.045   0.011   0.034   0.034   0.000   0.011   0.000   0.000   0.000   0.000   0.034   0.000 
# [30,32) [32,34) [34,36) [36,38) [38,40) [40,42) [42,44) [44,46) [46,48) [48,50) [50,52) [52,54) [54,56) [56,58) [58,60) 
# 0.011   0.011   0.000   0.000   0.000   0.000   0.000   0.000   0.000   0.000   0.000   0.000   0.000   0.011   0.000 
# [60,62) 
# 0.011 
#Histogram
hist(tor_injfatal, injfatal.breaks, right=FALSE, labels = TRUE, xlab = "People", 
     main = "Injuries & Fatalities per Tornado")
#Original Data
hist(tornado_data.orig$InjPlusFatal, breaks = seq(0,100, by=2), right=FALSE, 
     labels = TRUE, xlab = "People", main = "Injuries & Fatalities per Tornado (All Data)")

#Boxplot
boxplot(tor_injfatal, horizontal=TRUE, xlab = "People", main = "Injuries & Fatalities per Tornado")
text(x=fivenum(tor_injfatal),labels=fivenum(tor_injfatal),y=1.3)
rug(tor_injfatal)
#Original Data:
boxplot(tornado_data.orig$InjPlusFatal, horizontal=TRUE, xlab = "People", main = "Injuries & Fatalities per Tornado (All Data)")

#---------------------------------------------------------------------------------------------------------------
#Bivariate Analysis
plot(tor_length, tor_injfatal, xlab = "Miles", ylab = "People", main = "Tornado Length and Injuries & Fatalities")
abline(lm(tor_injfatal ~ tor_length))
summary(length_injury.regress)

# Call:
#   lm(formula = tor_injfatal ~ tor_length)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -12.181  -4.210  -1.639  -0.131  50.035 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.9801     1.4015   1.413 0.161305    
# tor_length    0.2893     0.0767   3.772 0.000297 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 9.883 on 86 degrees of freedom
# Multiple R-squared:  0.1419,	Adjusted R-squared:  0.132 
# F-statistic: 14.22 on 1 and 86 DF,  p-value: 0.0002971
