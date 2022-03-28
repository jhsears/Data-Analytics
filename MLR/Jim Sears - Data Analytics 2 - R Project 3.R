################################################################################
#
# R Project 3:  Multiple Regression Analysis
#
# Jim Sears
# Data Analytics 2 - ADS523 
# Bay Path University, Longmeadow, MA
# Applied Data Science Program, Graduate School 
# Submitted To: Professor Aja Shabana; Summer 2021
#
################################################################################
# Load required libraries

library(readxl)


################################################################################
# Create data frame of all data and inspect

# Source file: Sample House Price Data.xlsx
sample_house_price_data <- read_excel("G:/My Drive/Bay Path/Courses/ADS523Z1 - Data Analytics 2/R Project 3/Sample House Price Data.xlsx")
fix(sample_house_price_data)

str(sample_house_price_data)
# 'data.frame':	100 obs. of  19 variables:
#   $ id           : num  7.13e+09 6.41e+09 5.63e+09 2.49e+09 1.95e+09 ...
# $ date         : chr  "20141013T000000" "20141209T000000" "20150225T000000" "20141209T000000" ...
# $ price        : num  221900 538000 180000 604000 510000 ...
# $ bedrooms     : num  3 3 2 4 3 4 3 3 3 3 ...
# $ bathrooms    : num  1 2.25 1 3 2 4.5 2.25 1.5 1 2.5 ...
# $ sqft_living  : num  1180 2570 770 1960 1680 ...
# $ sqft_lot     : num  5650 7242 10000 5000 8080 ...
# $ floors       : num  1 2 1 1 1 1 2 1 1 2 ...
# $ waterfront   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ view         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ condition    : num  3 3 3 5 3 3 3 3 3 3 ...
# $ grade        : num  7 7 6 7 8 11 7 7 7 7 ...
# $ sqft_above   : num  1180 2170 770 1050 1680 ...
# $ sqft_basement: num  0 400 0 910 0 1530 0 0 730 0 ...
# $ yr_built     : num  1955 1951 1933 1965 1987 ...
# $ yr_renovated : num  0 1991 0 0 0 ...
# $ zipcode      : num  98178 98125 98028 98136 98074 ...
# $ lat          : num  47.5 47.7 47.7 47.5 47.6 ...
# $ long         : num  -122 -122 -122 -122 -122 ...


# Confirm no values are missing for variables of interest
dim(sample_house_price_data) # [1] 100  19
sum(is.na(sample_house_price_data)) # [1] 0


################################################################################
# Initial variable selection

names(sample_house_price_data)
# [1] "id"            "date"          "price"         "bedrooms"      "bathrooms"     "sqft_living"  
# [7] "sqft_lot"      "floors"        "waterfront"    "view"          "condition"     "grade"        
# [13] "sqft_above"    "sqft_basement" "yr_built"      "yr_renovated"  "zipcode"       "lat"          
# [19] "long" 

# Exclude variables: id, date, zipcode, lat, long, yr_renovated

# Formatting Data from remaining variables
price         <- sample_house_price_data$price
bedrooms      <- sample_house_price_data$bedrooms
bathrooms     <- sample_house_price_data$bathrooms
sqft_living   <- sample_house_price_data$sqft_living
sqft_lot      <- sample_house_price_data$sqft_lot
floors        <- sample_house_price_data$floors
waterfront    <- sample_house_price_data$waterfront
view          <- sample_house_price_data$view
condition     <- sample_house_price_data$condition
grade         <- sample_house_price_data$grade
sqft_above    <- sample_house_price_data$sqft_above
sqft_basement <- sample_house_price_data$sqft_basement
yr_built      <- sample_house_price_data$yr_built

# new data frame creation
data_01 <- data.frame(price, bedrooms, bathrooms, sqft_living, sqft_lot,
                          floors, waterfront, view, condition, grade, sqft_above, 
                          sqft_basement, yr_built)
dim(data_01) # [1] 100  13

################################################################################
# Multicollinearity check and further variable selection

# Creating Scatterplot Matrix
pairs(data_01)

plot(sqft_living, price)
abline(lm(price ~ sqft_living))
plot(sqft_basement, price)
abline(lm(price ~ sqft_basement))

# Regression Analysis
model_01<-lm(price ~ ., data_01)
summary(model_01) 

# Call:
#   lm(formula = price ~ ., data = data_01)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -444506 -112693   -8516  112267  782370 
# 
# Coefficients: (1 not defined because of singularities)
#                   Estimate  Std. Error t value Pr(>|t|)    
#   (Intercept)    5.254e+06  1.755e+06   2.993  0.00359 ** 
#   bedrooms       1.842e+04  3.038e+04   0.606  0.54584    
#   bathrooms     -2.840e+04  5.092e+04  -0.558  0.57842    
#   sqft_living    1.685e+02  5.700e+01   2.957  0.00399 ** 
#   sqft_lot       5.147e-01  1.794e+00   0.287  0.77484    
#   floors         2.100e+04  5.457e+04   0.385  0.70137    
#   waterfront     4.340e+05  2.068e+05   2.098  0.03875 *  
#   view           1.054e+05  2.706e+04   3.897  0.00019 ***
#   condition     -5.584e+04  2.753e+04  -2.029  0.04553 *  
#   grade          1.230e+05  2.827e+04   4.350 3.65e-05 ***
#   sqft_above    -5.655e+01  6.320e+01  -0.895  0.37332    
#   sqft_basement         NA         NA      NA       NA    
#   yr_built      -2.948e+03  8.880e+02  -3.319  0.00131 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 183000 on 88 degrees of freedom
# Multiple R-squared:  0.679,	Adjusted R-squared:  0.6388 
# F-statistic: 16.92 on 11 and 88 DF,  p-value: < 2.2e-16


# Exclude variables: bedrooms, bathrooms, sqft_lot, floors, sqft_above, sqft_basement


# new data frame creation
data_02 <- data.frame(price, sqft_living, waterfront, view,  condition, grade, yr_built)
dim(data_02) # [1] 100   7
pairs(data_02)

# Regression Analysis

model_02<-lm(price ~ ., data_02)
summary(model_02) 
# Call:
#   lm(formula = price ~ ., data = data_02)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -417622 -119010  -16962  107826  779870 
# 
# Coefficients:
#               Estimate  Std. Error  t value Pr(>|t|)    
#   (Intercept) 6121222.19 1372328.42   4.460 2.29e-05 ***
#   sqft_living     136.12      30.84   4.414 2.73e-05 ***
#   waterfront   445690.40  191064.34   2.333 0.021822 *  
#   view         102040.90   25635.06   3.981 0.000136 ***
#   condition    -60784.97   26332.63  -2.308 0.023196 *  
#   grade        116865.80   26234.62   4.455 2.34e-05 ***
#   yr_built      -3350.53     702.78  -4.768 6.87e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 179200 on 93 degrees of freedom
# Multiple R-squared:  0.6746,	Adjusted R-squared:  0.6536 
# F-statistic: 32.13 on 6 and 93 DF,  p-value: < 2.2e-16

