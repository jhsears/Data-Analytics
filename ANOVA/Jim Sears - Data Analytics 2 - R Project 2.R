################################################################################
#
# R Project 2:  ANOVA and Tukey Comparisons
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

# Source file: world_happiness_report_2021.xlsx
# Data source: https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021

world_happiness_report_2021 <- read_excel("G:/My Drive/Bay Path/Courses/ADS523Z1 - Data Analytics 2/R Project 2/world_happiness_report_2021.xlsx")
fix(world_happiness_report_2021)
str(world_happiness_report_2021)
# 'data.frame':	149 obs. of  20 variables:
#   $ Country name                              : chr  "Czech Republic" "Slovenia" "Kosovo" "Slovakia" ...
# $ Regional indicator                        : chr  "Central and Eastern Europe" "Central and Eastern Europe" "Central and Eastern Europe" "Central and Eastern Europe" ...
# $ Ladder score                              : num  6.96 6.46 6.37 6.33 6.25 ...
# $ Standard error of ladder score            : num  0.049 0.043 0.059 0.041 0.045 0.038 0.04 0.057 0.053 0.036 ...
# $ upperwhisker                              : num  7.06 6.55 6.49 6.41 6.34 ...
# $ lowerwhisker                              : num  6.87 6.38 6.26 6.25 6.17 ...
# $ Logged GDP per capita                     : num  10.56 10.53 9.32 10.37 10.5 ...
# $ Social support                            : num  0.947 0.948 0.821 0.936 0.935 0.941 0.898 0.832 0.873 0.927 ...
# $ Healthy life expectancy                   : num  70.8 71.4 63.8 69.2 67.9 ...
# $ Freedom to make life choices              : num  0.858 0.949 0.869 0.766 0.773 0.909 0.841 0.845 0.778 0.715 ...
# $ Generosity                                : num  -0.208 -0.101 0.257 -0.124 -0.203 -0.106 -0.165 -0.219 0.002 -0.162 ...
# $ Perceptions of corruption                 : num  0.868 0.806 0.917 0.911 0.826 0.527 0.735 0.938 0.835 0.8 ...
# $ Ladder score in Dystopia                  : num  2.43 2.43 2.43 2.43 2.43 2.43 2.43 2.43 2.43 2.43 ...
# $ Explained by: Log GDP per capita          : num  1.37 1.36 0.937 1.304 1.35 ...
# $ Explained by: Social support              : num  1.09 1.093 0.807 1.066 1.065 ...
# $ Explained by: Healthy life expectancy     : num  0.703 0.722 0.483 0.653 0.612 0.64 0.668 0.595 0.634 0.587 ...
# $ Explained by: Freedom to make life choices: num  0.58 0.69 0.593 0.468 0.476 0.641 0.558 0.564 0.482 0.405 ...
# $ Explained by: Generosity                  : num  0.052 0.122 0.356 0.107 0.056 0.119 0.08 0.045 0.189 0.082 ...
# $ Explained by: Perceptions of corruption   : num  0.046 0.085 0.014 0.018 0.073 0.263 0.13 0.001 0.066 0.089 ...
# $ Dystopia + residual                       : num  3.12 2.39 3.18 2.71 2.62 ...


# Confirm no values are missing for variables of interest
dim(world_happiness_report_2021) # [1] 149  20
sum(is.na(world_happiness_report_2021$`Ladder score`)) # [1] 0
sum(is.na(world_happiness_report_2021$`Standard error of ladder score`)) # [1] 0



################################################################################
# Selection of 4 regions for ANOVA

# Frequency table by region, or number of nations in each region
table(world_happiness_report_2021$'Regional indicator')

# Central and Eastern Europe Commonwealth of Independent States                          East Asia 
# 17                                 12                                  6 
# Latin America and Caribbean       Middle East and North Africa              North America and ANZ 
# 20                                 17                                  4 
# South Asia                     Southeast Asia                 Sub-Saharan Africa 
# 7                                  9                                 36 
# Western Europe 
# 21

# Generate list of Regions and their Aggregate Ladder Scores
Region.Avg.List <- setNames(aggregate(world_happiness_report_2021[, 'Ladder score'], 
                                      list(world_happiness_report_2021$'Regional indicator'), mean), 
                            c('Region', 'Avg. Ladder Score'))
Region.Avg.List[order(Region.Avg.List$'Avg. Ladder Score'),]
#                   Region                  Avg. Ladder Score
# 7                          South Asia          4.441857
# 9                  Sub-Saharan Africa          4.494472
# 5        Middle East and North Africa          5.219765
# 8                      Southeast Asia          5.407556
# 2  Commonwealth of Independent States          5.467000
# 3                           East Asia          5.810333
# 4         Latin America and Caribbean          5.908050
# 1          Central and Eastern Europe          5.984765
# 10                     Western Europe          6.914905
# 6               North America and ANZ          7.128500



# Generate list of Regions and their Aggregate Standard Deviations
Region.SD.List <- setNames(aggregate(world_happiness_report_2021[, 'Ladder score'], 
                                      list(world_happiness_report_2021$'Regional indicator'), sd), 
                            c('Region', 'SD of Ladder Score'))
Region.SD.List
#                 Region                    SD of Ladder Score
# 1          Central and Eastern Europe          0.4933254
# 2  Commonwealth of Independent States          0.4381160
# 3                           East Asia          0.4399135
# 4         Latin America and Caribbean          0.6934669
# 5        Middle East and North Africa          0.9992592
# 6               North America and ANZ          0.1380568
# 7                          South Asia          0.9934617
# 8                      Southeast Asia          0.6062712
# 9                  Sub-Saharan Africa          0.6548920
# 10                     Western Europe          0.6565195


# Select top 4 Regions in terms of Average Ladder Scores and comparative SD requirements:

# Region                      Nations      Avg. Ladder Score   SD of Ladder Score
# Western Europe                  21      6.914905            0.6565195
# Central and Eastern Europe      17      5.984765            0.4933254
# Latin America and Caribbean     20      5.908050            0.6934669
# Sub-Saharan Africa              36      4.494472            0.6548920


# Creating a Subset of Data for 4 selected regions and their ladder (happy) scores
region<- world_happiness_report_2021$`Regional indicator`
happy_score<-world_happiness_report_2021$`Ladder score`
full_data<-data.frame(region,region_abbr=abbreviate(region),happy_score)

world_happiness_report_2021_select<-subset(full_data,region=="Western Europe"|
                                             region=="Central and Eastern Europe"|
                                             region=="Latin America and Caribbean"|
                                             region=="Sub-Saharan Africa")

fix(world_happiness_report_2021_select)
str(world_happiness_report_2021_select)
# 'data.frame':	94 obs. of  3 variables:
#   $ region     : chr  "Central and Eastern Europe" "Central and Eastern Europe" "Central and Eastern Europe" "Central and Eastern Europe" ...
# $ region_abbr: chr  "CaEE" "CaEE" "CaEE" "CaEE" ...
# $ happy_score: num  6.96 6.46 6.37 6.33 6.25 ...


################################################################################
# Conducting ANOVA

select_data_anova<-aov(happy_score~region_abbr, world_happiness_report_2021_select)
summary(select_data_anova)
# Df Sum Sq Mean Sq F value Pr(>F)    
# region_abbr  3  84.72  28.240   69.32 <2e-16 ***
#   Residuals   90  36.66   0.407                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


################################################################################
# Creating and Graphing Tukey Comparisons
select_Tukey<-TukeyHSD(select_data_anova,ordered=TRUE)
plot(select_Tukey,las=1)

# Adjusting the Margins of the Graphics Window
par(mar=c(7,7,3,3))
plot(select_Tukey,las=1)



