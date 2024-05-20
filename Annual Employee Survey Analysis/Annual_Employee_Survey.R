# Loading the packages
library(readr)
library(sqldf)
library(mosaic)
library(gtsummary)
library(gt)
library(corrplot)
library(table1)
library(tigerstats)
library(ggplot2)
library(tidyverse)
library(psych)

# Loading all the datasets
BenchmarkData <- read_csv("BenchmarkData.csv",show_col_types = FALSE)
TrainingData <- read_csv("TrainingData.csv",show_col_types = FALSE)
PersonalData <- read_csv("PersonalData.csv",show_col_types = FALSE)
PerformanceData <- read_csv("PerformanceData.csv",show_col_types = FALSE)
CompensationData <- read_csv("CompensationData.csv",show_col_types = FALSE)
SelectionValidationData <- read_csv("SelectionValidationData.csv",show_col_types = FALSE)
SurveyData <- read_csv("SurveyData.csv",show_col_types = FALSE)

# Report a list of the employee IDs for those employees who responded with Strongly Agree (5) to the following item: “During the next 12 months, I will probably look for a new job outside of Recreation Unlimited.” (TI2).
employee_IDs <- sqldf("SELECT PersonalData.EmployeeID,SurveyData.TI2 FROM PersonalData 
                      LEFT JOIN SurveyData ON PersonalData.EmployeeID = SurveyData.EmployeeID
                      WHERE TI2 = 5")
 employee_IDs|>gt()|>tab_options(column_labels.background.color = "darkorange")

 
# Graph
T = data.frame('TI2'=c('TI_5','TI_rest'),'Values' = c(NROW(employee_IDs),
                NROW(SurveyData$TI2)-NROW(employee_IDs)))


 # Calculate percentages
pct <- round(100*T$Values/sum(T$Values),2)
 # Draw oie chart
pie(T$Values,labels = paste(T$TI2, sep = " ",',', pct,"%"), 
     col = rainbow(length(T$Values)), main = "Employees who responded with Strongly Agree (5)")
 

#Internal consistency reliabilities (Cronbach’s alphas) of job satisfaction items
alpha(SurveyData[,c("JS1","JS2","JS3","JS4")])

#Internal consistency reliabilities (Cronbach’s alphas) of engagement items
alpha(SurveyData[,c("Eng1","Eng2","Eng3")])

#Internal consistency reliabilities (Cronbach’s alphas) of turnover intentions items
alpha(SurveyData[,c("TI1","TI2","TI3")])


#Composite variables
SurveyData$JS_Overall <- rowMeans(SurveyData[,c("JS1","JS2","JS3","JS4")], na.rm=TRUE)
SurveyData$Eng_Overall <- rowMeans(SurveyData[,c("Eng1","Eng2","Eng3")], na.rm=TRUE)
SurveyData$TI_Overall <- rowMeans(SurveyData[,"TI3"], na.rm=TRUE)
composite_var <- SurveyData|>select(JS_Overall,Eng_Overall,TI_Overall)

#Measures of central tendency and dispersion for the three composite variables
JS_Overall_desc = favstats(~JS_Overall, data = composite_var)
Eng_Overall_desc = favstats(~Eng_Overall, data = composite_var)
TI_Overall_desc = favstats(~TI_Overall, data = composite_var)
Composite_Variable  = c("JS_Overall","Eng_Overall","TI_Overall")
Summary <- rbind(JS_Overall_desc,Eng_Overall_desc,TI_Overall_desc)
Summary <- cbind(Composite_Variable,Summary)
Summary|>gt()|>tab_options(column_labels.background.color = "darkgreen")

# Compare Recreation Unlimited’s survey responses to employees’ responses from the 23 competitor organizations.
BenchmarkData_t <- data.frame(cbind(names(BenchmarkData),t(BenchmarkData)))
BenchmarkData_t$X2 <- as.numeric(as.character(BenchmarkData_t$X2))
BenchmarkData_t_Mean <- BenchmarkData_t|>rename(BenchmarkResp = X1,Mean = X2)
survey_long <- SurveyData|>pivot_longer(cols = (2:11),names_to = 'survey_resp',values_to = 'Rating')|>
  select(survey_resp,Rating)

survey_long_mean <- survey_long|>group_by(survey_resp)|>summarise(Mean = round(mean(Rating),2))
comparison <- sqldf("SELECT survey_long_mean.survey_resp as `Survey question`,
                      survey_long_mean.mean as `Recreation's employees mean response`,
                      BenchmarkData_t_Mean.Mean as `Benchmark's employees mean response`
                      FROM survey_long_mean 
                      INNER JOIN BenchmarkData_t_Mean 
                      ON survey_long_mean.survey_resp = BenchmarkData_t_Mean.BenchmarkResp")
comparison|>gt()

comparison_long <- comparison|>
  pivot_longer(cols = (2:3),names_to = 'Vars',values_to = 'Mean_rating')

#comparison_long|>
ggplot(comparison_long,aes(x=`Survey question`,y=Mean_rating)) + 
  geom_line(aes(color = Vars,group = Vars))+ theme_bw()+
  ggtitle("Recreation Unlimited’s survey responses to employees’ responses from the 23 competitor  organizations comparisoon") + 
  
  theme(plot.title = element_text(lineheight=3, face="bold", color="black",size=14))+
  geom_text(aes(label = Mean_rating), size = 10, hjust = 0.5, vjust = 5, position = "stack",color = 'white')+ ylim(2.5,4)+
  theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"),
         axis.text.x = element_text(face="bold", color="#993333",size=14, angle=0),
         axis.text.y = element_text(face="bold", color="#993333",size=14, angle=0))+
  theme(legend.position="top",legend.text=element_text(size=14),legend.title=element_text(size=16)) +
  scale_fill_manual(values = c(`Recreation's employees mean response`='darkgreen',
                               `Benchmark's employees mean response` = 'darkmagenta'))


#Correlation
df <- sqldf("SELECT SurveyData.EmployeeID,SurveyData.JS_Overall,SurveyData.Eng_Overall,
                      SurveyData.TI_Overall,BARS,Sales FROM SurveyData
                      INNER JOIN PerformanceData 
                      ON SurveyData.EmployeeID = PerformanceData.EmployeeID")
#head(df)|>gt()
M = round(cor(df[,-1]),4)
corrplot(M, method="number",type = "lower")


# Regression Model
model = lm(Sales~JS_Overall+Eng_Overall+TI_Overall+BARS,data = df)
summary(model)
anova(model)




 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 