#Import dataset
library(readxl)
Docusign_Regression_Portfolio_Milestone <- 
  read_excel("Docusign Regression Portfolio Milestone.xlsx")

#Creating Manageable Variables Names (Model1)
Time <- Docusign_Regression_Portfolio_Milestone$`Time (minutes)`
UniqueCarrierForms <- Docusign_Regression_Portfolio_Milestone$
  `Number of Unique Carrier Forms Filled Out`
Emailssent <- Docusign_Regression_Portfolio_Milestone$`Number of Emails sent`
Numofcarriers <- Docusign_Regression_Portfolio_Milestone$
  `Number of Carriers that need emails`

#Summary of the Data
summary(Docusign_Regression_Portfolio_Milestone)

#Building the Regression Model1
RegressionModel <- lm(Time ~ UniqueCarrierForms + Emailssent + Numofcarriers, data = Docusign_Regression_Portfolio_Milestone)
summary(RegressionModel)

#Regression Model1 with the Correlative Variable "Number of Unique Carrier Forms Filled Out" aka UniqueCarrierForms
Unique_Forms_Regression_Model <- lm(Time ~ UniqueCarrierForms, data = Docusign_Regression_Portfolio_Milestone)
summary(Unique_Forms_Regression_Model)
Unique_Forms_Regression_Model

#Regression Plot1 with Regression Line
Regressionplot <- plot(UniqueCarrierForms, Time, ylab = "Time to Complete Docusign (Minutes)", 
     xlab = "Number of Unique Forms Filled Out Per Docusign", main = "Time to Complete Docusign as Determined by the Number
     of Unique Carrier Forms Required")
lines(lowess(UniqueCarrierForms, Time), col = "Red", lwd = 2)

#UniqueForms Model1 Correlation
cor(UniqueCarrierForms, Time)


#Model2 Variables Rename
Emailssent2 <- Docusign_Regression_Portfolio_Milestone_with_Outlier_Removed$`Number of Emails sent`
Numofcarriers2 <- Docusign_Regression_Portfolio_Milestone_with_Outlier_Removed$`Number of Carriers that need emails`
UniqueCarrierForms2 <- Docusign_Regression_Portfolio_Milestone_with_Outlier_Removed$`Number of Unique Carrier Forms Filled Out`
Time2 <- Docusign_Regression_Portfolio_Milestone_with_Outlier_Removed$`Time (minutes)`

#Model2 with Outlier Removed
RegressionModel2 <- lm(Time2 ~ Emailssent2 + UniqueCarrierForms2 + Numofcarriers2,
                       data = Docusign_Regression_Portfolio_Milestone_with_Outlier_Removed)
summary(RegressionModel2)

#Linear Models for Model 2Correlative variables "Number of Emails Sent" and "Number of Unique Carrier Forms"
Unique_Forms_Regression_Model2 <- lm(Time2 ~ UniqueCarrierForms2, data = Docusign_Regression_Portfolio_Milestone_with_Outlier_Removed)
summary(Unique_Forms_Regression_Model2)

Number_of_Emails_Sent_Model <- lm(Time2 ~ Emailssent2, data = Docusign_Regression_Portfolio_Milestone_with_Outlier_Removed)
summary(Number_of_Emails_Sent_Model)

#Regression Plot and Line for Model2 focusing on Time for Unique Carrier Forms
Regressionplot2 <- plot(UniqueCarrierForms2,Time2, ylab = "Time to Complete Docusign (Minutes)", 
                       xlab = "Number of Unique Forms Filled Out Per Docusign", main = "Time to Complete Docusign as Determined by the Number
     of Unique Carrier Forms Required")
lines(lowess(UniqueCarrierForms2, Time2), col = "Blue", lwd = 2)

#Regression Plot and Line for Model2 focusing on Time for the Number of Emails Sent
Regressionplot3 <- plot(Emailssent2, Time2, ylab = "Time to Complete Docusign (Minutes)",
                        xlab = "Number of Emails Sent to Carrier per Docusign",
                        main = "Time to Complete Docusign as Determined by 
                        the Number of Emails Needed by Carriers")
lines(lowess(Emailssent2, Time2), col = "Magenta", lwd = 2)

#UniqueForms and EmailsSent Correlation for Model 2
cor(UniqueCarrierForms2, Time2)
cor(Emailssent2, Time2)

#Model3 Variable Renames
Emailssent3 <- Docusign_Regression_Portfolio_Milestone_with_Email_Outliers_Removed$`Number of Emails sent`
Numofcarriers3 <- Docusign_Regression_Portfolio_Milestone_with_Email_Outliers_Removed$`Number of Carriers that need emails`
UniqueCarrierForms3 <- Docusign_Regression_Portfolio_Milestone_with_Email_Outliers_Removed$`Number of Unique Carrier Forms Filled Out`
Time3 <- Docusign_Regression_Portfolio_Milestone_with_Email_Outliers_Removed$`Time (minutes)`

#Regression Model3
RegressionModel3 <- lm(Time3 ~ Emailssent3 + Numofcarriers3 + UniqueCarrierForms3,
                       data = Docusign_Regression_Portfolio_Milestone_with_Email_Outliers_Removed)
summary(RegressionModel3)

#Regression Model3 with the Correlative Variable "Number of Unique Carrier Forms Filled Out" aka UniqueCarrierForms
Unique_Forms_Regression_Model3 <- lm(Time3 ~ UniqueCarrierForms3, data = Docusign_Regression_Portfolio_Milestone_with_Email_Outliers_Removed)
summary(Unique_Forms_Regression_Model3)

#Regression Plot3 and Regression Line
Regressionplot3 <- plot(UniqueCarrierForms3, Time3, ylab = "Time to Complete Docusign (Minutes)", 
                       xlab = "Number of Unique Forms Filled Out Per Docusign", main = "Time to Complete Docusign as Determined by the Number
     of Unique Carrier Forms Required")
lines(lowess(UniqueCarrierForms3, Time3), col = "Green", lwd = 2)

#UniqueForms Model3 Correlation
cor(UniqueCarrierForms3, Time3)

#Completion Rate Determination (in Hours) for Models1-3
Docusignscompleted <- nrow(Docusign_Regression_Portfolio_Milestone)
DocusignRate <- (Docusignscompleted/(sum(Time)/60))
DocusignRate

Docusignscompleted2 <- nrow(Docusign_Regression_Portfolio_Milestone_with_Outlier_Removed)
DocusignRate2 <- (Docusignscompleted2/(sum(Time)/60))
DocusignRate2

Docusignscompleted3 <- nrow(Docusign_Regression_Portfolio_Milestone_with_Email_Outliers_Removed)
DocusignRate3 <- (Docusignscompleted3/(sum(Time)/60))
DocusignRate3

#ANOVA
Anova1 <- aov(Time~UniqueCarrierForms, data = Docusign_Regression_Portfolio_Milestone)
summary(Anova1)
Anova1
