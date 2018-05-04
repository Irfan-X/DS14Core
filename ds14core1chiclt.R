# Chi-Square Test Function
#Inputs are Sample vector and Confidence Level
chisquaretest=function(sampledata, alpha)
{
# Find out the outliers and remove them by using 1.5IQR
  outlier=boxplot.stats(sampledata)$out
  outlier.na=ifelse(sampledata %in% outlier, NA, sampledata)
  outliersample=na.omit(outlier.na)
# Find out length and Variance of outlier free sample
  n=length(outliersample)
  ssd=var(outliersample)
# Calculate Confidence Levels
  lchi=alpha/200
  uchi=1-alpha/200
#Degrees of Freedom
  df=n-1
# Find out Chi-Square values for lower and upper intervals(Chi-Square Table)
  lchisqr=qchisq(lchi, df, lower.tail = F)
  uchisqr=qchisq(uchi,df,lower.tail = F)
#Calculate Consistency Intervals
  lchisqrci=sqrt((df*ssd)/lchisqr)
  uchisqrci=sqrt((df*ssd)/uchisqr)
#Print Results
  print(paste("The Chi-Square Test Consistency Intervals with a confidence of : ",(100-alpha),"%"), quote = F )
  print(paste("Lower Interval : ", as.numeric(round(lchisqrci,2))), quote = F)
  print(paste("Upper Interval : ", as.numeric(round(uchisqrci,2))), quote = F)
#Plot actual Sample Distribution with outliers
  boxplot(sampledata, main=" Actual Data with Outliers", col=" Red")
  hist(sampledata, main = "Actual Histogram with Outliers",xlab = "Actual Sample Data", col = "Blue")
#Plot Sample Distribution without outliers
  boxplot(outliersample, main="Data without Outliers", col = "Green")
  hist(outliersample,main = "Histogram Data without Outliers",xlab = "Outlier Free Sample", col = "Orange")
}


# Central Limit Theorem Demonstration
variable_=new.env()
#Inputs are  Sample Data, No of Xbars, Samples in each in Xbar
CLT=function(sampledata,noofxbars,samplesineachxbar)
{
#Iterate upto the No of Xbars
   for(i in 1:noofxbars)
  {
# Generate the samples
# Sampling here is done by replacement
    variable_$eachsample=sample(sampledata,size = samplesineachxbar,replace = T)
# Find out the Xbar value of each sample
    variable_$eachsamplexbar=c(variable_$eachsamplexbar,mean(variable_$eachsample))
# Calculate the Probabilities of each of the Xbars
    variable_$heightofeachsamplexbar=dnorm(variable_$eachsamplexbar,mean = as.integer(mean(variable_$eachsamplexbar)),sd=sd(variable_$eachsamplexbar))
   }
# Plot the Normal Distribution of Xbars
  plot(variable_$eachsamplexbar,variable_$heightofeachsamplexbar, main= "Normal Distribution of Sample Provided",col="blue", xlab = " Xbars", ylab = "Height of Xbars")
}
