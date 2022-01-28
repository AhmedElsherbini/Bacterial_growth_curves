#author_ahmed_elsherbini #ahmed.elsherbini@uni-tuebingen.de
#10-07-2021
#activate your models
library(growthcurver) 
library(xlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(Hmisc)
#this script is adapted for (epoch reader BioTEK)
#https://cran.r-project.org/web/packages/growthcurver/vignettes/Growthcurver-vignette.html
#load our data from 
data <- read.xlsx2("se and guuu14072021.xlsx",sheetIndex = 1) #You know where you can put the name of your file :)

#d <- growthdata  # load some sample, simulated dat
#lets remove temp. from our list of data
data[2] <- NULL 
#convert your data into a numeric rather than chch.
data = as.data.frame(lapply(data, as.numeric))
#Remove list elements with NULL
#lets make T of time small to be in line with subsequent work
names(data)[1][1] <- "time"
data$time <- data$time * 24 #fit it to hr.
#to draw only one curve 
#gc_fit1 <- SummarizeGrowth(data$time, data$S)
#gc_fit2 <- SummarizeGrowth(data$time, data$SE_and_stratium)
#plot(gc_fit1,  ylab="OD 600" , xlab = "Time hr") #if you  see the red line fitting with black dots you are in good fit :)
###############################################################################################################################
#here, it is critical to know what these values means
#n0 is the  starting population/starting OD600 in our case
#r growth rate
#K carrying capacity think like the saturated OD
#t_mid is the time needed to reach half of K (aka inflecting point)
#t_g generation doubling time
#sigma is the st. error, which is the how far your model is far from logistic regression model (perfect is 0 - .004 is good)
#auc is the area under logistic curve
#auc is the are under the empirical logistic curve 
#################################################################################################################################
#often, you will not  have a blank as a value, but if you have
#names(data)[5][1] <- "blank" #example that like A5 is the blank #change this as much as you
#PCA is used to explore the trend of diversity and outlier detection 
gc_out <- SummarizeGrowthByPlate(data)
output_file_name <- "out_put.txt"
write.table(gc_out, file = output_file_name, quote = FALSE, sep = "\t", row.names = FALSE)
df.melt.mod3 <- gc_out  %>% separate(col=sample, into=c('species', 'replicate_n'), sep='\\.') #I want here to group replicate together #note that the grouping starts from NA as first strain
tapply(df.melt.mod3$r, df.melt.mod3$species, mean) #to get the mean of the growth rate using
tapply(df.melt.mod3$auc_l, df.melt.mod3$species, mean)
tapply(df.melt.mod3$k, df.melt.mod3$species, mean)
tapply(df.melt.mod3$n0, df.melt.mod3$species, mean)

####################################################################
# Give the chart file a name
# Plot the bar chart 
par(mar=c(10,4,4,4))
barplot(gc_out$r,names.arg=gc_out$sample,col="blue",ylab= "growth rate",border="red",las=2 ,  cex.names=.1)
gc_out <- SummarizeGrowthByPlate(data, plot_fit = TRUE, plot_file = "gc_plots.pdf")
pca_gc_out <- as_data_frame(gc_out) 
rownames(pca_gc_out) <- pca_gc_out$sample
pca.res <- prcomp(pca_gc_out %>% select(k:sigma), center=TRUE, scale=TRUE)
as_data_frame(list(PC1=pca.res$x[,1],PC2=pca.res$x[,2], samples = pca_gc_out$sample)) %>% ggplot(aes(x=PC1,y=PC2, label=samples)) + geom_text(size = 3)
########################################################################################
df.melt2 <- melt(data, id.vars = 'time', measure.vars = names(data[-1]))
df.melt.mod2 <- df.melt2 %>% separate(col=variable, into=c('species', 'replicate_n'), sep='\\.')
names(df.melt.mod2)[4] <- "OD600"
names(df.melt.mod2)[1] <- "time_hr"

ggplot(df.melt.mod2, aes(x=time_hr, y= OD600 , group=species)) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(color=species)) + 
  stat_summary( fun.data= mean_cl_normal , geom='errorbar',width=0.5, aes(color=species)) +
  theme_bw()

#########################################################################################