#!/usr/bin/env Rscript

#read data from .csv
data <- read.csv(file="lice-count-dens-pou-2016-rpt-pac-dfo-mpo-aquaculture-eng.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

#trim data and add total lice count
colnames(data)[4] <- 'licensee'
data[data$licensee == "Greig", "licensee"] <- "Grieg"
data$total <- rowSums(data[,c(8:11)])
data <- data[,c("licensee", "total")]
data <- data[complete.cases(data),]
write.csv(data, file="trimmedData.csv")

#generate boxplot for all sources
png('img_boxplot.png')
boxplot(data$total, main='Sea Lice in Farmed Salmon (All Sources)', ylab='Avg. Sea Lice (per fish)')
dev.off()
#generate categorized boxplot
png('img_boxplot_categorized.png')
boxplot(total~licensee, data, main='Sea Lice in Farmed Salmon', ylab='Avg. Sea Lice (per fish)')
dev.off()
#generate histogram for all sources
png('img_hist.png')
hist(data$total, main='Sea Lice in Farmed Salmon (All Sources)', xlab='Avg. Sea Lice (per fish)')
dev.off()
#split dataset by licensee
grieg = subset(data, licensee == "Grieg")
cermaq = subset(data, licensee == "Cermaq")
marine = subset(data, licensee == "Marine Harvest")
#generate histogram for Grieg
png('img_hist_grieg.png')
hist(grieg$total, main='Sea Lice in Farmed Salmon (Grieg)', xlab='Avg. Sea Lice (per fish)')
dev.off()
#generate histogram for Cermaq
png('img_hist_cermaq.png')
hist(cermaq$total, main='Sea Lice in Farmed Salmon (Cermaq)', xlab='Avg. Sea Lice (per fish)')
dev.off()
#generate histogram for Marine Harvest
png('img_hist_marine.png')
hist(marine$total, main='Sea Lice in Farmed Salmon (Marine Harvest)', xlab='Avg. Sea Lice (per fish)')
dev.off()

#generate data summaries
sink("summary.txt")

#summary for aggregate data
cat("All data:")
cat("\nmean\t\t", mean(data$total))
cat("\nvariance\t", var(data$total))
cat("\nstd dev\t\t", sd(data$total))
#cat("\nQuartiles:\n")
q = quantile(data$total)
cat("\n  q1  q2  q3  q4\n")
cat(q)
iqr = q[4] - q[2]
mild = subset(data, (total < q[2] - 1.5*iqr) | (total > q[4] + 1.5*iqr))
extreme = subset(data, (total < q[2] - 3*iqr) | (total > q[4] + 3*iqr))
cat("\nNum outliers = ")
cat(nrow(mild) - nrow(extreme))
cat("\nNum extreme outliers = ")
cat(nrow(extreme))

#summary for Cermaq
cat("\n\nCermaq:")
cat("\nmean\t\t", mean(cermaq$total))
cat("\nvariance\t", var(cermaq$total))
cat("\nstd dev\t\t", sd(cermaq$total))
#cat("\nQuartiles:\n")
q = quantile(cermaq$total)
cat("\n  q1  q2  q3  q4\n")
cat(q)
iqr = q[4] - q[2]
mild = subset(cermaq, (total < q[2] - 1.5*iqr) | (total > q[4] + 1.5*iqr))
extreme = subset(cermaq, (total < q[2] - 3*iqr) | (total > q[4] + 3*iqr))
cat("\nNum outliers = ")
cat(nrow(mild) - nrow(extreme))
cat("\nNum extreme outliers = ")
cat(nrow(extreme))
#remove the largest value
cermaq <- subset(cermaq, total < max(cermaq$total))
cat("\nafter removing largest value:")
cat("\nmean\t\t", mean(cermaq$total))
cat("\nvariance\t", var(cermaq$total))
cat("\nstd dev\t\t", sd(cermaq$total))

cat("\n\nMarine Harvest:")
cat("\nmean\t\t", mean(marine$total))
cat("\nvariance\t", var(marine$total))
cat("\nstd dev\t\t", sd(marine$total))
#cat("\nQuartiles:\n")
q = quantile(marine$total)
cat("\n  q1  q2  q3  q4\n")
cat(q)
iqr = q[4] - q[2]
mild = subset(marine, (total < q[2] - 1.5*iqr) | (total > q[4] + 1.5*iqr))
extreme = subset(marine, (total < q[2] - 3*iqr) | (total > q[4] + 3*iqr))
cat("\nNum outliers = ")
cat(nrow(mild) - nrow(extreme))
cat("\nNum extreme outliers = ")
cat(nrow(extreme))
#remove the largest value
marine <- subset(marine, total < max(marine$total))
cat("\nafter removing largest value:")
cat("\nmean\t\t", mean(marine$total))
cat("\nvariance\t", var(marine$total))
cat("\nstd dev\t\t", sd(marine$total))

cat("\n\nGrieg:")
cat("\nmean\t\t", mean(grieg$total))
cat("\nvariance\t", var(grieg$total))
cat("\nstd dev\t\t", sd(grieg$total))
#cat("\nQuartiles:\n")
q = quantile(grieg$total)
cat("\n  q1  q2  q3  q4\n")
cat(q)
iqr = q[4] - q[2]
mild = subset(grieg, (total < q[2] - 1.5*iqr) | (total > q[4] + 1.5*iqr))
extreme = subset(grieg, (total < q[2] - 3*iqr) | (total > q[4] + 3*iqr))
cat("\nNum outliers = ")
cat(nrow(mild) - nrow(extreme))
cat("\nNum extreme outliers = ")
cat(nrow(extreme))
#remove the largest value
grieg <- subset(grieg, total < max(grieg$total))
cat("\nafter removing largest value:")
cat("\nmean\t\t", mean(grieg$total))
cat("\nvariance\t", var(grieg$total))
cat("\nstd dev\t\t", sd(grieg$total))

cat("\n\nAll data, recalculated after removing largest value from each licensee:")
data = rbind(cermaq, grieg, marine)
cat("\nmean\t\t", mean(data$total))
cat("\nvariance\t", var(data$total))
cat("\nstd dev\t\t", sd(data$total))
