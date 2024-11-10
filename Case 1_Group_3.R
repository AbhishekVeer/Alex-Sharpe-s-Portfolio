####Reading in an external dataframe.
d.f2 <- read.csv("C:/Users/karth/OneDrive/Desktop/bf_project.csv", stringsAsFactors = FALSE)
print(d.f2)

####Monthly return
abc_avg <- mean(d.f2$SP500)*100
abc_avg2 <- mean(d.f2$REYNOLDS)*100
abc_avg3 <- mean(d.f2$HASBRO)*100

#Annualized return
ann_abc <- abc_avg*12
ann_abc <- abc_avg2*12
ann_abc <- abc_avg3*12

#Monthly standard deviation
abc_sd <- sd(d.f2$SP500)*100
abc_sd2 <- sd(d.f2$REYNOLDS)*100
abc_sd3 <- sd(d.f2$HASBRO)*100

#Annualised standard deviation
ann_abc_sd <- abc_sd*sqrt(12)
ann_abc_sd <- abc_sd2*sqrt(12)
ann_abc_sd <- abc_sd3*sqrt(12)

##To create different portfolio combination
d.f2$SP500andCash <- d.f2$SP500 * 0.99
d.f2$SP500andRey <- (d.f2$SP500 * 0.99) + (d.f2$REYNOLDS * 0.01)
d.f2$SP500andHas <- (d.f2$SP500 * 0.99) + (d.f2$HASBRO * 0.01)

# Print the updated dataframe
print(d.f2)

d.f3 <- read.csv("C:/Users/karth/OneDrive/Desktop/bf_project.csv", stringsAsFactors = FALSE)
print(d.f3)

#Monthly return of newly created portfolio
abc_avg <- mean(d.f3$SP500.AND.CASH)*100
abc_avg2 <- mean(d.f3$SP500.AND.REY)*100
abc_avg3 <- mean(d.f3$SP.500.AND.HASBRO)*100

#Annualized return of newly created portfolio
ann_abc <- abc_avg*12
ann_abc <- abc_avg2*12
ann_abc <- abc_avg3*12

#Monthly standard deviation of newly created portfolio
abc_sd <- sd(d.f3$SP500.AND.CASH)*100
abc_sd2 <- sd(d.f3$SP500.AND.REY)*100
abc_sd3 <- sd(d.f3$SP.500.AND.HASBRO)*100

#Annualized standard deviation of newly created portfolio
ann_abc_sd <- abc_sd*sqrt(12)
ann_abc_sd <- abc_sd2*sqrt(12)
ann_abc_sd <- abc_sd3*sqrt(12)

#Effect of SP500 ON REYNOLDS
d.f <- read.csv("C:/JAGSOM TERM 3/Business Forecasting/Assignment1/Karthik GOD/SP500 VS REYNOLDS.csv", stringsAsFactors = FALSE)
print(d.f)

SLR <- lm(REYNOLDS~SP500,data = d.f)
summary(SLR)
anova(SLR)

#Effect of SP500 ON HASBRO
d.f <- read.csv("C:/JAGSOM TERM 3/Business Forecasting/Assignment1/Karthik GOD/SP500 VS HASBRO.csv", stringsAsFactors = FALSE)
print(d.f)

SLR <- lm(HASBRO~SP500,data = d.f)
summary(SLR)
anova(SLR)

