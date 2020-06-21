mecha_mpg <- read.csv('MechaCar_mpg.csv')


# MPG VS LENGTH LIN REGRESSION GRAPH
model <- lm(mpg ~ vehicle.length,mecha_mpg) #create linear model
yvals <- model$coefficients['vehicle.length']*mecha_mpg$vehicle.length +
model$coefficients['(Intercept)']
plt <- ggplot(mecha_mpg,aes(x=vehicle.length,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red")
# MPG VS wEIGHT LIN REGRESSION GRAPH
model <- lm(mpg ~ vehicle.weight,mecha_mpg) #create linear model
yvals <- model$coefficients['vehicle.weight']*mecha_mpg$vehicle.weight +
  model$coefficients['(Intercept)']
plt <- ggplot(mecha_mpg,aes(x=vehicle.weight,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red")
# MPG VS SPOILER ANGLE LIN REGRESSION GRAPH
model <- lm(mpg ~ spoiler.angle,mecha_mpg) #create linear model
yvals <- model$coefficients['spoiler.angle']*mecha_mpg$spoiler.angle +
  model$coefficients['(Intercept)']
plt <- ggplot(mecha_mpg,aes(x=spoiler.angle,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red")
# MPG VS GROUND CLEARANCE LIN REGRESSION GRAPH
model <- lm(mpg ~ ground.clearance,mecha_mpg) #create linear model
yvals <- model$coefficients['ground.clearance']*mecha_mpg$ground.clearance +
  model$coefficients['(Intercept)']
plt <- ggplot(mecha_mpg,aes(x=ground.clearance,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red")
# MPG VS AWD LIN REGRESSION
model <- lm(mpg ~ AWD,mecha_mpg) #create linear model
yvals <- model$coefficients['AWD']*mecha_mpg$AWD +
  model$coefficients['(Intercept)']
plt <- ggplot(mecha_mpg,aes(x=AWD,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red")


# ANOVA testing for  statistical difference btw. means of different samples
summary(aov(mpg ~ spoiler.angle + ground.clearance + vehicle.weight + 
              vehicle.length + AWD,data=mecha_mpg))


# ANALYSIS
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=mecha_mpg)
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=mecha_mpg))

lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=mecha_mpg)
summary(lm(mpg ~ vehicle.length  + ground.clearance,data=mecha_mpg))



# SUSPENSION
suspension <- read.csv('Suspension_Coil.csv')
# by lot
suspension_summary <- suspension %>% group_by(Manufacturing_Lot) %>% 
            summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), 
            Variance_PSI = var(PSI), SD_PSI=sd(PSI))
plt <- ggplot(suspension_summary,aes(x=Manufacturing_Lot,y=Mean_PSI)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="Manufacturing Lot",y="Mean PSI") + #add scatter plot with labels
  geom_errorbar(aes(ymin=Mean_PSI-SD_PSI,ymax=Mean_PSI+SD_PSI))
# overall
full_suspension_summary <- suspension %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), 
            Variance_PSI = var(PSI), SD_PSI=sd(PSI))
plt <- ggplot(full_suspension_summary,aes(x=1,y=Mean_PSI)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="All Suspension Coils",y="Mean PSI") + #add scatter plot with labels
  geom_errorbar(aes(ymin=Mean_PSI-SD_PSI,ymax=Mean_PSI+SD_PSI))


# T test
t.test(suspension$PSI, mu=1500, alternative = 'two.sided')
t.test(suspension$PSI, mu=1500, alternative = 'less')
t.test(suspension$PSI, mu=1497, alternative = 'greater')


# Comparison analysis
mecha_summary <- mecha_mpg %>% summarize(Mean_MPG=mean(mpg), 
                                         Media_MPG=median(mpg), 
                                         Mean_Weight = mean(vehicle.weight), 
                                         Mean_Length=mean(vehicle.length))

                                         