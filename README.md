# Mecha Car - RStudio
The sample data set (provided) involves car information such as vehicle weight, length, and mpg. Using multiple linear regression and relevant determined factors, the mpg can be approximated using a probability metric (r^2). The suspension coil data can also be scrutinized to determine the success of variance specification (|var| < 100 PSI). Overall, R is used here to perform the necessary statistical tests for hypothesis testing, linear regression modeling for predictions, and confidence interval verification.


## MPG Analysis
From an individual standpoint, the relevant data that showed correlation were ground clearance and vehicle length. Both were positive correlations, interpreted as an increase in ground clearance or vehicle length leads to an incerease in mpg. Using ANOVA (analysis of variance) testing, it can be corroborated the ground celarance and vehicle lengths have stronger correlations than the other factors. 

Some visuals pertaining to the single factor comparison model can be seen in the list below, such as [lengthVSmpg.png](lengthVSmpg.png) for a representation of linear regression.
- [spoilerVSmpg.png](spoilerVSmpg.png)
- [lengthVSmpg.png](lengthVSmpg.png)
- [clearanceVSmpg.png](clearanceVSmpg.png)
- [awdVSmpg.png](awdVSmpg.png)
- [weightVSmpg.png](weightVSmpg.png)

When using multiple linear regression, the r^2 value for this model is ~0.674, indicating that there is a 67% probability that future points will fit the linear model. The p-value for this is incredibly low (3.6e^-12), meaning that we can reject the null hypothesis that there is no correlation between these factors (vehicle length, ground clearance) and mpg. When using all factors, the r^2 value goes up to 0.71% and the p value remains fairly low (5.35e^-11). This model could be used alternatively to estimate with slightly less precision but in a closer ballpark (statistically favorable).

This model shows moderate effectiveness, as 7 out of 10 times is a fair amount of times. However, it should be noted that the model is expected to fail 3 times out of 10, making the correlation somewhat less than strong.

## Suspension Coil Analysis
The manufacturing team's capabilities were tested by reviewing the variance and spread of suspension coil specifications. The specs dictated that the variance of the coils should not exceed 100 pounds per inch. As can be seen in the [suspensionByLot.png](suspensionByLot.png) image or suspension_summary table, this was achieved in the first two lots, but not in the third. However, as an entire entity, manufacturing managed to get the variance under 100 PSI, as can be seen in the [allSuspensionCoils.png](allSuspensionCoils.png) image or full_suspension_summary table.

Some T-tests were also performed in order to determine whether the suspension coil's pound per inch results were statistically different from the mean population results of 1500 pounds per inch. The results showed a 95% confidence interval between 1497.5 and 1500 when performing a two-sided T-test (still one sample). When performing 2 one-sided T-tests, there was a a 95% confidence interval of the mean being between negative infinity and 1499.8 (p = 0.03), and a 95% confidence interval that it would be between 1497.7 and infinity (p = 0.003). The lower p value implies that we are more confident it is greater than 1497 than we are that it is less than 1500, but we are fairly confident of both.

## Comparison Study
The mecha car will have 45 miles per gallon (MPG) on average. This is based on a sample of 50 prototypes. Comparatively, according to fueleconomy.gov, the 2020 average MPG for fuel economy leaders (page 9) is 35.4 excluding all electric vehicles such as the Tesla Model S and the Volkswagen e-Golf. This is a strong indication that the mecha cars will be considered a top end product in terms of fuel economy, and therefore may be priced accordingly. A multiple sample T-test can be used to compare the mean mpg across industries, and ANOVA (analysis of variance) testing can be done to analyze variance from the respective mean for each industry. Using various known factors such as fuel efficiency or vehicle type, a pricepoint could be estimated using multiple factor linear regression. If the pricepoint was determined or assumed for analysis, one could also look at how price coupled with mpg and the other factors impacted vehicle sale success using a similar multiple factor linear regression.
