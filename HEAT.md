# ETC-ICM trends Mersin

### HEAT Method 

The HEAT calculations compare observed values with target values to calculate eutrophication ratio. The file [targets.R](/targets.R) generates a dataframe with target values.

###### **Table of target values** 

Parameter | Months | Value | Response
------------ | ------------ | ------------- | -------------
TP [µM] | Jan-Mar | 0.23 | +ve
NO3NO2 [µM] | Jan-Mar | 0.71 | +ve  
Chla [µM] | Apr-May | 1.00 | +ve
SD [m]  | Apr-Sep | 9.00 | -ve

[Tugrul et al. (2018)](https://link.springer.com/article/10.1007%2Fs11356-018-2529-6)

For each profile at a particular position on a particular date, we take the maximum value of chl or nutrient concentration at depths of 10 m or less. For example, if there was a Chl *a* measurement at 0 m and also one at 5 m, then we take the greatest of the two values. 

Now we have a single value of each parameter for each profile. We then collect all values of the parameter within the assessment unit from the relevant months (according to the table of target values) and calculate an average value for each combination of Year, Parameter and Assessment unit.

The HEAT assessment is made by comparing the mean parameter value with the target values. The calculations are done using  [Mersin_HEAT.R](/Mersin_HEAT.R) . First, data from the cruises is matched to assessment units shown in the introduction [README](/README.md) then the HEAT calculations are dones and plots of the results made.
 
### HEAT Results
The plots of HEAT classification by year in each assessment unit.

![HEAT results for Mersin transect](png/HEAT_transect_Mersin.png)
![HEAT results for Erdemli transect](png/HEAT_transect_Erdemli.png)
