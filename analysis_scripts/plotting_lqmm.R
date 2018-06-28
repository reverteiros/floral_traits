## this script plots lqmm outputs
require(plotrix)
#quartz()
# first plot the raw data
plot(joes_data$depth, joes_data$meantongue)
# abline for 0.10 quantile
ablineclip(4.313942, 0.490185, col="red")
# abline for 0.5 quantile
ablineclip(2.907355, 0.458117, col="orange")
# abline for 0.75 quantile
ablineclip(4.96868, 0, col="green")
