
install.packages("devtools")
require(devtools)
install_github("BeeIT", "ibartomeus")
library(BeeIT)

#dummy dataset
its <- rnorm(100, 10, 2)
families <- rep(c("Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae"),20)
Out <- ITconverter(IT = its, family = families)
plot(Out$tongue_length.tongue ~ Out$body_mass)