
library(MASS)

bivariate_p = 2
mu = c(0, 2)
x_variance = 2.0
cor = -0.75
n = 200
varX1 = 2.0
varX2 = 3.0

# Calculate covariance
covX1X2 = (cor*sqrt(varX1*varX2))
print(covX1X2)


data = mvrnorm(n, mu, Sigma = matrix(c(varX1, covX1X2, covX1X2, varX2), ncol = 2, byrow = TRUE))
# The R functions mvrnorm (from library MASS), cov, and cor should do the job.

empcov = cov(data)
empcor = cor(data)

print(empcov)
print(empcor)
print(covX1X2)


plot(data)
# data[,1]
kde = kde2d(data[,1], data[,2], n = n)

colours = c("red", "green", "purple", "yellow", "blue")

image(kde)

contour(kde, add = TRUE, col = colours)

persp(kde)

# install.packages("mvtnorm")
require(mvtnorm)

mygrid = expand.grid(.25*(-20:20),.25*(-20:20))


# solve(mygrid)
 densitydata = dmvnorm(mygrid, mean = mu, covX1X2, log = FALSE)
#densitydata = dmvnorm(mygrid, mu, covX1X2, logged = FALSE)

# tästä matriisi matrix(densitydata, nrow=41)

# contour, image, and persp 


