# Surface elevation data
install.packages("geoR")
require(geoR)
data(elevation)
str(elevation)   # List of coordinates!
# Create data frame from 'elevation'
elevation.df = data.frame(x = 50 * elevation$coords[,"x"], 
                          y = 50 * elevation$coords[,"y"],
                          z = 10 * elevation$data)
# We extract the x and y grid coordinates and the height values, 
# multiplying them by 50 and 10 respectively to convert to feet for the graphs.
# Create surface for grid coordinate points
elevation.loess = loess(z ~ x*y, data = elevation.df, degree = 2, span = 0.25)
# Extract height at coordinate points
elevation.fit = expand.grid(list(x = seq(10, 300, 1), y = seq(10, 300, 1)))
# predict() as estimate surface height of coordinate x,y of created surface region data frame
z = predict(elevation.loess, newdata = elevation.fit)
# Heights as single column from z 
elevation.fit$Height = as.numeric(z)
elevation.fit$Height

ggplot(elevation.fit, aes(x, y, fill = Height)) + geom_tile() +
  xlab("X Coordinate (feet)") + ylab("Y Coordinate (feet)") +
  ggtitle("Surface elevation data") +
  scale_fill_gradient(limits = c(7000, 10000), low = "white", high = "black") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))