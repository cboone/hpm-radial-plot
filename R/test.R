# Assumes that the current working directory is hpm-radial-plot.
# 
source("R/radial.plot.R")

# hpm.radial.plot(1:365)
# hpm.radial.plot(3:11, exact.range = FALSE)
# hpm.radial.plot(3:11)
# hpm.radial.plot(3:11, radial.range.labels = TRUE, horizontal.range.labels = FALSE)
# hpm.radial.plot(3:11, start = pi / 4)
hpm.radial.plot(3:11, start = pi / 4, radial.range.labels = TRUE)
# hpm.radial.plot(3:11, plot.type = "s p")
# hpm.radial.plot(3:11, plot.type = "s p", horizontal.labels = TRUE)
