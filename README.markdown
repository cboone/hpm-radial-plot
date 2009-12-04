# Hpm radial.plot(). #

By [Christopher Boone][1].

Experimental radial plotting, in R. Based on [the plotrix package, by Jim Lemon, et al.][2]


## Changes from the original. ##

- A more exact range can be used. The default is `exact.range = TRUE`. For example, compare the output of `hpm.radial.plot(3:11)` to `hpm.radial.plot(3:11, exact.range = FALSE)`.
- Many of the names used in the API have changed, for better clarity.


[1]: http://hypsometry.com
[2]: http://cran.r-project.org/web/packages/plotrix/index.html
