Package with functions to manipulate, extract, visualize and analyze sharkPulse data. 


## Installation

You can install the package directly from this GitHub repo using:

`devtools::install_github("sharkPulse/sharkPulseR")`


and load the data using the function `getSharkPulse()` with the argument `external = TRUE`

```
dat <- getSharkPulse(dbuser="spr",dbpass="spr_pass", external = TRUE)

```