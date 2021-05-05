# SPDT
A package to support SPDT data analyses. 

# Installation
Install with devtools package from PAskey github account

```R
install.packages("devtools")
devtools::install_github("PAskey/SPDT").
```

# Usage
You must first ensure your vpn is connected and running.
If vpn is running, then SPDT functions will connect to the SLD and bring data into your RStudio environment.
The primary function is SPDTdata(), which will bring up all SPDT type data (clipped fish), in standardized and cleaned format.

```R
library(SPDT)
SPDTdata()
```