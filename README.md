# SPDT
A package to support SPDT data analyses. 

# Installation
Install with devtools package from PAskey github account

```R
#First install the devtools package from CRAN with:
install.packages("devtools")

#Next install the SPDT package from PAskey Github repo as:
devtools::install_github("PAskey/SPDT").
```

# Usage
You must first ensure your **VPN is connected and running.**
If vpn is running, then SPDT functions will connect to the SLD and bring data into your RStudio environment.
The primary function is SPDTdata(), which will bring up all SPDT type data (clipped fish), in standardized and cleaned format.

```R
library(SPDT)
SPDTdata()
```

# Main functions and uses

  * **SPDTdata()** The main function to use from the package is SPDTdata(), 
  which will upload all necessary SLD data for SPDT type analyses. The main dataframes of interest are:
  **"idf"**, which is the individual level data frame with a row for each fish, and **"gdf"** which is grouped by
  age and clip.
  
  * In order to get to the SPDTdata() outputs there are two intermittent steps (functions) that are called within SPDTdata(): 1) **SLD2R()** which is the base raw data with some basic cleaning and formatting (dates, etc.), and 2) **linkClips()** which sorts through the releases data and links to the biological data to assign strains, ages, genotypes whenever possible by clip information.
  
  * One important addition to the SPDT data frames idf and gdf, is a correction factor for gillnet selectivity. Within the individual level data, a column called **"NetX"** is added. This value is the expansion factor that will be applied to this fish based on it's length, so that all fish observations can be corrected for gillnet selectivity. NetX = 1/p, where p is relative probability of capture. In the grouped data set(gdf), the NetX values are summed to get the total N for a group corrected by selectivity and added as column **"NetXN"**. This value should be used instead of N (only) if the fish were captured using a RIC standard gillnet. These expansions are calculated using the RICselect() function which is somewhat rustic at this point and will be updated as new mark-recapture information becomes available.
  
  * **RICselect(FLengths)** can be used to estimate the relative vulnerability of a fish to RIC gillnets (value between 0 and 1). It is based on a simple GAM model that is quite smoothed. Since there is little to no information for fish > 400mm, all fish >550 are given a vulnerability of 0.1. A demonstration of the vulnerability function is presented below. You can enter a single length or a vector of lengths into the function parentheses.
  
```R
library(SPDT)

# Create a vector of fish lengths from 50 to 650mm
Fish_lengths = c(50:650)

# Estimate the relative probability of capture for each of the fish lengths
pvals = RICselect(Fish_lengths)

#Plot the selectivity function
plot(pvals~Fish_lengths)

#However, you can access this selectivity funciton directly by simply typing
select_lookup
```

  * **UTM_to_latlong(x,y,z)** is pretty self explanatory and converts UTM (x = easting, y = northing, and z = zone) to lat long if needed.


# Lookup data sets within SPDT

There are a few lookup tables that are used for SPDT calculations and you can access as needed:

```R
library(SPDT)#Ignore this line if package already loaded

#To view the data frame in your console simple type the dataframe name as below.
#To load into your enviroment simply assign it a name e.g. Ages <- Ages

#Converting non-standard age notation to ages
Ages

#Converting "stock_strain_loc_name" in PARIS releases to "Strain"" in Biological table of SLD.
Strain_code_LU

#Lookup relative vulnerbaility to RIC gillnet associated with a given fish length
select_lookup
```