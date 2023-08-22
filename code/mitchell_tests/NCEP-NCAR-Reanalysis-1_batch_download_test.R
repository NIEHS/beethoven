###############################################################################
## test script to batch download data from NCEP-NCAR Reanalysis 1
## Mitchell Manware
## August 22, 2023


## set working directory
wd <- "/ddn/gs1/group/set/Projects/NRT-AP-Model/code"
setwd(wd)

## define base variable for building urls
base <- "https://psl.noaa.gov/thredds/fileServer/Datasets/ncep.reanalysis/Dailies/"

## define list of years
## FOR REPRODUCABILITY, ONLY THE YEARS OF INTEREST WOULD NEED TO BE CHANGED ##
years <- c("2018", "2019", "2020", "2021", "2022")

## define list of variables obtained from NCEP-NCAR Reanalysis 1
variables <- c("surface_gauss/air.2m.gauss.",     # air temperature (surface)
               "surface_gauss/dswrf.sfc.gauss.",  # downward shortwave radiation flux NEEDS TO BE CONFIRMED
               "surface/pr_wtr.eatm.",            # precipitable water for the entire atmosphere
               "surface/pres.sfc.",               # pressure (surface)
               "surface_gauss/shum.2m.gauss.",     # specific humidity at 2 m
               # "",     # u-wind at 10 m
               # "",     # v-wind at 10 m
               "surface_gauss/prate.sfc.gauss.",  # precipitation rate
               "surface_gauss/lhtfl.sfc.gauss.",  # latent heat flux
               "surface_gauss/shtfl.sfc.gauss.",  # sensible heat flux
               # "",       # upward longwave radiation on flux
               # "",       # omega: a term used to describe vertical motion in the atmosphere
               "other_gauss/tdc.eatm.gauss."       # cloud coverage
               )

## for loop to download data for each variable
for(v in 1:length(variables)){
  
  ## define current variable
  variable <- variables[v]
  
  for(y in 1:length(years)){
  
    ## define url
    url <- paste0(base, variable, years[y], ".nc")
    
    ## download data using the system() function and 'wget' linux command
    ## save data in the 'NCEP-NCAR-Reanalysis-1' data folder
    # system(paste0("wget --p /ddn/gs1/group/set/Projects/NRT-AP-Model/input/NCEP-NCAR-Reanalysis-1 ",
    #               url,
    #               " --no-check-certificate"))
    
    ## print url
    print(paste0("wget --no-e-P /ddn/gs1/group/set/Projects/NRT-AP-Model/input/NCEP-NCAR-Reanalysis-1 ",
                 url,
                 " --no-check-certificate"))
    
  }
}




