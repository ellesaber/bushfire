
#### Data Processing ####
source("script/process_ncdf.R")
  # subsets ncdf files for Victoria only and required time period
  # produces vic.dt and dt.full datatables of variables
source("script/process_fire.R")
  # reads in csv of fires 
  # converts AMG coords to lon/lat
  # matches fires to WRF coordinate
  # combines fires with WRF dataset for the fire dummy
  # subsets to only fire seasons
source("script/process_curing.R")
  # downloads curing files
  # extracts median value for 5km radius around each WRF gridpoint 
  # adds into weather dataset
source("script/process_rain.R")
  # downloads precip files
  # extracts median value for 5km radius around each WRF gridpoint 
  # adds into weather dataset 
source("script/process_vapour.R")
  # downloads vapour pressure files
  # extracts median value for 5km radius around each WRF gridpoint 
  # adds into weather dataset

# end result: dat0 datatable 
# contains all variables for fire seasons between 2000 and 2010



#### Model Building ####

source("model_sampling")
# create test and train for each district
# create balanced samples from training set

#### Data Visualisation #### 
source("plot_densities")

source("model_logit")


#### Error Analysis ####



