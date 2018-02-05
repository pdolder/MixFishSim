#############################################################
#############################################################

## VAST fit to simulated data from MixFishSim

# 05/02/2018

#############################################################
#############################################################

# Install TMB
# Must be installed from: https://github.com/kaskr/adcomp

# Install INLA
# Must be installed from: http://www.r-inla.org/download

#If Install geostatistical delta-GLMM package
if(!"VAST" %in% installed.packages()[,1]) devtools::install_github("james-thorson/VAST")
if(!"ThorsonUtilities" %in% installed.packages()[,1]) devtools::install_github("james-thorson/utilities")

# Load libraries
library(TMB)
library(ThorsonUtilities)
library(VAST)

library(INLA)
##INLA:::inla.dynload.workaround() ## Needed on older linux machines 

library(MixFishSim)

mod <- 'M0'
run <- mod 

# This is where all runs will be located
DateFile  <- file.path('.','results',paste(Sys.Date(),'_',run,'/', sep = ""))

dir.create(DateFile)

###############
# Settings
###############

#########################
### VAST CPP version ###
 # Version = "VAST_v2_8_0"
Version = "VAST_v4_0_0"

########################
## Spatial settings ###
########################
  Method = c("Grid", "Mesh")[2]
  n_x = c(10, 50, 100, 250, 500, 1000, 2000)[3] # Number of stations
  Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )     # Samples: Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
  strata.limits <- data.frame('STRATA'="All_areas") # Decide on strata for use when calculating indices
  Region = "Mixtopia"# Determine region
  Catch_units <- 'Kg'
#  max_dist <- 50
########################
#### Model settings ####
########################

  FieldConfig = c("Omega1"= 4, "Epsilon1"= 4, "Omega2"= 4, "Epsilon2"= 4) # 1=Presence-absence; 2=Density given presence; #Epsilon=Spatio-temporal; #Omega=Spatial
  RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) # Structure for beta or epsilon over time: 0=None (default); 1=WhiteNoise; 2=RandomWalk; 3=Constant
  ObsModel = c(2,0)  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
  OverdispersionConfig = c("eta1" = 0,"eta2" = 0) # 0 - number of factors
  BiasCorr = FALSE 
#######################
##### Save options ####
#######################
  # Save options for future records
  Record = ThorsonUtilities::bundlelist( c("Version","Method","n_x","FieldConfig","RhoConfig", "ObsModel", "OverdispersionConfig", "Kmeans_Config","Catch_units","BiasCorr","Region","strata.limits") )
  capture.output( Record, file=paste0(DateFile,"Record.txt"))
  save(Record, file=paste0(DateFile,"Record.RData"))

######################
#### Prepare data ####
######################

load(file.path("..", "..","tests", "TestResults.RData"))

logs <- combine_logs(res[["fleets_catches"]])

logs2 <- reshape2::melt(as.data.frame(logs), id = c("fleet", "vessel", "x", "y", "stepD", "angles", "day", "tow", "trip", "month", "year"))

logs2 <- logs2[logs2$variable %in% c("spp1","spp2","spp3","spp4"),]


  ac <- as.character
  DF <- data.frame(Survey        = paste(logs2$fleet, logs2$vessel, sep = "_"),
		   Year          = logs2$year,
		   Station       = 1:nrow(logs2),
		   Lat           = logs2$y,
		   Lon           = logs2$x,
		   AreaSwept_km2 = 1,
		   spp           = logs2$variable,
		   Kg            = logs2$value)

 species <- sort(unique(DF$spp)) 

  DF$SpeciesName <- factor(DF$spp) # drop empty factors
  DF$Ship        <- factor(DF$Survey)
  DF$Year        <- factor(DF$Year)

  ## Sample the data for a shortened run
  # let's sample 100 locations a year
  DF$sample_tag <- rep(seq_len(nrow(DF)/4/20), times = 4 * 20)   ## number of species, number of years
  sample_no <- unique(DF$sample_tag)
  DF <- DF[DF$sample_tag %in% sample(sample_no, 1000), ]


  an <- as.numeric
  Data_Geostat = data.frame("spp"=DF[,"SpeciesName"], 
		       "Year"=DF[,"Year"], 
		       "Catch_KG"=DF[,"Kg"], 
		       "AreaSwept_km2"= 1, 
		       "Vessel"= DF[,'Ship'] ,
		       "Lat"=DF[,"Lat"], 
		       "Lon"=DF[,"Lon"] )


  # Convert to psudo lat/lon
  Data_Geostat[,"Lat"] <- ((Data_Geostat[,"Lat"] - 1) * 0.05) + 48
  Data_Geostat[,"Lon"] <- ((Data_Geostat[,"Lon"] - 1) * 0.1) + -12

  range(Data_Geostat[,"Lat"])
  range(Data_Geostat[,"Lon"])


##############################
##### Extrapolation grid #####
##############################
  # Get extrapolation data
 Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn(Region=Region, strata.limits=strata.limits, observations_LL=Data_Geostat[,c('Lon','Lat')])

## max dist removed

  # Calculate spatial information for SPDE mesh, strata areas, and AR1 process
  Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn(n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=DateFile )


 


#### Prep data
Data_Geostat = cbind(Data_Geostat, Spatial_List$loc_i, "knot_i"=Spatial_List$knot_i)


################################
#### Make and Run TMB model ####
################################
  # Make TMB data list

## Note - removed the habitat covariates for testing !!!

# M0 - no covariates, random vessel
  TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "OverdispersionConfig" = OverdispersionConfig, "c_i"=as.numeric(Data_Geostat[,'spp'])-1, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_iz"=as.numeric(Data_Geostat[,'Year']), "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method)

  # Make TMB object
#  dyn.unload( paste0(DateFile,"/",dynlib(TMB:::getUserDLL())))
  setwd(DateFile) # so executables go to the right place...

# Parameters with fixed gear estimates and a map
  TmbList = Build_TMB_Fn("TmbData"=TmbData, "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_xi)
  Obj = TmbList[["Obj"]]

  ############
  # Run model
  ###########
  Opt = TMBhelper::Optimize(obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=file.path("..","..",DateFile), bias.correct=BiasCorr) 
  Report = Obj$report()

#################################  
######## Save outputs ###########
#################################
Save = list(Obj = Obj,"Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData, "Data_Geostat" = Data_Geostat)
save(Save, file=file.path('..',"..",DateFile,"Save.RData"))

  # Plot index
  SpatialDeltaGLMM::PlotIndex_Fn( DirName=file.path("..","..",DateFile), TmbData=Save$TmbData, Sdreport=Save$Opt$SD, Year_Set=seq(min(an(as.character(DF[,'Year']))),max(an(as.character(DF[,'Year'])))), strata_names=strata.limits[,1], category_names=levels(DF[,'SpeciesName']), use_biascorr=BiasCorr, cex = 0.3)



