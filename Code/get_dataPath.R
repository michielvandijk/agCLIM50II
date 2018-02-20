#'=================================================================================================
#' Project:  General
#' Subject:  Get data path 
#' Author:   Tom Morley
#' Contact:  tomas.morley@wur.nl
#' Output:   Correct datapath for user
#'=================================================================================================

# Use this file to set your path to the data
# check your computer username using
# Sys.info()["user"] and use this in the if
# statement. Then add your dataPath within 
# the {} brackets

# Michiel WEcR
if(Sys.info()["user"] == "dijk158") {
  dataPath <- "D:\\R\\agCLIM50II"}

if(Sys.info()["user"] == "dijk158") {
  GAMSPath <- "C:\\24.9"}



# Michiel IIASA
if(Sys.info()["user"] == "vandijkm") {
  dataPath <- "C:/Users/vandijkm/Dropbox/AgClim50 scenario results/AgMIP  Phase 2 output (restricted access)"}

if(Sys.info()["user"] == "vandijkm") {
  GAMSPath <- "C:/GAMS/win64/24.9"}
