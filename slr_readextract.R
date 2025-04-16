# Read and extract sea level rise data
# Author: John Voorheis, edited by Kyle Addison
# Last update: 10/5/22

# Reset environment, load packages, create directory
rm(list = ls())

packages <- c('tidyverse', 'sf', 'rgdal')
not_installed <- packages[!(packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)
lapply(packages, require, character.only = TRUE)

dir <- ('C:/Users/kgr2vr/The Lab Dropbox/Kyle Addison/Sea_Level_Rise/')

# Load list of raw folder names
all_gdb <- list.files(paste0(dir, '/Data_Raw'))
all_gdb <- all_gdb[grepl('.gdb', all_gdb)]
stub <- gsub('.gdb', '', all_gdb)

# Define first process function
# Acts as a "first pass" and processes data that have straightforward file names
process <- function(X) {
  file_gdb <- paste0(dir, '/Data_Raw/', X, '.gdb')
  fc_list <- ogrListLayers(file_gdb)
  layer_temp <- paste0(gsub('_slr_final_dist', '', X), '_slr_', R, 'ft')
  
  if(!file.exists(paste0(dir, '/', X, '_', R, 'ft.rda')) & layer_temp %in% fc_list){
    slr_df_Rft <- readOGR(dsn = file_gdb, layer = layer_temp, dropNULLGeometries = T)
    saveRDS(slr_df_Rft, file = paste0(dir, '/Output/', R, 'ft/', X, '_', R, 'ft.rda'))}}

# Define function to find irregular file names
# Identifies file names that are irregular and aggregates them to vector, saves as file
find_irr <- function() {
  irregular <- c()
  for (X in stub) {
    file_gdb <- paste0(dir, '/Data_Raw/', X, '.gdb')
    fc_list <- ogrListLayers(file_gdb)
    layer_temp <- paste0(gsub('_slr_final_dist', '', X), '_slr_', R, 'ft')
    
    if(!layer_temp %in% fc_list){
      irregular <- c(irregular, X)}}
  saveRDS(irregular, file = paste0(dir, '/Output/', R, 'ft/irregular', R, 'ft.rda'))
  return(irregular)}

# Define second process function for irregular names
# Acts as a "second pass" and processes identified irregular file names
process_i <- function(X) {
  file_gdb <- paste0(paste0(dir, '/Data_Raw/'), X, '.gdb')
  fc_list <- ogrListLayers(file_gdb)
  print(fc_list)
  all_Rft <- fc_list[grepl(paste0('slr_', R, 'ft'), fc_list)]
  
  if(!file.exists(paste0(dir, '/Output/', R, 'ft/', X, '_', R, 'ft.rda'))){
    # Condition for irregular file names with two frames for given R level
    if(length(all_Rft) == 2){
      slr_df_Rft <- readOGR(dsn = file_gdb, layer = all_Rft[1], dropNULLGeometries = T)
      slr_df_Rft_try <- readOGR(dsn = file_gdb, layer = all_Rft[2], dropNULLGeometries = T)
      
      # Converts to shapefile for compatability testing
      slr_df_Rft <- st_as_sf(slr_df_Rft)
      slr_df_Rft_try <- st_as_sf(slr_df_Rft_try)
      
      name_slr_df_Rft <- rownames(slr_df_Rft)
      name_slrdf_Rft_try <- rownames(slr_df_Rft_try)
      
      # Checks if variable names are the same, makes the same if not
      if (identical(name_slr_df_Rft, name_slrdf_Rft_try) != TRUE) {
        names(slr_df_Rft) <- c("Id", "gridcode", "Shape_Length", "Shape_Area", "geometry")
        names(slr_df_Rft_try) <- c("Id", "gridcode", "Shape_Length", "Shape_Area", "geometry")}
      
      # Checks if CRS are the same, makes the same if not
      if (st_crs(slr_df_Rft) != st_crs(slr_df_Rft_try)) {
        slr_df_Rft_try <- st_transform(slr_df_Rft_try, st_crs(slr_df_Rft))} 
      
      # Combines both layers, converts back to SPDF, and saves
      slr_df_Rft <- rbind(slr_df_Rft, slr_df_Rft_try)
      slr_df_Rft <- as_Spatial(slr_df_Rft)
      saveRDS(slr_df_Rft, file=paste0(dir, '/Output/', R, 'ft/', X, '_', R, 'ft.rda'))}
    
    # Condition for file names that are irregular but only one frame for given R level
    if(length(all_Rft) == 1){
      slr_df_Rft <- readOGR(dsn = file_gdb, layer = all_Rft[1], dropNULLGeometries = T)
      saveRDS(slr_df_Rft, file=paste0(dir, '/Output/', R, 'ft/', X, '_', R, 'ft.rda'))}}}

# Execute processing for 1ft-10ft
for (i in 1:10) {
  R <- i
  process(X = stub)
  irregular <- find_irr()
  process_i(X = irregular)}

