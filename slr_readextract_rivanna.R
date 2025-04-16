# Read and extract sea level rise data
# Author: John Voorheis, edited by Kyle Addison
# Last update: 10/4/22

# Reset environment, load packages, create directory
rm(list = ls())

library(tidyverse)
library(rgdal)
library(parallel)
library(sf)

dir <- getwd()

# Load list of raw files
all_gdb <- list.files(paste0(dir, '/Data_Raw'))
all_gdb <- all_gdb[grepl('.gdb', all_gdb)]
stub <- gsub('.gdb', '', all_gdb)

# Define first process function
process <- function(X) {
  file_gdb <- paste0(dir, '/Data_Raw/', X, '.gdb')
  fc_list <- ogrListLayers(file_gdb)
  layer_temp <- paste0(gsub('_slr_final_dist', '', X), '_slr_', R, 'ft')
  
  if(!file.exists(paste0(dir, '/', X, '_', R, 'ft.rda')) & layer_temp %in% fc_list){
    slr_df_Rft <- readOGR(dsn = file_gdb, layer = layer_temp, dropNULLGeometries = T)
    saveRDS(slr_df_Rft, file = paste0(dir, '/Output/', R, 'ft/', X, '_', R, 'ft.rda'))}}

# Define function to find irregular names
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
process_i <- function(X) {
  file_gdb <- paste0(paste0(dir, '/Data_Raw/'), X, '.gdb')
  fc_list <- ogrListLayers(file_gdb)
  print(fc_list)
  all_Rft <- fc_list[grepl(paste0('slr_', R, 'ft'), fc_list)]
  
  if(!file.exists(paste0(dir, '/Output/', R, 'ft/', X, '_', R, 'ft.rda'))){
    
    if(length(all_Rft) == 2){
      slr_df_Rft <- readOGR(dsn = file_gdb, layer = all_Rft[1], dropNULLGeometries = T)
      slr_df_Rft_try <- readOGR(dsn = file_gdb, layer = all_Rft[2], dropNULLGeometries = T)
      
      slr_df_Rft <- st_as_sf(slr_df_Rft)
      slr_df_Rft_try <- st_as_sf(slr_df_Rft_try)
      
      name_slr_df_Rft <- rownames(slr_df_Rft)
      name_slrdf_Rft_try <- rownames(slr_df_Rft_try)
      
      if (identical(name_slr_df_Rft, name_slrdf_Rft_try) != TRUE) {
        names(slr_df_Rft) <- c("Id", "gridcode", "Shape_Length", "Shape_Area", "geometry")
        names(slr_df_Rft_try) <- c("Id", "gridcode", "Shape_Length", "Shape_Area", "geometry")}
      
      if (st_crs(slr_df_Rft) != st_crs(slr_df_Rft_try)) {
        slr_df_Rft_try <- st_transform(slr_df_Rft_try, st_crs(slr_df_Rft))} 
      
      slr_df_Rft <- rbind(slr_df_Rft, slr_df_Rft_try)
      slr_df_Rft <- as_Spatial(slr_df_Rft)
      saveRDS(slr_df_Rft, file=paste0(dir, '/Output/', R, 'ft/', X, '_', R, 'ft.rda'))}
    
    if(length(all_Rft) == 1){
      slr_df_Rft <- readOGR(dsn = file_gdb, layer = all_Rft[1], dropNULLGeometries = T)
      saveRDS(slr_df_Rft, file=paste0(dir, '/Output/', R, 'ft/', X, '_', R, 'ft.rda'))}}}

# Parallelization set up
numCores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1
options(mc.cores = numCores)

# Run first and second processes
for (i in 1:10) {
  R <- i
  mclapply(X = stub, process)
  irregular <- find_irr()
  mclapply(X = irregular, process_i)}

