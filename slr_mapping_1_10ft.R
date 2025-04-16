#Test map for 1-10ft SLR

#setup
rm(list=ls())

library(sf)
library(dplyr)
library(ggplot2)
library(tmap)

dir <- 'C:/Users/kgr2vr/The Lab Dropbox/Kyle Addison/Sea_Level_Rise/'

stateshp <- st_read(paste0(dir, 'GIS/states/US_state_2010.shp'))
ctyshp <- st_read(paste0(dir, 'GIS/nhgis0003_shape/US_county_2010.shp'))


# Miami, Florida

flstate <- stateshp %>% filter(STATEFP10 == '12')
flctys <- ctyshp %>% filter(STATEFP10 == '12')
miami_cty <- flctys %>% filter(NAME10 == 'Miami-Dade')


FLfiles <- c()
for (i in 1:10) {
  FLfiles <- append(FLfiles, list.files(paste0(dir, 'Output/', i, 'ft'), pattern = 'FL'))}

#load files for each depth level

FLfiles_1ft <- list.files(paste0(dir, 'Output/1ft'), pattern = 'FL')
for (i in 1:length(FLfiles_1ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/1ft/', FLfiles_1ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_1ft[i], 1, nchar((FLfiles_1ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}

FLfiles_2ft <- list.files(paste0(dir, 'Output/2ft'), pattern = 'FL')
for (i in 1:length(FLfiles_2ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/2ft/', FLfiles_2ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_2ft[i], 1, nchar((FLfiles_2ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}

FLfiles_3ft <- list.files(paste0(dir, 'Output/3ft'), pattern = 'FL')
for (i in 1:length(FLfiles_3ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/3ft/', FLfiles_3ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_3ft[i], 1, nchar((FLfiles_3ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}

FLfiles_4ft <- list.files(paste0(dir, 'Output/4ft'), pattern = 'FL')
for (i in 1:length(FLfiles_4ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/4ft/', FLfiles_4ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_4ft[i], 1, nchar((FLfiles_4ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}

FLfiles_5ft <- list.files(paste0(dir, 'Output/5ft'), pattern = 'FL')
for (i in 1:length(FLfiles_5ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/5ft/', FLfiles_5ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_5ft[i], 1, nchar((FLfiles_5ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}

FLfiles_6ft <- list.files(paste0(dir, 'Output/6ft'), pattern = 'FL')
for (i in 1:length(FLfiles_6ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/6ft/', FLfiles_6ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_6ft[i], 1, nchar((FLfiles_6ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}

FLfiles_7ft <- list.files(paste0(dir, 'Output/7ft'), pattern = 'FL')
for (i in 1:length(FLfiles_7ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/7ft/', FLfiles_7ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_7ft[i], 1, nchar((FLfiles_7ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}

FLfiles_8ft <- list.files(paste0(dir, 'Output/8ft'), pattern = 'FL')
for (i in 1:length(FLfiles_8ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/8ft/', FLfiles_8ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_8ft[i], 1, nchar((FLfiles_8ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}

FLfiles_9ft <- list.files(paste0(dir, 'Output/9ft'), pattern = 'FL')
for (i in 1:length(FLfiles_9ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/9ft/', FLfiles_9ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_9ft[i], 1, nchar((FLfiles_9ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}

FLfiles_10ft <- list.files(paste0(dir, 'Output/10ft'), pattern = 'FL')
for (i in 1:length(FLfiles_10ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/10ft/', FLfiles_10ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_10ft[i], 1, nchar((FLfiles_10ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}


#transform and crop
FL_MFL_slr_final_dist_1ft <- st_transform(FL_MFL_slr_final_dist_1ft, st_crs(miami_cty))
miami_slr_1ft <- st_intersection(FL_MFL_slr_final_dist_1ft, miami_cty)

FL_MFL_slr_final_dist_2ft <- st_transform(FL_MFL_slr_final_dist_2ft, st_crs(miami_cty))
miami_slr_2ft <- st_intersection(FL_MFL_slr_final_dist_2ft, miami_cty)

FL_MFL_slr_final_dist_3ft <- st_transform(FL_MFL_slr_final_dist_3ft, st_crs(miami_cty))
miami_slr_3ft <- st_intersection(FL_MFL_slr_final_dist_3ft, miami_cty)

FL_MFL_slr_final_dist_4ft <- st_transform(FL_MFL_slr_final_dist_4ft, st_crs(miami_cty))
miami_slr_4ft <- st_intersection(FL_MFL_slr_final_dist_4ft, miami_cty)

FL_MFL_slr_final_dist_5ft <- st_transform(FL_MFL_slr_final_dist_5ft, st_crs(miami_cty))
miami_slr_5ft <- st_intersection(FL_MFL_slr_final_dist_5ft, miami_cty)

FL_MFL_slr_final_dist_6ft <- st_transform(FL_MFL_slr_final_dist_6ft, st_crs(miami_cty))
miami_slr_6ft <- st_intersection(FL_MFL_slr_final_dist_6ft, miami_cty)

FL_MFL_slr_final_dist_7ft <- st_transform(FL_MFL_slr_final_dist_7ft, st_crs(miami_cty))
miami_slr_7ft <- st_intersection(FL_MFL_slr_final_dist_7ft, miami_cty)

FL_MFL_slr_final_dist_8ft <- st_transform(FL_MFL_slr_final_dist_8ft, st_crs(miami_cty))
miami_slr_8ft <- st_intersection(FL_MFL_slr_final_dist_8ft, miami_cty)

FL_MFL_slr_final_dist_9ft <- st_transform(FL_MFL_slr_final_dist_9ft, st_crs(miami_cty))
miami_slr_9ft <- st_intersection(FL_MFL_slr_final_dist_9ft, miami_cty)

FL_MFL_slr_final_dist_10ft <- st_transform(FL_MFL_slr_final_dist_10ft, st_crs(miami_cty))
miami_slr_10ft <- st_intersection(FL_MFL_slr_final_dist_10ft, miami_cty)


#mapping
miami1_10ft <- {
  ggplot()+
    geom_sf(data = miami_cty, fill = '#D0D2D4', color = NA)+
    geom_sf(data = miami_slr_10ft, aes(fill = '10ft'), color = NA)+
    geom_sf(data = miami_slr_9ft, aes(fill = '9ft'), color = NA)+
    geom_sf(data = miami_slr_8ft, aes(fill = '8ft'), color = NA)+
    geom_sf(data = miami_slr_7ft, aes(fill = '7ft'), color = NA)+
    geom_sf(data = miami_slr_6ft, aes(fill = '6ft'), color = NA)+
    geom_sf(data = miami_slr_5ft, aes(fill = '5ft'), color = NA)+
    geom_sf(data = miami_slr_4ft, aes(fill = '4ft'), color = NA)+
    geom_sf(data = miami_slr_3ft, aes(fill = '3ft'), color = NA)+
    geom_sf(data = miami_slr_2ft, aes(fill = '2ft'), color = NA)+
    geom_sf(data = miami_slr_1ft, aes(fill = '1ft'), color = NA)+
    scale_fill_manual(name = 'Water Level', 
                      breaks = c('1ft', '2ft', '3ft', '4ft', '5ft', '6ft', '7ft', '8ft', '9ft', '10ft'),
                      values = c('1ft' = '#bbdffb', '2ft' = '#90cbf9', '3ft' = '#64b7f6', '4ft' = '#41a7f5', '5ft' = '#1e97f3', '6ft' = '#1a8ae5', '7ft' ='#1477d2', '8ft' = '#1065c0', '9ft' = '#0747a1', '10ft' = '#012454'))+
    #labs(title = 'Miami Sea Level Rise')+
    theme_void()+
    theme(
      text = element_text(color = "#22211d"), 
      panel.background = element_rect(fill = "#FFFFFF", color = NA), 
      
      plot.background = element_rect(fill = "#FFFFFF", color = NA), 
      plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.margin = margin(l=0, unit = "pt"),
      legend.position = 'right',
      legend.direction = 'vertical',
      legend.text=element_text(size=10),
      legend.title=element_text(size=15),
    )
}

miami1_10ft
ggsave(paste0(dir, 'Graphics/miami_1_10_test.png'), dpi = 1200)








