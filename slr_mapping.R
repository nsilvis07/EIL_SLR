# Mapping SLR data
# cities: Miami, Boston, NYC

rm(list=ls())

library(sf)
library(dplyr)
library(ggplot2)
library(tmap)

dir <- 'C:/Users/kgr2vr/The Lab Dropbox/Kyle Addison/Sea_Level_Rise/'

stateshp <- st_read(paste0(dir, 'GIS/nhgis0007_shape/US_state_2010.shp'))
ctyshp <- st_read(paste0(dir, 'GIS/nhgis0003_shape/US_county_2010.shp'))


# Miami, Florida

flstate <- stateshp %>% filter(STATEFP10 == '12')
flctys <- ctyshp %>% filter(STATEFP10 == '12')
miami_cty <- flctys %>% filter(NAME10 == 'Miami-Dade')


FLfiles <- c()
for (i in 1:10) {
  FLfiles <- append(FLfiles, list.files(paste0(dir, 'Output/', i, 'ft'), pattern = 'FL'))}


FLfiles_4ft <- list.files(paste0(dir, 'Output/4ft'), pattern = 'FL')
for (i in 1:length(FLfiles_4ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/4ft/', FLfiles_4ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_4ft[i], 1, nchar((FLfiles_4ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}


FLfiles_7ft <- list.files(paste0(dir, 'Output/7ft'), pattern = 'FL')
for (i in 1:length(FLfiles_7ft)) {
  tempRDS <- readRDS(paste0(dir, 'Output/7ft/', FLfiles_7ft[i]))
  tempSF <- st_as_sf(tempRDS)
  name <- substr(FLfiles_7ft[i], 1, nchar((FLfiles_7ft)[i]) - 4)
  print(name)
  assign(name, tempSF, envir = .GlobalEnv)}


# miami0ft <- {
#   ggplot()+
#     geom_sf(data = miami_cty, fill = 'lightgrey', color = NA)+
#     theme_void()}
# miami0ft

# miami0ft <- {
#   ggplot()+
#     geom_sf(data = miami_cty, fill = '#D0D2D4', color = NA)+
#     scale_fill_manual(name = 'Water Level', 
#                       breaks = c('0ft'),
#                       values = c('0ft' = '#D0D2D4'))+
#     labs(title = 'Miami Sea Level Rise')+
#     theme_void()+
#     theme(
#       text = element_text(color = "#22211d"), 
#       panel.background = element_rect(fill = "#FFFFFF", color = NA), 
#       
#       plot.background = element_rect(fill = "#FFFFFF", color = NA), 
#       plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
#       plot.margin = unit(c(0, 0, -5, 0), "cm"),
#       
#       legend.background = element_rect(fill = "transparent", color = NA),
#       legend.margin = margin(l=-30, unit = "pt"),
#       legend.position = 'right',
#       legend.direction = 'vertical',
#       legend.text=element_text(size=10),
#       legend.title=element_text(size=15),
#     )
# }
# miami0ft


FL_MFL_slr_final_dist_4ft <- st_transform(FL_MFL_slr_final_dist_4ft, st_crs(miami_cty))
miami_slr_4ft <- st_intersection(FL_MFL_slr_final_dist_4ft, miami_cty)
# miami4ft <- {
#   ggplot()+
#     geom_sf(data = miami_cty, aes(color = '0ft'))+
#     geom_sf(data = miami_slr_4ft, aes(color = '4ft'))+
#     scale_colour_manual(name = 'Water Level', breaks = c('0ft', '4ft'), values = c('0ft' = 'lightgrey','4ft' = '#14A4DD'))+
#     theme_void()}
# miami4ft

miami4ft <- {
  ggplot()+
    geom_sf(data = miami_cty, fill = '#D0D2D4', color = NA)+
    geom_sf(data = miami_slr_4ft, aes(fill = '4ft'), color = NA)+
    scale_fill_manual(name = 'Water Level', 
                      breaks = c('0ft', '4ft'),
                      values = c('0ft' = '#D0D2D4', '4ft' = '#14A4DD'))+
    labs(title = 'Miami Sea Level Rise')+
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
miami4ft


FL_MFL_slr_final_dist_7ft <- st_transform(FL_MFL_slr_final_dist_7ft, st_crs(miami_cty))
miami_slr_7ft <- st_intersection(FL_MFL_slr_final_dist_7ft, miami_cty)

# miami7ft <- {
#   ggplot()+
#     geom_sf(data = miami_cty, fill = 'lightgrey', color = NA)+
#     geom_sf(data = miami_slr_7ft, fill = '#1D5D99', color = NA)+
#     theme_void()}
# miami7ft

miami7ft <- {
  ggplot()+
    geom_sf(data = miami_cty, fill = '#D0D2D4', color = NA)+
    geom_sf(data = miami_slr_7ft, aes(fill = '7ft'), color = NA)+
    scale_fill_manual(name = 'Water Level', 
                      breaks = c('0ft', '7ft'),
                      values = c('0ft' = '#D0D2D4', '7ft' = '#1D5D99'))+
    labs(title = 'Miami Sea Level Rise')+
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
miami7ft

# miami4_7ft <- {
#   ggplot()+
#     geom_sf(data = miami_cty, fill = 'lightgrey', color = NA)+
#     geom_sf(data = miami_slr_7ft, fill = '#1D5D99', color = NA)+
#     geom_sf(data = miami_slr_4ft, fill = '#14A4DD', color = NA)+
#     labs(title = 'Miami Sea Level Rise')+
#     theme_void()}
# miami4_7ft


miami4_7ft <- {
  ggplot()+
    geom_sf(data = miami_cty, fill = '#D0D2D4', color = NA)+
    geom_sf(data = miami_slr_7ft, aes(fill = '7ft'), color = NA)+
    geom_sf(data = miami_slr_4ft, aes(fill = '4ft'), color = NA)+
    scale_fill_manual(name = 'Water Level', 
                       breaks = c('4ft', '7ft'),
                       values = c('4ft' = '#14A4DD', '7ft' = '#1D5D99'))+
    labs(title = 'Miami Sea Level Rise')+
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
miami4_7ft

ggsave(paste0(dir, 'Graphics/test.png'), dpi = 300)


# Boston, Massachusetts

rm(list=ls())

dir <- 'C:/Users/kgr2vr/The Lab Dropbox/Kyle Addison/Sea_Level_Rise/'

ctyshp <- st_read(paste0(dir, 'GIS/nhgis0003_shape/US_county_2010.shp'))

boston_cty <- ctyshp %>% filter(NAME10 == 'Suffolk') %>% filter(STATEFP10 == '25')


MAfile_4ft <- list.files(paste0(dir, 'Output/4ft'), pattern = 'MA')
MAfile_4ft <- readRDS(paste0(dir, 'Output/4ft/', MAfile_4ft))
MAfile_4ft <- st_as_sf(MAfile_4ft)

MAfile_4ft <- st_transform(MAfile_4ft, st_crs(boston_cty))
boston_slr_4ft <- st_intersection(MAfile_4ft, boston_cty)

boston4ft <- {
  ggplot()+
    geom_sf(data = boston_cty, fill = '#D0D2D4', color = NA)+
    geom_sf(data = boston_slr_4ft, aes(fill = '4ft'), color = NA)+
    scale_fill_manual(name = 'Water Level', 
                      breaks = c('0ft', '4ft'),
                      values = c('0ft' = '#D0D2D4', '4ft' = '#14A4DD'))+
    labs(title = 'Boston Sea Level Rise')+
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
boston4ft


MAfile_7ft <- list.files(paste0(dir, 'Output/7ft'), pattern = 'MA')
MAfile_7ft <- readRDS(paste0(dir, 'Output/7ft/', MAfile_7ft))
MAfile_7ft <- st_as_sf(MAfile_7ft)

MAfile_7ft <- st_transform(MAfile_7ft, st_crs(boston_cty))
boston_slr_7ft <- st_intersection(MAfile_7ft, boston_cty)

boston7ft <- {
  ggplot()+
    geom_sf(data = boston_cty, fill = '#D0D2D4', color = NA)+
    geom_sf(data = boston_slr_7ft, aes(fill = '7ft'), color = NA)+
    scale_fill_manual(name = 'Water Level', 
                      breaks = c('0ft', '7ft'),
                      values = c('0ft' = '#D0D2D4', '7ft' = '#1D5D99'))+
    labs(title = 'Boston Sea Level Rise')+
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
boston7ft


boston4_7ft <- {
  ggplot()+
    geom_sf(data = boston_cty, fill = '#D0D2D4', color = NA)+
    geom_sf(data = boston_slr_7ft, aes(fill = '7ft'), color = NA)+
    geom_sf(data = boston_slr_4ft, aes(fill = '4ft'), color = NA)+
    scale_fill_manual(name = 'Water Level', 
                      breaks = c('4ft', '7ft'),
                      values = c('4ft' = '#14A4DD', '7ft' = '#1D5D99'))+
    labs(title = 'Boston Sea Level Rise')+
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
boston4_7ft

ggsave(paste0(dir, 'Graphics/Boston4_7.png'), dpi = 300)



# New York, New York

rm(list=ls())

dir <- 'C:/Users/kgr2vr/The Lab Dropbox/Kyle Addison/Sea_Level_Rise/'

ctyshp <- st_read(paste0(dir, 'GIS/nhgis0003_shape/US_county_2010.shp'))

NY_cty <- ctyshp %>% filter(STATEFP10 == '36') %>% filter(NAME10 == 'New York' | NAME10 == 'Kings' | NAME10 == 'Bronx' | NAME10 == 'Richmond' | NAME10 == 'Queens')


NYfile_4ft <- list.files(paste0(dir, 'Output/4ft'), pattern = 'NY')
NYfile_4ft <- readRDS(paste0(dir, 'Output/4ft/NY_Metro_slr_final_dist_4ft.rda'))
NYfile_4ft <- st_as_sf(NYfile_4ft)


NYfile_4ft <- st_transform(NYfile_4ft, st_crs(NY_cty))
NY_slr_4ft <- st_intersection(NYfile_4ft, NY_cty)

NY4ft <- {
  ggplot()+
    geom_sf(data = NY_cty, fill = '#D0D2D4', color = NA)+
    geom_sf(data = NY_slr_4ft, aes(fill = '4ft'), color = NA)+
    scale_fill_manual(name = 'Water Level', 
                      breaks = c('0ft', '4ft'),
                      values = c('0ft' = '#D0D2D4', '4ft' = '#14A4DD'))+
    labs(title = 'New York Sea Level Rise')+
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
NY4ft


NYfile_7ft <- list.files(paste0(dir, 'Output/7ft'), pattern = 'NY')
NYfile_7ft <- readRDS(paste0(dir, 'Output/7ft/NY_Metro_slr_final_dist_7ft.rda'))
NYfile_7ft <- st_as_sf(NYfile_7ft)

NYfile_7ft <- st_transform(NYfile_7ft, st_crs(NY_cty))
NY_slr_7ft <- st_intersection(NYfile_7ft, NY_cty)

NY7ft <- {
  ggplot()+
    geom_sf(data = NY_cty, fill = '#D0D2D4', color = NA)+
    geom_sf(data = NY_slr_7ft, aes(fill = '7ft'), color = NA)+
    scale_fill_manual(name = 'Water Level', 
                      breaks = c('0ft', '7ft'),
                      values = c('0ft' = '#D0D2D4', '7ft' = '#1D5D99'))+
    labs(title = 'New York Sea Level Rise')+
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
NY7ft


NY4_7ft <- {
  ggplot()+
    geom_sf(data = NY_cty, fill = '#D0D2D4', color = NA)+
    geom_sf(data = NY_slr_7ft, aes(fill = '7ft'), color = NA)+
    geom_sf(data = NY_slr_4ft, aes(fill = '4ft'), color = NA)+
    scale_fill_manual(name = 'Water Level', 
                      breaks = c('4ft', '7ft'),
                      values = c('4ft' = '#14A4DD', '7ft' = '#1D5D99'))+
    labs(title = 'New York Sea Level Rise')+
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
NY4_7ft

ggsave(paste0(dir, 'Graphics/NY4_7.png'), dpi = 300)

#ggsave('NY4_7ft_v2.png', dpi = 300)
#ggsave(paste0(dir, 'Graphics/NY4_7.png'), dpi = 300)



