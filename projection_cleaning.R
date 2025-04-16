# script for cleaning slr projection raster data, overlaying with detailed slr shapefiles

# set up
rm(list=ls())

library(dplyr)
library(janitor)
library(haven)
library(raster)
library(sf)
library(terra)
library(ggplot2)
library(exactextractr)
library(stars)
library(tigris)

dir <- 'C:/Users/kgr2vr/The Lab Dropbox/Kyle Addison/Sea_Level_Rise/'

# check if cleaned projection file exists
# if not, execute code to create it
if (!file.exists(paste0(dir, 'data_proj/slr_1deg_proj.dta'))) {
  
  # cleaning
  proj_all <- read.csv(paste0(dir, 'data_proj/Sea_Level_Rise_Datasets_2022/SLR_TF U.S. Sea Level Projections.csv'))
  
  # trim dimensions
  proj_all_c <- proj_all[17:nrow(proj_all),]
  proj_all_c <- proj_all_c[,1:28]
  row.names(proj_all_c) <- NULL
  
  # edit variable names
  proj_all_c[1,] <- gsub(" ", "_", proj_all_c[1,])
  proj_all_c[1,] <- gsub("(", "", proj_all_c[1,], fixed=TRUE)
  proj_all_c[1,] <- gsub(")", "", proj_all_c[1,], fixed=TRUE)
  proj_all_c[1,] <- gsub("_cm", "", proj_all_c[1,])
  proj_all_c <- proj_all_c %>% row_to_names(row_number = 1)
  row.names(proj_all_c) <- NULL
  colnames(proj_all_c)[colnames(proj_all_c) == 'US_Coastline_Intersect_1_=_yes'] <- 'US_Coastline_Intersect'
  colnames(proj_all_c)[colnames(proj_all_c) == 'RSL_contribution_from_VLM_trend:/year'] <- 'RSL_contribution_from_VLM'
  colnames(proj_all_c)[colnames(proj_all_c) == 'Offset_1992_to_2000'] <- 'offset_92_00'
  colnames(proj_all_c)[colnames(proj_all_c) == 'Offset_2000_to_2005'] <- 'offset_00_05'
  colnames(proj_all_c)[colnames(proj_all_c) == 'RSL_contribution_from_VLM'] <- 'rsl_vlm'
  colnames(proj_all_c)[colnames(proj_all_c) == 'Regional_Classification'] <- 'region'
  colnames(proj_all_c)[colnames(proj_all_c) == 'US_Coastline_Intersect'] <- 'coast_int'
  colnames(proj_all_c)[colnames(proj_all_c) == 'Long'] <- 'lon'
  names(proj_all_c) <- tolower(colnames(proj_all_c))
  
  # convert vars from char to num
  vars <- c('lat', 'lon', 'coast_int', 'rsl_vlm', 'offset_92_00', 'offset_00_05', colnames(proj_all_c[starts_with('rsl2', vars = colnames(proj_all_c))]))
  
  proj_all_c <- proj_all_c %>% mutate_at(vars, as.numeric)
  
  # split up, clean scenario variable
  proj_all_c$level <- proj_all_c$scenario
  colnames(proj_all_c)[colnames(proj_all_c) == 'scenario'] <- 'gmslr'
  proj_all_c <- proj_all_c %>% relocate(level, .after=gmslr)
  proj_all_c$gmslr <- as.numeric(substr(proj_all_c$gmslr, 1, 3))
  proj_all_c$level <- substr(proj_all_c$level, nchar(proj_all_c$level)-3, nchar(proj_all_c$level))
  proj_all_c$level <- gsub(" ", "", proj_all_c$level)
  proj_all_c$level <- gsub("MED", "med", proj_all_c$level)
  proj_all_c$level <- gsub("LOW", "low", proj_all_c$level)
  proj_all_c$level <- gsub("HIGH", "high", proj_all_c$level)
  
  # remove global mean sea level rise obs
  proj_all_c <- proj_all_c %>% filter(!psmsl_site == 'GMSL')
  
  # convert slr from cm to ft
  vars_ft <- c('rsl_vlm', 'offset_92_00', 'offset_00_05', colnames(proj_all_c[starts_with('rsl2', vars = colnames(proj_all_c))]))
  proj_all_c <- proj_all_c %>% mutate_at(vars(vars_ft), .funs = funs(. / 30.48))
  proj_all_c[,vars_ft] <- round(proj_all_c[,vars_ft], digits=4)
  
  # save
  write_dta(proj_all_c, paste0(dir, 'data_proj/slr_1deg_proj.dta'))
}


# draft overlap code

# state shapefile for comparison
# sf_state <- st_read(paste0(dir, 'GIS/nhgis0007_shape/US_state_2010.shp'))
# sf_state <- sf_state %>% filter(STUSPS10 %in% c('TX', 'LA', 'MS', 'AL', 'FL', 'GA', 'SC', 'NC', 'VA', 'NC',
#                                                 'VA', 'DC', 'MD', 'DE', 'NJ', 'NY', 'CT', 'RI', 'MA', 'NH', 'ME'))
# "+proj=merc +lat_ts=56.5"
# "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# "+init=EPSG:4326"

# # create/load mercator projection of coastal states
# if(file.exists(paste0(dir, 'GIS/nat_east_coast_merc/nat_east_coast_merc.shp'))) {
#   sf_state <- st_read(paste0(dir, 'GIS/nat_east_coast_merc/nat_east_coast_merc.shp'))
# } else {
#   sf_state <- st_transform(sf_state, "+proj=merc +lat_ts=56.5")
#   if(!dir.exists(paste0(dir, 'GIS/nat_east_coast_merc'))) {
#     dir.create(paste0(dir, 'GIS/nat_east_coast_merc'))
# }  else {
#     st_write(sf_state, paste0(dir, 'GIS/nat_east_coast_merc/nat_east_coast_merc.shp'))
#     sf_state <- st_read(paste0(dir, 'GIS/nat_east_coast_merc/nat_east_coast_merc.shp'))}}






# RI

# load inundation data
# names <- c()
# 
# for (i in 1:10) {
#   files_ri <- list.files(path=paste0(dir, 'Output/', i, 'ft'), pattern = "RI")
#   names <- append(names, files_ri)
# 
#   for (j in 1:length(files_ri)) {
#     tempRDS <- readRDS(paste0(dir, 'Output/', i, 'ft/', files_ri[j]))
#     tempSF <- st_as_sf(tempRDS)
#     name <- substr(files_ri[j], 1, nchar((files_ri)[j]) - 4)
#     print(name)
#     assign(name, tempSF, envir = .GlobalEnv)}
# }

# read in slr data, filter to area & time of interest
slr_orig <- read_dta(paste0(dir, 'data_proj/slr_1deg_proj.dta'))
slr_proj <- slr_orig %>% filter(gmslr == 1) %>%
  filter(level == 'med') %>%
  filter((lon - as.integer(lon)) == 0) %>%
  filter((lat - as.integer(lat)) == 0) %>%
  # filter(lon < 0 & lon > -110) %>%
  # filter(lat > 20)
  filter(lon < 0) %>%
  filter(lat > 20)

slr <- slr_proj %>% dplyr::select(lon, lat, rsl2100)
colnames(slr) <- c('x', 'y', 'z')

# load single projection to reproject raster with, gen raster
files_ri <- list.files(path=paste0(dir, 'Output/3ft'), pattern = "RI")
slr_proj_ex <- st_as_sf(readRDS(paste0(dir, 'Output/3ft/', files_ri[1])))

slr_ras <- rasterFromXYZ(xyz=slr,
                         res=c(1,1),
                         crs="+init=EPSG:4326")
slr_ras <- projectRaster(slr_ras, crs=crs(slr_proj_ex))
slr_df <- as.data.frame(slr_ras, xy=TRUE)

p_slr <- ggplot() +
          geom_raster(data=slr_df, aes(x=x, y=y, fill=z))
p_slr
ggsave(filename=paste0(dir, 'Graphics/slr_raster.png'))


ri_bg <- block_groups(state='RI',
                      year=2010)
ri_bg <- st_transform(ri_bg, st_crs(slr_ras))

p_ri <- ggplot() +
          geom_sf(data=slr_proj_ex, fill="lightblue", color=NA) +
          geom_sf(data=ri_bg, fill=NA, color="black")
p_ri
# ggsave(filename=paste0(dir, 'Graphics/slr_RI_3ft.png'))

ri_proj <- ri_bg
ri_proj$proj <- exact_extract(x = slr_ras,
                              y = ri_bg,
                              fun='mean')

p_proj <- ggplot() +
            geom_sf(ri_proj, mapping = aes(fill=proj))
p_proj
# ggsave(filename=paste0(dir, 'Graphics/slr_RI_ras_extract.png'))

# this ^ extracts the area's projected slr from the raster and assigns to block group
# next step is to overlay inundation data at each level (1-10) and retain polygons where they equal the bg's extracted value
# final value we care about will be from inundation, using blocks instead of bg may be more descriptive
# need to make sure crs for all data is correct


# find min and max value of slr
ri_proj$proj_rd <- floor(ri_proj$proj)
ri_proj$proj_ru <- ceiling(ri_proj$proj)

# slr_min <- min(ri_proj$proj)
# slr_min_r <- floor(slr_min)
# slr_max <- max(ri_proj$proj)
# slr_max_r <- ceiling(slr_max)

# intersect inundation with proj for each level between min and max
levels <- seq(min(ri_proj$proj_rd), max(ri_proj$proj_rd), 1)

p_proj_rd <- ggplot() +
              geom_sf(ri_proj, mapping = aes(fill=proj_rd))
p_proj_rd
# ggsave(filename=paste0(dir, 'Graphics/slr_RI_ras_extract_rd.png'))

p_proj_rd_in <- ggplot() +
                  geom_sf(ri_proj, mapping = aes(fill=proj_rd), size=.5) +
                  geom_sf(data=slr_proj_ex, fill="lightblue", color=NA)
p_proj_rd_in
# ggsave(filename=paste0(dir, 'Graphics/slr_RI_ras_extract_rd_in.png'))

# for (i in levels) {
  i <- 3
  all_files <- list.files(paste0(dir, 'Output/', i, 'ft/'), pattern = "RI", full.names = TRUE)
  inund_temp <- st_as_sf(readRDS(all_files))
  
  ri_proj_agg <- ri_proj
  ri_proj_agg <- ri_proj_agg[0,]
  
  for (j in 1:nrow(ri_proj)) {
    ri_proj_temp <- ri_proj[j,]
    sf_use_s2(FALSE)
    proj_int_temp <- st_intersects(ri_proj_temp, inund_temp, sparse = FALSE)
    proj_int_temp <- unique(c(proj_int_temp))
    if (TRUE %in% proj_int_temp) {
      ri_proj_temp$slr_real <- i
    } else {
      ri_proj_temp$slr_real <- 0
    }
    ri_proj_agg <- rbind(ri_proj_agg, ri_proj_temp)
    
  }
# }

p_ri_agg <- ggplot() +
              geom_sf(ri_proj_agg, mapping = aes(fill=slr_real), size=.5)
p_ri_agg
ggsave(filename=paste0(dir, 'Graphics/slr_RI_real.png'))



# for (j in length(all_files)) {


    # ri_proj$int_temp <- exact_extract(x = inund_temp,
    #                                   y = ri_proj,
    #                                   fun='mean')
    ri_proj_temp <- ri_proj %>% filter(proj_rd == i)
    int_temp <- st_intersection(inund_temp, ri_proj_temp)

    # inund_temp$slr <- i
    # inund_temp_ras <- st_rasterize(inund_temp %>% dplyr::select(slr, geometry))
    # ri_proj$slr_test <- exact_extract(x = inund_temp_ras,
    #                                   y = ri_proj,
    #                                   fun = 'mean')



# past bits of code

# slr_ras_sf <- st_as_sf(slr_df, coords=c('x', 'y'))
# st_crs(slr_ras_sf) <- "+init=EPSG:4326"
# slr_ras_sf <- st_transform(slr_ras_sf, st_crs(RI_slr_final_dist_1ft))


# p_ras <- ggplot() +
#           geom_raster(data=slr_df, aes(x=x, y=y, fill=z)) +
#           geom_sf(data=ri_sf, color="black", fill=NA)
# p_ras
