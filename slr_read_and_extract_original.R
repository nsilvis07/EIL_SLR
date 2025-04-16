library(tidyverse)
library(rgdal)

all_gdb <- list.files("C:/Users/voorh302/Documents/slr")
all_gdb <- all_gdb[grepl(".gdb", all_gdb)]
stub <- gsub(".gdb", "", all_gdb)

#try_flood_test <- sf::st_read(dsn = file_gdb, layer = "S_FLD_HAZ_AR")
#try_flood <- sf::as_Spatial(try_flood_test %>% sf::st_cast(to = "MULTIPOLYGON"))
weirdos <- c()
for(i in stub){
  print(i)
  file_gdb <- paste0("C:/Users/voorh302/Documents/slr/", i, ".gdb")
  fc_list = ogrListLayers(file_gdb )
  layer_temp <- paste0(gsub("_slr_final_dist", "", i), "_slr_10ft")
  if(!file.exists(paste0("C:/Users/voorh302/Documents/slr/", i, "_10ft.rda")) & layer_temp %in% fc_list){
  slr_df_10ft <- readOGR(dsn = file_gdb, layer = layer_temp, dropNULLGeometries = T)
  save(slr_df_10ft, file=paste0("C:/Users/voorh302/Documents/slr/", i, "_10ft.rda"))
  }
  if(!layer_temp %in% fc_list){
    weirdos <- c(weirdos, i)
  }
}
save(weirdos, file="C:/Users/voorh302/Documents/slr/weirdos.rda")


#Some .gdbs have weird names, mostly in Louisiana
load("C:/Users/voorh302/Documents/slr/weirdos.rda")

for(i in weirdos){
file_gdb <- paste0("C:/Users/voorh302/Documents/slr/", i, ".gdb")
fc_list = ogrListLayers(file_gdb )
#print(fc_list)
all_10ft <- fc_list[grepl("slr_10ft", fc_list)]
if(!file.exists(paste0("C:/Users/voorh302/Documents/slr/", i, "_10ft.rda"))){
if(length(all_10ft)==2){
  slr_df_10ft <- readOGR(dsn = file_gdb, layer = all_10ft[1], dropNULLGeometries = T)
  slr_df_10ft_try <- readOGR(dsn = file_gdb, layer = all_10ft[2], dropNULLGeometries = T)
  slr_df_10ft <- rbind(slr_df_10ft, slr_df_10ft_try)
  save(slr_df_10ft, file=paste0("C:/Users/voorh302/Documents/slr/", i, "_10ft.rda"))
}
if(length(all_10ft)==1){
  slr_df_10ft <- readOGR(dsn = file_gdb, layer = all_10ft[1], dropNULLGeometries = T)
  save(slr_df_10ft, file=paste0("C:/Users/voorh302/Documents/slr/", i, "_10ft.rda"))
}
}
}

