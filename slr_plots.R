# SLR plots
# Running script of plots needed for SLR visuals

rm(list=ls())

library(pacman)
p_load('tidyverse', 'ggplot2', 'haven', 'extrafont', 'sf', 'sp', 'raster', 'tigris', 'exactextractr', 'stars', 'ggnewscale')

dir <- 'C:/Users/kgr2vr/The Lab Dropbox/Kyle Addison/Sea_Level_Rise/'

# raster projections figure
proj_full <- read_dta(paste0(dir, 'data_proj/slr_1deg_proj.dta'))

proj <- proj_full %>% filter((lon - as.integer(lon)) == 0) %>%
                      filter((lat - as.integer(lat)) == 0) %>%
                      filter(lon > -125 & lon < -65) %>%
                      filter(lat > 24 & lat < 50) %>%
                      filter(gmslr == 1) %>%
                      filter(level == "med") %>%
                      dplyr::select(lon, lat, as.name(paste0('rsl2100')))

colnames(proj) <- c('x', 'y', 'z')
# proj_ras <- st_as_sf(proj, coords=c("x", "y"), crs=5070)
# ras_temp <- raster(extent(proj_ras), res=1, crs=crs(proj_ras))
# proj_ras <- rasterize(proj_ras, ras_temp)
# proj_n <- as.data.frame(proj_ras, xy=TRUE) %>% dplyr::select(x, y, z)


# proj <- as.data.frame(proj, xy=TRUE)

states <- st_read(paste0(dir, 'GIS/states/US_state_2010.shp')) %>% 
          filter(!STUSPS10 %in% c("AK", "HI", "PR", "GU", "AS", "VI", "MP"))
states <- st_transform(states, crs=5070)

p_ras <- ggplot() +
          geom_tile(data=proj,
                    aes(x=x,
                        y=y,
                        fill=z)) +
          geom_sf(data=states,
                  fill="transparent",
                  color="black",
                  size=1) +
          # coord_sf(crs=5070) +
          theme(text=element_text(family="CMU Serif"),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks = element_blank(),
                plot.background = element_rect(fill='white', color='white'),
                panel.background = element_rect(fill='transparent'),
                panel.border = element_blank())
p_ras



# inundation figures

# ft <- 1
# st="RI"
# 
# # plot_in <- function(ft, st) {
# 
#   bg <- block_groups(state=st, year=2010)
#   bg <- st_transform(bg, crs=5070)
#   bg_w <- bg %>% filter(ALAND10 == 0)
#   
#   ri_c <- st_read(paste0(dir, 'GIS/ri_coast/Coastal_Waters.shp'))
#   ri_c <- st_transform(ri_c, crs=5070)
#   ri_c <- st_union(ri_c)
#   
#   p_c <- ggplot()+
#             geom_sf(data=ri_c,
#                     fill="blue")
#   p_c
#   
# 
#   # for (i in ft) {
#   i <- ft
#     
#     dir_in_temp <- list.files(paste0(dir, 'Output/', i, 'ft'), pattern=st)
#     in_temp <- st_as_sf(readRDS(paste0(dir, 'Output/', i, 'ft', '/', dir_in_temp)))
#     in_temp <- st_transform(in_temp, crs=5070)
#     in_temp <- st_difference(in_temp, ri_c)
#     
#     p_in <- ggplot() +
#               geom_sf(data=in_temp,
#                       fill="lightblue")
#     p_in
#     
#     p_temp <- ggplot() +
#                 # geom_sf(data=bg_w,
#                 #         fill="white",
#                 #         color="white") +
#                 geom_sf(data=ri_c,
#                         fill="blue",
#                         color="blue") +
#                 geom_sf(data=bg,
#                         fill="transparent",
#                         linewidth=.05) +
#                 theme(text=element_text(family="CMU Serif"),
#                       axis.text.x=element_blank(),
#                       axis.text.y=element_blank(),
#                       axis.ticks = element_blank(),
#                       plot.background = element_rect(fill='white', color='white'),
#                       panel.background = element_rect(fill='transparent'),
#                       panel.border = element_blank())
#     p_temp
#     
#     ggsave(filename=paste0(dir, 'Graphics/inun_', tolower(st), '_', i, 'ft_t.png'),
#            plot=p_temp,
#            width=4,
#            height=5,
#            units="cm",
#            dpi=1000)
#     
#   # }
# # }
# 
# 
# plot_in(ft=c(10), st="RI")


ft <- 1
st="RI"

# bg <- block_groups(state=st, year=2010)
# bg <- st_read(paste0(dir, 'GIS/tl_2019_44_bg/tl_2019_44_bg.shp'))
bg <- st_read(paste0(dir, 'GIS/cb_2020_us_bg_500k/cb_2020_us_bg_500k.shp')) %>% filter(STATEFP == 44)
# bg <- st_transform(bg, crs=5070)
# bg_w <- bg %>% filter(ALAND == 0)
bg_u <- st_union(bg)

for (i in 1:10) {

  dir_in_temp <- list.files(paste0(dir, 'Output/', i, 'ft'), pattern=st)
  in_temp <- st_as_sf(readRDS(paste0(dir, 'Output/', i, 'ft', '/', dir_in_temp)))
  in_temp <- st_transform(in_temp, crs=st_crs(bg_u))
  in_temp <- st_make_valid(in_temp)
  
  in_temp <- st_simplify(in_temp, dTolerance = .001)
  
  in_temp <- st_intersection(in_temp, bg_u)
  
  in_temp$inun <- i
  in_temp <- in_temp %>% dplyr::select(inun, geometry)
  
  # bbox <- st_bbox(in_temp)
  # # res <- 3
  # ras_ext <- extent(bbox["xmin"], bbox["xmax"],
  #                   bbox["ymin"], bbox["ymax"])
  # 
  # in_temp_ras <- st_rasterize(in_temp, field="inun",
  #                             raster=ras_ext, res=3)
  # in_temp_ras <- as(in_temp_ras, "Raster")
  # in_temp_ras_df <- as.data.frame(in_temp_ras, xy=TRUE)
  # colnames(in_temp_ras_df) <- c("x", "y", "inun")
  
  in_name <- paste0("in", i)
  assign(in_name, in_temp)
}

# bbox <- st_polygon(list(rbind(c(-71.9, 41.3),
#                               c(-71.9, 42.05),
#                               c(-71.05, 42.05),
#                               c(-71.05, 41.3),
#                               c(-71.9, 41.3))))
# 
# ri_c <- st_read(paste0(dir, 'GIS/ri_coast/Coastal_Waters.shp'))
# ri_c <- st_transform(ri_c, crs=st_crs(bg_u))
# ri_c <- st_make_valid(ri_c)
# ri_c <- st_union(ri_c)
# ri_c <- st_crop(ri_c, bbox)
# ri_ci <- st_intersection(ri_ci, in1)
# ri_cid <- st_difference(ri_ci, in1)

# p_1_c <- ggplot() +
#           geom_sf(data=in1,
#                   fill='red',
#                   color='red') +
#           geom_sf(data=ri_ci,
#                   fill="blue",
#                   color="blue") +
#           geom_sf(data=bg,
#                   fill="transparent",
#                   linewidth=.05)
# p_1_c
# ggsave(filename=paste0(dir, 'Graphics/inun_', tolower(st), '_1_c.png'),
#        plot=p_1_c,
#        width=4,
#        height=5,
#        units="cm",
#        dpi=1000)


values = c('1ft' = '#bbdffb', '2ft' = '#90cbf9', '3ft' = '#64b7f6', '4ft' = '#41a7f5', '5ft' = '#1e97f3',
           '6ft' = '#1a8ae5', '7ft' ='#1477d2', '8ft' = '#1065c0', '9ft' = '#0747a1', '10ft' = '#012454')

values_in = c('1' = '#bbdffb', '2' = '#90cbf9', '3' = '#64b7f6', '4' = '#41a7f5', '5' = '#1e97f3',
           '6' = '#1a8ae5', '7' ='#1477d2', '8' = '#1065c0', '9' = '#0747a1', '10' = '#012454')

p_test <- ggplot() +
            geom_sf(data=bg,
                    fill="gray",
                    color="darkgray",
                    linewidth=.1) +
            geom_sf(data=in_temp,
                    aes(fill="#bbdffb",
                        color="#bbdffb")) +
            # geom_raster(data=in1,
            #             aes(x=x,
            #                 y=y,
            #                 fill=factor(inun))) +
            # scale_fill_manual(values=values_in,
            #                   na.value=NA) +
            # coord_fixed() +
            theme(text=element_text(family="CMU Serif"),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks = element_blank(),
                  legend.position="none",
                  plot.background = element_rect(fill='white', color='white'),
                  panel.background = element_rect(fill='transparent'),
                  panel.border = element_blank())
p_test


ggsave(filename=paste0(dir, 'Graphics/inun_', tolower(st), '_test1.png'),
       plot=p_test,
       width=4,
       height=5,
       units="cm",
       dpi=1500)


p_temp <- ggplot() +
            geom_sf(data=bg,
                    fill="gray",
                    color="darkgray",
                    linewidth=.1) +
            geom_sf(data=in10,
                    fill='#012454',
                    color='#012454') +
            geom_sf(data=in9,
                    fill='#0747a1',
                    color='#0747a1') +
            geom_sf(data=in8,
                    fill='#1065c0',
                    color='#1065c0') +
            geom_sf(data=in7,
                    fill='#1477d2',
                    color='#1477d2') +
            geom_sf(data=in6,
                    fill='#1a8ae5',
                    color='#1a8ae5') +
            geom_sf(data=in5,
                    fill='#1e97f3',
                    color='#1e97f3') +
            geom_sf(data=in4,
                    fill='#41a7f5',
                    color='#41a7f5') +
            geom_sf(data=in3,
                    fill='#64b7f6',
                    color='#64b7f6') +
            geom_sf(data=in2,
                    fill='#90cbf9',
                    color='#90cbf9') +
            geom_sf(data=in1,
                    fill='#bbdffb',
                    color='#bbdffb') +
            # geom_sf(data=ri_ci,
            #         fill="#F0FFFF",
            #         color="#F0FFFF") +
            # geom_sf(data=bg_w,
            #         fill="white",
            #         color="white",
            #         linewidth=.51) +
            # geom_sf(data=bg,
            #         fill="transparent",
            #         linewidth=.05) +
            theme(text=element_text(family="CMU Serif"),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks = element_blank(),
                  plot.background = element_rect(fill='white', color='white'),
                  panel.background = element_rect(fill='transparent'),
                  panel.border = element_blank())
p_temp

ggsave(filename=paste0(dir, 'Graphics/inun_', tolower(st), '_all_t2.png'),
       plot=p_temp,
       width=4,
       height=5,
       units="cm",
       dpi=2000)





# extraction process 

slr_orig <- read_dta(paste0(dir, 'data_proj/slr_1deg_proj.dta'))

# parameters
gmslrs <- unique(slr_orig$gmslr)
levels <- unique(slr_orig$level)
years <- c(2050, 2100)

states <- c("RI")

# 1.5 med 2100
i <- 4
j <- 1
k <- 2

slr <- slr_orig %>% filter((lon - as.integer(lon)) == 0) %>%
                    filter((lat - as.integer(lat)) == 0) %>%
                    filter(lon >= -72 & lon <= -71) %>%
                    filter(lat >= 41 & lat <= 42.5) %>%
                    filter(gmslr == gmslrs[i]) %>%
                    filter(level == levels[j]) %>%
                    dplyr::select(lon, lat, as.name(paste0('rsl', years[k])))

colnames(slr) <- c('x', 'y', 'z')

# load single inundation file to reproject raster with, gen raster
files_st <- list.files(path=paste0(dir, 'Output/10ft'), pattern=states)
slr_proj_ex <- st_as_sf(readRDS(paste0(dir, 'Output/10ft/', files_st[1])))

slr_ras <- rasterFromXYZ(xyz=slr,
                         res=c(1,1),
                         crs="+init=EPSG:4326")
slr_ras <- projectRaster(slr_ras, crs=crs(slr_proj_ex))

# blocks shapefile
# st_b <- block_groups(state=states, year=2019)
st_b <- st_read(paste0(dir, 'GIS/cb_2020_us_bg_500k/cb_2020_us_bg_500k.shp')) %>% filter(STATEFP == 44)
st_b <- st_transform(st_b, st_crs(slr_ras))

# PLOT 1a
slr_p <- slr %>% mutate(z=round(floor(z), 1))

# bbox <- st_polygon(list(rbind(c(-72, 41),
#                               c(-72, 42.5),
#                               c(-71, 42.5),
#                               c(-71, 41),
#                               c(-72, 41)))) %>%
#         st_sfc() %>% 
#         st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
#         st_transform(crs=crs(slr_proj_ex))
# slr_p <- crop(slr_p, bbox)

p_1a_col <- c("4" = '#012454', "5" = '#1e97f3')

p_1a <- ggplot() +
        geom_tile(data=slr_p,
                  aes(x=x,
                      y=y,
                      fill=factor(z))) +
        geom_sf(data=st_b,
                fill='transparent',
                linewidth=.2) +
        scale_fill_manual(values=p_1a_col) +
        coord_sf(xlim=c(-72, -71), ylim=c(41, 42.25)) +
        labs(x="", y="", fill="Projected SLR") +
        theme(text=element_text(family="CMU Serif"),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks = element_blank(),
              legend.position = 'right',
              legend.key.size = unit(.75, 'cm'),
              legend.text = element_text(size=12),
              legend.title = element_text(, size=15),
              legend.title.position = "top",
              legend.title.align = .5,
              plot.background = element_rect(fill='white', color='white'),
              panel.background = element_rect(fill='transparent'),
              panel.border = element_blank())
p_1a
# ggsave(filename=paste0(dir, 'Graphics/ri_fig_1a.png'),
#        plot=p_1a,
#        width=10,
#        height=10,
#        units="cm",
#        dpi=600)


# assign proj slr to blocks
st_proj <- st_b
st_proj$proj <- exact_extract(x=slr_ras,
                              y=st_b,
                              fun='mean')

# calc floor estimate, round neg to 0
st_proj$proj_fl <- floor(st_proj$proj)
st_proj$proj_fl[st_proj$proj_fl == -1] <- 0


# PLOT 1b

p_1b_col <- c("4" = '#012454', "5" = '#1e97f3')

p_1b <- ggplot() +
          geom_sf(data=st_proj,
                  aes(fill=as.factor(proj_fl)),
                  linewidth=.2) +
          scale_fill_manual(name="Projected SLR",
                            values=p_1b_col) +
          coord_sf(xlim=c(-72, -71), ylim=c(41, 42.25)) +
          labs(x="", y="", fill="Projected SLR") +
          theme(text=element_text(family="CMU Serif"),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks = element_blank(),
                legend.position = 'right',
                legend.key.size = unit(.75, 'cm'),
                legend.text = element_text(size=12),
                legend.title = element_text(size=15),
                legend.title.position = "top",
                legend.title.align = .5,
                plot.background = element_rect(fill='white', color='white'),
                panel.background = element_rect(fill='transparent'),
                panel.border = element_blank())
p_1b
# ggsave(filename=paste0(dir, 'Graphics/ri_fig_1b.png'),
#        plot=p_1b,
#        width=10,
#        height=10,
#        units="cm",
#        dpi=600)


# calc range of proj slr, initialize agg df
inc <- seq(min(st_proj$proj_fl, na.rm=TRUE), max(st_proj$proj_fl, na.rm=TRUE), 1)

st_proj_agg <- st_proj[0,]

# loop over range, load inundation for each
# intersect inundation with proj for each increment in range
for (x in inc) {
  # x <- inc[1]
  print(x)
  st_proj_temp <- st_proj %>% filter(proj_fl == x)
  st_proj_temp_u <- st_union(st_proj_temp)
  
  if (x != 0) {
    
    all_files <- list.files(paste0(dir, 'Output/', x, 'ft/'), pattern=states, full.names=TRUE)
    print(paste0('number of files: ', length(all_files)))
    
    if(length(all_files) > 1) {
      
      # initialize
      inun_temp <- st_as_sf(readRDS(all_files[1]))
      colnames(inun_temp) <- c("Id", "gridcode", "Shape_Length", "Shape_Area", "geometry")
      
      for (y in 2:length(all_files)) {
        file_temp <- st_as_sf(readRDS(all_Files[y]))
        colnames(file_temp) <- c("Id", "gridcode", "Shape_Length", "Shape_Area", "geometry")
        inun_temp <- rbind(inun_temp, file_temp)
      }
      
    } else {
      inun_temp <- st_as_sf(readRDS(all_files))
      inun_temp <- st_transform(inun_temp, crs=st_crs(st_proj_temp_u))
      inun_temp <- st_make_valid(inun_temp)
      inun_temp <- st_intersection(inun_temp, st_proj_temp_u)
    }
    inun_temp$slr <- x
    
    inun_ras_temp <- st_rasterize(inun_temp %>% dplyr::select(slr, geometry))
    inun_ras_temp <- as(inun_ras_temp, "Raster")
    inun_ras_temp_df <- as.data.frame(inun_ras_temp, xy=TRUE)
    
    st_proj_temp$slr_real <- exact_extract(x=inun_ras_temp,
                                           y=st_proj_temp,
                                           fun="max",
                                           default_value=0)
    st_proj_agg <- rbind(st_proj_agg, st_proj_temp)
    
    proj_name <- paste0("st_proj_temp", x)
    assign(proj_name, st_make_valid(st_proj_temp))
    inun_int <- st_intersects(inun_temp, st_proj_temp, sparse=FALSE)
    overlap <- apply(inun_int, 1, any)
    as_name <- paste0("inun_fin", x)
    assign(as_name, inun_temp[overlap,])
  }
}

# PLOT 2a1    
st_proj_temp_agg <- rbind(st_proj_temp4, st_proj_temp5)
inun_fin_agg <- rbind(inun_fin4, inun_fin5)

    p_2a_col_p <- c("4" = '#012454', "5" = '#1e97f3')
    p_2a_col_i <- c("4" = "red", "5" = "green")
    
    p_2a1 <- ggplot() +
                geom_sf(data=st_proj_temp_agg,
                        aes(fill=as.factor(proj_fl)),
                        linewidth=.2) +
                scale_fill_manual(name="Projected SLR",
                                  values=p_2a_col_p) +
                ggnewscale::new_scale_fill() +
                geom_sf(data=inun_fin_agg,
                        aes(fill=as.factor(slr),
                            color=as.factor(slr))) +
                scale_fill_manual(name="Inundation (ft)",
                                  values=p_2a_col_i) +
                scale_color_manual(name="Inundation (ft)",
                                   values=p_2a_col_i) +
                theme(text=element_text(family="CMU Serif"),
                      axis.text.x=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks = element_blank(),
                      legend.position = 'right',
                      legend.key.size = unit(.75, 'cm'),
                      legend.text = element_text(size=12),
                      legend.title = element_text(size=15),
                      legend.title.position = "top",
                      legend.title.align = .5,
                      plot.background = element_rect(fill='white', color='white'),
                      panel.background = element_rect(fill='transparent'),
                      panel.border = element_blank())
    p_2a1
    
    ggsave(filename=paste0(dir, 'Graphics/ri_fig_2a1_t.png'),
           plot=p_2a1,
           width=10,
           height=10,
           units="cm",
           dpi=600)
    
    
    # TEST PRESERVE
    # 
    # p_2a1 <- ggplot() +
    #   geom_sf(data=st_proj_temp,
    #           aes(fill=as.factor(proj_fl)),
    #           linewidth=.2) +
    #   scale_fill_manual(name="Projected SLR",
    #                     values=p_2a_col_p) +
    #   ggnewscale::new_scale_fill() +
    #   geom_sf(data=inun_fin,
    #           aes(fill=as.factor(slr)),
    #           color="red") +
    #   scale_fill_manual(name="Inundation (ft)",
    #                     values=p_2a_col_i) +
    #   theme(text=element_text(family="CMU Serif"),
    #         axis.text.x=element_blank(),
    #         axis.text.y=element_blank(),
    #         axis.ticks = element_blank(),
    #         legend.position = 'right',
    #         legend.key.size = unit(.75, 'cm'),
    #         legend.text = element_text(size=12),
    #         legend.title = element_text(size=15),
    #         legend.title.position = "top",
    #         legend.title.align = .5,
    #         plot.background = element_rect(fill='white', color='white'),
    #         panel.background = element_rect(fill='transparent'),
    #         panel.border = element_blank())
    # p_2a1



st_proj_agg$slr_real[is.na(st_proj_agg$slr_real)] <- 0

# st_proj_add <- as.data.frame(st_proj_agg) %>%
#                dplyr::select('GEOID', 'slr_real') %>%
#                rename(blockid = GEOID)
# st_proj_add$gmslr <- gmslrs[i]
# st_proj_add$level <- levels[j]
# st_proj_add$year <- years[k]


# PLOT 3

p_3_col <- c("0" = "gray", "4" = '#012454', "5" = '#1e97f3')

p_3 <- ggplot() +
        geom_sf(data=st_proj_agg,
                aes(fill=as.factor(slr_real)),
                linewidth=.2) +
        scale_fill_manual(name="Realized SLR",
                          values=p_3_col) +
        theme(text=element_text(family="CMU Serif"),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks = element_blank(),
              legend.position = 'right',
              legend.key.size = unit(.75, 'cm'),
              legend.text = element_text(size=12),
              legend.title = element_text(size=15),
              legend.title.position = "top",
              legend.title.align = .5,
              plot.background = element_rect(fill='white', color='white'),
              panel.background = element_rect(fill='transparent'),
              panel.border = element_blank())
p_3

ggsave(filename=paste0(dir, 'Graphics/ri_fig_3.png'),
       plot=p_3,
       width=10,
       height=10,
       units="cm",
       dpi=600)




