#library a bunch of packages we may (or may not) use - install them first if not installed already. 
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(here)
library(spatialreg)
library(spdep)
library(dplyr)
library(raster)


#read in some attribute data
reg_data <- read_csv("~/Downloads/reg_data.csv", 
                               col_names = TRUE, 
                               locale = locale(encoding = 'Latin1'))

reg_queen <- reg_data %>% 
  dplyr::filter(postcode_area != "ZE")

uk_postcode <-dir_info(here::here("cleaned_data",
                                  "GB_Postcodes"))%>%
  
  #$ means exact match
  dplyr::filter(str_detect(path, 
                           "PostalArea.shp$"))%>%
    dplyr::select(path)%>%
    pull()%>%
    #read in the file in
    st_read()

class(uk_postcode)
crs(uk_postcode)
uk_postcode$area_sqkm <- st_area(uk_postcode)

post_area<- uk_postcode%>%
  dplyr::select(PostArea,area_sqkm)

uk_postcode$PostArea
#merge boundaries and data
reg_data_map <- uk_postcode%>%
  right_join(.,
            reg_data, 
            by = c("PostArea"="postcode_area"))

#merge boundaries and data
reg_data_queen <- uk_postcode%>%
  right_join(.,
             reg_queen, 
             by = c("PostArea"="postcode_area"))

#check all of the columns have been read in correctly
Datatypelist <- reg_data_map %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

Regressiondata2<- reg_data_map%>%
  clean_names()%>%
  dplyr::select(log_demand,
                log_supply,
                log_1_car,log_off_street,log_rent,log_income,log_high_edu,log_driving)

model2 <- lm(log_demand ~ log_supply + log_off_street + log_avg_hour +log_high_edu + log_income + log_fast_chargers + log_age_18_55
               , data = reg_data_map)

#show the summary of those outputs
tidy(model2)

glance(model2)

#and for future use, write the residuals out
model_data2 <- model2 %>%
  augment(., Regressiondata2)

# also add them to the shapelayer
reg_data_map <- reg_data_map %>%
  mutate(model2resids = residuals(model2))

#calculate the centroids of all Postcode in the UK
coordsW <- reg_data_map %>%
  st_centroid()%>%
  st_geometry()

# Source From: https://crd230.github.io/lab8.html
# Visualize the residuals in the regression.
tm_shape(reg_data_map, unit = "mi") +
  tm_polygons(col = "model2resids", style = "equal",palette = "Reds", 
              border.alpha = 0, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "Residuals from OLS Model in Great Britain",  main.title.size = 0.95, frame = FALSE, legend.outside = TRUE,
            attr.outside = TRUE)

# Source From: https://crd230.github.io/lab8.html
# Visualize the charging demand in GB
tm_shape(reg_data_map, unit = "mi") +
  tm_polygons(col = "electricity_demand (kWh)", style = "quantile",palette = "Reds", 
              border.alpha = 0, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "Charging Demand (kWh), Great Britain 2021 ",  main.title.size = 0.95, frame = FALSE, legend.outside = TRUE, 
            attr.outside = TRUE)

# apply the queen weighted matrix
LWard_nb <- reg_data_map %>%
  poly2nb(., queen=T)

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="W")

lm.morantest(model2, Lward.queens_weight)

moran.mc(reg_data_map$log_demand, Lward.queens_weight, nsim=999)

lm.LMtests(model2, Lward.queens_weight, test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))

error_dv_model2_queen <- errorsarlm(log_demand ~ log_supply + log_off_street + log_avg_hour + log_high_edu +log_age_18_55 + log_income +log_fast_chargers,
                                data = reg_data_map, 
                                nb2listw(LWard_nb, style="C"), 
                                method = "eigen")

summary(error_dv_model2_queen)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")

reg_data_map <- reg_data_map %>%
  mutate(error_dv_model2_queen_resids = residuals(error_dv_model2_queen))

queeb4Moran <- reg_data_map %>%
  st_drop_geometry()%>%
  dplyr::select(error_dv_model2_queen_resids)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()

queeb4Moran

moran.mc(reg_data_map$error_dv_model2_queen_resids, Lward.queens_weight, 999)

# Source From: https://crd230.github.io/lab8.html
# Visualize the Residuals from Spatial Error Model
tm_shape(reg_data_map, unit = "mi") +
  tm_polygons(col = "error_dv_model2_queen_resids", style = "equal",palette = "Reds", 
              border.alpha = 0, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "Residuals from Spatial Error Model in Great Britain",  main.title.size = 0.95, frame = FALSE, legend.outside = TRUE,
            attr.outside = TRUE)

#or nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="W")

lm.morantest(model2, Lward.knn_4_weight)

lm.LMtests(model2, Lward.knn_4_weight, test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))

Nearest_neighbour <- reg_data_map %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

Nearest_neighbour

plot(coordsW)

#plot them
plot(LWard_knn, st_geometry(coordsW), col="red")

#lagsarlm
#errorsarlm
slag_dv_model1_knn4 <- errorsarlm(log_demand ~ log_supply + log_off_street + log_avg_hour + log_high_edu + log_income +log_fast_chargers,
                                 data = reg_data_map, 
                                 nb2listw(LWard_knn, style="C"), 
                                 method = "eigen")

summary(slag_dv_model1_knn4)

summary(impacts(slag_dv_model1_knn4), adjust_k=TRUE)

#impacts(slag_dv_model1_knn4,listw = nb2listw(LWard_knn, style="C"))

glance(slag_dv_model1_knn4)

#write out the residuals

reg_data_map <- reg_data_map %>%
  mutate(slag_dv_model1_knn_resids = residuals(slag_dv_model1_knn4))

KNN4Moran <- reg_data_map %>%
  st_drop_geometry()%>%
  dplyr::select(slag_dv_model1_knn_resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

KNN4Moran

moran.mc(reg_data_map$slag_dv_model1_knn_resids, Lward.knn_4_weight, 999)

