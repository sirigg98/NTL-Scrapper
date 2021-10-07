#Loading packages needed
library(nightlightstats) #Github repo: https://github.com/JakobMie/nightlightstats
library(raster)
library(sf) # Need raster and sf packages for nightlightstats
library(dplyr) 


# Outcome for district-level analysis: Average monthly Night-time light intensity by district

############################# CLEANING RAW NIGHT-TIME LIGHT INTENSITY DATA #######################################

# Need to download monthly district-level raw nightlight data for desired time period first. Download raw data here: https://eogdata.mines.edu/download_dnb_composites.html.
# Nightlight_calculate function from nightlightstats package stores the data in a spatial point dataframe conducive to analysis. 

# One-stop function for nightlight_calculate for a certain time period (01/starty to 12/endy). 
# Notes: Download ALL raw files first. Takes a while to run.

store_nightlight <- function(starty, endy){
  
  light_panel_full <- NULL #define as Null to bind with after each iteration
  
  for (y in starty:endy){
    for (i in 1:12){
      
      if (i<10) {  #need separate code for when month is oct, nov, dec (double digits)  
        nightlight_calculate(
          area_names = "Nepal",
          time = paste0(y, "-0", i), #YYYY-MM format
          corrected_lights = TRUE, #Correct for stray lights
          shapefile_location = "/Users/siriggurung/Downloads/gadm36_NPL_shp", #Specifying Nepal Shapefile with district borders
          shapefiles = "gadm36_NPL_3.shp",
          light_location = paste0("/Users/siriggurung/Downloads/light/",y, "/",i),
          admlevel = 3) #At the district level
      }
      
      else {    
        nightlight_calculate(
          area_names = "Nepal",
          time = paste0(y,"-", i),
          corrected_lights = TRUE,
          shapefile_location = "/Users/siriggurung/Downloads/gadm36_NPL_shp",
          shapefiles = "gadm36_NPL_3.shp",
          light_location = paste0("/Users/siriggurung/Downloads/light/",y, "/",i),
          admlevel = 3)
      }
      
      lights_panel_full <- rbind(lights_panel_full, lights) #Bind after each iteration of the loop. 
      
    }
  }
}

store_nightlight(2014, 2017)
#Final data set is lights_panel_full. Should be up in the environment. Data from jan 2014-dec 2017.

#Saving as lights_panel.dta
foreign::write.dta(lights_panel_full, "/Users/siriggurung/Desktop/Amherst work/Senior Year/Thesis/lights_panel.dta")




########## CLEANING 2011 NEPAL CENSUS AND DHS DATA FOR TIME-INVARIANT, DISTIRCT-LEVEL COVARIATES ##############

# Finding/calculating relevant controls for district level analysis. Might not use all, might have find more. 

# loading census 2011 data. Downloaded from IPUMS international.  10% representative.
data_census <- read.csv("/Users/siriggurung/Downloads/ipumsi_00009.csv", sep=",")

# loading DHS 2011 data for proportion of migrants to India
data_DHS <- foreign::read.dta("/Users/siriggurung/Downloads/NP_2011_DHS_10182020_1829_151281/NPHR61DT/NPHR61FL.DTA")

# A dataset with Nepal DHS district codes and the district to facilitate merge later. DHS only has district names
district_codesDHS <- read.csv("/Users/siriggurung/Downloads/DHS2011 district codes.csv") %>%
  rename("shdistrict" = "X1", "District" = "Taplejung") %>% 
  rbind(c(1, "Taplejung")) 

district_codesDHS$shdistrict <- as.numeric(district_codesDHS$shdistrict)


#Population by districts. Not full population, but most variables are proportions.
pop_dist <- data_census %>%
  group_by(GEO2_NP2011) %>%
  summarize(n = n())


#Dummy for districts bordering India
border <- data_census %>% 
  distinct(GEO2_NP2011) %>%
  mutate(border = ifelse(GEO2_NP2011 %in% c(1001, 1002, 1003, 1004, 2005, 2006, 3015, 30016, 4017, 4018,
                                            4019, 6032, 6033, 6034, 6035, 9048, 9049, 9050, 10056, 11057, 
                                            11058,13071, 14072, 14073, 14074, 14075), 1, 0))
                                            # List of districts near the Indian Border. District codes used. 


#Madhesi ethnicity proportion of district pop
ethnicity <- data_census %>%
         mutate(madhes_m = ifelse(ETHNICNP == 9, 1, 0), #Dummy for middle caste madhesi. Ethnicity codes used. Caste grouped as madhesis according to census 2011 guidelines
              madhes_h = ifelse(ETHNICNP %in% c(18, 27, 48, 58), 1, 0), #Dummy for high caste madhesi
              madhes_l = ifelse(ETHNICNP %in% c(16, 19, 21, 34, 47, 30, 26, 28, 31, 34, 33, 38, 
                                           37, 55, 63, 43, 25, 44, 56, 49, 50, 64, 72, 94, 
                                           110, 59, 119, 124, 76, 65), 1, 0), #Dummy for low caste madhesi
              madhes_d = ifelse(ETHNICNP %in% c(17, 22, 23, 41, 39, 40, 54, 75, 111,116, 87, 104, 
                                           106, 105, 103), 1, 0)) %>%  #Dummy for dalit madhesi
  group_by(GEO2_NP2011) %>%
  summarize(num_h = sum(madhes_h), num_m = sum(madhes_m), 
            num_l = sum(madhes_l), num_d = sum(madhes_d)) %>% # Calculating prop of high, mid, low, and dalit madhesi by district
  left_join(pop_dist, by = "GEO2_NP2011") %>%
  mutate(prop_h = num_h/n, prop_m = num_m/n, prop_l = num_l/n, prop_d = num_d/n, 
         prop_madhes = prop_h + prop_m + prop_l + prop_d) %>%
  select(GEO2_NP2011, prop_h, prop_m, prop_l, prop_d, prop_madhes)


#Mother tongue hindi proportion by district:
hindi <- data_census %>%
  mutate(d = ifelse(MTONGNP == 16, 1, 0)) %>%
  group_by(GEO2_NP2011) %>%
  summarize(prop_hindi = sum(d)/n())


# Calculating literacy rate by district
lit_dist <- data_census %>%
  mutate(i = ifelse(LIT == 2, 1, 0)) %>% #Creating dummies for literate individuals
  group_by(GEO2_NP2011) %>%
  summarize(prop_lit = sum(i)/n())


# Calculating prop of population in domestic or agricultural work
emp_dist <- data_census %>%
  mutate(i_agri = ifelse(INDGEN == 10, 1, 0), # Creating dummies for agricultural work
         i_domes = ifelse(INDGEN == 120, 1, 0)) %>% #for domestic work
  group_by(GEO2_NP2011) %>%
  summarize(prop_agri = sum(i_agri)/n(), prop_dome = sum(i_domes)/n()) 


# Calculating prop of population in domestic or agricultural work
rural_prop <- data_census %>%
  mutate(i_rural = ifelse(URBAN == 0, 1, 0)) %>% # Creating rural dummies
  group_by(GEO2_NP2011) %>%
  summarize(prop_rural = sum(i_rural)/n()) 


# Calculating mean prop of migrants to india by district. Using DHS data.
data_DHS <- data_DHS %>%
  select(hhid, shdistrict, sh28c_01, sh28c_02, sh28c_03, sh28c_04, # Houhsehold members in wide format. 
         sh28c_05, sh28c_06, sh28c_07, sh28c_08, sh28c_09, sh28c_10, 
         sh28c_11, sh28c_12, sh28c_13, sh28c_14, sh28c_15, sh28c_16) %>%
  gather(key = "person", value = "mig", -c(shdistrict, hhid)) %>% # Changing to long formate
  arrange(hhid, person) %>%
  mutate(d_india = ifelse(mig == "india", 1, 0)) %>%  # Creating dummy for whether household member is a migrant to india
  mutate(d_india = ifelse(is.na(mig), 0, d_india)) %>% 
  group_by(shdistrict) %>%
  summarize(meanhh_mig_india = sum(d_india)/n()) %>% # Prop of migrants to India by district
  left_join(district_codesDHS, by = "shdistrict") %>% # Join with district codes by distict name
  select(shdistrict, District, meanhh_mig_india) %>%
  rename("GEO2_NP2011" = "shdistrict") # Renaming DHS codes to facilitate join with census data


#Joining all datasets from above
data_joined <- left_join(pop_dist, border, by = "GEO2_NP2011") %>%
  left_join(ethnicity, by = "GEO2_NP2011") %>% 
  left_join(hindi, by = "GEO2_NP2011") %>% 
  left_join(emp_dist, by = "GEO2_NP2011") %>% 
  left_join(lit_dist, by = "GEO2_NP2011")  %>% 
  left_join(rural_prop, by = "GEO2_NP2011")  %>% 
  mutate(GEO2_NP2011 = ifelse(floor(log10(GEO2_NP2011)) + 1 < 5, ##If num of digits is less than 5 (or equal to 4 in this case)....
                              substr(GEO2_NP2011, 3, 4), # Need to do this to join with DHS data (DHS codes are last two digits of census codes). 
                              substr(GEO2_NP2011, 4, 5))) #First two digits differ by admin zones; can't subtract a constant

data_joined$GEO2_NP2011 <- as.numeric(data_joined$GEO2_NP2011)

data_joined <- data_joined %>%
  right_join(data_DHS, by ="GEO2_NP2011") %>% #Joining mogrant prop data
  right_join(covars, by = "GEO2_NP2011") #Joining covars dataset. 

#Saving as dist_covariates.dta
foreign::write.dta(data_joined, "/Users/siriggurung/Desktop/Amherst work/Senior Year/Thesis/dist_covariates.dta")



