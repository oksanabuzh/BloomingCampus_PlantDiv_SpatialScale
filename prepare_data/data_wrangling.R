# data wrangling


library(tidyverse)

# metadata -----------

# file Sampling5.0Data.csv:

# 1) X in cover columns means that we forgot to give the cover during the sample 
# to the species that were present on the plot. 

#  2) for 10m2 in the 3.16 column we gave cover for every species 
# by itself (not mean from two 1m2 columns). When we had a species in both corner 
# we only entered it once and left it blank for the other

# 3) phenology and height were measured for each species only on 10m2 scale

# data ---------------

# mowing data
Mowing_data <- read_csv("data/mowing_events_2025.csv") %>% 
  pivot_longer(cols = c(September,	July,	May,	March),
               names_to = "Month",
               values_to = "n_mow_events_befre_sampling") 

# litter and other cover data
Cover_data <- read_csv("data/raw_data/BC_2025_Cover_Data.csv") %>% 
  filter(Scale_m2 == 1) %>%
  select(-Date, -Scale_m2, -Veg_Total_Cover, -"10m_Max_Cryptogam_Height", -Remarks)

# traits data
traits <- read_csv("data/traits.csv")


# 10m2 scale --------------------------------------------------------------------------
# prepare phenology data
# phenology and height were measured only on 10m2 scale
data_10m2 <- read_csv("data/raw_data/Sampling5.0Data.csv") %>%
  mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
  mutate(Month = lubridate::month(Date, label = TRUE, abbr = FALSE),
         .after =Date, .keep = "unused") %>% 
  mutate(Month = case_when(Month == "April" ~ "March",
                           Month == "August" ~ "July",
                           Month == "October" ~ "September",
                           .default = Month)) %>%
  rename("cover_10_m2" = "3.16m",
         EuroMed = "Euro+Med Taxon") %>% # 10m2 scale
  mutate(cover_10_m2=ifelse(cover_10_m2=="X", 0.001, cover_10_m2)) %>% # X in cover columns means that we forgot to give the cover during the sample 
  # to the species that were present on the plot. We replace "X" with a very low cover value to account for the species in species richness
  mutate(cover_10_m2=as.numeric(cover_10_m2),
         Juvenile = as.numeric(Juvenile),
         PostFruiting = as.numeric(PostFruiting)) %>%
  filter(Layer!="B", Layer!="L", # remove bryophytes & lichens
         !is.na(cover_10_m2)) %>%  # there is one NA in cover that is that species is not present
  mutate(height=ifelse(height=="X", NA, height)) %>%
  mutate(height = vapply(strsplit(height, ",\\s*"),
                         function(vals) {
                           nums <- as.numeric(vals)
                           nums <- nums[!is.na(nums)]
                           if (length(nums) == 0) NA_real_ else mean(nums)
                         }, numeric(1))) %>% 
  select(PlotNo, Subplot, Month, Layer, Taxon, EuroMed, cover_10_m2, height, phen,	Seedling, Juvenile, 
         FlowerBud, Flowering, Fruiting, PostFruiting) %>% 
  summarise(cover_10_m2=mean(cover_10_m2, na.rm = TRUE),  # When species was in both 1m2 corners, it was entered in 10m2 only once (e.g. NE subplot) and left it blank for the other corner
            height=mean(height, na.rm = TRUE),
            Seedling=mean(Seedling, na.rm = TRUE), 
            Juvenile=mean(Juvenile, na.rm = TRUE), 
            FlowerBud=mean(FlowerBud, na.rm = TRUE), 
            Flowering=mean(Flowering, na.rm = TRUE), 
            Fruiting=mean(Fruiting, na.rm = TRUE),
            PostFruiting=mean(PostFruiting, na.rm = TRUE),
            .by=c("PlotNo", "Month", "Taxon", "EuroMed"))  %>% 
  mutate(across(c(Seedling, Juvenile, FlowerBud, Flowering, Fruiting, PostFruiting),
                ~ replace(., is.na(.), 0))) 

str(data_10m2)

# 10m2 scale --------------------------------------------------------------------------
data_all <- read_csv("data/raw_data/Sampling5.0Data.csv") %>%
  select(-`3.16m`) %>% # remove 10m2 cover column
  mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
  mutate(Month = lubridate::month(Date, label = TRUE, abbr = FALSE),
         .after =Date, .keep = "unused") %>% 
  mutate(Month = case_when(Month == "April" ~ "March",
                           Month == "August" ~ "July",
                           Month == "October" ~ "September",
                           .default = Month)) %>%
  rename("cover_1_m2" = "1m",
         "cover_0.1_m2" = "0.32m",
         "cover_0.01_m2" = "0.1m",
         "cover_0.001_m2" = "0.03m",
         "cover_0.0001_m2" = "0.01m",
         EuroMed = "Euro+Med Taxon") %>%
  mutate(Layer=ifelse(Layer=="S", "Seedling", Layer),
         cover_1_m2=ifelse(cover_1_m2=="X", 0.001, cover_1_m2), # X in cover_1_m2 columns means that we forgot to give the cover_1_m2 during the sample  to the species that were present on the plot. We replace "X" with a very low cover_1_m2 value to account for the species in species richness
         cover_0.1_m2=ifelse(cover_0.1_m2=="X", 0.001, cover_0.1_m2),
         cover_0.0001_m2=ifelse(cover_0.0001_m2=="X", 0.001, cover_0.0001_m2)) %>% 
  mutate(cover_1_m2=as.numeric(cover_1_m2),
         cover_0.1_m2=as.numeric(cover_0.1_m2),
         cover_0.0001_m2=as.numeric(cover_0.0001_m2)) %>%
  filter(Layer!="B", Layer!="L", # remove bryophytes & lichens
         Subplot!="EXT", # EXT means that we found species in the big 10^2m plot that are not inside the 1^2m nested subplots. Such species always have 0% cover_1_m2 in NW and SE corners
         !is.na(cover_1_m2)) %>%  # there is one NA in cover_1_m2 that is that species is not present
  select(PlotNo, Subplot, Month, Layer, Taxon, EuroMed, cover_0.0001_m2, cover_0.001_m2, cover_0.01_m2, cover_0.1_m2, cover_1_m2) %>% 
  summarise(cover_1_m2=sum(cover_1_m2, na.rm = TRUE),  # sum cover_1_m2 for repeted species to get unique species per plot
            cover_0.1_m2=sum(cover_0.1_m2, na.rm = TRUE),
            cover_0.01_m2=sum(cover_0.01_m2, na.rm = TRUE),
            cover_0.001_m2=sum(cover_0.001_m2, na.rm = TRUE),
            cover_0.0001_m2=sum(cover_0.0001_m2, na.rm = TRUE),
           .by=c("PlotNo", "Subplot", "Month", "Taxon", "EuroMed")) %>% 
  # merge with phenology and height data
  left_join(data_10m2 %>% 
              select(-cover_10_m2, -EuroMed),
            by=c("PlotNo", "Month", "Taxon")) %>%
  pivot_longer(cols = starts_with("cover_"),
               names_to = "Scale_m2",
               values_to = "cover") %>% 
  bind_rows(
    data_10m2 %>%
      rename(cover = "cover_10_m2") %>%
      mutate(Scale_m2 = "cover_10_m2",
             Subplot=NA)
    ) %>% 
  mutate(Scale_m2 = case_when(Scale_m2 == "cover_10_m2" ~ 10,
                              Scale_m2 == "cover_1_m2" ~ 1,
                              Scale_m2 == "cover_0.1_m2" ~ 0.1,
                              Scale_m2 == "cover_0.01_m2" ~ 0.01,
                              Scale_m2 == "cover_0.001_m2" ~ 0.001,
                              Scale_m2 == "cover_0.0001_m2" ~ 0.0001)) %>%
  relocate(c("Scale_m2","cover"), .after=EuroMed) %>%
  mutate(Month = factor(Month, levels = c("March", "May", "July", "September"))) %>% 
  mutate(Biomass = height * cover, .after=height) %>%  # biomass is calculated using scale-specific cover
  arrange(PlotNo,  Month, Taxon, Scale_m2)

str(data_all)

write_csv(data_all, "data/processed_data/vegetation_2025_all_scales.csv")





