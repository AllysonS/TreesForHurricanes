### Wind Resistance Species Study - initial model fitting
### Created by: Allyson Salisbury
### Project website: github.com/AllysonS/TreesForHurricanes/upload/main
### License: MIT

###
### Libraries ----
###

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(caret)
library(randomForest)
library(combinat) 
library(stringr)

###
### Set working directory ----
###

setwd()

###
### Import data ----
###

## study methods data
metho <- read_xlsx("Wind_resistance_methods.xlsx", sheet = "Data", 
                   range = "A2:AC107") # can trim off this IBTRaCS since some studies have multiple tropical cyclones
methsel <- metho %>% dplyr::select(Wind_Author_Year, Language, Country, State_Territory_Prefecture, Urban_Rural, Urban_Rural2,
                                   `Long-Term_Study`, Sampling_score, PreInventory_score, Damage_Categorization_Score, 
                                   Risk_Condition_Assessment_score, Biometric_score) %>%
  rowwise() %>%
  mutate(QualityScore = sum(Sampling_score, PreInventory_score, Damage_Categorization_Score, 
                            Risk_Condition_Assessment_score, Biometric_score))

## consolidated table of damage, trait, and environmental data
alldfo <- read.csv("01_spp_damage_trait_env.csv")
alldf <- alldfo %>%
  left_join(methsel, by = "Wind_Author_Year") %>%
  mutate(RatingYN = ifelse(is.na(Wind_resistance_rating), "N", "Y"),
         Wind_resistance_rating = factor(Wind_resistance_rating, levels = c("LOWEST", "MEDIUM LOW", "MEDIUM HIGH", "HIGHEST"))) %>%
  # filter(!is.na(ANY_damage_percent)) %>%
  filter(!(is.na(Mortality_percent) & is.na(Damaged_ALL_percent) & is.na(Root_Fail_percent) & is.na(Stem_Fail_percent))) %>%
  mutate(ObsNum = seq(1:nrow(.))) %>%
  mutate(across(c(Urban_Rural, AngioGymno, LeafType, Family, Order, Genus, Biome_Name, Realm_Name), 
                factor)) %>%
  mutate(ANY_damage_percent = ifelse(!is.na(Damaged_ALL_percent), Damaged_ALL_percent,
                                     ifelse(!is.na(Mortality_percent), Mortality_percent, 
                                            ifelse(!is.na(Root_Fail_percent), Root_Fail_percent,
                                                   ifelse(!is.na(Stem_Fail_percent), Stem_Fail_percent, NA))))) %>%
  filter(Palm == "No" & Monocot == "No") # This analysis is just for trees

## Common names
comm <- read.csv("Species_common_names.csv")

###
### Prep data ----
###

## subselect tree species with Duryea ratings
durdf <- alldf %>% 
  filter(!is.na(Wind_resistance_rating) & Palm == "No" & Monocot == "No")
nrow(durdf) # 399

## predictors to use in the model
preds <- c("Urban_Rural", "AngioGymno", "YearSincePrevTC_50km",
              "MeanWoodDensity.gcm3", "MeanPlantHeight.m", "LMA.gm2", "LeafType",
              "Lat", "Long", "Biome_Name", "ANY_damage_percent")

## exclude incomplete data for training the model
durdf2 <- durdf %>% filter(!is.na(MeanWoodDensity.gcm3)) %>%
  filter(!is.na(LMA.gm2)) %>%
  filter(!is.na(MeanPlantHeight.m)) 
nrow(durdf2) # should be 369

## split training and testing data 
set.seed(7)
ratings <- c("LOWEST", "MEDIUM LOW", "MEDIUM HIGH", "HIGHEST")
tmp <- lapply(ratings, function(x){
  # hold <- durdf2[which(durdf2 $ Wind_resistance_rating == x), ]
  durdf2 %>% filter(Wind_resistance_rating == x) %>%
    slice_sample(n = round(0.70 * nrow(.)), replace = FALSE)
})
train101 <- bind_rows(tmp)
test101 <- durdf2 %>%
  filter(!(ObsNum %in% unlist(train101 $ ObsNum)))
nrow(train101) # should be 277
nrow(test101) # should be 92

## Create sets of data missing different types of numeric data (all other data columns are complete)
misslistInput <- list(OriginalData = test101,
                      No.Damage = mutate(test101, ANY_damage_percent = NA),
                      No.Wood.Density = mutate(test101, MeanWoodDensity.gcm3 = NA),
                      No.Plant.Height = mutate(test101, MeanPlantHeight.m = NA),
                      No.LMA = mutate(test101, LMA.gm2 = NA), 
                      No.Wood.Density.and.Plant.Height = mutate(test101, MeanWoodDensity.gcm3 = NA, MeanPlantHeight.m = NA), 
                      No.Wood.Density.and.LMA = mutate(test101, MeanWoodDensity.gcm3 = NA, LMA.gm2 = NA), 
                      No.Plant.Height.and.LMA = mutate(test101, MeanPlantHeight.m = NA, LMA.gm2 = NA),
                      No.Wood.Density.Plant.Height.and.LMA = mutate(test101, MeanWoodDensity.gcm3 = NA, MeanPlantHeight.m = NA, LMA.gm2 = NA)) 



###
### Set up model ----
###

## set up 10-fold cross validation with 5 repeats
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5) # number = k

## run the model with training data
set.seed(7)
rftrain <- train(x = train101[ , preds], y = train101[ , "Wind_resistance_rating"],
                 tuneLength = 10, ntrees = 1000, method = "rf", trControl = ctrl)

## apply model to training data
train101 $ Pred <- predict(rftrain, newdata = train101)

## Extract mtry (the number of variables tried at each node)
rftrain $ bestTune # should be 8

# ## trying out basic random forest model
# rftrain2 <- randomForest(x = train101[ , preds], y = train101[ , "Wind_resistance_rating"], ntree = 1000,
#                          mtry = 8)

###
### Test model ----
###

## apply model to test data
## type = "raw" returns the actual classification
## type = "prob" returns the probability a species is classified into a particular group
test101 $ Pred <- predict(rftrain, newdata = test101, type = "raw")
test101 <- bind_cols(test101, predict(rftrain, newdata = test101, type = "prob"))  

## Evaluate accuracy metrics for training and test data
## Use adjusted Kappa since we have multiple categories as the response variable
predictedlst <- list(Training = train101, Testing = test101)
metricslst <- lapply(1:length(predictedlst), function(x){
  df <- predictedlst[[x]]
  cm <- confusionMatrix(data = df $ Pred, reference = df $ Wind_resistance_rating)
  adjK <- DescTools::CohenKappa(x = cm $ table, weights = "Equal-Spacing", conf.level = 0.05)
  acc <- cm $ overall
  data.frame(Model = names(predictedlst)[x], 
             Accuracy = acc["Accuracy"], 
             AccuracyLower = acc["AccuracyLower"], 
             AccuracyUpper = acc["AccuracyUpper"],
             NoInfoRate = acc["AccuracyNull"], 
             AccuracyPValue = acc["AccuracyPValue"], 
             AdjKappa = adjK["kappa"], 
             AdjKappaLower = adjK["lwr.ci"], 
             AdjKappaUpper = adjK["upr.ci"])
})

###
### Variable importance ----
###

## create a table of variable importance
impdf <- varImp(rftrain, scale = FALSE, type = 2) ## caret wrapper

## default variable importance plot
# plot(impdf)

## make usable dataframe
impdfrf <- data.frame(impdf $importance)


###
### Create a heat map of test species ----
###

# # create data frame of the "correct category" mark to heat map
# stardf <- test101 %>%
#  dplyr::select(LCVP_short, Wind_resistance_rating) %>%
#   distinct() %>%
#   dplyr::rename("ProbRating" = "Wind_resistance_rating") %>%
#   mutate(PredProb = 0.5) # just a placeholder
# 
# ## show predicted probabilities for each test species on a heat map
# test101 %>%
#   pivot_longer(cols = c("LOWEST", "MEDIUM LOW", "MEDIUM HIGH", "HIGHEST"), 
#                names_to = "ProbRating", values_to = "PredProb") %>%
#   group_by(LCVP_short, ProbRating) %>%
#   summarize(PredProb = mean(PredProb, na.rm = T)) %>%
#   mutate(ProbRating = factor(ProbRating, levels = c("LOWEST", "MEDIUM LOW", "MEDIUM HIGH", "HIGHEST"))) %>%
#   ggplot(aes(x = ProbRating, y = LCVP_short, fill = PredProb)) +
#   geom_tile(color = "gray20") +
#   geom_point(data = stardf, aes(x = ProbRating, y = LCVP_short)) +
#   scale_fill_gradient(low = "#F7D060", high = "#FF6D60") 

###
### Effects of Missing Data ----
###

# ## use the random forest models to predict rating for the test data set
# misstest <- lapply(misslistInput, function(x){
#   # ## remove test data from original data list
#   # alldfnotest <- alldf %>% filter(!(ObsNum %in% unlist(x $ ObsNum)))
#   # ## Add other original back in to improve imputation
#   # datnew <- bind_rows(x, alldfnotest) %>% dplyr::select(all_of(preds))
#   # ## Impute missing data
#   # imp <- preProcess(datnew, method = "bagImpute") #method = "knnImpute", k = 5 does not do a good job
#   imp <- preProcess(alldf[ , preds], method = "bagImpute") #method = "knnImpute", k = 5 does not do a good job
#   impdat <- predict(imp, x[ , preds])
#   ## predict imputed data to get wind rating
#   tst <- data.frame(Pred = predict(rftrain, newdata = impdat[ ,  preds])) 
#   return(bind_cols(x, tst))
# })
# names(misstest) <- names(misslistInput)
# 
# ## Evaluate accuracy metrics for training and test data
# ## Use adjusted Kappa since we have multiple categories as the response variable
# missmetricslst <- lapply(1:length(misstest), function(x){
#   df <- misstest[[x]]
#   cm <- confusionMatrix(data = df $ Pred, reference = df $ Wind_resistance_rating)
#   adjK <- DescTools::CohenKappa(x = cm $ table, weights = "Equal-Spacing", conf.level = 0.05)
#   acc <- cm $ overall
#   data.frame(Model = names(misstest)[x], 
#              Accuracy = acc["Accuracy"], 
#              AccuracyLower = acc["AccuracyLower"], 
#              AccuracyUpper = acc["AccuracyUpper"],
#              NoInfoRate = acc["AccuracyNull"], 
#              AccuracyPValue = acc["AccuracyPValue"], 
#              AdjKappa = adjK["kappa"], 
#              AdjKappaLower = adjK["lwr.ci"], 
#              AdjKappaUpper = adjK["upr.ci"])
# })
# missmetricsdf <- bind_rows(missmetricslst) %>%
#   mutate(Model = gsub(Model, pattern = "\\.", replace = " "))
# 
# ## graph overall accuracy
# missmetricsdf %>%
#   ggplot(aes(x = Model, y = Accuracy)) +
#   geom_errorbar(aes(x = Model, ymin = AccuracyLower, ymax = AccuracyUpper), width = 0) +
#   geom_point(size = 3, shape = 18) +
#   coord_flip(ylim = c(0, 1)) +
#   theme_bw()
# 
# ## graph adjusted kappa
# missmetricsdf %>%
#   ggplot(aes(x = Model, y = AdjKappa)) +
#   geom_errorbar(aes(x = Model, ymin = AdjKappaLower, ymax = AdjKappaUpper), width = 0) +
#   geom_point(size = 3, shape = 18) +
#   coord_flip(ylim = c(0, 1)) +
#   theme_bw()
# # the closer Kappa is to 1 the better; equal-spacing is for when the difference between groups is the same or deserves the same weight
# # https://www.datanovia.com/en/lessons/weighted-kappa-in-r-for-two-ordinal-variables/ 

###
### New species with incomplete data ----
###

## vector of numeric trait predictors
numpreds <- c("MeanWoodDensity.gcm3", "MeanPlantHeight.m", "LMA.gm2")

## Count number of in entire dataset species with incomplete data
traitqty <- alldf %>% dplyr::select(LCVP_Taxon, all_of(numpreds)) %>% 
  distinct() %>%
  mutate(No.Wood.Density = ifelse(is.na(MeanWoodDensity.gcm3), "yes", "no"),
         No.Plant.Height = ifelse(is.na(MeanPlantHeight.m), "yes", "no"),
         No.LMA = ifelse(is.na(LMA.gm2), "yes", "no"),
         No.Wood.Density.and.Plant.Height = ifelse(is.na(MeanWoodDensity.gcm3) & is.na(MeanPlantHeight.m), "yes", "no"),
         No.Wood.Density.and.LMA  = ifelse(is.na(MeanWoodDensity.gcm3) & is.na(LMA.gm2), "yes", "no"),
         No.Plant.Height.and.LMA =  ifelse(is.na(MeanPlantHeight.m) & is.na(LMA.gm2), "yes", "no"),
         No.Wood.Density.and.Height.and.LMA = ifelse(is.na(MeanWoodDensity.gcm3) & is.na(MeanPlantHeight.m) & is.na(LMA.gm2), "yes", "no")) %>%
  summarize(Missing.Wood.Density = length(No.Wood.Density[No.Wood.Density == "yes"]), #
            Missing.Plant.Height = length(No.Plant.Height[No.Plant.Height == "yes"]),
            Missing.LMA = length(No.LMA[No.LMA == "yes"]),
            Missing.Wood.Density.and.Plant.Height = length(No.Wood.Density.and.Plant.Height[No.Wood.Density.and.Plant.Height == "yes"]), #
            Missing.Wood.Density.and.LMA  = length(No.Wood.Density.and.LMA[No.Wood.Density.and.LMA == "yes"]),# 
            Missing.Plant.Height.and.LMA =  length(No.Plant.Height.and.LMA[No.Plant.Height.and.LMA == "yes"]),
            Missing.Wood.Density.and.Height.and.LMA = length(No.Wood.Density.and.Height.and.LMA[No.Wood.Density.and.Height.and.LMA == "yes"]), #
            Total.Species = n()) %>%
  pivot_longer(cols = everything(), names_to = "Trait", values_to = "Quantity") %>%
  mutate(Trait = gsub(Trait, pattern = "\\.", replacement = " "))

###
### Predict ratings for new species ----
###

## Remove observations that are missing wood density data or both plant height and LMA since imputing these values leads to poor model performance
newspp <- alldf %>% 
  filter(!(is.na(MeanWoodDensity.gcm3))) %>%
  filter(!(is.na(MeanPlantHeight.m) & is.na(LMA.gm2))) %>%
  filter(!(is.na(LeafType))) %>% # NA values in LeafType prevent predict after bagImpute from working properly
  filter(is.na(Wind_resistance_rating)) %>%
  mutate(ImputedTrait = ifelse(is.na(MeanWoodDensity.gcm3) | is.na(MeanPlantHeight.m), "Yes", "No"))

## impute missing data using entire data set (maximizes observations)
impall <- preProcess(alldf[ , preds], method = "bagImpute") 
newsppimp <- predict(impall, newdata = newspp[ , preds]) 

## Predict imputed data to get wind rating
## type = "raw" returns the actual classification
## type = "prob" returns the probability a species is classified into a particular group
newpreddf <- data.frame(newspp, 
                        Pred = predict(rftrain, newdata = newsppimp, type = "raw"),
                        Prob = predict(rftrain, newdata = newsppimp, type = "prob")) %>%
  mutate(Probability = ifelse(Pred == "LOWEST", Prob.LOWEST, 
                              ifelse(Pred == "MEDIUM LOW", Prob.MEDIUM.LOW,
                                     ifelse(Pred == "MEDIUM HIGH", Prob.MEDIUM.HIGH, Prob.HIGHEST)))) # extract probability of class belonging from relevant column

## make a nice version to export as raw output data
rawdf <- newpreddf %>%
  dplyr::select(-Wind_resistance_rating) %>%
  dplyr::rename("Damage_Data_Source" = "Wind_Author_Year", "Wood_Density.gm3" = "MeanWoodDensity.gcm3", 
                "Max_Height.m" = "MeanPlantHeight.m", "Wind_resistance_rating" = "Pred") %>%
  mutate(Confidence = ifelse(Probability <= 0.33, "Low Confidence", 
                             ifelse(Probability <= 0.66, "Moderate Confidence", "High Confidence")),
         Wind_resistance_rating = str_to_title(Wind_resistance_rating)) %>%
  dplyr::select(Scientific_Name, LCVP_Taxon, LCVP_GlobalID, 
                Wind_resistance_rating,
                Probability, Confidence,
                Genus, Family, Order,
                Tropical_Cyclone_Basin, Lat, Long, Biome_Name,
                Country, State_Territory_Prefecture, Urban_Rural, 
                IBT_Year, IBTRaCS, SID, YearSincePrevTC_50km, 
                Wood_Density.gm3, Max_Height.m, LMA.gm2, LeafType, 
                AngioGymno, ANY_damage_percent,
                ImputedTrait, 
                TraitSourceID)
  
write.csv(rawdf, "02_new_species_ratings_raw_output.csv", row.names = FALSE)
  

###
### Create a table for the workbook tool and manuscript ----
###

## Note: some species with multiple observations ended up with multiple rating assignments
## For the purpose of sharing data on github, I kept the rating assignments separate so that readers
## can make a judgement call how they will if they want to use the data. 
## For the purpose of the interactive community spreadsheet and manuscript, I consolidated the ratings assignment
## so that each species only has one rating, but those with multiple ratings receive a low confidence. 
## This should simplify the spreadsheet, plus the spreadsheet will have an option for users to adjust the rating
## to meet their local experiences as needed.

## Note that results are summarized over Tropical Cyclone Basin to simplify the workbook, rather than having results separated by TC Basin
## create a temporary table to flag species with multiple ratings and adjust confidence score accordingly
tmpdf <- newpreddf %>% 
  dplyr::select(Tropical_Cyclone_Basin, Family, LCVP_Taxon, LCVP_short, Country, Pred, Probability, ImputedTrait) %>% # trim down table
  group_by(Family, LCVP_Taxon, LCVP_short, ImputedTrait) %>% # consolidate multiple observations of same species
  summarise(Country = paste0(unique(Country), collapse = "; "), 
            Tropical_Cyclone_Basin = paste0(unique(Tropical_Cyclone_Basin), collapse = "; "),
            Wind_resistance_rating = paste0(unique(Pred), collapse = "; "), ## NECESSARY?
            Probability = max(Probability),
            Confidence = ifelse(Probability <= 0.33, "Low Confidence", 
                               ifelse(Probability <= 0.66, "Moderate Confidence", "High Confidence"))) %>% # assign categorical confidence level
  mutate(Confidence = ifelse(ImputedTrait == "Yes", "Low Confidence", Confidence), # assign low confidence if species had imputed traits
         Confidence = ifelse(grepl(x = Wind_resistance_rating, pattern = "\\;"), "Low Confidence", Confidence), # assign low confidence if a species was assigned different ratings based on separate observations
         MultRatings = ifelse(grepl(x = Wind_resistance_rating, pattern = "\\;"), "Yes", "No")) %>% # note if a species was assigned different ratings based on separate observations
  dplyr::select(-Probability)

## summarize table by species and select rating with highest probability
newxls <- newpreddf %>% 
  dplyr::select(Family, LCVP_Taxon, LCVP_short, Pred, Probability, ImputedTrait) %>% # trim down table
  dplyr::rename("Wind_resistance_rating" = "Pred") %>%
  group_by(Family, LCVP_Taxon, LCVP_short, ImputedTrait) %>% # consolidate multiple observations of same species
  slice(which.max(Probability)) %>% # for species with multiple ratings, choose the rating that had the highest probability
  left_join(dplyr::select(ungroup(tmpdf), Tropical_Cyclone_Basin, LCVP_Taxon, LCVP_short, Country, MultRatings, Confidence), by = c("LCVP_Taxon", "LCVP_short")) %>%
  arrange(Tropical_Cyclone_Basin, Family, LCVP_Taxon) %>%
  dplyr::select(-c(Probability)) # exclude numeric probability from spreadsheet tool

nrow(newxls)

## Create a matching table of the original species from Duryea
origrating <- alldf %>% #filter(Wind_Author_Year %in% c("Duryea et al 2007a", "Duryea et al 2007b")) %>%
  filter(!is.na(Wind_resistance_rating)) %>%
  dplyr::select(Tropical_Cyclone_Basin, Family, LCVP_Taxon, LCVP_short, Wind_resistance_rating, Country) %>%
  group_by(Family, LCVP_Taxon, LCVP_short) %>%
  summarise(Country = paste0(unique(Country), collapse = "; "), 
            Tropical_Cyclone_Basin = paste0(unique(Tropical_Cyclone_Basin), collapse = "; "),
            Wind_resistance_rating = paste0(unique(Wind_resistance_rating), collapse = "; ")) %>% 
  mutate(ImputedTrait = "No", 
         MultRatings = "No",
         Confidence = "Original Rating") %>%
  distinct()

nrow(origrating) == length(unique(durdf $ LCVP_Taxon)) # Note that origrating contains a few species that had a rating but not enough data for the model

## Combine the new and original species into a single table for the workbook
workbook <- bind_rows(newxls, origrating) %>%
  left_join(comm, by = "LCVP_Taxon") %>%
  dplyr::select(Tropical_Cyclone_Basin, Family, LCVP_short, LCVP_Taxon, Common_Name, Country, Wind_resistance_rating,
                Confidence, ImputedTrait, MultRatings) %>%
  mutate(Wind_resistance_rating = str_to_title(Wind_resistance_rating)) %>%
  dplyr::rename("Scientific_Name" = "LCVP_short") %>%
  bind_rows(data.frame(Scientific_Name = "Unknown", LCVP_Taxon = "Unknown", # leave Common_Name blank so user can fill that in themselves
                       Wind_resistance_rating = "Unknown", Confidence = "Unknown")) # add a row for unknown species in the workbook
  



nrow(workbook) == length(unique(durdf $ LCVP_Taxon)) + length(unique(newspp $ LCVP_Taxon)) + 1 # should be TRUE, plus 1 accounts for Unknown

## Export Workbook
write.csv(workbook, "02_new_species_ratings_workbook.csv", row.names = FALSE, na = "")
