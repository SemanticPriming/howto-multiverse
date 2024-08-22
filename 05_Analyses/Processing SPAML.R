en_data_all <- read.csv("05_Analyses/Data/en_full_data.csv.gz", encoding = "UTF-8") 
en_stimuli_data <- read.csv("05_Analyses/Data/en_words.csv", encoding = "UTF-8")
de_data_all <- read.csv("05_Analyses/Data/de_full_data.csv.gz", encoding = "UTF-8") 
de_stimuli_data <- read.csv("05_Analyses/Data/de_words.csv", encoding = "UTF-8")
match_stimuli <- read.csv("05_Analyses/Data/de_matched.csv", encoding = "UTF-8") %>%
  mutate_at(vars(en_cue, en_target), ~tolower(.)) #converts the English stimuli to lower case to later match with the other datasets

#create pairs in the stimuli files

en_stimuli_data$pair <- paste(en_stimuli_data$en_cue, en_stimuli_data$en_target, sep = "-")
de_stimuli_data$pair <- paste(de_stimuli_data$de_cue, de_stimuli_data$de_target, sep = "-")

processSPAML <- function(data_all, stimuli_data, language){
  
  ##create demographics only data
  demos <- data_all %>% 
    filter(sender == "Demographics Form")
  
  ##create experiment information data
  exp <- data_all %>%
    filter(sender == "Consent Form")
  
  ## deal with double consent form issue
  # here are the doubles  
  if(language == "english") {
    second_one <- exp %>% 
      filter(sender_id == 1)
    
    # find all the rows with sender_id == 0 and observation is in second one
    dup_rows <- exp %>% 
      filter(sender_id == 0 & temp_obs %in% second_one$temp_obs)
    
    # remove dup rows from en data all 
    data_all <- data_all %>% 
      anti_join(dup_rows)
  }

  participant_DF <- demos[,c("observation","timestamp", "which_year_were_you_born", "native_language")] #only select relevant participant info for the multiverse
  participant_DF$year <- as.numeric(substr(participant_DF$timestamp, 1, 4))
  participant_DF$age <- participant_DF$year - participant_DF$which_year_were_you_born
  participant_DF$native_match <- agrepl(language, tolower(participant_DF$native_language), max.distance = 0.2) & tolower(participant_DF$native_language) != "dutch" #checks whether native language matches with the language of the experiment 
  
  
  ##grab only real trials 
  real_trials <- data_all %>% #data frame
    filter(sender == "Stimulus Real") %>%  #filter out only the real stimuli
    select(observation, fix_sender, response, response_action, ended_on, duration,
           colnames(data_all)[grep("^time", colnames(data_all))], 
           word, class, correct_response, correct)
  
  real_trials <- real_trials %>% 
    filter(!(observation == "d71abbb5c-79bf" & fix_sender == "07_00_027_1")) %>%  # 2106 trials were due to a bad set that did not get included in the algorithm to shuffle the trials. 
    filter(observation %in% exp$observation) # remove trials from observations without a Consent form row
  
  # figure out trial type ----
  # only select only a few columns
  priming_trials <- real_trials %>% 
    # note that we don't exclude trials here because we need to keep 
    # them in order to pair together cue-target
    # they will be excluded in a minute 
    select(observation, duration, word, class, response, correct, 
           fix_sender, timestamp, ended_on) %>% 
    arrange(observation, fix_sender)
  # add trial code and if it's cue/target
  priming_trials$trial_code <- NA
  priming_trials$which <- NA
  priming_trials$block <- NA
  # add that information 
  for (person in unique(priming_trials$observation)){
    
    priming_trials$trial_code[priming_trials$observation == person] <- 
      rep(1:401, each = 2, length.out = length(priming_trials$trial_code[priming_trials$observation == person]))
    
    priming_trials$which[priming_trials$observation == person] <-
      rep(c("cue", "target"), times = 2, 
          length.out = length(priming_trials$trial_code[priming_trials$observation == person]))
    
    priming_trials$block[priming_trials$observation == person] <-
      rep(1:8, each = 100, 
          length.out = length(priming_trials$trial_code[priming_trials$observation == person]))
    
  }
  
  priming_trials <- priming_trials %>% 
    left_join(participant_DF[,c("observation","age", "native_match")], by = "observation")
  
  
  # for target trials, create a string pasting together the prime (previous trial) and target; for prime trials it just gives NA
  priming_trials$pair <- ifelse(priming_trials$which == "target", paste(lag(priming_trials$word), priming_trials$word, sep = "-"), NA)
  
  priming_trials <- priming_trials %>% 
    left_join(stimuli_data[,c("pair", "type")], by = "pair")
  
  return(priming_trials)
}

en_SPAML <- processSPAML(data_all = en_data_all, stimuli_data = en_stimuli_data, language = "english")
de_SPAML <- processSPAML(data_all = de_data_all, stimuli_data = de_stimuli_data, language = "deutsch")

# Create dataframe with translations
translations <- en_stimuli_data %>% 
  filter(type != "nonword") %>%
  select(type, en_cue, en_target) %>%
  pivot_wider(names_from = type, values_from = en_cue) %>% 
  mutate(target_id = row_number()) %>%
  left_join(match_stimuli[,c("en_cue", "de_cue")], by = c("related" = "en_cue")) %>% 
  left_join(match_stimuli[,c("en_cue", "de_cue")], by = c("unrelated" = "en_cue")) %>% 
  left_join(match_stimuli[,c("en_target", "de_target")], by = c("en_target")) %>%
  `colnames<-` (c("en_target", "en_cue_rel", "en_cue_unrel", "target_id", "de_cue_rel", "de_cue_unrel", "de_target"))

translations$en_rel <- paste(translations$en_cue_rel, translations$en_target, sep = "-")
translations$en_unrel <- paste(translations$en_cue_unrel, translations$en_target, sep = "-")
translations$de_rel <- paste(translations$de_cue_rel, translations$de_target, sep = "-")
translations$de_unrel <- paste(translations$de_cue_unrel, translations$de_target, sep = "-")

#note that a substantial number of the unrelated German pairs (de_unrel) do not occur in the data because the resulting similarity (cosine distance) was too high
#hence we can't simply use translations to match them, and we also can't just use the translations of the targets, because some English targets had the same translations
#the following part of the code is a way to still create matching item pairings across the two languages

german_targets <- table(translations$de_target)
duplicates <- names(german_targets[german_targets > 1]) #targets that occur more than once in the data
uniques <- names(german_targets[german_targets == 1]) #targets that occur once in the data

translations$match <- translations$de_unrel %in% de_stimuli_data$pair #if you just translate the English items, do you then get a pairing that is actually used in German (TRUE or FALSE)

translations_duplicates = translations[translations$de_target %in% duplicates,] #these are the pairings involving the duplicates
translations_duplicates_match = translations_duplicates[translations_duplicates$match,] #these are the pairings involving the duplicates that match with English translations
translations_duplicates_nomatch = translations_duplicates[!translations_duplicates$match,] #these are the pairings involving the duplicates that do not match with English translations

# get unique unrelated pairs that are actually used in the study for the duplicate targets (this doesn't occur for related pairs)
for (i in translations_duplicates_nomatch$de_target){
  translations_duplicates$de_unrel[translations_duplicates$de_target == i & !translations_duplicates$de_unrel %in% de_stimuli_data$pair] <- de_stimuli_data$pair[de_stimuli_data$type == "unrelated" & de_stimuli_data$de_target == i & !de_stimuli_data$pair %in% translations_duplicates_match$de_unrel]
}  
  
de_SPAML$target_id <- NULL
for (i in 1: nrow(de_SPAML)){
  if(de_SPAML$type[i] == "nonword" | de_SPAML$which[i] == "cue"){
    de_SPAML$target_id[i] <- NA #for all nonword trials and cue trials target_id is NA
  } else if(de_SPAML$word[i] %in% uniques){ #if the target features in only one related_cue-unrelated_cue-target triplet, it's ok to use target id corresponding with the target
    de_SPAML$target_id[i] <- translations$target_id[translations$de_target == de_SPAML$word[i]]
  } else if(de_SPAML$type[i] == "related"){ #for duplicate targets featuring in related pairs, we can use the translation of the pairing to match the id's
    de_SPAML$target_id[i] <- translations_duplicates$target_id[translations_duplicates$de_rel == de_SPAML$pair[i]]
  } else { #for duplicate targets featuring in unrelated pairs, we can use the translation of the pairing to match the id's, some of which we modified in the previous for loop to make sure they actually occurred in the experiment
    de_SPAML$target_id[i] <- translations_duplicates$target_id[translations_duplicates$de_unrel == de_SPAML$pair[i]]
  }
}

#for English, we can just use the targets to match the ids
en_SPAML <- left_join(en_SPAML, translations[, c("target_id", "en_target")], by = c("word" = "en_target"))

write.csv(de_SPAML, "05_Analyses/Data/de_SPAML.csv", row.names = FALSE)
write.csv(en_SPAML, "05_Analyses/Data/en_SPAML.csv", row.names = FALSE)

#old code matching based on translated pairs (doesn't work, because some unrelated pairs had to be reshuffled; see above)
#en_key <- pivot_longer(translations[, c("target_id", "en_rel", "en_unrel")], cols = -c("target_id"), names_to = "type",  values_to = "pair")  
#de_key <- pivot_longer(translations[, c("target_id", "de_rel", "de_unrel")], cols = -c("target_id"), names_to = "type",  values_to = "pair")  
#de_SPAML <- left_join(de_SPAML, de_key[, c("target_id", "pair")], by = c("pair"))


#some code for item-level priming correlations
#de_unrelated = aggregate(duration ~ target_id, data = de_SPAML[de_SPAML$type=="unrelated" & !is.na(de_SPAML$type),], FUN = median)
#de_related = aggregate(duration ~ target_id, data = de_SPAML[de_SPAML$type=="related" & !is.na(de_SPAML$type),], FUN = median)
#de_unrelated[,2] -de_related[,2]

#en_unrelated = aggregate(duration ~ target_id, data = en_SPAML[en_SPAML$type=="unrelated" & !is.na(en_SPAML$type),], FUN = median)
#en_related = aggregate(duration ~ target_id, data = en_SPAML[en_SPAML$type=="related" & !is.na(en_SPAML$type),], FUN = median)

#cor(de_unrelated[,2] - de_related[,2], en_unrelated[,2] - en_related[,2])
