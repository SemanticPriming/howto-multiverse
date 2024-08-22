library(qualtRics)
library(stringi)
library(dplyr)
library(tidyr)
library(purrr)
#library(devtools) #uncomment if you need to install the silvermantest package
#install_github("jenzopr/silvermantest")
library(silvermantest)
library(ggplot2)
library(gridExtra)

set.seed(42) #relevant for Silverman's test

source("05_Analyses/Functions multiverse.R")
#source("05_Analyses/Processing SPAML.R") #if you run for the first time comment out this line

en_SPAML <- read.csv("05_Analyses/Data/en_SPAML.csv")
de_SPAML <- read.csv("05_Analyses/Data/de_SPAML.csv")

dataSurvey <- read_survey("04_Pathway_Survey/Data/Pathway validation survey.csv")
dataSurvey <- dataSurvey[as.POSIXct(dataSurvey$StartDate, tz = "UTC") > as.POSIXct("2024-01-12 13:30:00", tz = "UTC"),] # removes responses before the actual start of the survey (i.e., pilot runs, and so-called survey previews)
dataSurvey <- dataSurvey[dataSurvey$Progress==100,] #only keep completed surveys #56 completed responses from 60 participants in total (in four case, two people collaborated)
round(median(dataSurvey$`Duration (in seconds)`)/60, 0)

names(dataSurvey) <- gsub("ID", "", names(dataSurvey))
attributes(dataSurvey)$column_map["qname"] <- names(dataSurvey)
legend <- attributes(dataSurvey)$column_map

#Comprehension check questions
dataSurvey$Comprehension <- c(dataSurvey$Q48 == "Semantic priming at the item level in terms of RTs") + c(dataSurvey$Q49 == "Continuous lexical decision (responding to both primes and targets)") + c(dataSurvey$Q51 == "Determining the robustness of a finding by considering different data-processing pathways that are arguably equivalent") + c(dataSurvey$Q53 == "For each cluster, you can select more than one, or all options as appropriate") + c(dataSurvey$Q55 == "You have to order all appropriate options from best/most preferred (1) to worst/least preferred (n = number of appropriate options), yet still appropriate")
propComprehension <- prop.table(table(dataSurvey$Comprehension))
propExpertise <- prop.table(table(factor(dataSurvey$Q38_1, levels = c("Very low", "Low", "Moderate", "High", "Very high"))))
propConfidence <- prop.table(table(factor(dataSurvey$Q38_2, levels = c("Very low", "Low", "Moderate", "High", "Very high"))))

png("05_Analyses/Images/Proficiency.png", width = 1100, height = 1200, res = 250)
par(mfrow = c(3, 1))

stackedBar(propExpertise, main = "Self-rated expertise", labels = names(propExpertise), posLegend = c(-0.3, 0.7), margins = c(3.5, 4, 1.5, 10) + 0.1)
stackedBar(propConfidence, main = "Self-rated confidence", margins = c(3.5, 4, 1.5, 10) + 0.1)
stackedBar(propComprehension, main = "Number of correctly answered comprehension questions", labels = names(propComprehension), posLegend = c(-0.2, -0.1))
dev.off()

###################################################
## Multiverse based on appropriateness threshold ##
###################################################

#Questions about appropriateness of a given option
variableNames <- paste0("Q", seq(1, 35, 2), "_")
selectedVars <- unlist(lapply(variableNames, function(x) grep(x, names(dataSurvey), value = TRUE))) #select the corresponding variable names
dataApprop <- dataSurvey[, selectedVars] #dataset containing the appropriateness variables
propApprop <- lapply(dataApprop, function(x) sum(x == "Appropriate")/length(x)) #proportion of participants who indicated that a certain option was appropriate

questionsThreshold <- names(propApprop[propApprop > .50]) #for a given category, most participants (>50%) actually agreed that the option is appropriate
questionsThresholdDescription <- cbind(questionsThreshold, sub("^.*?-", "", legend$description[legend$qname %in% questionsThreshold])) #description corresponding to those options that meet the threshold
#Q25_2 was not correctly worded hence not included in the analyses below

en_SPAML <- list(base = en_SPAML)
de_SPAML <- list(base = de_SPAML)

#old code, gives memory problems
#en_dataMultiverse <- en_SPAML %>% decision1(exclBelow18 = TRUE) %>% decision2(exclNonNative = TRUE) %>% decision4(exclBelow100trials = TRUE) %>% decision5(exclSameResp = TRUE) %>% decision6(exclAltResp = TRUE) %>% decision7(excl40 = TRUE, excl70 = TRUE, excl3SD = TRUE, exclMAD = TRUE, excl25NW = TRUE, excl25W = TRUE, excl30WNW = TRUE, exclProptest = TRUE)  %>% decision8(excl25 = TRUE, excl50 = TRUE) %>% decision9(exclError = TRUE) %>% decision10(exclFirstTrial = TRUE, keepRegardless = TRUE) %>% decision11(exclNegRTs = TRUE) %>% decision12(excl25250 = TRUE, keepRegardless = TRUE) %>% decision13(exclTO50 = TRUE) %>% decision14(exclPart2.5SD = TRUE, exclPartMAD = TRUE, keepRegardless = TRUE) %>% decision15(exclIt2.5SD = TRUE, keepRegardless = TRUE) %>% decision16(excl50ms = TRUE, excl100ms = TRUE, excl150ms = TRUE, excl160ms = TRUE, excl200ms = TRUE) %>% decision17(excl2500ms = TRUE, excl3000ms = TRUE) %>% decision18(exclTrial3SD = TRUE, exclIt3SD = TRUE, exclPart3SD = TRUE) %>% removeNonwords() %>% removeFillerwords() %>% ztransform() %>% itemPriming() 
#de_dataMultiverse <- de_SPAML %>% decision1(exclBelow18 = TRUE) %>% decision2(exclNonNative = TRUE) %>% decision4(exclBelow100trials = TRUE) %>% decision5(exclSameResp = TRUE) %>% decision6(exclAltResp = TRUE) %>% decision7(excl40 = TRUE, excl70 = TRUE, excl3SD = TRUE, exclMAD = TRUE, excl25NW = TRUE, excl25W = TRUE, excl30WNW = TRUE, exclProptest = TRUE)  %>% decision8(excl25 = TRUE, excl50 = TRUE) %>% decision9(exclError = TRUE) %>% decision10(exclFirstTrial = TRUE, keepRegardless = TRUE) %>% decision11(exclNegRTs = TRUE) %>% decision12(excl25250 = TRUE, keepRegardless = TRUE) %>% decision13(exclTO50 = TRUE) %>% decision14(exclPart2.5SD = TRUE, exclPartMAD = TRUE, keepRegardless = TRUE) %>% decision15(exclIt2.5SD = TRUE, keepRegardless = TRUE) %>% decision16(excl50ms = TRUE, excl100ms = TRUE, excl150ms = TRUE, excl160ms = TRUE, excl200ms = TRUE) %>% decision17(excl2500ms = TRUE, excl3000ms = TRUE) %>% decision18(exclTrial3SD = TRUE, exclIt3SD = TRUE, exclPart3SD = TRUE) %>% removeNonwords() %>% removeFillerwords() %>% ztransform() %>% itemPriming() 
#correlations(de_dataMultiverse, en_dataMultiverse)

#source("05_Analyses/Multiverse analysis.R") #if you run for the first time comment out this line

resultsDefaultOrder <- read.csv("05_Analyses/Results/correlationMultiverseDefaultOrder.csv")
resultsAltOrder <- read.csv("05_Analyses/Results/correlationMultiverseAltOrder.csv")

summary(resultsDefaultOrder)
summary(resultsAltOrder)

png("05_Analyses/Images/Multiverse.png", width = 2000, height = 800, res = 250)
par(mfrow = c(1, 2))
hist(resultsDefaultOrder$correlation, xlab = "Correlation", main = "Default order validation survey", las = 1)
hist(resultsAltOrder$correlation, xlab = "Correlation", main = "Alternative order", las = 1)
dev.off()

###########################################
## Multiverse as a many-analyst approach###
###########################################

dataOrder <- dataSurvey[, grep("^Q37", names(dataSurvey))]
steps <- apply(dataOrder, 1, function(x) sort(x[!is.na(x)])) #extract all the non-NAs for every row (i.e., all the processing steps), and sort them
labels <- lapply(steps, function (x) as.numeric(sub(".*_", "", names(x)))) #extract the question labels corresponding to these steps

#question labels higher than 36 refer to steps one would always take (e.g., removing nonwords); they don't have alternatives
#the remaining labels can be divided in odd and even numbers
#odd numbers always refer to questions where participants had to indicate whether an option was appropriate; if these are included, it means that a participant only considered one option as appropriate for that particular decision
#even numbers refer to questions where participants sorted the appropriate options from best to worst (yet still appropriate); to construct the single pipeline for a participant, the top option is selected
#this all happens in the following for loop
for(i in 1:length(labels)) {
  labels[[i]] <- sapply(labels[[i]], function (x) {
    if(x > 36) paste0("Q", x)
    else if (x %% 2 != 0) { #odd numbers
      var <- paste0("Q", x, "_")
      subset <- dataSurvey[i, grep(var, names(dataSurvey), value = TRUE)]
      colnames(subset)[subset[1,]=="Appropriate"]
    }
    else { #even numbers
      var <- paste0("Q", x, "_")
      subset <- dataSurvey[i, grep(var, names(dataSurvey), value = TRUE)]
      names <- colnames(subset)[subset[1,]==1]
      names[!is.na(names)]
    } 
  })
}


# Apply the function processLabels to each element of labels (see Functions multiverse.R)
labels <- lapply(labels, function(x) sapply(x, processLabels))

#Only keep the pipelines where the last two decisions involve calculating the priming effects and then correlating them
keep <- sapply(labels, checkLastTwo)
keepLabels <- labels[keep]
keepLabels <- lapply(keepLabels, function(x){x[! x %in% c("decision13_2", "decision23")]}) # remove the "decision" to calculate the correlation (decision23) and the incorrectly worded decision (see above)

#source("05_Analysis/Many analyst.R") #if you run for the first time comment out this line

correlationIndividualPipelines <- read.csv("05_Analyses/Results/correlationIndividualPipelines.csv")
colnames(correlationIndividualPipelines) <- c("correlation", "pValue")

#Manually check all pipelines to see whether they are internally consistent
correlationIndividualPipelines$consistent <- rep(1, nrow(correlationIndividualPipelines))
correlationIndividualPipelines$consistent[1] <- 0 #the decision to perform a proportion test to examine whether participants perform above chance occurs after removing nonwords, so the pipeline is inconsistent
correlationIndividualPipelines$consistent[2] <- 0 #the decision to remove items if they don't reach 50% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[7] <- 0 #the decision to remove participants if they are faster than 250 ms on more than 25% of trials occurs after removing trials with RTs faster than 250 ms, hence not consistent
correlationIndividualPipelines$consistent[9] <- 0 #the decision to remove participants if they are faster than 250 ms on more than 25% of trials occurs after removing trials with RTs faster than 250 ms, hence not consistent
correlationIndividualPipelines$consistent[10] <- 0 #the decision to remove items if they don't reach 50% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[18] <- 0 #the decision to remove items if they don't reach 50% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[20] <- 0 #the decision to remove participants if they don't reach the MAD accuracy criterion occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[23] <- 0 #the decision to remove participants if they are faster than 250 ms on more than 25% of trials occurs after removing trials with RTs faster than 250 ms, hence not consistent
correlationIndividualPipelines$consistent[27] <- 0 #the decision to remove items if they don't reach 50% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[30] <- 0 #the decision to remove participants if they don't reach the 3 SD accuracy criterion occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[31] <- 0 #the decision to remove items if they don't reach 50% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[36] <- 0 #the decision to remove participants if they don't reach the 3 SD accuracy criterion occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[37] <- 0 #the decision to remove items if they don't reach 25% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[40] <- 0 #the decision to remove items if they don't reach 25% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[42] <- 0 #the decision to remove items if they don't reach 50% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[43] <- 0 #the decision to remove items if they don't reach 50% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[45] <- 0 #the decision to remove items if they don't reach 50% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[50] <- 0 #the decision to remove items if they don't reach 50% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[51] <- 0 #the decision to remove items if they don't reach 50% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent[53] <- 0 #the decision to remove items if they don't reach 50% correct occurs after removing incorrect trials, hence not consistent
correlationIndividualPipelines$consistent <- factor(correlationIndividualPipelines$consistent, labels = c("Inconsistent", "Consistent"))
correlationIndividualPipelines$SilvermanIncluded <- sapply(keepLabels, function(x){"decision3_1" %in% x}) # paths that included Silverman's test
correlationIndividualPipelines$SilvermanIncluded <- factor(correlationIndividualPipelines$SilvermanIncluded, labels = c("Not included", "Included"))

plot1 <- ggplot(correlationIndividualPipelines, aes(x = "", y = correlation)) +
  geom_violin(trim = FALSE) + 
  geom_jitter(aes(color = consistent), width = 0.2, size = 1.5) +
  scale_color_manual(values = c("Consistent" = "blue", "Inconsistent" = "red")) +
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "Correlation",
       color = "Consistency paths") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot2 <- ggplot(correlationIndividualPipelines, aes(x = "", y = correlation)) +
  geom_violin(trim = FALSE) + 
  geom_jitter(aes(color = SilvermanIncluded), width = 0.2, size = 1.5) +
  scale_color_manual(values = c("Not included" = "blue", "Included" = "red")) +
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "Correlation",
       color = "Silverman's test") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

png("05_Analyses/Images/Many-analyst.png", width = 2000, height = 800, res = 250)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

summary(correlationIndividualPipelines$correlation)
