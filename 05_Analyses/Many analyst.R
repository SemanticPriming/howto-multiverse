set.seed(42)

correlationIndividualPipelines <- matrix(NA, ncol = 2, nrow = length(keepLabels))
for (i in 1:length(keepLabels)){
  correlationIndividualPipelines[i,] <- keepLabels[[i]] %>% pipeline()
  print(i)
}

write.csv(correlationIndividualPipelines, "../Analyses/Results/correlationIndividualPipelines.csv", row.names = FALSE)
