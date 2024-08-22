correlationMultiverseDefaultOrder <- matrix(NA, ncol = 2, nrow = 8*2*2*2*3*2*5*2*3)
iteration <- 1
for (i in 1:8){
  for (j in 1:2){
    for (k in 1:2){
      for (l in 1:2){
        for (m in 1:3){
          for (n in 1:2){
            for (o in 1:5){
              for (p in 1:2){
                for (q in 1:3){
                  en_dataMultiverse <- en_SPAML %>% decision1(exclBelow18 = TRUE) %>% decision2(exclNonNative = TRUE) %>% decision4(exclBelow100trials = TRUE) %>% decision5(exclSameResp = TRUE) %>% decision6(exclAltResp = TRUE) %>% decision7(excl40 = i==1, excl70 = i==2, excl3SD = i==3, exclMAD = i==4, excl25NW = i==5, excl25W = i==6, excl30WNW = i==7, exclProptest = i==8) %>% decision8(excl25 = j==1, excl50 = j==2) %>% decision9(exclError = TRUE) %>% decision10(exclFirstTrial = k==1, keepRegardless = k==2) %>% decision11(exclNegRTs = TRUE) %>% decision12(excl25250 = l==1, keepRegardless = l==2) %>% decision13(exclTO50 = TRUE) %>% decision14(exclPart2.5SD = m==1, exclPartMAD = m==2, keepRegardless = m==3) %>% decision15(exclIt2.5SD = n==1, keepRegardless = n==2) %>% decision16(excl50ms = o==1, excl100ms = o==2, excl150ms = o==3, excl160ms = o==4, excl200ms = o==5) %>% decision17(excl2500ms = p==1, excl3000ms = p==2) %>% decision18(exclTrial3SD = q==1, exclIt3SD = q==2, exclPart3SD = q==3) %>% removeNonwords() %>% removeFillerwords() %>% ztransform() %>% itemPriming() 
                  de_dataMultiverse <- de_SPAML %>% decision1(exclBelow18 = TRUE) %>% decision2(exclNonNative = TRUE) %>% decision4(exclBelow100trials = TRUE) %>% decision5(exclSameResp = TRUE) %>% decision6(exclAltResp = TRUE) %>% decision7(excl40 = i==1, excl70 = i==2, excl3SD = i==3, exclMAD = i==4, excl25NW = i==5, excl25W = i==6, excl30WNW = i==7, exclProptest = i==8) %>% decision8(excl25 = j==1, excl50 = j==2) %>% decision9(exclError = TRUE) %>% decision10(exclFirstTrial = k==1, keepRegardless = k==2) %>% decision11(exclNegRTs = TRUE) %>% decision12(excl25250 = l==1, keepRegardless = l==2) %>% decision13(exclTO50 = TRUE) %>% decision14(exclPart2.5SD = m==1, exclPartMAD = m==2, keepRegardless = m==3) %>% decision15(exclIt2.5SD = n==1, keepRegardless = n==2) %>% decision16(excl50ms = o==1, excl100ms = o==2, excl150ms = o==3, excl160ms = o==4, excl200ms = o==5) %>% decision17(excl2500ms = p==1, excl3000ms = p==2) %>% decision18(exclTrial3SD = q==1, exclIt3SD = q==2, exclPart3SD = q==3) %>% removeNonwords() %>% removeFillerwords() %>% ztransform() %>% itemPriming() 
                  correlationMultiverseDefaultOrder[iteration,] <- matchItems(en_dataMultiverse, de_dataMultiverse) %>% correlations() %>% unlist()
                  iteration <- iteration + 1
                  print(iteration)
                }
              }
            }
          }
        }
      }
    }  
  }
}
#Note: for the paths that include a proportion test to see whether participants are performing above chance on word and nonword trials (decision7), it will return a warning for the German data because one participant has very few trials where they actually responded to
#This participant wasn't filtered out in the minimum 100 trials step (decision4), because there we also counted timeout trials
colnames(correlationMultiverseDefaultOrder) <- c("correlation", "pValue")
write.csv(correlationMultiverseDefaultOrder, "../Analyses/Results/correlationMultiverseDefaultOrder.csv", row.names = FALSE)

correlationMultiverseAltOrder <- matrix(NA, ncol = 2, nrow = 8*2*2*2*3*2*5*2*3)
iteration <- 1
for (i in 1:8){
  for (j in 1:2){
    for (k in 1:2){
      for (l in 1:2){
        for (m in 1:3){
          for (n in 1:2){
            for (o in 1:5){
              for (p in 1:2){
                for (q in 1:3){
                  en_dataMultiverse <- en_SPAML %>% decision1(exclBelow18 = TRUE) %>% decision2(exclNonNative = TRUE) %>% decision4(exclBelow100trials = TRUE) %>% decision5(exclSameResp = TRUE) %>% decision6(exclAltResp = TRUE) %>% decision7(excl40 = i==1, excl70 = i==2, excl3SD = i==3, exclMAD = i==4, excl25NW = i==5, excl25W = i==6, excl30WNW = i==7, exclProptest = i==8) %>% decision8(excl25 = j==1, excl50 = j==2) %>% decision9(exclError = TRUE) %>% decision10(exclFirstTrial = k==1, keepRegardless = k==2) %>% decision11(exclNegRTs = TRUE) %>% decision12(excl25250 = l==1, keepRegardless = l==2) %>% decision13(exclTO50 = TRUE) %>% decision14(exclPart2.5SD = m==1, exclPartMAD = m==2, keepRegardless = m==3) %>% decision15(exclIt2.5SD = n==1, keepRegardless = n==2) %>% decision16(excl50ms = o==1, excl100ms = o==2, excl150ms = o==3, excl160ms = o==4, excl200ms = o==5) %>% decision17(excl2500ms = p==1, excl3000ms = p==2) %>% decision18(exclTrial3SD = q==1, exclIt3SD = q==2, exclPart3SD = q==3) %>% ztransform() %>% removeNonwords() %>% removeFillerwords() %>% itemPriming() 
                  de_dataMultiverse <- de_SPAML %>% decision1(exclBelow18 = TRUE) %>% decision2(exclNonNative = TRUE) %>% decision4(exclBelow100trials = TRUE) %>% decision5(exclSameResp = TRUE) %>% decision6(exclAltResp = TRUE) %>% decision7(excl40 = i==1, excl70 = i==2, excl3SD = i==3, exclMAD = i==4, excl25NW = i==5, excl25W = i==6, excl30WNW = i==7, exclProptest = i==8) %>% decision8(excl25 = j==1, excl50 = j==2) %>% decision9(exclError = TRUE) %>% decision10(exclFirstTrial = k==1, keepRegardless = k==2) %>% decision11(exclNegRTs = TRUE) %>% decision12(excl25250 = l==1, keepRegardless = l==2) %>% decision13(exclTO50 = TRUE) %>% decision14(exclPart2.5SD = m==1, exclPartMAD = m==2, keepRegardless = m==3) %>% decision15(exclIt2.5SD = n==1, keepRegardless = n==2) %>% decision16(excl50ms = o==1, excl100ms = o==2, excl150ms = o==3, excl160ms = o==4, excl200ms = o==5) %>% decision17(excl2500ms = p==1, excl3000ms = p==2) %>% decision18(exclTrial3SD = q==1, exclIt3SD = q==2, exclPart3SD = q==3) %>% ztransform() %>% removeNonwords() %>% removeFillerwords() %>% itemPriming() 
                  correlationMultiverseAltOrder[iteration,] <- matchItems(en_dataMultiverse, de_dataMultiverse) %>% correlations() %>% unlist()
                  iteration <- iteration + 1
                  print(iteration)
                }
              }
            }
          }
        }
      }
    }  
  }
}

colnames(correlationMultiverseAltOrder) <- c("correlation", "pValue")
write.csv(correlationMultiverseAltOrder, "../Analyses/Results/correlationMultiverseAltOrder.csv", row.names = FALSE)
