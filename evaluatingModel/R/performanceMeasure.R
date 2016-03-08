#' Performance Measure function
#'
#' This function allows you to measure performance of actual and predicted values from a model
#' @param actual and predicted values
#' @return object with various performance measures
#' @keywords confusion matrix
#' @export
#' @examples
#' performanceMeasure()

performanceMeasure <- function(actual, predicted) {
  #TODO: should be able to take a confusion matrix of any dimension.
  
  cM = table(actual,predicted)
  print("Confusion Matrix")
  print(cM)
  if (sum(dim(cM) == c(2,2))==2) {
    
    TN <- cM[1]
    TP <- cM[4]
    FP <- cM[2]
    FN <- cM[3]
    
    accuracy <- (TP + TN)/(TP+TN+FP+FN)
    precision <- TP/(TP + FP)
    recall <- TP/ (TP + FN)
    PPV <- precision
    sensitivity <- recall
    TPR <- recall
    specificity<- TN/(TN + FP)
    TNR <- specificity
    NPV <- TN/(TN + FN)
    FPR <- FP/(FP + TN)
    FNR <- 1 - TPR
    FDR <- 1 - PPV
    F1score <- 2*TP/(2*TP + FP + FN)
    MCC <- (TP * TN - FP * FN)/((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    type1error <- FP
    type2error <- FN
    
    print("accuracy, precision, recall")
    print(c(accuracy, precision, recall))
    
    result <- list(
      "accuracy"=accuracy,
      "precision"=precision,
      "recall"=recall,
      "sensitivity"=sensitivity,
      "specificity"=specificity,
      "PPV"=PPV,
      "TPR"=TPR,
      "NPV"=NPV,
      "FPR"=FPR,
      "FNR"=FNR,
      "FDR"=FDR,
      "F1score"=F1score,
      "MCC"=MCC,
      "type1error"=type1error,
      "type2error"=type2error
    )
  }
  
  else
  {
    print("--Currently supports only a 2x2 confusion matrix--")
    result <- NULL
  }
  return(result)
}