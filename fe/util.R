compute_eval_measures_v1 <- function(cm) {
  acc = sum(diag(cm))/sum(cm)
  p = cm[2,2]/sum(cm[,2])
  r = cm[2,2]/sum(cm[2,])
  f1 = 2*p*r/(p+r)
  res = c(accuracy = acc, precision = p, recall = r, F1 = f1)
  round(res, 4)
}


compute_eval_measures_v2 <- function(TP, TN, FP, FN) {
  acc = (TP+TN)/(TP+TN+FP+FN)
  p = TP/(TP + FP)
  r = TP/(TP + FN)
  f1 = 2*p*r/(p+r)
  c(accuracy = acc, 
    precision = p,
    recall = r,
    F1 = f1)
}