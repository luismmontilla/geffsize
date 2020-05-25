#library(effsize)

formula = cat_tissue~treatment*tissue*time
data = rawdata
control = "Control"

batch.cohen <- function(formula, control, data) {

  df <- model.frame(formula, data)

  ctrl.pst <- colSums(data == paste(control))
  ctrl.pst <- names(ctrl.pst[ctrl.pst>0])

  fctrs <- colnames(df)
  fctrs <- fctrs[fctrs != formula[[2]] & fctrs != ctrl.pst]


}
