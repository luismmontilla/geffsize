#library(effsize)

rawdata <- read.csv("cors_raw.csv")

formula = cat_tissue~treatment*tissue*time
data = rawdata
control = "Control"


batch.cohen <- function(formula, control, data) {

  df <- model.frame(formula, data)

  ctrl.pst <- colSums(data == paste(control))
  ctrl.pst <- names(ctrl.pst[ctrl.pst>0])


  fctrs <- colnames(df)
  fctrs <- fctrs[fctrs != formula[[2]] & fctrs != ctrl.pst ]

  if(length(fctrs) > 0){
    fctrs.list <- lapply(as.list(paste("df","$",fctrs, sep="")),
                   function(x) parse(text=x)[[1]])

    subdf <- split(df, lapply(fctrs.list, eval))

    #drop extra columns
    subdf <- lapply(subdf,
                    function(x) x[(names(x) %in% c(paste(formula[[2]]),
                                                          ctrl.pst))])


    ctrl.lvls <- unique(eval(parse(text=paste("df","$",ctrl.pst, sep=""))))

    if(length(ctrl.lvls) > 2){

      ctrl.lvls <- ctrl.lvls[ctrl.lvls != control]

      lapply(subdf,
             function(x) cohen.d(x[x[2] == ctrl.lvls[i],1], x[x[2] == control,1]))

    } else {

    }


  } else {
  #cuando hay solo un factor para comparar
  }



}

combn(unique(c(data$tissue,data$treatment)), 2)

model.frame(cat_tissue~treatment*tissue*time, rawdata)

for(i in 1:length(z)) {
  w <- droplevels(df[(lvl == z[[i]][1] | lvl == z[[i]][2]),])
}
