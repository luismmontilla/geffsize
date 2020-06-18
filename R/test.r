library(effsize)

rawdata <- read.csv("cors_raw.csv")

formula = cat_tissue~treatment*tissue*time
data = rawdata
ref = "Control"
pooled=TRUE
paired=FALSE
na.rm=FALSE
mu=0
hedges.correction=FALSE
conf.level=0.95
noncentral=FALSE
within=TRUE
subject=NA


batch.cohen <- function(formula, ref, data,
                        pooled=TRUE,
                        paired=FALSE,
                        na.rm=FALSE,
                        mu=0,
                        hedges.correction=FALSE,
                        conf.level=0.95,
                        noncentral=FALSE,
                        within=TRUE,
                        subject=NA, ...) {

  df <- model.frame(formula, data)

  ctrl.pst <- colSums(data == paste(ref))
  ctrl.pst <- names(ctrl.pst[ctrl.pst>0])


  fctrs <- colnames(df)
  fctrs <- fctrs[fctrs != formula[[2]] & fctrs != ctrl.pst ]
  #fctrs <- fctrs[fctrs != formula[[2]]]

  if(length(fctrs) > 0){
    fctrs.list <- lapply(as.list(paste("df","$",fctrs, sep="")),
                   function(x) parse(text=x)[[1]])

    subdf <- split(df, lapply(fctrs.list, eval))

    #drop extra columns
    subdf <- lapply(subdf,
                    function(x) x[(names(x) %in% c(paste(formula[[2]]), ctrl.pst))]
                    )

    subdf <- lapply(subdf,
                     function(x) {
                       microdf <- split(x, x[ctrl.pst], drop = TRUE)
                       #remerge dfs:
                       microdf <- lapply(microdf,
                                         function(x) {
                                           if (unique(x[,2]) != ref) {
                                             rbind(x, microdf[[ref]])
                                             }
                                         }
                                         )
                       #drop null elements:
                       microdf <- microdf[lengths(microdf) != 0]

                       microdf <- lapply(microdf,
                              function(x) {
                                treatment = x[x[,2] != ref,1]
                                  control = x[x[,2] == ref,1]

                                cohen.d(treatment,
                                        control,
                                        pooled,
                                        paired,
                                        na.rm,
                                        mu,
                                        hedges.correction,
                                        conf.level,
                                        noncentral,
                                        within,
                                        subject
                                        )
                              })

                       microdf <- lapply(microdf,
                              function(x) {
                                data.frame(
                                  Estimate = x$estimate,
                                  Lower.CI = x$conf.int[1],
                                  Upper.CI = x$conf.int[2]
                                )
                              })

                       microdf <- as.data.frame(matrix(unlist(microdf),
                                                  nrow = length(microdf),
                                                  byrow = TRUE,
                                                  dimnames = list(names(microdf),
                                                    names(microdf[[1]]))))


                       }
                     )

    lapply(subdf,
           function(x) {
             #x$test <- names(subdf)
             colnames(x)[3] <- ctrl.pst
           })




    } else {

      es.list <- lapply(subdf,
                        cohen.d(x[x[2] != ref,1],
                                x[x[2] == ref,1])
                        )

      #build df

    }


  } else {
  #cuando hay solo un factor para comparar
  }



}


}
