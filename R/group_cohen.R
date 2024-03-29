group_cohen <- function(f,
                        ref,
                        d,
                        pooled = TRUE,
                        paired = FALSE,
                        na.rm = FALSE,
                        mu = 0,
                        hedges.correction = FALSE,
                        conf.level = 0.95,
                        noncentral = FALSE,
                        within = TRUE,
                        subject = NA, ...) {

  dfr <- model.frame(f, d)

  #define the factor containing the reference
  ctrl.pst <- colSums(d == paste(ref))
  ctrl.pst <- names(ctrl.pst[ctrl.pst>0])

  if(length(ctrl.pst) == 0) {
    stop("Name of the control is not contained in the dataset")
  } else {
    #define additional factors that do not contain the reference
    fctrs <- colnames(dfr)
    fctrs <- fctrs[fctrs != f[[2]] & fctrs != ctrl.pst]

    if(length(fctrs) > 0){

      dfr <- tidyr::unite(data = dfr,
                           col = "merged",
                           all_of(fctrs))

      subdf <- split(dfr, dfr$merged)


      #drop extra columns
      subdf <- lapply(subdf,
                      function(x) x[(names(x) %in% c(paste(f[[2]]), ctrl.pst))]
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

                                            effsize::cohen.d(treatment,
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

      subdf <- lapply(subdf,
                      function(x) {
                        z <- tibble::rownames_to_column(x, var = paste(ctrl.pst))
                      }
      )

      final_es <- subdf %>%
        dplyr::bind_rows(.id = "factors") %>%
        tidyr::separate(factors, into = fctrs)

        final_es

    } else {

      microdf <- split(df, df[ctrl.pst], drop = TRUE)

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

                          effsize::cohen.d(treatment,
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

      final_es <- tibble::rownames_to_column(microdf, var = paste(ctrl.pst))

      final_es
    }



  }
}
