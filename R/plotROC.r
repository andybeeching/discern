# !/usr/bin/Rscript

#' Utility function to plot multiple ROC curves against each other.
#' 
#'   - Comparison helps determine which models favour more TPs or less FPs
#'   - Curves are shown overlaid in the same panel upto a max threshold, upon
#'     when a new panel will be created.
#'
#' @param models    {List}   - List of model objects (Models expected to have
#'                             been trained with the "ROC" error metric.
#' @param res       {String} - Response variable of each model
#' @param predictor {String} - Variable to predict in cv dataset
#' @param cv        {Data}   - Cross-validation data set containing Predictor
#' @param labels    {List}   - *Optional*; List of labels for each model
#' @param colors    {List}   - *Optional*; List of colors for each model
#' @param maxCurves {Number} - *Optional*; # curves per panel, defaults to 3.
#' @export
#' @examples
#' plotROC(
#'   models    = list(glmFit4, adaFit, rfFit, svmFit, cfFit),
#'   res       = "Foo",
#'   predictor = "Bar",
#'   cv        = valid,
#'   colors    = c("black", "orange", "yellow", "green", "red")
#' )

plotROC <- function(models, res, predictor, cv, colors, titles, maxCurves = 3) {

  len    <- length(models)
  scores <- list()
  panels <- max(c(ceiling(len/maxCurves), 1))
  par(mfrow=c(1, panels))

  for (i in 1:len) {
    model      <- models[[i]]
    label      <- if (length(labels) < i) model$method else labels[[i]]
    auc        <- round(max(model$result$ROC), digits=4)
    color      <- if (length(colors) < i) rainbow(i) else colors[[i]]
    
    # track panels
    isLimit    <- i > maxCurves & maxCurves %% (i-1) == 0
    isFirst    <- i == 1
    isLast     <- i == len
    
    # predict and gernerate curve
    predProb   <- predict(model, cv, type="prob")
    
    curve      <- roc(
      response  = cv[[res]],
      predictor = predProb[[predictor]],
      levels    = levels(cv[[res]])
    )

    scores[[length(scores)+1]] <- paste(type, auc, sep=" - ")

    if (isLimit) {
      a <- i - maxCurves
      b <- i - 1

      legend("bottomright",
        legend=scores[a:b],
        lwd=c(2,2),
        col=colors[a:b]
      )
    }

    if (isFirst | isLimit) {
      plot(curve, type="S", col=color, ylim=c(0,1))
    } else {
      plot(curve, add=TRUE, col=color)
    }

    if (isLast) {
      a <- i - ((i - (maxCurves * (panels-1))) - 1)
      b <- i

      legend("bottomright",
        legend=scores[a:b],
        lwd=c(2,2),
        col=colors[a:b]
      )
    }
  }
}