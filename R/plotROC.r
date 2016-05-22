# !/usr/bin/Rscript

#' Utility function to plot multiple ROC curves against each other. Wraps the
#' *roc* method of the pROC package.
#'
#'   - Comparison helps determine which models favour more TPs or less FPs
#'   - Curves are shown overlaid in the same panel upto a max threshold, upon
#'     when a new panel will be created.
#'   - Refer to pROC#roc method documentation for further information on the
#'     *res*, *predictor*, and *cv* parameters.
#'
#' @param models    {List}   - List of model objects (models expected to have
#'                             been trained with the ROC performance metric).
#' @param res       {String} - The response variable to predict.
#' @param cv        {Data}   - Dataset containing variables in the formula.
#' @param labels    {List}   - *Optional*; List of labels for each model
#' @param colors    {List}   - *Optional*; List of colors for each model
#' @param maxCurves {Number} - *Optional*; # curves per panel, defaults to 3.
#' @export
#' @examples
#' plotROC(
#'   models    = list(glmFit, rfFit)
#'   res       = "Foo",
#'   cv        = validDf,
#'   colors    = c("black", "orange")
#' )

library(pROC)

plotROC <- function(models, res, cv, colors, labels, maxCurves = 3) {

  len    <- length(models)
  scores <- list()
  panels <- max(c(ceiling(len/maxCurves), 1))
  par(mfrow=c(1, panels))

  for (i in 1:len) {
    model <- models[[i]]
    label <- if (length(labels) < i) model$method else labels[[i]]
    auc   <- round(max(model$result$ROC), digits=4)
    color <- if (length(colors) < i) rainbow(i) else colors[[i]]

    # track panels
    isLimit <- i > maxCurves & maxCurves %% (i-1) == 0
    isFirst <- i == 1
    isLast  <- i == len

    # predict and generate curve
    predProb <- as.numeric(predict(model, cv, type="prob"))

    curve <- roc(
      response  = cv[[res]],
      predictor = predProb[[res]],
      levels    = levels(cv[[res]])
    )

    # create labels
    scores[[length(scores)+1]] <- paste(label, auc, sep=" - ")

    if (isLimit) {
      a <- i - maxCurves
      b <- i - 1

      legend("bottomright",
        legend=scores[a:b],
        lwd=c(2,2),
        col=colors[a:b]
      )
    }

    # draw curve and manage partitions
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