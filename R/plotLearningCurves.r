# !/usr/bin/Rscript

#' Utility function to plot learning curves of multiple models.
#' 
#'   - Comparison helps determine if models contain bias or variance
#'   - Ideally want low training error and good generalization
#'   - Basic algorithm: split original data at different ratios
#'   - Re-train passed-in models on each split, and capture the error rate for
#'     both the train and valid data-sets.
#'   - NOTE: CV error is evaluated on _entire_ CV set, not subset
#'   - Plot the error (the score) versus the dataset size
#'
#' @param models    {List}   - List of model objects.
#' @param metric    {String} - Metric models were trained with
#' @param cv        {Data}   - Cross-validation data set (containing predictor) 
#' @param labels    {List}   - *Optional* List of labels associated with models
#' @param colors    {List}   - *Optional* List of colors associated with models
#' @param seed      {Number} - Seed to use for training each model
#' @export
#' @examples
#' plotLearningCurves(
#'   models     = list(cfFit),
#'   metric     = "ROC",
#'   ctrlFn     = fitControl,
#'   cv         = valid,
#'   colors     = c("orange", "blue"),
#'   labels     = c("Glm Model 4", "Glm Model 6", "CForest"),
#'   seed       = SEED
#' )

plotLearningCurves <- function(models, labels, metric, ctrlFn, cv, colors, seed = 1) {

  len <- length(models)
  par(mfrow=c(1, len))

  for (i in 1:len) {
    model  <- models[[i]]
    label  <- labels[[i]]

    # build the formula to pass in for training
    #   - difficult to do from each model due to R internal structure
    #   - extract term object from model and access it for various original
    #     inputs. This object is produced from the original model formula.
    #   - co-variants (right-hand side of formula) are particularly tricky,
    #     as the they are held in a object type of "language"
    #   - Therefore, once obtained they need to be further subset and cast to
    #     a string (along with the response var, aka "res"), and then finally
    #     cast as a formula for input into createLearnData method.
    #   - Extract training method from original call to caret train() fn
    trms   <- terms(model)
    res    <- as.character(trms[[2]])
    covars <- trms[[3]]
    covars <- paste(covars[2], '+', covars[3])
    frmla  <- paste(res, '~', covars)
    frmla  <- as.formula(frmla)
    method <- model$call[[4]]
    tColor <- if (length(colors) < 1) rainbow[[1]] else colors[[1]]
    vColor <- if (length(colors) < 1) rainbow[[2]] else colors[[2]]
    
    scores <- createLearnData(seq(0.1, 0.9, by=0.1),
      raw.munged,
      res,
      frmla,
      method,
      ctrlFn,
      cv,
      SEED
    )

    plot(scores$m, scores$trainScore,
      ylim=c(1, 0.75),
      xlab="Training Observations",
      ylab="Accuracy",
    )

    title(label)
    points(scores$m, scores$validScore)

    lines(scores$m, scores$trainScore, lwd=2, col=tColor)
    lines(scores$m, scores$validScore, lwd=2, col=vColor)

    legend("topleft",
      legend=c("Training score", "Validation Score"),
      lwd=c(2,2),
      col=c(tColor, vColor)
    )
  }
}

createLearnData <- function(ratios, df, res, frmla, method, ctrlFn, cv, seed=1) {

  results <- data.frame(ratios, m=NA, trainScore=NA, validScore=NA)

  set.seed(seed);

  for(i in 1:length(ratios)) {
    inTrain <- createDataPartition(y=df[[res]], p=ratios[i], list=FALSE)
    train <- df[inTrain,]

    trainFit <- train(as.formula(frmla),
      data      = train,
      method    = method,
      metric    = "ROC",
      trControl = ctrlFn
    )

    summary(trainFit)

    results$m[i] <- as.numeric(nrow(trainFit$trainingData))

    trainPred <- predict(trainFit, train)
    trainConfMatrix <- confusionMatrix(trainPred, train[[res]])
    results$trainScore[i] <- trainConfMatrix$overall["Accuracy"]

    validPred <- predict(trainFit, cv)
    validConfMatrix <- confusionMatrix(validPred, cv[[res]])
    results$validScore[i] <- validConfMatrix$overall["Accuracy"]
  }

  return (results)
}