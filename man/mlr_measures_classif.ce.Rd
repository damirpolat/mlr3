% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeasureSimple.R
\name{mlr_measures_classif.ce}
\alias{mlr_measures_classif.ce}
\title{Classification Error}
\description{
Classification measure defined as \deqn{
  \frac{1}{n} \sum_{i=1}^n \left( t_i \neq r_i \right).
}{
  mean(t != r).
}
}
\note{
The score function calls \code{\link[mlr3measures:ce]{mlr3measures::ce()}} from package \CRANpkg{mlr3measures}.

If the measure is undefined for the input, \code{NaN} is returned.
This can be customized by setting the field \code{na_value}.
}
\section{Dictionary}{

This \link{Measure} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link{mlr_measures} or with the associated sugar function \code{\link[=msr]{msr()}}:\preformatted{mlr_measures$get("ce")
msr("ce")
}
}

\section{Meta Information}{


\itemize{
\item Type: \code{"classif"}
\item Range: \eqn{[0, 1]}{[0, 1]}
\item Minimize: \code{TRUE}
\item Required prediction: \code{response}
}

}

\seealso{
\link[mlr3misc:Dictionary]{Dictionary} of \link[=Measure]{Measures}: \link{mlr_measures}

\code{as.data.table(mlr_measures)} for a complete table of all (also dynamically created) \link{Measure} implementations.

Other classification measures: 
\code{\link{mlr_measures_classif.acc}},
\code{\link{mlr_measures_classif.auc}},
\code{\link{mlr_measures_classif.bacc}},
\code{\link{mlr_measures_classif.bbrier}},
\code{\link{mlr_measures_classif.costs}},
\code{\link{mlr_measures_classif.dor}},
\code{\link{mlr_measures_classif.fbeta}},
\code{\link{mlr_measures_classif.fdr}},
\code{\link{mlr_measures_classif.fnr}},
\code{\link{mlr_measures_classif.fn}},
\code{\link{mlr_measures_classif.fomr}},
\code{\link{mlr_measures_classif.fpr}},
\code{\link{mlr_measures_classif.fp}},
\code{\link{mlr_measures_classif.logloss}},
\code{\link{mlr_measures_classif.mbrier}},
\code{\link{mlr_measures_classif.mcc}},
\code{\link{mlr_measures_classif.npv}},
\code{\link{mlr_measures_classif.ppv}},
\code{\link{mlr_measures_classif.prauc}},
\code{\link{mlr_measures_classif.precision}},
\code{\link{mlr_measures_classif.recall}},
\code{\link{mlr_measures_classif.sensitivity}},
\code{\link{mlr_measures_classif.specificity}},
\code{\link{mlr_measures_classif.tnr}},
\code{\link{mlr_measures_classif.tn}},
\code{\link{mlr_measures_classif.tpr}},
\code{\link{mlr_measures_classif.tp}}

Other multiclass classification measures: 
\code{\link{mlr_measures_classif.acc}},
\code{\link{mlr_measures_classif.bacc}},
\code{\link{mlr_measures_classif.costs}},
\code{\link{mlr_measures_classif.logloss}},
\code{\link{mlr_measures_classif.mbrier}}
}
\concept{classification measures}
\concept{multiclass classification measures}
