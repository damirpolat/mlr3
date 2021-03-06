% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TaskClassif_iris.R
\name{mlr_tasks_iris}
\alias{mlr_tasks_iris}
\title{Iris Classification Task}
\format{
\link[R6:R6Class]{R6::R6Class} inheriting from \link{TaskClassif}.
}
\source{
\url{https://en.wikipedia.org/wiki/Iris_flower_data_set}

Fisher RA (1936).
\dQuote{The Use of Multiple Measurements in Taxonomic Problems.}
\emph{Annals of Eugenics}, \bold{7}(2), 179--188.
\doi{10.1111/j.1469-1809.1936.tb02137.x}.
}
\description{
A classification task for the popular \link[datasets:iris]{datasets::iris} data set.
}
\section{Construction}{
\preformatted{mlr_tasks$get("iris")
tsk("iris")
}
}

\section{Meta Information}{

\itemize{
\item Task type: \dQuote{classif}
\item Dimensions: 150x5
\item Properties: \dQuote{multiclass}
\item Has Missings: \code{FALSE}
\item Target: \dQuote{Species}
\item Features: \dQuote{Petal.Length}, \dQuote{Petal.Width}, \dQuote{Sepal.Length}, \dQuote{Sepal.Width}
}
}

\seealso{
\link[mlr3misc:Dictionary]{Dictionary} of \link[=Task]{Tasks}: \link{mlr_tasks}

\code{as.data.table(mlr_tasks)} for a complete table of all (also dynamically created) \link[=Task]{Tasks}.

Other Task: 
\code{\link{TaskClassif}},
\code{\link{TaskRegr}},
\code{\link{TaskSupervised}},
\code{\link{TaskUnsupervised}},
\code{\link{Task}},
\code{\link{mlr_tasks_boston_housing}},
\code{\link{mlr_tasks_breast_cancer}},
\code{\link{mlr_tasks_german_credit}},
\code{\link{mlr_tasks_mtcars}},
\code{\link{mlr_tasks_pima}},
\code{\link{mlr_tasks_sonar}},
\code{\link{mlr_tasks_spam}},
\code{\link{mlr_tasks_wine}},
\code{\link{mlr_tasks_zoo}},
\code{\link{mlr_tasks}}
}
\concept{Task}
