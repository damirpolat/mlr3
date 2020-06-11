#' @title Container for Results of `resample()`
#'
#' @include mlr_reflections.R
#'
#' @description
#' This is the result container object returned by [resample()].
#'
#' Note that all stored objects are accessed by reference.
#' Do not modify any object without cloning it first.
#'
#' @template param_measures
#'
#' @section S3 Methods:
#' * `as.data.table(rr)`\cr
#'   [ResampleResult] -> [data.table::data.table()]\cr
#'   Returns a copy of the internal data.
#' * `c(...)`\cr
#'   ([ResampleResult], ...) -> [BenchmarkResult]\cr
#'   Combines multiple objects convertible to [BenchmarkResult] into a new [BenchmarkResult].
#'
#' @export
#' @examples
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv", folds = 3)
#' rr = resample(task, learner, resampling)
#' print(rr)
#'
#' rr$aggregate(msr("classif.acc"))
#' rr$prediction()
#' rr$prediction()$confusion
#' rr$warnings
#' rr$errors
ResampleResult = R6Class("ResampleResult",
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([Task])\cr
    #'   Single task all learners are trained on.
    #' @param learner ([Learner])\cr
    #'   Exemplary learner used to fit the individual learners.
    #' @param resampling ([Resampling])\cr
    #'   Instantiated resampling.
    #'   Number of iterations must match the number of states
    #' @param states (`list()`)\cr
    #'   List of learner states (this includes the fitted models).
    #' @param predictions (list of [Prediction])\cr
    #'   list of prediction objects.
    #' @param uhash (`character(1)`)\cr
    #'   Unique hash for this `ResampleResult`. If `NULL`, a new unique hash is calculated.
    #'   This unique hash is primarily needed to group information in [BenchmarkResult]s.
    initialize = function(task, learner, resampling, states, predictions, uhash = NULL) {
      assert_task(task)
      assert_learner(learner, task = task)
      assert_resampling(resampling, instantiated = TRUE)
      assert_list(states)
      assert_list(predictions, len = length(states))
      # assert_list(predictions, "Prediction", len = length(states))
      uhash = if (is.null(uhash)) UUIDgenerate() else assert_string(uhash)

      private$.data = list(
        task = task, learner = learner, resampling = resampling, states = states, predictions = predictions, uhash = uhash
      )
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function() {
      n = length(private$.data$states)
      catf("%s of %i iterations", format(self), n)
      catf(str_indent("* Task:", private$.data$task$id))
      catf(str_indent("* Learner:", private$.data$learner$id))

      warnings = self$warnings
      catf(str_indent("* Warnings:", sprintf("%i in %i iterations", nrow(warnings), uniqueN(warnings, by = "iteration"))))

      errors = self$errors
      catf(str_indent("* Errors:", sprintf("%i in %i iterations", nrow(errors), uniqueN(errors, by = "iteration"))))
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help("mlr3::ResampleResult")
    },

    #' @description
    #' Combined [Prediction] of all individual resampling iterations, and all provided predict sets.
    #' Note that performance measures do not operate on this object,
    #' but instead on each prediction object separately and then combine the performance scores
    #' with the aggregate function of the respective [Measure].
    #'
    #' @param predict_sets (`character()`)\cr
    #'   Subset of `{"train", "test"}`.
    #' @return [Prediction].
    prediction = function(predict_sets = "test") {
      do.call(c, self$predictions(predict_sets = predict_sets))
    },

    #' @description
    #' List of prediction objects, sorted by resampling iteration.
    #' If multiple sets are given, these are combined to a single one for each iteration.
    #'
    #' @param predict_sets (`character()`)\cr
    #'   Subset of `{"train", "test"}`.
    #' @return List of [Prediction] objects, one per element in `predict_sets`.
    predictions = function(predict_sets = "test") {
      map(private$.data$predictions, function(li) {
        do.call(c, li[predict_sets])
      })
    },

    #' @description
    #' Returns a table with one row for each resampling iteration, including all involved objects:
    #' [Task], [Learner], [Resampling], iteration number (`integer(1)`), and [Prediction].
    #' Additionally, a column with the individual (per resampling iteration) performance is added for each [Measure] in `measures`,
    #' named with the id of the respective measure id.
    #' If `measures` is `NULL`, `measures` defaults to the return value of [default_measures()].
    #'
    #' @param ids (`logical(1)`)\cr
    #'   If `ids` is `TRUE`, extra columns with the ids of objects (`"task_id"`, `"learner_id"`, `"resampling_id"`) are added to the returned table.
    #'   These allow to subset more conveniently.
    #'
    #' @return [data.table::data.table()].
    score = function(measures = NULL, ids = TRUE) {
      task = self$task
      measures = as_measures(measures, task_type = task$task_type)
      assert_measures(measures, task = task, learner = private$.data$learner)
      assert_flag(ids)
      tab = self$data

      for (m in measures) {
        set(tab, j = m$id, value = measure_score_data(m, tab))
      }

      if (ids) {
        tab[, c("task_id", "learner_id", "resampling_id") := list(ids(task), ids(learner), ids(resampling))]
        setcolorder(tab, c("task", "task_id", "learner", "learner_id", "resampling", "resampling_id", "iteration", "prediction"))[]
      }

      tab[]
    },

    #' @description
    #' Calculates and aggregates performance values for all provided measures, according to the respective aggregation function in [Measure].
    #' If `measures` is `NULL`, `measures` defaults to the return value of [default_measures()].
    #'
    #' @return Named `numeric()`.
    aggregate = function(measures = NULL) {
      measures = as_measures(measures, task_type = self$task$task_type)
      assert_measures(measures, task = private$.data$task, learner = private$.data$learner)
      set_names(map_dbl(measures, function(m) m$aggregate(self)), ids(measures))
    },

    #' @description
    #' Subsets the [ResampleResult], reducing it to only keep the iterations specified in `iters`.
    #'
    #' @param iters (`integer()`)\cr
    #'   Resampling iterations to keep.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    filter = function(iters) {
      n = length(private$.data$states)
      iters = unique(assert_integerish(iters, min.len = 1L, lower = 1L, upper = n, any.missing = FALSE, coerce = TRUE))

      private$.data$states = private$.data$states[iters]
      private$.data$predictions = private$.data$predictions[iters]
      invisible(self)
    }
  ),

  active = list(
    #' @field data ([data.table::data.table()])\cr
    #'   Tabular representation of stored data storage.
    #'   We discourage users to directly work with this field.
    data = function(rhs) {
      assert_ro_binding(rhs)

      data = private$.data
      learners = self$learners
      data.table(task = list(data$task), learner = learners, resampling = list(data$resampling), iteration = seq_along(learners), prediction = data$predictions)
    },

    #' @field task ([Task])\cr
    #' The task [resample()] operated on.
    task = function(rhs) {
      assert_ro_binding(rhs)
      private$.data$task
    },

    #' @field learners (list of [Learner])\cr
    #' List of trained learners, sorted by resampling iteration.
    learners = function(rhs) {
      assert_ro_binding(rhs)

      proto = private$.data$learner
      lapply(private$.data$states, function(state) {
        learner = proto$clone()
        learner$state = state
        learner
      })
    },

    #' @field resampling ([Resampling])\cr
    #' Instantiated [Resampling] object which stores the splits into training and test.
    resampling = function(rhs) {
      assert_ro_binding(rhs)
      private$.data$resampling
    },

    #' @field uhash (`character(1)`)\cr
    #' Unique hash for this object.
    uhash = function(rhs) {
      if (missing(rhs)) {
        return(private$.data$uhash)
      }
      private$.data$uhash = assert_string(rhs)
    },

    #' @field warnings ([data.table::data.table()])\cr
    #' A table with all warning messages.
    #' Column names are `"iteration"` and `"msg"`.
    #' Note that there can be multiple rows per resampling iteration if multiple warnings have been recorded.
    warnings = function(rhs) {
      assert_ro_binding(rhs)

      states = private$.data$states
      rbindlist(map(states, "log"), idcol = "iteration", use.names = TRUE)[class == "warning"]
    },

    #' @field errors ([data.table::data.table()])\cr
    #' A table with all error messages.
    #' Column names are `"iteration"` and `"msg"`.
    #' Note that there can be multiple rows per resampling iteration if multiple errors have been recorded.
    errors = function(rhs) {
      assert_ro_binding(rhs)

      states = private$.data$states
      rbindlist(map(states, "log"), idcol = "iteration", use.names = TRUE)[class == "error"]
    }
  ),

  private = list(
    .data = NULL
  )
)

#' @export
as.data.table.ResampleResult = function(x, ...) {
  x$data
}

#' @export
c.ResampleResult = function(...) {
  do.call(c, lapply(list(...), as_benchmark_result))
}

#' @rdname as_benchmark_result
#' @export
as_benchmark_result.ResampleResult = function(x, ...) {
  BenchmarkResult$new(cbind(x$data, data.table(uhash = x$uhash)))
}
