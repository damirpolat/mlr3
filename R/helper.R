hashes = function(x) {
  map_chr(unname(x), "hash")
}

hash = function(...) {
  digest::digest(list(...), algo = "xxhash64")
}

# updating join:
# replaces values in x with values in y
ujoin = function(x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s", cn), collapse = ","), ")"))
  x[y, eval(expr), on = key][]
}

translate_types = function(x) {
  r_types = mlr_reflections$task_feature_types
  p_types = names(mlr_reflections$task_feature_types)
  factor(map_values(x, r_types, p_types), levels = p_types)
}

allow_partial_matching = list(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)

get_progressor = function(n, label = NA_character_) {
  if (!isNamespaceLoaded("progressr")) {
    return(NULL)
  }

  progressr::progressor(steps = n, label = label)
}


replace_with = function(x, needle, replacement) {
  ii = (x == needle)
  x = rep(x, 1L + (length(replacement) - 1L) * ii)
  replace(x, ii, replacement)
}

#' Concentrate a table of R6 objects
#'
#' @description
#' Given a table `tab` with list columns of R6 objects, walks over the columns referenced via
#' `cols` and replaces R6 objects with the string of their respective hash.
#' The replaced objects are returned in a nested list.
#'
#' This operation can be reversed with the [dilute()] function.
#'
#' @param tab (`data.table()`)\cr
#'   Table with list columns of R6 objects.
#' @param cols (`character()`)\cr
#'   Subset of columns of `tab`.
#'
#' @return (named `list()`).
#' Named list of named lists of distinct extracted R6 objects.
#' First level is named after column names, second level after hash strings.
#'
#' @noRd
concentrate = function(tab, cols) {
  objects = named_list(cols)
  for (col in cols) {
    values = tab[[col]]
    hashes = hashes(values)
    uniq = !duplicated(hashes)
    objects[[col]] = setNames(values[uniq], hashes[uniq])
    set(tab, j = col, value = hashes)
  }

  objects # tab is changed in-place
}

#' Dilute a table with R6 objects
#'
#' @description
#' Given a table `tab` with columns of R6 hashes and a named list `objects` of R6 objects,
#' replaces the hashes with the corresponding R6 object.
#'
#' This operation reverses the [concentrate()] function.
#'
#' @param tab (`data.table()`)\cr
#'   Table with character columns of R6 hashes.
#' @param objects (named `list()`)\cr
#'   List as returned by [concentrate()].
#'
#' @return (`data.table()`).
#' Hashes are replaced by the corresponding R6 objects.
#'
#' @noRd
dilute = function(tab, objects) {
  for (col in names(objects)) {
    values = objects[[col]]
    set(tab, j = col, value = values[tab[[col]]])
  }
  tab
}
