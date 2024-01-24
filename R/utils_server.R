
# function maps the selected tab and sub-tab to the relevant documentation page
get_doc_page <- function(input){

  pagestring <- switch(
    input$active_tab,
    "overview-panel" = "data_input",
    "screening-panel" = "screening",
    "imputation-panel" = "imputation",
    "treat-panel" = "outliers",
    "normalisation-panel" = "normalisation",
    "compose-panel" = "compose",
    "stats-panel" = "stats",
    "correlations-panel" = "correlations",
    "map-panel" = "map_bar",
    "bubble-panel" = "bubble",
    "bar-panel" = "map_bar",
    "profiles-panel" = "profiles",
    "reweighting-panel" = "reweighting",
    "remove_elements-panel" = "remove_elements",
    "sensitivity-panel" = "sensitivity"
  )

  url_doc <- paste0("docs/", pagestring, ".html")

  url_doc
}

# supposed location of general bookmark folder
bookmark_path <- function(){
  paste0(getwd(), "/shiny_bookmarks")
}

# location of specific session bookmark folder based on URL
get_url_bookmark_path <- function(URL){
  paste0(
    bookmark_path(), "/",
    strsplit(URL, "state_id_=")[[1]][2]
  )
}

#' Select data set to operate on
#'
#' Helper function which, given a calling function, returns the data set to
#' operate on. This is assuming the order of data functions as given in the
#' function definition.
#'
#' @param coin A coin
#' @param calling_function The calling function, as a string.
#'
#' @return A data set name, as a string
f_select_dset <- function(coin, calling_function){

  # data sets created by each back end function
  dset_order <- get_dset_order()

  stopifnot(calling_function %in% names(dset_order))

  dsets_available <- get_dset_names(coin)

  # eligible data sets are any that are before the one created by the calling function
  # according to the order specified by dset_order (the assumed order of operations in the app)
  # e.g. if calling_function is "f_outlier_treatment" eligible dsets are "Raw", "Screened" and "Imputed".
  dsets_eligible <- dset_order[1 : (which(names(dset_order) == calling_function)-1)]

  # the last eligible dset in available dsets is the one to pick
  dsets_to_pick <- dsets_eligible[dsets_eligible %in% dsets_available]
  dsets_to_pick[length(dsets_to_pick)] |>
    as.character()

}

# single reference for the data operations in the app
get_dset_order <- function(){
  c(f_read_input = "Raw",
    f_unit_screening = "Screened",
    f_imputation = "Imputed",
    f_outlier_treatment = "Treated",
    f_normalisation = "Normalised",
    f_aggregation = "Aggregated")
}

get_coinr_function_order <- function(){
  c("new_coin", "can_regen", "Screen", "Impute", "Treat", "qNormalise", "Aggregate")
}

# return names of all data sets present in coin
get_dset_names <- function(coin){
  names(coin$Data)
}

f_get_group_codes <- function(coin){
  imeta <- coin[["Meta"]][["Ind"]]
  imeta$iCode[imeta$Type == "Group"]
}

f_get_group_list <- function(coin){

  group_codes <- f_get_group_codes(coin)
  no_groups_exist <- (length(group_codes) == 0)

  if (no_groups_exist) {
    return(NULL)
  }

  l_out <- as.list(group_codes)
  names(l_out) <- COINr::icodes_to_inames(coin, group_codes)
  l_out

}

f_get_group_choices <- function(coin, iCode_group){
  coin$Meta$Unit[[iCode_group]] |> unique()
}



# gets number of levels in index
get_n_levels <- function(coin) {
  coin[["Meta"]][["maxlev"]]
}


#' Get list of available indicator/aggregate codes
#'
#' This is for use in dropdown menus, e.g. in plotting.
#'
#' @param coin The coin
#' @param code_types Either `"Aggregate"` (for just aggregate codes), `"Indicator"` (for
#' just indicator codes) or `"all"` for both.
#' @param with_levels If `TRUE`, outputs a named list where the list names are of the format
#' "Level: iCode" or "Level: iName". These names are what users will see in the dropdown menus.
#' Else if `FALSE` just outputs a character vector of iCodes.
#' @param use_names if `TRUE` uses iNames in the named list, else uses iCodes.
#'
#' @return A list or character vector, for input into e.g. `selectInput()`.
get_indicator_codes <- function(coin, code_types = "all",
                                with_levels = TRUE, use_names = TRUE) {

  stopifnot(code_types %in% c("all", "Aggregate", "Indicator"))

  if (code_types == "all") {
    code_types <- c("Indicator", "Aggregate")
  }

  # Filter iMeta to code types of interest, then
  # sort iMeta by highest level downwards (better for display in dropdown menus)
  imeta <- coin[["Meta"]][["Ind"]]
  imeta <- imeta[imeta[["Type"]] %in% code_types, ]
  imeta <- imeta[order(imeta[["Level"]], decreasing = TRUE), ]

  if (with_levels) {

    codes_to_display <- if (use_names) imeta[['iName']] else imeta[['iCode']]

    l_out <- as.list(imeta[["iCode"]])
    names(l_out) <- glue::glue("{imeta[['Level']]}: {codes_to_display}")
    l_out


  } else {
    imeta[["iCode"]]
  }

}


#' Get iCodes
#'
#' Get iCodes at specified level in the coin
#'
#' @param coin A coin
#' @param Level Index level, as integer
#'
#' @return Vector of codes
get_iCodes <- function(coin, Level = 1) {

  stopifnot(
    Level %in% 1:get_n_levels(coin)
  )

  coin[["Meta"]][["Lineage"]][[Level]] |>
    unique()
}

get_index_code <- function(coin){
  get_iCodes(coin, coin$Meta$maxlev)
}

#' Find number of children of iCode
#'
#' @return the number of children of a given iCode (in the indicator framework,
#' in the level immediately below, if there is one)
#' @noRd
get_number_of_children <- function(coin, iCode){
  sum(coin[["Meta"]][["Ind"]][["Parent"]] == iCode, na.rm = TRUE)
}

get_icode_level <- function(coin, iCode){

  imeta <- coin[["Meta"]][["Ind"]]
  imeta[["Level"]][imeta[["iCode"]] == iCode]

}

# get the data operation performed immediately before the one specified
get_preceding_operation <- function(coin, operation){

  operations <- names(coin[["Log"]])
  operations <- operations[operations != "can_regen"] # not a data operation

  operations[which(operations == operation) - 1]
}

# get the data operation performed immediately before the one specified
get_next_operation <- function(coin, operation){

  operations <- names(coin[["Log"]])
  operations <- operations[operations != "can_regen"] # not a data operation

  operations[which(operations == operation) + 1]
}

# check whether a specific data operation was run or not (returns logical)
operation_was_run <- function(coin, operation){
  !is.null(coin$Log[[operation]])
}

# Function which checks whether coin is outdated and needs to be regenerated,
# then regenerates if needed.
# This can happen e.g. when user inserts a data operation in the chain
# retrospectively,
regen_outdated_coin <- function(coin, last_run){

  stopifnot(is.coin(coin))

  # check if any data operations are ahead of last_run
  dsets <- names(coin$Data)
  operations <- get_dset_order()
  last_operation_number <- which(last_run == names(operations))

  # if we are already on the last operation, exit and return coin
  if(last_operation_number == length(operations)) return(coin)

  # else find out which ops are ahead in the chain
  ops_ahead_of_last_run <- operations[(last_operation_number + 1):length(operations)]

  # now find which need to be regenerated
  dsets2regen <- dsets[dsets %in% ops_ahead_of_last_run]
  ops2regen <- names(operations)[operations %in% dsets2regen]

  # exit if nothing to regen
  if(length(ops2regen) == 0) return(coin)

  # now re-run
  coin <- regen_coin_ops(coin, ops2regen)

  # message
  dset_created <- operations[names(operations) == last_run]

  showNotification(
    ui = glue::glue(
      dset_created,
      " dataset created and successive data operations recalculated!"
    ),
    type = "message",
    duration = 5
  )

  coin
}

# Undoes a data operation that was previously run in the coin, and recalculates
# the results. Also sends a message to the user.
undo_data_op <- function(coin, op_to_undo, send_toast = TRUE){

  stopifnot(is.coin(coin))

  if(op_to_undo == "f_normalisation"){
    stop("Cannot undo normalisation as it is obligatory for aggregation.")
  }

  dsets_run <- names(coin$Data)

  all_dsets <- get_dset_order()

  ops_run <- names(all_dsets)[all_dsets %in% dsets_run]

  stopifnot(op_to_undo %in% ops_run)

  dset_to_undo <- all_dsets[names(all_dsets) == op_to_undo]

  op_number <- which(ops_run == op_to_undo)

  op_log_name <- dset_to_COINr_func(dset_to_undo)

  # check dset of op is pointing to the op before
  parent_dset <- coin$Log[[op_log_name]]$dset

  if(parent_dset != dsets_run[op_number - 1]){
    stop("Tried to remove data operation but expected data set order not respected.")
  }

  # delete log of operation and dataset
  coin$Log[[op_log_name]] <- NULL
  coin$Data[[dset_to_undo]] <- NULL

  # also delete analysis and results folder: since coin is recalculated, these
  # should be regenerated.
  coin$Analysis <- NULL
  coin$Results <- NULL

  op_is_last <- op_number == length(ops_run)

  # if op is not the last one, need to re-route dset chain
  if(!op_is_last){
    next_dset <- dsets_run[op_number + 1]
    next_op_log_name <- dset_to_COINr_func(next_dset)
    # reroute next op to use dset that was used by the deleted op
    coin$Log[[next_op_log_name]]$dset <- parent_dset
  }

  # remove operation to remove, plus f_read_input which is never rerun
  ops_to_rerun <- ops_run[-c(1, op_number)]

  # now re-run
  coin <- regen_coin_ops(coin, ops_to_rerun)

  # user message
  if(send_toast){
    showNotification(
      ui = glue::glue(
        "Operation was removed and all other operations recalculated."
      ),
      type = "message",
      duration = 5
    )
  }

  coin

}

dset_to_COINr_func <- function(dset){
  switch(dset,
         Raw = "new_coin",
         Screened = "Screen",
         Imputed = "Impute",
         Treated = "Treat",
         Normalised = "qNormalise",
         Aggregated = "Aggregate")
}

# function that regenerates the coin using parameters stored in Log
regen_coin_ops <- function(coin, ops2regen){

  if("f_unit_screening" %in% ops2regen){
    coin <- f_unit_screening(
      coin,
      screening_type = coin$Log$Screen$unit_screen,
      dat_thresh = coin$Log$Screen$dat_thresh,
      dset = f_select_dset(coin, "f_unit_screening")
    )
  }

  if("f_imputation" %in% ops2regen){
    coin <- f_imputation(
      coin,
      dset = f_select_dset(coin, "f_imputation"),
      imputation_method = coin$Log$Impute$f_i,
      imputation_group = coin$Log$Impute$use_group
    )
  }

  if("f_outlier_treatment" %in% ops2regen){
    coin <- f_outlier_treatment(
      coin,
      dset = f_select_dset(coin, "f_outlier_treatment")
    )
  }

  if("f_normalisation" %in% ops2regen){
    coin <- f_normalisation(
      coin,
      dset = f_select_dset(coin, "f_normalisation"),
      norm_method = coin$Log$qNormalise$f_n,
      norm_params = coin$Log$qNormalise$f_n_para[[1]]
    )
  }

  if("f_aggregation" %in% ops2regen){
    coin <- f_aggregation(
      coin,
      dset = "Normalised",
      aggregation_type = coin$Log$Aggregate$f_ag,
      weight_set = coin$Log$Aggregate$w
    )
  }

  # reorder log
  correct_log_order <- get_coinr_function_order()
  coin$Log <- coin$Log[correct_log_order[correct_log_order %in% names(coin$Log)]]

  # reorder datasets
  correct_dset_order <- get_dset_order()
  coin$Data <- coin$Data[correct_dset_order[correct_dset_order %in% names(coin$Data)]]

  coin

}

# Builds example coin for this app using the app wrapper functions. Used for
# Testing.
build_example_coin_app <- function(
    run_ops = c("screen", "impute", "treat", "normalise", "aggregate")){

  # input data
  coin <- f_read_input(system.file("example_input_WH.xlsx", package = "findcompositeindicator"), input_type = "xlsx")

  # screening
  if("screen" %in% run_ops){
    coin <- f_unit_screening(
      coin,
      screening_type = "byNA",
      dat_thresh = 0.7,
      dset = f_select_dset(coin, "f_unit_screening")
    )
  }

  # impute
  if("impute" %in% run_ops){
    coin <- f_imputation(
      coin,
      dset = f_select_dset(coin, "f_imputation"),
      imputation_method = "i_median_grp",
      imputation_group = "WB_REGION_group"
    )
  }

  # treat
  if("treat" %in% run_ops){
    coin <- f_outlier_treatment(
      coin,
      dset = f_select_dset(coin, "f_outlier_treatment")
    )
  }

  # normalise
  if("normalise" %in% run_ops){
    coin <- f_normalisation(
      coin,
      dset = f_select_dset(coin, "f_normalisation"),
      norm_method = "n_minmax",
      norm_params = c(1, 100)
    )
  }

  # aggregate
  if("aggregate" %in% run_ops){
    coin <- f_aggregation(
      coin,
      dset = "Normalised",
      aggregation_type = "a_amean",
      weight_set = "Original"
    )
  }

  coin

}

# Not in operator
#
# For convenience, rather than always `!(x, %in% y)`
#
# @param x A scalar or vector
# @param y A scalar or vector
#
# @return TRUE if x is not in y, FALSE otherwise
'%nin%' <- function(x,y){
  !('%in%'(x,y))
}

# can a vector be coerced to numeric?
possibly_numeric <- function(x){
  suppressWarnings(all(!is.na(as.numeric(as.character(x)))))
}

# convert integer columns to numeric (intended for iData)
df_int_2_numeric <- function(X){

  # convert integer cols to numeric (iData)
  rnames <- row.names(X)
  col_names <- names(X)
  X <- lapply(col_names, function(col_name){
    x <- X[[col_name]]
    if(is.integer(x)){
      message("iData column '", col_name, "' converted from integer to numeric.")
      as.numeric(x)
    } else x
  }) |> as.data.frame()
  row.names(X) <- rnames
  names(X) <- col_names
  X

}
