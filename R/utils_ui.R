weights_slider_group <- function(coin, Level, ns_id){

  last_weights <- f_get_last_weights(coin)
  weights_at_level <- last_weights[last_weights$Level == Level, ]

  icodes_level <- get_iCodes(coin, Level)

  div(
    #class = "label-left", # uncomment this to make labels go to the top
    lapply(weights_at_level$iCode, function(iCode){
      weights_slider(
        inputId = iCode,
        initial_weight = weights_at_level$Weight[weights_at_level$iCode == iCode],
        label = NULL,
        ns_id = ns_id)
    })
  )
}

#' Generate 0-1 weights slider
#'
#' A single 0 -1 slider for weights
#'
#' @param inputId Id of slider
#' @param label Display label of slider
#' @param initial_weight The initial weight of the slider
#' @param ns_id Optional namespace ID
weights_slider <- function(inputId, initial_weight = 0.5, label = NULL, ns_id = NULL) {

  label <- label %||% inputId

  if (!is.null(ns_id)){
    inputId <- NS(ns_id, inputId)
  }

  sliderInput(inputId, label = label, min = 0, max = 1, value = initial_weight, step = 0.2, ticks = TRUE)
}

# Toast notification that a data set has been created
make_dset_toast <- function(dset_name){

  stopifnot(is.character(dset_name))

  showNotification(
    ui = glue::glue(
      dset_name,
      " data set successfully created."
    ),
    type = "default",
    duration = 5
  )
}

# Toast notification that a data set has been created
send_input_error <- function(error_message, error_context){

  stopifnot(is.character(error_message))

  error_message <- paste0("In: ", error_context, ". ", error_message)

  showNotification(
    ui = error_message,
    type = "error",
    duration = 10
  )
}

make_bookmark_button <- function(bookmark_type){

  if(bookmark_type == "URL"){
    bookmarkButton()
  } else if (bookmark_type == "RDS"){
    actionButton("save_session", "Save session", icon = icon("floppy-disk"))
  }
}


# Adds the FIND logo to an echarts plot.
# This is a fixed copy of shinyfind::add_logo since this function throws a bug when
# run within the installed shinycompositeindicator package.
add_find_logo <- function(e, image = "www/img/logo_header.svg",
                          subtitle = "Source: finddx.org",
                          height = 35, top = "auto", bottom = "0px", left = "right"){

  logo <- list(logo = list(height = height, backgroundColor = list(image = image)))

  e <- echarts4r::e_title(e, text = "{logo| }", left = left, bottom = bottom,
                          top = top, textStyle = list(fontStyle = "normal", rich = logo),
               subtext = subtitle, itemGap = 4)
  e

}
