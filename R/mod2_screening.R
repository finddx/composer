
# UI ----------------------------------------------------------------------

screening_ui <- function(id) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Unit screening",

      "Optionally screen units according to data availability, proportion of zeros, or both.
    Select how to screen units and the threshold to use.",
      "Excluded units will be highlighted in the table when the operation is run.",

      # screening type
      selectInput(
        ns("i_screening_type"),
        "Screen units by:",
        choices = c("Data availability" = "byNA",
                    "Non-zero values" = "byzeros",
                    "Both" = "byNAandzeros"
        ),
        selected = "byNA"
      ),

      # data thresholds
      numericInput(
        inputId = ns("i_dat_thresh"),
        label = "Minimum (fraction):",
        value = 0,
        min = 0,
        max = 1,
        step = 0.1
      ),

      # run
      actionButton(inputId = ns("i_unitscreening_btn"), label = "Run"),
      actionButton(inputId = ns("i_undo"), label = "Undo")
    ),


    # MAIN PANEL
    layout_columns(

      col_widths = c(6, 6, 12),
      row_heights = c(1, 5),

      # INFO BOXES
      value_box(
        title = "Included units",
        value = textOutput(NS(id, "num_included")),
        textOutput(NS(id, "txt_included")),
        theme = "text-info",
        showcase = icon("check"), showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE
      ),
      value_box(
        title = "Excluded units",
        value = textOutput(NS(id, "num_excluded")),
        textOutput(NS(id, "txt_excluded")),
        theme = "text-info",
        showcase = icon("xmark"), showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE
      ),

      # SCREENED DATA TABLE
      card(
        full_screen = TRUE,
        card_header("Screened data"),
        card_body(
          DT::dataTableOutput(ns("dataScreened"))
        )
      )

    )

  )

}


# SERVER ------------------------------------------------------------------

screening_server <- function(id, r_shared, coin) {

  moduleServer(id, function(input, output, session) {

    # Run screening on button click
    observeEvent(input$i_unitscreening_btn, {

      req(coin())
      coin_screened <- f_unit_screening(
        coin(),
        screening_type = input$i_screening_type,
        dat_thresh = input$i_dat_thresh,
        dset = f_select_dset(coin(), "f_unit_screening")
      )
      make_dset_toast("Screened")
      # regenerate any subsequent ops if needed
      coin_screened <- regen_outdated_coin(coin_screened, "f_unit_screening")
      coin(coin_screened) # updates the coin reactiveVal with the Screened object

    })

    # undo on click
    observeEvent(input$i_undo, {
      req(coin()$Data$Screened)
      coin(undo_data_op(coin(), "f_unit_screening"))
    })

    # screened data table
    output$dataScreened <- DT::renderDataTable({
      req(coin()$Data$Screened)
      f_style_screening_table(coin())
    }, server = FALSE)


    # Value boxes -------------------------------------------------------------

    # units summary
    l_units <- reactive({

      req(coin()$Data$Screened)

      inclusion_summary <- coin()$Analysis$Screened$DataSummary

      if(!is.null(inclusion_summary)){

        # find which units included/excluded and number
        list(
          included = inclusion_summary$uCode[inclusion_summary$Included],
          excluded = inclusion_summary$uCode[!inclusion_summary$Included],
          n_included = sum(inclusion_summary$Included),
          n_excluded = sum(!inclusion_summary$Included)
        )

      } else {
        NULL
      }
    })

    # included units number
    output$num_included <- renderText({
      req(l_units())
      l <- l_units()
      n_tot <- l$n_included + l$n_excluded
      prc_included <- round(l$n_included/(n_tot)*100, 0)
      glue::glue("{l$n_included}/{n_tot} ({prc_included}%)")
    })

    # included units text
    output$txt_included <- renderText({
      req(l_units())
      f_first_few(l_units()$included)
    })

    # excluded units number
    output$num_excluded <- renderText({
      req(l_units())
      l <- l_units()
      n_tot <- l$n_included + l$n_excluded
      prc_excluded <- round(l$n_excluded/(n_tot)*100, 0)
      glue::glue("{l$n_excluded}/{n_tot} ({prc_excluded}%)")
    })

    # excluded units text
    output$txt_excluded <- renderText({
      req(l_units())
      f_first_few(l_units()$excluded)
    })


  })

}


# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Generate screened data set
#'
#' Generates screened data set on the coin.
#'
#' Note if we keep the order of data operations fixed, this should run always on the
#' "Raw" data set, so the `dset` argument probably doesn't need to be specified.
#'
#' @param coin The coin class object
#' @param dset The data set to apply screening to. Defaults to `"Raw"` and should be left at
#' this default unless we change the order of data operations.
#' @param screening_type One of `c("byNA", "byzeros", "byNAandzeros")`.
#' @param dat_thresh Number between 0 and 1, from "Min. data availability" input.
#' @param zero_thresh Number between 0 and 1 from "Min. non-zero" input.
#'
#' @importFrom COINr Screen
#' @importFrom rlang %||%
#'
#' @return The updated coin
#' @export
f_unit_screening <- function(coin, dset = NULL, screening_type, dat_thresh = NULL, zero_thresh = NULL){

  dset <- dset %||% "Raw"

  coin <- COINr::Screen(coin,
                        dset = dset, # if we let the order of operations vary this should be dset = input$i_dataset_unitscreening
                        unit_screen = screening_type,
                        dat_thresh = dat_thresh,
                        nonzero_thresh = zero_thresh,
                        out2 = "coin",
                        write_to = "Screened") |>
    suppressMessages()

  coin

}

#' Generate screened DT table
#'
#' Uses DT to generate a table to display, with rows of any screened units
#' coloured differently to show they have been removed. Colours maybe to be
#' adjusted.
#'
#' @param coin The coin
#'
#' @return A DT table
#' @export
f_style_screening_table <- function(coin) {

  screened_dataset_exists <- !is.null(coin[["Data"]][["Screened"]])
  stopifnot(screened_dataset_exists)

  removed_units <- coin[["Analysis"]][["Screened"]][["RemovedUnits"]]
  removed_units_exists <- !is.null(removed_units)
  stopifnot(removed_units_exists)

  df_raw <- COINr::get_dset(coin, dset = "Raw", also_get = "uName")

  # add a col to df_raw to indicate row formatting (based on in/out)
  df_raw$excluded <- ifelse(df_raw$uCode %in% removed_units, 1, 0)

  # round to 4 significant figures
  df_raw <- COINr::signif_df(df_raw, digits = 4)

  # Make DT table:
  # point DT to this col for formatting, then hide it
  # TODO maybe adjust the colours here
  # The +1 and -1 here and elsewhere are to do with two things:
  # 1. That DT uses JS which uses 0 as the first index
  # 2. That the rownames are counted as a column unless removed
  # See https://rstudio.github.io/DT/ (note just above sec 2.5)
  DT::datatable(
    df_raw,
    rownames = FALSE,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      width = "auto",
      columnDefs = list(
        list(targets = ncol(df_raw) - 1, visible = FALSE)
      ),
      buttons =
        list('copy', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        ))
    )
  ) |>
    DT::formatStyle(
      'excluded',
      target = 'row',
      backgroundColor = DT::styleEqual(c(0, 1), c('white', '#ffd2d7')),
      color =  DT::styleEqual(c(0, 1), c('black', '#a1a1a1'))
    )

}

# HTML summary of included units
f_print_included_units <- function(l_units){

  included_units <- l_units$included
  n_included <- l_units$n_included

  included_units_string <- f_first_few(included_units)

  prc_included <- round(n_included/(n_included + l_units$n_excluded)*100, 0)

  glue::glue("<b>Included units: {n_included} ({prc_included}%)</b><br>",
             "{included_units_string}")

}

# HTML summary of excluded units
f_print_removed_units <- function(l_units) {

  removed_units <- l_units$excluded
  n_removed <- l_units$n_excluded

  removed_units_string <- f_first_few(removed_units)

  prc_excluded <- round(n_removed/(n_removed + l_units$n_included)*100, 0)

  glue::glue("<b>Removed units: {n_removed} ({prc_excluded}%)</b><br>",
             "{removed_units_string}")

}

# Print first few in a list with a "more" comment
f_first_few <- function(x, n_print = 5){

  x <- x[!is.na(x)]

  n <- length(x)

  if (n == 0) return("NONE")

  if (n > n_print){

    first_x <- paste0(x[1:n_print], collapse = ", ")
    glue::glue("{first_x} + {n - 5} more")

  } else {
    toString(x)
  }

}
