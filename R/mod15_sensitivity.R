# UI ----------------------------------------------------------------------

sensitivity_ui <- function(id, sidebar_width) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Sensitivity analysis",

      "Use the check boxes to select which assumptions to vary as part of the sensitivity
      analysis. Assumptions can only be tested if that operation was run - e.g. testing imputation
      is only possibly if you have run the imputation step previously. Click 'Run' to run the
      sensitivity analysis. Please note, the sensitivity analysis can take some time to run.",

      checkboxInput(ns("i_SA_switch_imp"), label = "Imputation", value = FALSE),
      checkboxInput(ns("i_SA_switch_treat"), label = "Outlier treatment", value = FALSE),
      checkboxInput(ns("i_SA_switch_agg"), label = "Aggregation method", value = FALSE),
      checkboxInput(ns("i_SA_switch_weights"), label = "Perturb weights", value = FALSE),

      sliderInput(ns("i_SA_weight_prc"), label = "Perturb weights by:",
                  min = 5, max = 100, value = 25, step = 5, post = "%"),
      numericInput(ns("i_SA_N_rep"), label = "Number of replications",
                   value = 100, min = 10, max = 1000, step = 10),
      actionButton(ns("i_SA_run"), label = "Run"),

      verbatimTextOutput(NS(id, "SA_message")),

    ),


    # MAIN PANEL
    card(
      full_screen = TRUE,
      card_header("Rank confidence intervals"),
      # UA
      card_body(
        plotOutput(NS(id, "UA_plot"))
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Sensitivity indices"),
      # SA
      card_body(
        plotOutput(NS(id, "SA_plot"))
      )
    )
  )

}

# SERVER ------------------------------------------------------------------

sensitivity_server <- function(id, r_share, coin) {

  moduleServer(id, function(input, output, session) {

    # Grey out operations which were not run
    observe({
      if (!operation_was_run(coin(), "Impute")){
        shinyjs::disable("i_SA_switch_imp")
      } else {
        shinyjs::enable("i_SA_switch_imp")
      }
      if (!operation_was_run(coin(), "Treat")){
        shinyjs::disable("i_SA_switch_treat")
      } else {
        shinyjs::enable("i_SA_switch_treat")
      }
    })

    observeEvent(input$i_SA_run, {

      req(coin())
      req(r_share$results_calculated)

      # check for incompatibility: testing aggregation method implies use of
      # geometric mean which is incompatible with 0 or negative values
      if(input$i_SA_switch_agg){
        can_aggregate <- f_check_can_aggregate(coin(), "a_gmean")

        if (!can_aggregate) {
          showNotification(
            ui = glue::glue(
              "Cannot perform sensitivity analysis on aggregation method because this ",
              "uses the geometric mean, and there are negative or zero values in your ",
              "normalised data. Adjust normalisation parameters to avoid this (e.g. set ",
              "the minimum value to be a postive number)."
            ),
            type = "error",
            duration = 10
          )
          req(FALSE) # cannot run SA so do not proceed from here
        }
      }

      # check for only one uncertainty - then run UA and no SA plot
      run_UA <- reactive({
        sum(input$i_SA_switch_imp, input$i_SA_switch_treat,
            input$i_SA_switch_agg, input$i_SA_switch_weights) == 1
      })

      # RUN SA

      # This captures the message output so the user can see progress
      withCallingHandlers({
        shinyjs::html("SA_message", "")
        r_share$SA_results <- f_run_SA(
          coin(),
          test_imputation = input$i_SA_switch_imp,
          test_treat = input$i_SA_switch_treat,
          test_aggregation = input$i_SA_switch_agg,
          test_weights = input$i_SA_switch_weights,
          perturb_weights = input$i_SA_weight_prc,
          N_rep = input$i_SA_N_rep,
          run_UA = run_UA()
        )
        r_share$can_plot_SA <- !run_UA()
      },
      # add = FALSE overwrites so we only see the latest message
      message = function(m) {
        shinyjs::html(id = "SA_message", html = m$message, add = FALSE)
      })

    })

    # PLOT UA
    output$UA_plot <- renderPlot({
      req(r_share$SA_results)
      COINr::plot_uncertainty(r_share$SA_results)
    })

    # PLOT SA
    output$SA_plot <- renderPlot({
      req(r_share$SA_results)
      req(r_share$can_plot_SA) # only run SA plot if SA type (more than one uncertainty)
      COINr::plot_sensitivity(r_share$SA_results, ptype = "box")
    })

  })

}

# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Run sensitivity analysis
#'
#' For the moment, let's assume that users can only test sensitivity to operations
#' that they have actually run in the app. E.g. the user can only test the
#' imputation option if they have actually run the imputation tab. Otherwise, this
#' option will be greyed out in the app. Hence this function assumes that if a
#' switch is TRUE then that option has been run, else will return an error.
#'
#' @param coin The coin
#' @param test_imputation Logical: if TRUE tests the effect of switching imputation
#' on and off.
#' @param test_treat Logical: if TRUE tests the effect of switching outlier treatment
#' on and off.
#' @param test_aggregation Logical: if TRUE tests the effect of switching between
#' arithmetic and geometric mean.
#' @param test_weights Logical: if TRUE tests the effect of perturbing weights
#' @param perturb_weights Percentage perturbation to apply to weights, if `test_weights = TRUE`
#' @param N_rep The number of replications to run (true number is N_rep*(n_assumptions + 2))
#' @param run_UA Logical: if `TRUE` runs an uncertainty analysis rather than a sensitivity analysis.
#' This is intended to be set `TRUE` when only one uncertainty is investigated.
#'
#' @return List of sensitivity analysis results
#' @export
#'
f_run_SA <- function(coin, test_imputation, test_treat, test_aggregation, test_weights,
                     perturb_weights, N_rep, run_UA){

  # Assemble specification lists for each data operation
  # Start with a blank list and add to it depending on which switches are TRUE.
  SA_specs <- list()

  # IMPUTATION
  if (test_imputation) {
    stopifnot(operation_was_run(coin, "Impute"))
    SA_specs$Imputation <- list(Address = "$Log$Impute$disable",
                                Distribution = c(TRUE, FALSE),
                                Type = "discrete")
  }

  # OUTLIER TREATMENT
  if (test_treat) {
    stopifnot(operation_was_run(coin, "Treat"))
    SA_specs$Treatment <- list(Address = "$Log$Treat$disable",
                               Distribution = c(TRUE, FALSE),
                               Type = "discrete")
  }

  # AGGREGATION
  # NOTE: in server function we already check whether zeroes or negative values
  # are present, so at this point normalisation should be compatible with the
  # geometric mean.
  if (test_aggregation) {
    stopifnot(operation_was_run(coin, "Aggregate"))
    SA_specs$Aggregation <- list(Address = "$Log$Aggregate$f_ag",
                                 Distribution = c("a_amean", "a_gmean"),
                                 Type = "discrete")
  }

  # WEIGHTS
  if (test_weights) {
    stopifnot(operation_was_run(coin, "Aggregate"))

    # build noisy weights list
    nominal_weights <- f_get_last_weights(coin)

    # noise is applied to levels depending on number of levels. If only 2
    # levels, applied to level 1. Else, applied to all levels ABOVE indicator
    # level, except the index level itself.
    n_levels <- get_n_levels(coin)
    stopifnot(n_levels > 1) # shouldn't happen but you never know...
    levels_to_perturb <- if (n_levels == 2) 2 else 2:(n_levels - 1)

    # perturb_weights is a percentage value in [5, 100]
    noise_specs = data.frame(Level = levels_to_perturb,
                             NoiseFactor = perturb_weights/100)

    # outputs a list of replications of weights, with noise applied
    noisy_wts <- COINr::get_noisy_weights(
      w = nominal_weights,
      noise_specs = noise_specs,
      Nrep = 100
    )

    SA_specs$Weights <- list(Address = "$Log$Aggregate$w",
                             Distribution = noisy_wts,
                             Type = "discrete")
  }

  if(length(SA_specs) == 0){
    return(NULL)
  }

  index_code <- get_iCodes(coin, coin$Meta$maxlev)

  SA_type <- if (run_UA) "UA" else "SA"

  COINr::get_sensitivity(
    coin, SA_specs = SA_specs, N = N_rep, SA_type = SA_type,
    dset = "Aggregated", iCode = index_code, Nboot = 100, check_addresses = FALSE
  )

}


