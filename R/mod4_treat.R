
# UI ----------------------------------------------------------------------

treat_ui <- function(id, sidebar_width) {

  ns <- NS(id)


  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Outlier treatment",

      "Optionally treat outliers. Outliers are detected automatically, and treated
    using Winsorisation and log transformations as necessary. Cells are coloured
    in the table according to treatment: orange for high Winsorisation, blue for
    low Winsorisation, and green for log-transformed points.",

      "Click on table columns to visualise indicators before and after treatment.",

      actionButton(ns("i_outlier_btn"), "Run"),
      actionButton(inputId = ns("i_undo"), label = "Remove operation"),

      # SCATTER PLOT
      plotlyOutput(ns("o_plot_treated"), height = "400px")

    ),


    # MAIN PANEL
    layout_columns(

      col_widths = c(6, 6, 12),
      row_heights = c(1, 5),


      # INFO BOXES
      value_box(
        title = "Indicators with outliers before treatment",
        value = textOutput(ns("treated_n_pre")),
        textOutput(ns("treatedText_pre")),
        theme = "text-info",
        showcase = icon("info"), showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE
      ),
      value_box(
        title = "Indicators with outliers after treatment",
        value = textOutput(ns("treated_n_post")),
        textOutput(ns("treatedText_post")),
        theme = "text-info",
        showcase = icon("info"), showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE
      ),

      # TREATED DATA TABLE
      card(
        full_screen = TRUE,
        card_header("Imputed data"),
        card_body(
          DT::dataTableOutput(ns("dataTreated"))
        )
      )
    )
  )

}

# SERVER ------------------------------------------------------------------

treat_server <- function(id, r_shared, coin) {

  moduleServer(id, function(input, output, session) {

    # treat data on click
    observeEvent(input$i_outlier_btn, {
      req(coin())
      coin_treated <- f_outlier_treatment(
        coin(),
        dset = f_select_dset(coin(), "f_outlier_treatment")
      )
      make_dset_toast("Treated")

      # regenerate any subsequent ops if needed
      coin_treated <- regen_outdated_coin(coin_treated, "f_outlier_treatment")
      coin(coin_treated)
    })

    # undo on click
    observeEvent(input$i_undo, {
      req(coin()$Data$Treated)
      coin(undo_data_op(coin(), "f_outlier_treatment"))
    })

    # get treatment info
    l_treated <- reactive({
      req(coin()$Data$Treated)
      get_treat_info(coin())
    })

    # treated data table
    output$dataTreated <- DT::renderDataTable({
      req(l_treated())
      f_style_treatment_table(l_treated(), coin())
    }, server = FALSE)


    # Value boxes -------------------------------------------------------------

    # no. outliers before treatment
    output$treated_n_pre <- renderText({
      req(l_treated())
      l_treated()$n_with$outliers_pre
    })

    # no. outliers after treatment
    output$treated_n_post <- renderText({
      req(l_treated())
      l_treated()$n_with$outliers_post
    })

    # summary before treatment
    output$treatedText_pre <- renderText({
      req(l_treated())
      f_print_treat_pre(l_treated())
    })

    # summary after treatment
    output$treatedText_post <- renderText({
      req(l_treated())
      f_print_treat_post(l_treated())
    })

    # scatter plot
    output$o_plot_treated <- renderPlotly({
      req(l_treated())
      req(input$dataTreated_columns_selected)
      f_plot_scatter_treat(
        l_treated(),
        coin(),
        input$dataTreated_columns_selected,
        f_select_dset(coin(), "f_outlier_treatment")
      )
    })

  })

}

# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Treat data set
#'
#' The expected inputs are skew and kurtosis thresholds (numbers > 0), and the maximum number of
#' points to Winsorise (integer >= 0). More flexibility could be added to this function, e.g.
#' disabling the log transformation or Winsorisation.
#'
#' @param coin A coin
#' @param dset Data set to use
#' @param skew_thresh Absolute skew threshold
#' @param kurt_thresh Kurtosis threshold
#' @param max_winsorise Max number of points to Winsorise for each indicator.
#'
#' @return The updated coin
#'
#' @importFrom COINr qTreat
f_outlier_treatment <- function(coin, dset = NULL, skew_thresh = 2,
                                kurt_thresh = 3.5, max_winsorise = 5) {

  dset <- dset %||% f_select_dset(coin, "f_outlier_treatment")

  # this is the full specification list for data treatment. Most of this is
  # would be set as default by COINr::Treat() but I specify explicitly here
  # so we can make future adjustments more easily if needed.
  # see: https://bluefoxr.github.io/COINr/articles/treat.html#coins
  treatment_specs <- list(
    f1 = "winsorise",
    f1_para = list(na.rm = TRUE,
                   winmax = max_winsorise,
                   skew_thresh = skew_thresh,
                   kurt_thresh = kurt_thresh,
                   force_win = FALSE),
    f2 = "log_CT",
    f2_para = list(na.rm = TRUE),
    f_pass = "check_SkewKurt",
    f_pass_para = list(na.rm = TRUE,
                       skew_thresh = skew_thresh,
                       kurt_thresh = kurt_thresh)
  )

  COINr::Treat(coin, dset = dset, global_specs = treatment_specs) |>
    suppressMessages()

}

# Returns a list of information about the data treatment, which is used to display
# output to the user.
get_treat_info <- function(coin){

  dset_treated <- COINr::get_dset(coin, dset = "Treated", also_get = "uName")

  treatment_details <- tidy_treatment_details(coin$Analysis$Treated$Dets_Table)

  outliers_before_treatment <- sum(!treatment_details$Pass_0, na.rm = TRUE)
  outliers_after_treatment <- sum(!treatment_details$Pass_1, na.rm = TRUE)

  list(
    dset_treated = dset_treated,
    treatment_details = treatment_details,
    n_with = list(
      outliers_pre = outliers_before_treatment,
      outliers_post = outliers_after_treatment
    )
  )

}


# Tidy up the data treatment details data frame produced in f_outlier_treatment()
#
#' @importFrom dplyr rename
# The aim is to rename and adjust some columns to make it more interpretable.
# This is slightly messy unfortunately.
tidy_treatment_details <- function(X){

  X <- dplyr::rename(
    X,
    "Pass_0" = "check_SkewKurt0.Pass",
    "Skew_0" = "check_SkewKurt0.Skew",
    "Kurt_0" = "check_SkewKurt0.Kurt",
    "Pass_1" = "check_SkewKurt1.Pass",
    "Skew_1" = "check_SkewKurt1.Skew",
    "Kurt_1" = "check_SkewKurt1.Kurt"
  )

  # General column for declaring the data treatment performed.
  # Declare number of Winsorised points.
  X$Treatment <- ifelse(X$Pass_0, "-", paste0("Win: ", X$winsorise.nwin))

  # Fill in the blanks for pass, skew and kurt ----

  # If indicator passed at 0, it must have passed at 1
  X$Pass_1 <- X$Pass_1 | X$Pass_0

  # Bring over skew and kurtosis values from 0 to 1
  #  for indicators that passed at 0
  X$Skew_1[X$Pass_0] <- X$Skew_0[X$Pass_0]
  X$Kurt_1[X$Pass_0] <- X$Kurt_0[X$Pass_0]


  # If the log transformation is invoked anywhere ----
  # We have to make some further adjustments

  if (!is.null(X$check_SkewKurt2.Pass)) {

    # record instances of Log transformation
    X$Treatment <- ifelse(X$Pass_1, X$Treatment, "Log")

    log_rows <- which(!X$Pass_1) # this excludes any NAs

    # Bring any skew, kurt and pass from 2 to 1
    X$Skew_1[log_rows] <- X$check_SkewKurt2.Skew[log_rows]
    X$Kurt_1[log_rows] <- X$check_SkewKurt2.Kurt[log_rows]

    X$Pass_1 <- X$Pass_1 | X$check_SkewKurt2.Pass

  }

  #----

  X <- X[c("iCode", "Pass_0", "Skew_0", "Kurt_0", "Treatment", "Pass_1", "Skew_1", "Kurt_1")]

  X$Treatment[is.na(X$Treatment)] <- "-"

  # round to 2 decimal places
  COINr::round_df(X, 2)

}

# Generates the styled DT table to display the treated data set.
# l is expected to be r_coin_treated() in the server
f_style_treatment_table <- function(l, coin){

  # round
  df_display <- COINr::signif_df(l$dset_treated, 4)

  ncol_display <- ncol(df_display)

  ## CELL HIGHLIGHTING ----

  treated_points <- coin$Analysis$Treated$Treated_Points
  stopifnot(!is.null(treated_points))

  # convert to numeric
  treated_points[treated_points == ""] <- 0
  treated_points[treated_points == "winhi"] <- 1
  treated_points[treated_points == "winlo"] <- 2
  treated_points[, l$treatment_details$Treatment == "Log"] <- 3
  treated_points <- sapply(treated_points, as.numeric) |>
    as.data.frame()

  # highlight colours different treatment types
  highlight_colours <- c("white", "orange", "lightblue", "lightgreen")

  ## FOOTER SUMMARY ----

  # character vector for treatment type per col
  treat_summary <- c("", "Treatment", l$treatment_details$Treatment)

  summary_row = htmltools::withTags(table(
    DT::tableHeader(l$dset_treated),
    DT::tableFooter(treat_summary)
  ))

  # bind display and highlight dfs for DT
  X <- cbind(df_display, treated_points)

  # The +1 and -1 here and elsewhere are to do with two things:
  # 1. That DT uses JS which uses 0 as the first index
  # 2. That the rownames are counted as a column unless removed
  # See https://rstudio.github.io/DT/ (note just above sec 2.5)
  DT::datatable(
    X,
    rownames = FALSE,
    caption = NULL,
    container = summary_row,
    selection = list(target = "column", mode = "single", selected = 2),
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons =
        list("copy", list(
          extend = 'collection',
          buttons = list(list(extend = 'csv',
                              filename = 'composer_treated_data',
                              title = "Treated"), # note: any spaces here result in weird output!
                         list(extend = 'excel',
                              filename = 'composer_treated_data',
                              title = "Treated"),
                         list(extend = 'pdf',
                              filename = 'composer_treated_data',
                              title = "Treated")),
          text = 'Download'
        )),
      columnDefs = list(
        list(
          visible=FALSE,
          targets=ncol_display:(ncol(X)-1)
        )
      ),
      scrollX = TRUE
    )
  ) |>
    DT::formatStyle(
      columns = 3:ncol_display,
      valueColumns = (ncol_display + 1):ncol(X),
      backgroundColor = DT::styleEqual(c(0:3), highlight_colours)
    )

}

# Info about outliers before treatment
# l is expected to be r_coin_treated() in the server
f_print_treat_pre <-  function(l){

  n <- l$n_with$outliers_pre
  iCode_with_outliers <- l$treatment_details$iCode[!l$treatment_details$Pass_0]
  f_first_few(iCode_with_outliers)

}

# Info about outliers after treatment
# l is expected to be r_coin_treated() in the server
f_print_treat_post <-  function(l){

  n <- l$n_with$outliers_post
  iCode_with_outliers <- l$treatment_details$iCode[!l$treatment_details$Pass_1]
  f_first_few(iCode_with_outliers)
}

# Plot scatter plot before/after treatment, of selected indicator
# l is expected to be r_coin_treated() in the server
# icol_selected is the column index selected.
f_plot_scatter_treat <- function(l, coin, icol_selected, dset1){

  # get icode to plot
  icode_plot <- names(l$dset_treated)[icol_selected + 1]

  if (icode_plot %in% c("uCode", "uName")) return(NULL)

  # TODO if we allow users to skip operations, will have to change dset1 here
  iCOINr::iplot_scatter(
    coin,
    iCodes = icode_plot,
    dsets = c(dset1, "Treated"),
    Levels = 1,
    axes_label = "iName+dset",
    marker_colour = pal_find()[5]
  )
}
