
# UI ----------------------------------------------------------------------

stats_ui <- function(id) {

  ns <- NS(id)

  layout_columns(
    col_widths = c(6, 6, 12),
    row_heights = list("40vh", "55vw"),

    # DISTRIBUTION PLOT
    card(
      full_screen = TRUE,
      card_header("Distributions"),

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          selectInput(
            inputId = ns('i_violinplot_type'),
            label = 'Plot type',
            choices = c("Violin", "Histogram"))
        ),
        card_body(
          plotlyOutput(ns("o_plot_violin"))
        )
      )
    ),

    # SCATTER PLOT
    card(
      full_screen = TRUE,
      card_header("Scatter plot"),

      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          selectInput(inputId = ns('i_scatterplot_yvar'),
                      label = 'Plot against',
                      choices = NULL),
          input_switch(ns("scat_logx"), label = "Log X"),
          input_switch(ns("scat_logy"), label = "Log Y")
        ),
        card_body(
          plotlyOutput(ns("o_plot_scatter"))
        )
      )
    ),

    # STATS TABLE
    card(
      full_screen = TRUE,
      card_header("Scatter plot"),

        card_body(
          DT::dataTableOutput(ns("o_stats_table"), width = "95%")
        )
      )
  )
}

# SERVER ------------------------------------------------------------------

stats_server <- function(id, r_share, coin) {

  moduleServer(id, function(input, output, session) {

    # set reactives
    l_stats <- reactiveVal(NULL)
    icode_selected <- reactiveVal(NULL)

    # STATS TABLE
    output$o_stats_table <- DT::renderDataTable({
      req(r_share$results_calculated)
      l_stats((f_stat_table(coin())))
      f_style_stats_table(l_stats())
    }, server = FALSE)

    # update selected indicator reactive
    observeEvent(input$o_stats_table_rows_selected, {
      req(r_share$results_calculated)
      icode_selected(
        l_stats()$FlaggedStats$iCode[input$o_stats_table_rows_selected]
      )
    })

    # VIOLIN PLOT
    output$o_plot_violin <- plotly::renderPlotly({
      req(icode_selected())
      req(coin())
      iCOINr::iplot_dist(coin(), dset = "Raw", iCode = icode_selected(), ptype = input$i_violinplot_type)
    })

    # SCATTER PLOT
    output$o_plot_scatter <- renderPlotly({
      req(icode_selected())
      req(coin())
      iCOINr::iplot_scatter(
        coin(),
        dsets = "Raw",
        iCodes = c(icode_selected(), input$i_scatterplot_yvar),
        Levels = 1,
        log_axes = c(input$scat_logx, input$scat_logy)
      )
    })

    # update scatter plot y variable
    observe({
      req(coin())
      req(r_share$results_calculated)
      updateSelectInput(inputId = "i_scatterplot_yvar", choices = get_iCodes(coin()))
    }) |> bindEvent(r_share$results_calculated)

  })

}


# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Table of stats and flags for indicators
#'
#' Outputs a list with two data frames: `.$FlaggedStats` and `.$Flags`. The first
#' is a table to display which contains things like max, min, skew and kurt,
#' data availability and so on. The second is an equivalent-sized df but with
#' logical columns: this specifies which elements of the first df represent "flags".
#' E.g. if data availability for an indicator is below `dat_avail_thresh`, it will
#' return `TRUE`. The output of this function is intended to be fed into `f_style_screening_table()`
#' or something similar.
#'
#' NOTE: we will have to think what the default for `dset` is. Normally it makes most
#' sense to look at the raw data, but at the moment this table is positioned in the
#' "results" end of the app. To discuss.
#'
#' @param coin The coin
#' @param dset Name of data set to analyse.
#' @param dat_avail_thresh Number between 0 and 1.
#' @param skew_thresh Positive number. Probably we don't need to expose the user to this.
#' @param kurt_thresh Positive number. Probably we don't need to expose the user to this.
#' @param duplicate_thresh Number between 0 and 1: threshold for the max fraction of repeated
#' values. Above this will be flagged.
#' @param collinear_thresh Number between 0 and 1: correlation pairs above this will be flagged
#' as collinear.
#' @param neg_corr_thresh As previous but for negative correlations.
#'
#' @export
f_stat_table <- function(
    coin,
    dset = "Raw",
    dat_avail_thresh = 0.66,
    skew_thresh = 2,
    kurt_thresh = 3.5,
    duplicate_thresh = 0.5,
    collinear_thresh = 0.9,
    neg_corr_thresh = -0.4) {

  stopifnot(is.coin(coin))
  # skip checks on other args because constrained by Shiny

  # Univariate stats ----

  # this df is big and will be tidied and focused before output
  df_stats <- COINr::get_stats(
    coin,
    dset = dset,
    t_skew = skew_thresh,
    t_kurt = kurt_thresh,
    t_avail = dat_avail_thresh,
    out2 = "df"
  )

  # to display: the base table to which I add
  df_display <- df_stats[c("iCode", "Min", "Max", "Frc.Avail", "Frc.Same")]

  skew_values <- signif(df_stats$Skew, digits = 2)
  kurt_values <- signif(df_stats$Kurt, digits = 2)
  df_display[["Skew/Kurt"]] <- glue::glue("{skew_values} / {kurt_values}")

  # FLAGS: used to highlight df_display
  # in the flag df TRUE means it is flagged as having a problem

  low_data_avail <- df_stats$Flag.Avail == "LOW"
  high_prc_duplicates <- df_stats$Frc.Same > duplicate_thresh
  has_outliers <- df_stats$Flag.SkewKurt == "OUT"

  df_flag <- data.frame(
    iCode = df_display$iCode,
    Min = FALSE,
    Max = FALSE,
    Frc_Avail = low_data_avail,
    Frc_Same = high_prc_duplicates,
    Skew_Kurt = has_outliers
  )

  # Bivariate (correlations) ----

  # if we check the correlations of a data set BEFORE normalisation, we have to
  # flip the sign of the correlations of any indicators with negative directionality.
  # This is taken care of by get_corr_flags().

  adjust_directions <- !(dset %in% c("Aggregated", "Normalised"))

  # Outputs a df of any correlation pairs above collinear_thresh
  #  within the same group at level 2.
  df_collinear <- COINr::get_corr_flags(
    coin,
    dset = dset,
    cor_thresh = collinear_thresh,
    thresh_type = "high",
    grouplev = 2,
    cortype = "spearman",
    use_directions = adjust_directions
  )

  # as above but for negative correlations
  df_negcorr <- COINr::get_corr_flags(
    coin,
    dset = dset,
    cor_thresh = neg_corr_thresh,
    thresh_type = "low",
    grouplev = 2,
    cortype = "spearman",
    use_directions = adjust_directions
  )

  # any iCodes appearing in these are flagged
  df_flag$Collinear <- df_flag$iCode %in% c(df_collinear$Ind1, df_collinear$Ind2)
  df_flag$Neg_Corr <- df_flag$iCode %in% c(df_negcorr$Ind1, df_negcorr$Ind2)

  # add correlation entries to display df

  pairs_colin <- f_gather_correlations(df_collinear)
  collinear_rows <- match(df_display$iCode, names(pairs_colin))
  df_display$Collinear <- pairs_colin[collinear_rows]

  pairs_neg <- f_gather_correlations(df_negcorr)
  neg_corr_rows <- match(df_display$iCode, names(pairs_neg))
  df_display$Neg_Corr <- pairs_neg[neg_corr_rows]

  # TODO we may also want to make this available for export perhaps.
  list(
    FlaggedStats = df_display,
    Flags = df_flag
  )

}

# l_stat_table is the list output of f_stat_table()
f_filter_stat_table <-  function(l_stat_table){

  rows_with_flags <- rowSums(l_stat_table$Flags[-1]) > 0

  list(
    FlaggedStats = l_stat_table$FlaggedStats[rows_with_flags, ],
    Flags = l_stat_table$Flags[rows_with_flags, ]
  )

}

#' Styling for indicator stats table
#'
#' @param l A list - the output of [f_stat_table()]
#' @param table_caption Optional caption
#' @param highlight_colour Colour to highlight flagged cells
#'
#' @return DT table
f_style_stats_table <- function(l, table_caption = NULL, highlight_colour = "#ffc266"){

  df_display <- l$FlaggedStats
  df_highlight <- l$Flags

  stopifnot(identical(dim(df_display), dim(df_highlight)))

  ncol_display <- ncol(df_display)

  make_binary_for_DT <- function(x) {
    if (is.logical(x)) return(as.numeric(x))
    x
  }

  df_highlight_numeric <- lapply(df_highlight, make_binary_for_DT) |>
    as.data.frame()

  # join dfs: the latter will be hidden in the output
  X <- cbind(df_display, df_highlight_numeric)

  # highlight colours for 0 and 1 respectively
  highlight_colours <- c("white", highlight_colour)

  # column names
  column_names <- c("Indicator", "Min", "Max", "Availability", "Same",
                    "Skew/kurtosis", "Collinear with", "Neg. corr. with")

  # The +1 and -1 here and elsewhere are to do with two things:
  # 1. That DT uses JS which uses 0 as the first index
  # 2. That the rownames are counted as a column unless removed
  # See https://rstudio.github.io/DT/ (note just above sec 2.5)
  DT::datatable(
    X,
    rownames = FALSE,
    caption = table_caption,
    colnames = column_names,
    selection = list(target = "row", mode = "single", selected = 1),
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      buttons =
        list('copy', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        )),
      columnDefs = list(
        list(
          visible=FALSE,
          targets=ncol_display:(ncol(X)-1)
        )
      )
    )
  ) |>
    DT::formatStyle(
      columns = 1:ncol_display,
      valueColumns = (ncol_display + 1):ncol(X),
      backgroundColor = DT::styleEqual(c(0,1), highlight_colours)) |>
    DT::formatPercentage(4:5)

}

