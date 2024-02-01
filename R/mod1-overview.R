
# UI ----------------------------------------------------------------------

#' @importFrom plotly plotlyOutput renderPlotly
#'
overview_ui <- function(id = NULL, sidebar_width) {

  ns <- NS(id)

  page_sidebar(

    fillable = FALSE,

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Data upload",
      "Browse to your Excel file which must contain 'idata' and 'imeta' tabs
        (see instructions). Click 'load' and check the output in the main window.",

      tags$a(href="www/FIND-CI_input_template.xlsx", "Download the input template"),
      tags$a(href="www/example_input_WH.xlsx", "Download the example data set"),

      fileInput(ns("i_indicator_data"), "Choose Excel File",
                multiple = FALSE,
                accept = c(".xlsx")
      ),
      actionButton(ns("i_load_overview"), "Load Data")
    ),


    # WELCOME MESSAGE
    card(
      id = ns("welcome-card"),
      card_header("Welcome"),
         card_body(
           h2("Welcome to the composer app"),

           span(
             "Composer is an app for building and analysing composite indicators. It is based on the ",
             tags$a(href="https://bluefoxr.github.io/COINr/", "COINr"),
             " R package."
           ),

           "To begin using the app, download the template and upload your data using the controls in the sidebar.",

           span(
             tags$a(href="https://finddx.github.io/composer/", "Full documentation is available online"),
             " and is also accessible by clicking the question mark icon in the top right of every page."
           )
         )
    ),

    # FRAMEWORK PLOT
    card(full_screen = TRUE,
         card_header("Index structure"),
         card_body(
           plotlyOutput(ns("framework"), height = "700px")
         )
    ),

    # MESSAGES
    card(
      card_header("Messages"),
      card_body(
        verbatimTextOutput(ns("init_o"))
      )
    )

  )
}

# SERVER ------------------------------------------------------------------

overview_server <-  function(id, r_share, coin) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(input$i_load_overview, {

      req(input$i_indicator_data)

      excel_file <- input$i_indicator_data
      coin_init <- f_read_input(excel_file$datapath, input_type = "xlsx")
      if(is.null(coin_init)) req(FALSE) # this is when there is an input error

      # check for country data
      uCodes <- coin_init$Meta$Unit$uCode
      l_check <- f_check_ISO3(uCodes)

      # store conclusion for other modules
      r_share$is_country_data <- l_check$is_country

      # user notification
      showNotification(
        ui = l_check$Message,
        type = "message",
        duration = 10
      )

      # write to global coin
      coin(coin_init)

      # hide welcome message
      shinyjs::hide("welcome-card", anim = TRUE, animType = "fade", time = 1)
    })

    # text summary
    output$init_o <- renderPrint({
      req(coin())
      print(coin())
    })

    # framework plot
    output$framework <- renderPlotly({
      req(coin())
      iCOINr::iplot_framework(coin())
    })

    # reset session
    observeEvent(input$i_reset_overview, {
      session$reload()
    })

    # load session
    observeEvent(input$i_load_session_button,{
      r_share$load_session <- input$i_load_session_button
    })

  })
}

# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Read file inputs
#'
#' Reads file inputs as either two csv files or one xlsx file, and returns a coin.
#' We can decide whether to allow the user to input csvs or xlsx, or just constrain them to use one.
#' For the moment I put both here.
#'
#' If the input is csv, the two files must follow the formats specified in <https://bluefoxr.github.io/COINr/reference/new_coin.html>.
#' Otherwise if the input is xlsx, the same format is followed but both tables are in a single xlsx file
#' with tabs called "iData" and "iMeta".
#'
#' Note that if there is a problem with the input files, this function will output error messages
#' which we should display somewhere, e.g. in a text output box, as they can help to point to
#' the problem and the solution for the user.
#'
#' @param path1 First file path. If `input_type = "xlsx"` this will be the path to the
#' xlsx file. Otherwise, it will be the path to the first csv file which should contain
#' a data frame with indicator data in the `iData` format.
#' @param path2 Second file path. If `input_type = "csv"` this should be the path to
#' a csv file with an `iMeta` table inside. If `input_type = "xlsx"` this argument is ignored.
#' @param input_type One of `"xlsx"` or `"csv"`.
#'
#' @return A coin-class object
#'
#' @importFrom readxl excel_sheets read_xlsx
#' @importFrom tools file_ext
#' @importFrom COINr new_coin
#'
#' @examples
#' # excel
#' # coin <- f_read_input("./data/example_input.xlsx", input_type = "xlsx")
#' # csv
#' # coin <- f_read_input("./data/iData_example.csv",
#' #                      "./data/iMeta_example.csv",
#' #                      input_type = "csv")
#' @export
f_read_input <- function(path1, path2, input_type){

  # checks
  stopifnot(input_type %in% c("csv", "xlsx"))

  na_strings <- c("", "NA")

  # depending on input_type
  if(input_type == "csv"){

    stop("This file type has been disabled to reduce package dependencies.")
    # # check file is the right type
    # file_ext1 <- tools::file_ext(path1)
    # # check
    # if(file_ext1 != "csv"){
    #   send_input_error("The file at the first path specified is not a csv file as expected.", "Input file")
    #   return(NULL)
    #   #stop("The file at the first path specified is not a csv file as expected.")
    # }
    # file_ext2 <- tools::file_ext(path2)
    # # check
    # if(file_ext2 != "csv"){
    #   send_input_error("The file at the second path specified is not a csv file as expected.", "Input file")
    #   return(NULL)
    #   #stop("The file at the second path specified is not a csv file as expected.")
    # }
    #
    # # read in files
    # iData <- suppressMessages(readr::read_csv(path1, na = na_strings))
    # iMeta <- suppressMessages(readr::read_csv(path2, na = na_strings))

  } else {

    # check file is the right type
    file_ext <- tools::file_ext(path1)
    # check
    if(file_ext != "xlsx"){
      send_input_error("The file at the path specified is not an xlsx file as expected.", "Input file")
      return(NULL)
      #stop("The file at the path specified is not an xlsx file as expected.")
    }

    # find sheet names
    sheet_names <- readxl::excel_sheets(path1)
    # check
    if(!all(c("idata", "imeta") %in% tolower(sheet_names))){
      send_input_error("The xlsx file does not contain one or more of the expected tab names. Expected tabs are 'iData' and 'iMeta'.", "Input file")
      return(NULL)
      #stop("The xlsx file does not contain one or more of the expected tab names. Expected tabs are 'iData' and 'iMeta'.", call. = FALSE)
    }
    idata_sheet <- sheet_names[tolower(sheet_names) == "idata"]
    imeta_sheet <- sheet_names[tolower(sheet_names) == "imeta"]

    # read in tabs now
    iData <- readxl::read_xlsx(path1, sheet = idata_sheet, na = na_strings)
    iMeta <- readxl::read_xlsx(path1, sheet = imeta_sheet, na = na_strings)

  }

  # in-app validation checks (verbose checks with more helpful messages)
  error_message <- validate_iData(iData)

  if(!is.null(error_message)){
    send_input_error(error_message, "iData tab")
    return(NULL)
  }
  error_message <- validate_iMeta(iMeta)
  if(!is.null(error_message)){
    send_input_error(error_message, "iMeta tab")
    return(NULL)
  }
  error_message <- cross_validate(iData, iMeta)
  if(!is.null(error_message)){
    send_input_error(error_message, "iData/iMeta cross-check")
    return(NULL)
  }

  # Build coin, but return any COINr errors or warnings to the user that were
  # not previously caught
  coin <- tryCatch(
    expr = {
      COINr::new_coin(iData = iData, iMeta = iMeta, quietly = TRUE)
    },
    error = function(e){
      send_input_error(as.character(e), "COINr internal")
      return(NULL)
    },
    warning = function(w){
      # user notification
      showNotification(
        ui = w,
        type = "warning",
        duration = 10
      )
    }
  )
  # if NULL coin means error...
  if(is.null(coin)) return(NULL)

  coin

}


# check whether data is country level, based on codes
# Also return a display message to return to the user.
f_check_ISO3 <- function(uCodes){

  n_codes <- length(uCodes)
  detected_ISO3s <- intersect(uCodes, composer::valid_countries$ISO3)
  n_detected <- length(detected_ISO3s)

  if (n_detected == 0){

    Message <- "Your input data does not contain any ISO3c country codes, therefore mapping is disabled."
    is_country <- FALSE

  } else if (n_detected < n_codes) {

    not_ISO3s <- setdiff(uCodes, detected_ISO3s)
    not_ISO3s_string <- utils::head(not_ISO3s, 5) |> toString()

    Message <- glue::glue("Some ISO3c country codes detected but some are not recognised. You can still ",
                          "proceed but mapping will be disabled. Unrecognised codes (first five): {not_ISO3s_string}.")
    is_country <- FALSE
  } else {
    Message = "Your input data is recognised as country data."
    is_country <- TRUE
  }

  # output
  list(
    is_country = is_country,
    Message = Message
  )

}


#' Verbose check on iData tab
#'
#' Checks and messages to pass to the user for basic errors in the iData tab of
#' the input spreadsheet.
#' Note COINr does many of these checks anyway, but the intention is to generate
#' some user-friendly versions here. Called in [f_read_input()].
#'
#' @param iData Table imported from user input spreadsheet.
#'
#' @return Message as text, or `NULL` if no errors.
#' @export
#'
validate_iData <- function(iData){


  # ID cols -----------------------------------------------------------------

  if(!is.data.frame(iData)){
    return("Cannot recognise your data as valid table - something very wrong!")
  }

  if("uCode" %nin% names(iData)){
    return("Expected column 'uCode' not found in your data - did you change the column name or delete it?")
  }

  if("uName" %nin% names(iData)){
    return("Expected column 'uName' not found in your data - did you change the column name or delete it?")
  }

  if(!is.character(iData$uCode)){
    return("The 'uCode' column is expected to be formatted as text but it is not (did you enter numbers here?)")
  }

  if(!is.character(iData$uName)){
    return("The 'uName' column is expected to be formatted as text but it is not (did you enter numbers here?)")
  }

  if(any(is.na(iData$uCode))){
    return("Missing values found in 'uCode' column - please correct.")
  }

  if(any(is.na(iData$uName))){
    return("Missing values found in 'uName' column - please correct.")
  }


  # Data cols ---------------------------------------------------------------

  if(ncol(iData) < 3){
    return("No data columns found in your data?")
  }

  # # get only data cols (exclude those with all NAs which are removed later)
  # iData_ <- iData[(names(iData) %nin% c("uCode","uName")) &
  #                   !(colSums(!is.na(iData)) == 0)]
  #
  # not_numeric <- names(iData_)[!sapply(iData_, is.numeric)]
  # if(length(not_numeric) > 0){
  #   return(paste0("One or more of your data columns have non-numeric entries: ", toString(not_numeric)))
  # }

  duplicate_icodes <- names(iData)[duplicated(names(iData))]
  if(length(duplicate_icodes) > 0){
    return(paste0("Duplicate indicator codes detected: ", toString(duplicate_icodes)))
  }

  duplicate_ucodes <- iData$uCode[duplicated(iData$uCode)]
  if(length(duplicate_ucodes) > 0){
    return(paste0("Duplicate uCode codes detected: ", toString(duplicate_ucodes)))
  }

  NULL

}


#' Verbose check on indicator metadata tab
#'
#' Checks and messages to pass to the user for basic errors in metadata rows of
#' the Data tab in the input template. Called in [f_read_input()].
#'
#' @param iMeta Table imported from user input spreadsheet.
#'
#' @return Message as text, or `NULL` if no errors.
#' @export
#'
validate_iMeta <- function(iMeta){

  if(!is.data.frame(iMeta)){
    return("Cannot recognise indicator data as valid table - something very wrong!")
  }

  # REQUIRED COLS -----------------------------------------------------------

  # required cols
  required_cols <- c("Level", "iCode", "Parent", "Direction", "Type", "Weight")
  if(!all(required_cols %in% colnames(iMeta))){
    return("One or more expected col names not found (Level, iCode, Parent, Direction, Type, Weight).")
  }

  # check col types
  col_numeric <- c("Level", "Direction", "Weight")
  col_char <- setdiff(required_cols, col_numeric)

  # numeric
  num_check <- sapply(iMeta[col_numeric], is.numeric)
  if(!all(num_check)){
    return(paste0("One or more of the following columns is not numeric: ", paste0(col_numeric, collapse = "/")))
  }

  # char
  char_check <- sapply(iMeta[col_char], is.character)
  if(!all(char_check)){
    return(paste0("One or more of the following columns is not character: ", paste0(col_char, collapse = "/")))
  }


  # Type has to be one of the following
  itypes <- c("Indicator", "Aggregate", "Group", "Denominator", "Other")
  if(any(iMeta$Type %nin% itypes)){
    return("One or more entries in Type is not allowed - should be one of 'Indicator', 'Aggregate', 'Group', 'Denominator', or 'Other'.")
  }


  iMeta_inds <- iMeta[iMeta$Type %in% c("Indicator", "Aggregate"), ]

  # weights
  w_col <- iMeta_inds$Weight

  if(any(is.na(w_col))){
    return("One or more missing values in the 'weights' row - please fill in.")
  }

  if(!possibly_numeric(w_col)){
    return("One or more entries in the 'weights' row are not numbers.")
  }

  if(any(as.numeric(w_col) < 0)){
    return("Negative weights detected?")
  }

  # directions
  d_col <- iMeta_inds$Direction

  if(any(is.na(d_col))){
    return("One or more missing values in the 'Direction' column - please fill in.")
  }

  if(!possibly_numeric(d_col)){
    return("One or more entries in the 'Direction' column are not numbers.")
  }

  if(any(as.numeric(d_col) %nin% c(-1, 1))){
    return("Values in the 'Direction' column found that are not -1 or 1. Please fix.")
  }

  # parents
  maxlev <- max(iMeta_inds$Level)

  p_col <- iMeta_inds$Parent[iMeta_inds$Level < maxlev]

  if(any(is.na(p_col))){
    return("One or more missing values in the 'Parent' column - please fill in.")
  }

  if(possibly_numeric(p_col)){
    return("One or more entries in the 'Parent' column look like numbers - please ensure codes start with a letter.")
  }

  # names
  n_col <- iMeta_inds$iName

  if(any(is.na(n_col))){
    return("One or more missing values in the 'iName' column - please fill in.")
  }

  if(possibly_numeric(n_col)){
    return("One or more entries in the 'iName' column look like numbers - please ensure names start with a letter.")
  }

  NULL

}

# Verbose cross-validation checks between iData and iMeta
# This is a subset of checks inside COINr. Presumes iData and iMeta have both
# been individually checked already.
# Returns either NULL (no error) or a message which is then passed to the user.
cross_validate <- function(iData, iMeta){

  # change any integer to numeric
  iData_codes <- colnames(iData)[colnames(iData) %nin% c("uCode", "uName", "Time")]
  iData[iData_codes] <- df_int_2_numeric(iData[iData_codes])

  # CROSS CHECKS
  # Make sure iData codes are all in iMeta, excluding special codes

  missing_iMeta <- iData_codes[iData_codes %nin% iMeta$iCode]
  if(length(missing_iMeta) > 0){
    return(paste0("Column names from iData not found in iMeta: ", paste(missing_iMeta, collapse = ", " ),
                  ". All columns in iData must have corresponding rows in iMeta, except for 'uCode' and 'uName'."))
  }

  iMeta_indcodes <- iMeta$iCode[iMeta$Type != "Aggregate"]
  missing_iData <- iMeta_indcodes[iMeta_indcodes %nin% colnames(iData)]
  if(length(missing_iData) > 0){
    return(paste0("Entries in iCode column of iMeta not found in iData: ", paste(missing_iMeta, collapse = ", " ),
           " Any entries in iMeta with Type 'Indicator' must have corresponding columns in iData."))
  }

  # we need indicator codes
  iCodes <- iMeta$iCode[iMeta$Type == "Indicator"]
  non_numeric_inds <- !(sapply(iData[iCodes], is.numeric))
  if(any(non_numeric_inds)){
    return(paste0("Non-numeric indicators detected. The following have been labelled as 'Indicator' but refer to non-numeric columns in iData (not allowed): \n", paste(iCodes[non_numeric_inds], collapse = ", " ),
         "\n. This may occur if you have text in indicator columns, or have incorrectly formatted some numbers as text in Excel."))
  }

  # check for any parents with no children
  icodes_agg <- iMeta$iCode[iMeta$Type == "Aggregate"]
  icodes_agg_nokids <- icodes_agg[icodes_agg %nin% iMeta$Parent]
  if(length(icodes_agg_nokids) > 0){
    return(paste0("Aggregate iCode(s) found in iMeta that do not have any children (not named in 'Parent' column). Codes: ", icodes_agg_nokids))
  }

  NULL
}
