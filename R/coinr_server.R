coinr_server <- function(input, output, session) {

  #bs_themer()

  options(
    shiny.maxRequestSize=30*1024^2, # set max file upload size to 30 MB
    shiny.launch.browser = TRUE # launch browser by default
  )

  bookmark_type <- "URL" # set either "URL" or "RDS"

  # the coin which will be passed between modules
  coin <- reactiveVal(NULL)

  # list for passing shared reactives between modules (except the coin)
  r_share <- reactiveValues(
    is_country_data = NULL,
    data_is_normalised = FALSE,
    results_calculated = FALSE,
    SA_results = NULL, # sensitivity analysis results, in order to store on save session
    load_session = NULL
  )

  # Modules -----------------------------------------------------------------

  overview_server("overview", r_share, coin)
  screening_server("screening", r_share, coin)
  imputation_server("imputation", r_share, coin)
  treat_server("treat", r_share, coin)
  normalisation_server("normalisation", r_share, coin)
  compose_server("compose", r_share, coin)
  stats_server("stats", r_share, coin)
  correlations_server("correlations", r_share, coin)
  map_server("map", r_share, coin)
  bubble_server("bubble", r_share, coin)
  bar_server("bar", r_share, coin)
  profiles_server("profiles", r_share, coin)
  reweighting_server("reweighting", r_share, coin)
  remove_elements_server("remove_elements", r_share, coin)
  sensitivity_server("sensitivity", r_share, coin)

  # Top level components ----------------------------------------------------

  # add paths to resources for the app (files to download etc.)
  # This makes files in package source inst/app/www available to the app at www/
  addResourcePath('www', system.file('app/www', package = "composer"))

  # Documentation
  output$docu <- renderUI({
    page2display <- get_doc_page(input)
    tags$iframe(seamless="seamless",
                src= page2display,
                width="100%",
                height=800)
  })

  # Export to Excel
  output$export_to_excel <- downloadHandler(
    filename = function() {
      paste("composer_results-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(con) {
      req(coin())
      COINr::export_to_excel(coin(), fname = con)
    }
  )

  # Export to R
  output$export_to_R <- downloadHandler(
    filename = function() {
      paste("composer_coin-", Sys.Date(), ".RDS", sep = "")
    },
    content = function(con) {
      req(coin())
      saveRDS(coin(), file = con)
    }
  )

  # Save session ------------------------------------------------------------

  # things NOT to include in bookmark
  setBookmarkExclude(c("overview-i_indicator_data", "overview-i_load_session", "overview-i_load_overview"))

  # set bookmark type based on settings in app.R
  if(bookmark_type == "URL"){
    bookmark_2_URL(coin, r_share)
  } else if (bookmark_type == "RDS"){
    bookmark_2_RDS(session, input, coin, r_share)
  }

  # Load session ------------------------------------------------------------

  # Basically the reverse process of save session.
  # - Load RDS file
  # - Manually create bookmark folder and populate with expected files
  # - Refresh session using stored URL which points to the folder
  # - Load the coin and any other reactives needed

  # when user clicks load session button, read RDS file to get url
  # put url in browser and reload session. This effectively loads the previous
  # session.
  observeEvent(r_share$load_session, {

    # read session file
    l_sesh <- readRDS(input$`overview-i_load_session`$datapath)

    # Create bookmark folder - this will be automatically accessed when the URL is called.
    sesh_folder <- get_url_bookmark_path(l_sesh$url)
    dir.create(sesh_folder, recursive = TRUE)

    # unpack session info into this new directory
    saveRDS(l_sesh$input, paste0(sesh_folder, "/input.RDS"))
    saveRDS(l_sesh$values, paste0(sesh_folder, "/values.RDS"))

    # now reload app using the URL
    updateQueryString(l_sesh$url)
    session$reload()

  })

}

# This is the code to use standard Shiny bookmarking. Assumes there is PERSISTENT
# storaage wherever the app is being run.
bookmark_2_URL <- function(coin, r_share){

  # The coin and other things have to be added to the bookmark state$value
  onBookmark(function(state){
    state$values$coin_saved <- coin()
    state$values$r_share <- reactiveValuesToList(r_share)
  })

  # When session is restored we have to:
  # - restore the coin from the state$values
  # - extract analysis tables again
  # - make a copy of the coin with no indicators removed, for plotting
  onRestored(function(state){

    # restore coin
    coin(state$values$coin_saved)
    # restore reactive values list (has to be done in loop unfortunately)
    for(rname in names(state$values$r_share)){
      r_share[[rname]] <- state$values$r_share[[rname]]
    }

  })

}

# Notes: This is a bit/lot of a hack. We use server bookmarking, and this creates
# a URL and stores all of the app state in a folder. However, we don't have
# persistent storage, so we have to manually access this folder and package
# everything in a RDS file to download. This is further complicated because
# we can't do this directly inside a downloadHandler() function, so there is a
# JS trick I found somewhere.
bookmark_2_RDS <- function(session, input, coin, r_share){

  # this is a list which will contain all the session info to download
  r_save <- reactiveVal(NULL)

  # Manually trigger bookmark on button click
  observeEvent(input$save_session, {
    req(coin())
    # manually bookmark
    session$doBookmark()
  })

  # The coin has to be added to the bookmark state$value
  onBookmark(function(state){
    state$values$coin_saved <- coin()
    state$values$r_share <- r_share
  })

  # After bookmarking, collect all bookmarked data from folder, plus URL and save
  # to reactive value. This is accessed below by the download handler.
  # NOTE when deployed, we need to check where the bookmark folder will be.
  onBookmarked(function(url){

    # deduce folder name based on url
    folder_name <- get_url_bookmark_path(url)
    # get input (app state) and values (other variables, like the coin)
    b_input <- readRDS(paste0(folder_name, "/input.RDS"))
    b_values <- readRDS(paste0(folder_name, "/values.RDS"))

    # package everything and save to reactive list
    l_save <- list(url = url, input = b_input, values = b_values)
    r_save(l_save)

    # delete bookmark folder
    unlink(folder_name, recursive = TRUE)

    # manually trigger download handler
    shinyjs::runjs("$('#download_session')[0].click();")
  })

  # Download RDS file: this is triggered manually by the JS above
  output$download_session <- downloadHandler(
    filename = function() {
      paste("app_session", Sys.Date(), ".RDS", sep="")
    },
    content = function(file) {
      # the RDS file contains the reactive list defined above
      saveRDS(r_save(), file)
    }
  )

  # When session is restored we have to manually extract the coin and the
  # shared reactives.
  onRestored(function(state){

    # load coin
    coin(state$values$coin_saved)

    # there may be a better way to update a reactive list...
    r_share$is_country_data <- state$values$r_share$is_country_data
    r_share$data_is_normalised <- state$values$r_share$data_is_normalised
    r_share$results_calculated <- state$values$r_share$results_calculated
    r_share$SA_results <- state$values$r_share$SA_results

    # delete bookmark folder
    unlink(state$dir, recursive = TRUE)

  })

}
