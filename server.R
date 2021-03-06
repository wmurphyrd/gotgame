
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(magrittr)
library(tidyverse)
library(googlesheets)
library(cellranger)
library(jsonlite)

gs_tracker_id <- "1rJY91THJIVZ0j6WM0vL0rZsTyCK8hvMg7w8JK2jveK4"
master_list_name <- "MASTER 2018 Texters"
gs_secrets <- readLines(".secrets") %>% fromJSON()
# debugging
if (interactive()) { gs_secrets$redirect_uri <- "http://127.0.0.1:7625/" }
nrow_if_present <- function(x, default = NA) {
  ifelse(is.null(x), default, nrow(x))
}

shinyServer(function(input, output, session) {
  all_files <- tibble(name = character(0))
  gs_auth_token <- NULL
  newFiles <- reactive({
    req(input$uploads)
    new_uploads <- input$uploads %>%
      filter(!name %in% all_files$name)
    if (nrow(new_uploads) > 0) {
      new_uploads %>%
        mutate(
          data = lapply(datapath, read_csv,
                        col_types = cols(.default = col_character())),
          type = ifelse(grepl("contact-history", name),
                        "contacts",
                        ifelse(grepl("question-responses", name),
                               "questions",
                               ifelse(grepl("uncontacted-targets", name),
                                      "uncontacteds",
                                      NA)
                               )
                        )
        ) %>%
        filter(!is.na(type)) %>%
        mutate(
          data = lapply(data, function (d) {
            names(d)[names(d) %in% c("Contacted On", "Marked On")] <- "date"
            names(d)[names(d) %in% c("Contacted By", "Marked By")] <- "Texter"
            d
          }),
          campaign = sapply(data, function (d) { d$`Campaign Name`[1] })
        ) %>%
        bind_rows(all_files) %>%
        # ignore duplicates that made it past the filename check
        group_by(campaign, type) %>%
        filter(row_number(campaign) == 1) %>%
        ungroup() ->>
        all_files
    }
    all_files %>%
      select(campaign, type, data) %>%
      # ensure all three types make it into the set
      mutate(
        type = factor(type, levels = c("contacts", "uncontacteds", "questions"))
      ) %>%
      spread(type, data, drop = FALSE) %>%
      mutate(
        contacted_count = sapply(contacts, nrow_if_present),
        uncontacted_count = sapply(uncontacteds, nrow_if_present, default = 0),
        people = contacted_count + uncontacted_count,
        ptg = contacted_count / people
      ) %>%
      select(-contacted_count, -uncontacted_count) %>%
      mutate(contacts = !sapply(contacts, is.null),
             uncontacteds = !sapply(uncontacteds, is.null),
             questions = !sapply(questions, is.null)) %>%
      mutate_at(vars(contacts, uncontacteds, questions),
                funs(factor(., labels = c("Not Uploaded", "OK"),
                            levels = c(FALSE, TRUE))))
  })
  output$campaign_status <- renderTable({
    newFiles()
  })
  merged_data <- reactive({
    newFiles()
    all_files %>%
      filter(type %in% c("contacts", "questions")) %>%
      select(type, data) %>% {
        if (nrow(.)) {
          unnest(., data)
        }
      }
  })
  target_day_data <- reactive({
    req(merged_data()) %>%
      mutate(
        date = sapply(strsplit(date, " ", fixed = FALSE), first),
        # fix dates ruined by opening reports in excel and re-saving
        date = ifelse(grepl("[[:digit:]]{4}[[:space:]]*$", date),
                      format(as.Date(.$date, "%m/%d/%Y"), "%Y-%m-%d"),
                      date)
      ) %>%
      filter(date == as.character(input$day))
  })
  texter_day_totals <- reactive({
    target_day_data() %>%
      group_by(Texter, type) %>%
      summarize(total = n()) %>%
      ungroup () %>%
      {
        if (isTruthy(organizer_lookup())) {
          left_join(., organizer_lookup(), by = c(Texter = "Name")) %>%
            # avoid inflated texting counts if duplicates in texter tracker
            group_by(Texter, type) %>%
            filter(row_number(Texter) == 1) %>%
            ungroup() %>%
            mutate(
              Organizer = ifelse(is.na(Organizer), "Unassigned", Organizer)
            )
        } else {
          mutate(., Organizer = "Not-Connected")
        }
      }
  })
  ldb_react <- reactive({
    texter_day_totals() %>%
      select(Organizer, Texter, type, Total = total) %>%
      mutate(type = factor(type, levels = c("contacts", "questions"))) %>%
      spread(type, Total, drop = FALSE, fill = 0) %>%
      # spread operation creates empty rows for false organizer/texter combos
      filter(contacts > 0 | questions > 0) %>%
      rename(Texts = contacts, `Questions Answered` = questions) %>%
      arrange(desc(Texts)) %>%
      bind_rows(summarize(., Organizer = "", Texter = "Total",
                          Texts = sum(Texts),
                          `Questions Answered` = sum(`Questions Answered`)))
  })
  output$leaderboard <- renderTable({
    ldb_react()
  })
  output$ldb_download <- downloadHandler(
    filename = function() {
      paste("leaderboard-", as.Date(input$day), ".csv", sep="")
    },
    content = function (file) {
      ldb_react() %>%
        mutate_at(vars(Texts, `Questions Answered`),
                  funs(format(., scientific = FALSE))) %>%
        write_csv(file, na = "")
    }
  )
  organizer_lookup <- reactive({
    if (isTruthy(tracker_sheet())) {
      gs_read(tracker_sheet(), master_list_name, cell_cols(1:4)) %>%
        select(Name, Organizer)
    }
  })
  ldb_organizers_react <- reactive({
    texter_day_totals() %>%
      group_by(Organizer, type) %>%
      summarize(total = sum(total)) %>%
      ungroup () %>%
      select(Organizer, type, Total = total) %>%
      mutate(type = factor(type, levels = c("contacts", "questions"))) %>%
      spread(type, Total, drop = FALSE, fill = 0) %>%
      rename(Texts = contacts, `Questions Answered` = questions) %>%
      arrange(desc(Texts)) %>%
      bind_rows(summarize(., Organizer = "Total",
                          Texts = sum(Texts),
                          `Questions Answered` = sum(`Questions Answered`)))
  })
  output$leaderboard_organizers <- renderTable({
    ldb_organizers_react()
  })
  output$ldb_org_download <- downloadHandler(
    filename = function() {
      paste("organizers-leaderboard-", as.Date(input$day), ".csv", sep="")
    },
    content = function (file) {
      ldb_organizers_react() %>%
        mutate_at(vars(Texts, `Questions Answered`),
                  funs(format(., scientific = FALSE))) %>%
        write_csv(file, na = "")
    }
  )
  output$merged_download <- downloadHandler(
    filename = function() {
      paste("merged-reports-", as.Date(input$day), ".zip", sep="")
    },
    content = function (file) {
      tmpdir <- tempdir()
      reports <- character(0)
      all_files %>%
        group_by(type) %>%
        summarize(data = list(bind_rows(data))) %>% {
          mapply(function (type, data) {
            loc <- file.path(
              tmpdir,
              paste0("merged-", type, "-", as.Date(input$day), ".csv")
            )
            reports <<- c(reports, loc)
            write_csv(data, loc, na = "")
          }, type = .$type, data = .$data)
        }
      zip(file, reports, flags = "-mj9X")
    }
  )
  tracker_sheet <- reactive({
    if (is.null(isolate(access_token()))) { return() }
    gs_key(gs_tracker_id)
  })
  output$loginButton <- renderUI({
    if (!is.null(isolate(access_token()))) {
      tags$a(
        icon("check"),
        "Texter Tracker is Connected",
        disabled = TRUE,
        class = "btn btn-default"
      )
    } else {
      tags$a(
        icon("plug"),
        "Connect to Texter Tracker",
        href = gs_webapp_auth_url(client_id = gs_secrets$client_id,
                                  redirect_uri = gs_secrets$redirect_uri),
        class = "btn btn-default"
      )
    }
  })
  ## Get auth code from return URL
  access_token <- reactive({
    ## gets all the parameters in the URL. The auth code should be one of them.
    pars <- parseQueryString(session$clientData$url_search)
    
    if (length(pars$code) > 0) {
      ## extract the authorization code
      gs_webapp_get_token(auth_code = pars$code,
                          client_id = gs_secrets$client_id,
                          client_secret = gs_secrets$client_secret,
                          redirect_uri = gs_secrets$redirect_uri)
    } else {
      NULL
    }
  })
  output$date_debug <- renderText({
    paste("DEBUG: Selected date:", as.character(input$day),
          "Report dates:",
          merged_data() %>%
            mutate(date = substring(date, 1, 10)) %>%
            use_series("date") %>%
            unique() %>%
            paste(collapse = ", ")
    )
  })
})
