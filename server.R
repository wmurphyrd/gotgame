
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

gs_tracker_id <- "1rJY91THJIVZ0j6WM0vL0rZsTyCK8hvMg7w8JK2jveK4"
master_list_name <- "MASTER 2018 Texters"
nrow_if_present <- function(x) { ifelse(is.null(x), NA, nrow(x)) }

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
      mutate(contacted_count = sapply(contacts, nrow_if_present),
             uncontacted_count = sapply(uncontacteds, nrow_if_present),
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
      mutate(date = substring(date, 1, 10)) %>%
      filter(date == as.character(input$day))
  })
  ldb_react <- reactive({
    target_day_data() %>% {
      if (isTruthy(organizer_lookup())) {
        left_join(., organizer_lookup(),
                  by = c(Texter = "Name")) %>%
          mutate(Organizer = ifelse(is.na(Organizer), "Unassigned", Organizer))
      } else {
        mutate(., Organizer = "Not-Connected")
      }
    } %>%
      group_by(Organizer, Texter, type) %>%
      summarize(total = n()) %>%
      ungroup () %>%
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
      write_csv(ldb_react(), file, na = "")
    }
  )
  organizer_lookup <- reactive({
    if (isTruthy(tracker_sheet())) {
      gs_read(tracker_sheet(), master_list_name, cell_cols(1:4)) %>%
        select(Name, Organizer)
    }
  })
  ldb_organizers_react <- reactive({
    req(organizer_lookup()) %>%
      select(Name, Organizer) %>%
      left_join(target_day_data(), ., by = c(Texter = "Name")) %>%
      mutate(Organizer = ifelse(is.na(Organizer), "Unassigned", Organizer)) %>%
      group_by(Organizer, type) %>%
      summarize(total = n()) %>%
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
      write_csv(ldb_organizers_react(), file, na = "")
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
        icon("plug"),
        "Texter Tracker is Connected",
        disabled = TRUE,
        class = "btn btn-default"
      )
    } else {
      tags$a(
        icon("plug"),
        "Connect to Texter Tracker",
        href = gs_webapp_auth_url(),
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
      gs_webapp_get_token(auth_code = pars$code)
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
