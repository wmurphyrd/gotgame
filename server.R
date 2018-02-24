
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(magrittr)
library(tidyverse)

shinyServer(function(input, output) {
  all_files <- tibble(name = character(0))
  files_table <- tibble(campaign = character(), contacts = list(),
                        uncontacteds = list(), questions = list())
  newFiles <- reactive({
    req(input$uploads)
    new_uploads <- input$uploads %>%
      filter(!name %in% all_files$name)
    if (nrow(new_uploads) > 0) {
      new_uploads %>%
        mutate(
          data = lapply(datapath, read_csv,
                        col_types = cols(.default = col_character())),
          data = lapply(data, function (d) {
            names(d)[names(d) %in% c("Contacted On", "Marked On")] <- "date"
            names(d)[names(d) %in% c("Contacted By", "Marked By")] <- "Texter"
            # d$`myVotersId` <- as.character(d$`myVotersId`)
            d
          }),
          type = ifelse(grepl("contact-history", name),
                        "contacts",
                        ifelse(grepl("question-responses", name),
                               "questions",
                               ifelse(grepl("uncontacted-targets", name),
                                      "uncontacteds",
                                      NA)
                               )
                        ),
          
          campaign = sapply(data, function (d) { d$`Campaign Name`[1] })
        ) %>%
        filter(!is.na(type)) %>%
        bind_rows(all_files) ->>
        all_files
    }
    all_files %>%
      select(campaign, type, data) %>%
      # ensure all three types make it into the set are available
      mutate(
        type = factor(type, levels = c("contacts", "uncontacteds", "questions"))
      ) %>%
      spread(type, data, drop = FALSE) %>%
      mutate(contacts = !sapply(contacts, is.null),
             uncontacteds = !sapply(uncontacteds, is.null),
             questions = !sapply(questions, is.null)) %>%
      mutate_at(vars(contacts, uncontacteds, questions),
                funs(factor(., labels = c("Not Uploaded", "OK"),
                            levels = c(FALSE, TRUE))))
  })
  output$fileStatus <- renderTable({
    newFiles()
  })
  ldb_react <- reactive({
    newFiles()
    all_files %>%
      filter(type %in% c("contacts", "questions")) %>%
      select(type, data) %>%
      unnest(data) %>%
      mutate(date = substring(date, 1, 10)) %>%
      filter(date == as.character(input$day)) %>%
      group_by(Texter, type) %>%
      summarize(total = n()) %>%
      ungroup () %>%
      select(Texter, type, Total = total) %>%
      mutate(type = factor(type, levels = c("contacts", "questions"))) %>%
      spread(type, Total, drop = FALSE, fill = 0) %>%
      rename(Texts = contacts, `Questions Answered` = questions) %>%
      arrange(desc(Texts)) %>%
      bind_rows(summarize(., Texter = "Total", Texts = sum(Texts),
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

})
