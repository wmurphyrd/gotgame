
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Ground Game Leaderboard/Progress"),

  # Sidebar with a slider input for number of bins
  fluidRow(
    column(
      6,
      wellPanel(
        h3("Report Files"),
        uiOutput('loginButton'),
        helpText("Optional: login to Google Sheets to access organizer info.",
                 "Do this first if you're going to use it,",
                 "as it resets the uploads"),
        fileInput("uploads", "Upload Ground Game Reports",
                  multiple = TRUE, accept = "text/csv"),
        helpText("Upload the Contact History, Uncontacteds, and Question",
                 "Response reports for each active campaign"),
        dateInput("day", "Choose Leaderboard Day"),
        textOutput("date_debug"),
        conditionalPanel(
          "output.campaign_status",
          downloadButton("merged_download", label = "Download Merged Reports"),
          helpText("Downloads all uploaded report data as zip archive with a",
                   "single merged spreadsheet for each report type.")
        )
      )
    ),
    column(
      6,
      wellPanel(
        h3("Organizer Leaderboard"),
        conditionalPanel(
          "output.leaderboard_organizers",
          downloadButton("ldb_org_download",
                         label = "Download Organizers Leaderboard")
        ),
        tableOutput("leaderboard_organizers")
      )
    )
  ),
  fluidRow(
    column(
      6,
      wellPanel(
        h3("Campaign Progress"),
        conditionalPanel(
          "output.campaign_status",
          div("TODO: export campaign status to texter tracker",
              class = "btn btn-default", disabled = TRUE)
        ),
        tableOutput("campaign_status")
      )
    ),
    column(
      6,
      wellPanel(
        h3("Texter Leaderboard"),
        conditionalPanel(
          "output.leaderboard",
          downloadButton("ldb_download", label = "Download Leaderboard")
        ),
        tableOutput("leaderboard")
      )
    )
  )
))
