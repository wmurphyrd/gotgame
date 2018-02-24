
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
        h3("Inputs"),
        fileInput("uploads", "Upload Ground Game Reports",
                  multiple = TRUE, accept = "text/csv"),
        helpText("Upload the Contact History, Uncontacteds, and Question Response reports for each active campaign"),
        dateInput("day", "Choose Leaderboard Day"),
        actionButton("gs_connect", "Connect to Texter Tracker", icon("plug")),
        helpText("Optional: login to Google Sheets to access organizer info.")
      )
    ),

    # Show a plot of the generated distribution
    column(
      6,
      wellPanel(
        h3("Report Upload Status"),
        tableOutput("fileStatus"),
        downloadButton("merged_download", label = "Download Merged Reports"),
        helpText("Downloads all uploaded report data as a single spreadsheet.")
      )
    )
  ),
  fluidRow(
    column(
      6,
      wellPanel(
        h3("Texter Leaderboard"),
        downloadButton("ldb_download", label = "Download Leaderboard"),
        tableOutput("leaderboard")
      )
    ),
    column(
      6,
      wellPanel(
        h3("Organizer Leaderboard"),
        downloadButton("ldb_org_download",
                       label = "Download Organizers Leaderboard"),
        tableOutput("leaderboard_organizers")
      )
    )
  )
))
