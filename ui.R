
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
  sidebarLayout(
    sidebarPanel(
      fileInput("uploads", "Upload Ground Game Reports",
                multiple = TRUE, accept = "text/csv"),
      p("Upload the Contact History, Uncontacteds, and Question Response reports for each active campaign")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("fileStatus"),
      dateInput("day", "Choose Leaderboard Day"),
      downloadButton("ldb_download", label = "Download Leaderboard"),
      tableOutput("leaderboard")
    )
  )
))
