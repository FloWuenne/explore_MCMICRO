library(shiny)
library(semantic.dashboard)
#library(shinydashboard)
library(shinyFiles)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview",tabName = "overview", icon = icon("home")),
            menuItem("Sample view",tabName = "sample_view", icon = icon("user"))
            )
        ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "overview",
                    fluidRow(
                        column(width = 4,
                            infoBoxOutput("n_samples_box"),
                        ),
                        column(width = 8,
                            box(plotlyOutput("n_cells_plot"))
                        )
                        ),
                    fluidRow(
                        column(width = 4,
                        ## aggregated parameters (for many cores)       
                        selectizeInput("tec_param","Select which parameter you want to visualize:",
                                       choices = c("mean_Area","mean_Eccentricity","mean_Solidity")),
                        selectizeInput("tec_corr","Select which parameter you want to correlate against:",
                                       choices = c("mean_Area","mean_Eccentricity","mean_Solidity"))
                        ## raw parameters
                        # selectizeInput("tec_param","Select which parameter you want to visualize:",
                        #                choices = c("Area","MajorAxisLength","MinorAxisLength","Eccentricity","Solidity","Extent","Orientation")),
                        # selectizeInput("tec_corr","Select which parameter you want to correlate against:",
                        #                choices = c("Area","MajorAxisLength","MinorAxisLength","Eccentricity","Solidity","Extent","Orientation"))
                        ),
                        column(width = 6,
                        box(plotlyOutput("tec_plot"))
                        ),
                        column(width = 6,
                        box(plotlyOutput("corrplot"))
                        )
                    )
                    ),
            tabItem(tabName = "sample_view",
                    fluidRow(
                        box(uiOutput('sample_selection')),
                        ),
                    fluidRow(
                        box(plotOutput('marker_intensity')
                    )
                    )
                    )
            )
        )
    )
)