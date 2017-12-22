library(shiny)
library(tidyverse)
library(purrr)
library(DT)

results <- read_csv("Data/election_results.csv")
turnout <- read_csv("Data/election_turnout.csv")

state_proportions <- read_csv("Data/acs_state_proportions.csv")
state_totals <- read_csv("Data/acs_state_totals.csv")

county_proportions <- read_csv("Data/acs_county_proportions.csv")
county_totals <- read_csv("Data/acs_county_totals.csv")

congress_proportions <- read_csv("Data/acs_congress_proportions.csv")
congress_totals <- read_csv("Data/acs_congress_totals.csv")

sldu_proportions <- read_csv("Data/acs_sldu_proportions.csv")
sldu_totals <- read_csv("Data/acs_sldu_totals.csv")

sldl_proportions <- read_csv("Data/acs_sldl_proportions.csv")
sldl_totals <- read_csv("Data/acs_sldl_totals.csv")

turnout_variable_choices <- c(
  "Registered Voters" = "registered_voters", 
  "Total Votes Cast" = "total_votes_cast",
  "Turnout Proportion" = "prop_turnout", 
  "Advance Votes Cast" = "advance_votes",
  "Proportion of Votes Advance" = "prop_votes_advance",
  "Provisional Votes Cast" = "provisional_votes",
  "Proportion of Votes Provisional" = "prop_votes_provisional"
)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", 
              href = "https://s3.amazonaws.com/kansas-voting-data/styles.css")
              
  ),
  
  #tags$head(
  #  tags$link(rel = "stylesheet", type = "text/css", 
   #           href = "http://127.0.0.1:5000/static/styles.css")
  #  
  #),

  navbarPage(title = "Kansas Voting Data", windowTitle = "Kansas Voting Data", 
             fluid = FALSE,
    tabPanel("Election Results",
             sidebarLayout(
               sidebarPanel(
                 selectInput("results_election", 
                             label = h4("Election"),
                             choices = sort(unique(results$election)),
                             selected = "General"),
                 selectInput("results_year",
                             label = h4("Year"),
                             choices = sort(unique(results$year)),
                             selected = 2016),
                 selectInput("results_race",
                             label = h4("Race"),
                             choices = sort(unique(results$race)),
                             selected = "President / Vice President")
                 ),
               mainPanel(
                 plotOutput("results_plot"),
                 dataTableOutput("results_table")
                 )
               )),
    
    tabPanel("General Election Turnout",
             sidebarLayout(
               sidebarPanel(
                 selectInput("turnout_year",
                             label = h4("Year"),
                             choices = sort(unique(turnout$year)),
                             selected = 2016),
                 selectInput("turnout_county",
                             label = h4("County"),
                             choices = sort(unique(turnout$county))),
                 selectInput("turnout_variable",
                             label = h4("Variable"),
                             choices = turnout_variable_choices)
                 ),
               mainPanel(
                 dataTableOutput("turnout_table"),
                 br(),
                 plotOutput("density_turnout")
                 )
               )),
    
    tabPanel("Candidate History",
             mainPanel(
               dataTableOutput("candidate_history"),
               width=12
               )),
    
    navbarMenu("Demographics",
               tabPanel("State",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "state_variable",
                              label = h4("Variable"),
                              choices = unique(state_proportions$Variable)
                            ),
                            selectInput(
                              "state_type",
                              label = h4("Type"),
                              choices = c("Proportions", "Totals")
                            )
                          ),
                          mainPanel(
                            dataTableOutput("state_demographics")
                          ))
                        ),
               tabPanel("County",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "county_variable",
                              label = h4("Variable"),
                              choices = unique(county_proportions$Variable)
                            ),
                            selectInput(
                              "county_type",
                              label = h4("Type"),
                              choices = c("Proportions", "Totals")
                            )
                          ),
                          mainPanel(
                            dataTableOutput("county_demographics")
                          ))
                        ),
               tabPanel("Congressional District",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "congress_variable",
                              label = h4("Variable"),
                              choices = unique(congress_proportions$Variable)
                            ),
                            selectInput(
                              "congress_type",
                              label = h4("Type"),
                              choices = c("Proportions", "Totals")
                            )
                          ),
                          mainPanel(
                            dataTableOutput("congress_demographics")
                          ))
                        ),
               tabPanel("SLDU",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "sldu_variable",
                              label = h4("Variable"),
                              choices = unique(sldu_proportions$Variable)
                            ),
                            selectInput(
                              "sldu_type",
                              label = h4("Type"),
                              choices = c("Proportions", "Totals")
                            )
                          ),
                          mainPanel(
                            dataTableOutput("sldu_demographics")
                          ))
                        ),
               tabPanel("SLDL",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "sldl_variable",
                              label = h4("Variable"),
                              choices = unique(sldl_proportions$Variable)
                            ),
                            selectInput(
                              "sldl_type",
                              label = h4("Type"),
                              choices = c("Proportions", "Totals")
                            )
                          ),
                          mainPanel(
                            dataTableOutput("sldl_demographics")
                          ))
                        ))
    )
  )

server <- function(input, output) {
  
  ## ELECTION RESULTS
  # election results bar plot
  output$results_plot <- renderPlot({
    results_dat <- results %>% 
      filter(election == input$results_election) %>% 
      filter(year == input$results_year) %>% 
      filter(race == input$results_race) %>% 
      filter(proportion > 0)
    if (nrow(results_dat) == 0) {
      results_plot <- ggplot() + 
        annotate(
          "text", x = .5, y = .5, size = 6, 
          label = "This combination of election/year/race did not happen
          \nOr data not available via Kansas Secretary of State statistics"
        ) +
        theme_void()
    } else if (input$results_election == "Primary") {
      results_plot <- ggplot(results_dat, 
                             aes(x = race, y = proportion, fill = candidate)) +
        geom_bar(width = 1, stat = "identity") +
        theme_minimal() +
        theme(text = element_text(size = 18),
              axis.text.x = element_blank(),
              panel.grid.major = element_blank()) +
        labs(y = "Proportion", x = NULL,
             title = input$results_race, 
             subtitle = paste(input$results_year, 
                              input$results_election,
                              "Election")) +
        scale_fill_discrete(name = "Candidate") +
        facet_wrap( ~ party)
    } else {
      results_plot <- ggplot(results_dat, 
                             aes(x = race, y = proportion, fill = candidate)) +
        geom_bar(width = 1, stat = "identity") +
        theme_minimal() +
        theme(text = element_text(size = 18),
              axis.text.x = element_blank(),
              panel.grid.major = element_blank()) +
        labs(y = "Proportion", x = NULL,
             title = input$results_race, 
             subtitle = paste(input$results_year, 
                           input$results_election,
                           "Election")) +
        scale_fill_discrete(name = "Candidate")
    }
    results_plot
  })
  
  # election results table
  output$results_table <- renderDataTable(
    datatable({
      results %>% 
        filter(election == input$results_election) %>% 
        filter(year == input$results_year) %>% 
        filter(race == input$results_race) %>% 
        filter(proportion > 0) %>% 
        select(party, candidate, votes, proportion) %>% 
        arrange(desc(proportion))
      },
      options = list(
        paging = FALSE, searching = FALSE,
        buttons = c("copy", "csv", "excel", "pdf"), dom = "Bfrtip"
      ),
      rownames = NULL, extensions="Buttons", 
      colnames = c("Party", "Candidate", "Votes", "Proportion"))
    )
  
  ## GENERAL ELECTION TURNOUT
  # turnout table
  output$turnout_table <- renderDataTable(
    datatable({
      turnout %>% 
        filter(year == input$turnout_year) %>% 
        filter(county == input$turnout_county) %>% 
        transmute(
          `Registered Voters` = registered_voters,
          `Total Votes Cast` = total_votes_cast,
          `Turnout Proportion` = prop_turnout,
          `Advance Votes Cast` = prop_votes_advance,
          `Proportion of Votes Advance` = prop_votes_advance,
          `Provisional Votes Cast` = provisional_votes,
          `Proportion of Votes Provisional` = prop_votes_provisional
        ) %>% 
        gather("Variable", "Value")
    },
    options = list(paging = FALSE, searching = FALSE,
                   buttons = c("copy", "csv", "excel", "pdf"), dom = "Bfrtip"),
    rownames = NULL, extensions = "Buttons")
  )
  
  # turnout density plot
  output$density_turnout <- renderPlot({
    if (max(turnout[, input$turnout_variable]) > 1.1) {
      vline <- turnout %>%
        filter(county == input$turnout_county) %>% 
        filter(year == input$turnout_year) %>% 
        select(input$turnout_variable) %>% 
        slice(1) %>% 
        c() %>% 
        as.numeric() / 1000
      density_turnout <- ggplot(
        filter(turnout, year == input$turnout_year),
        aes(x = get(input$turnout_variable) / 1000)) +
        geom_density(alpha = .5, fill = "#0072B2") +
        geom_vline(xintercept = vline, linetype = 2) +
        labs(
          x = paste(names(
            turnout_variable_choices[turnout_variable_choices 
                                     == input$turnout_variable]),
            "(in Thousands)"),
             y = "Density") +
        theme_light() +
        theme(text = element_text(size = 18))
      density_turnout
    } else {
      vline <- turnout %>%
        filter(county == input$turnout_county) %>% 
        filter(year == input$turnout_year) %>% 
        select(input$turnout_variable) %>% 
        slice(1) %>% 
        c() %>% 
        as.numeric()
      density_turnout <- ggplot(
        filter(turnout, year == input$turnout_year),
        aes(x = get(input$turnout_variable))) +
        geom_density(alpha = .5, fill = "#0072B2") +
        geom_vline(xintercept = vline, linetype = 2) +
        labs(x = names(turnout_variable_choices[turnout_variable_choices == 
                                                  input$turnout_variable]),
             y = "Density") +
        theme_light() +
        theme(text = element_text(size = 18))
      density_turnout
    }
  })
  
  ## CANDIDATE HISTORY
  # candidate history table
  output$candidate_history <- renderDataTable(
    datatable(
      arrange(results[, c(3, 2, 7, 1, 6, 4, 5)], candidate), rownames = NULL,
      colnames = c("Candidate", "Party", "Year", 
                 "Race", "Election", "Votes", "Proportion"),
      options = list(
        buttons = c("copy", "csv", "excel", "pdf"), dom = "Bfrtip"),
      extensions = "Buttons"
    )
  )
  
  ## DEMOGRAPHICS, STATE
  output$state_demographics <- renderDataTable(
    datatable({
      if (input$state_type == "Proportions") {
        state_proportions %>% 
          filter(Variable == input$state_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          set_names(c("Year", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      } else if (input$state_type == "Totals") {
        state_totals %>% 
          filter(Variable == input$state_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          set_names(c("Year", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      }
    }, options = list(
      buttons = c("copy", "csv", "excel", "pdf"), 
      dom = "Bfrtip", scrollX = TRUE),
    extensions = "Buttons")
  )
  
  ## DEMOGRAPHICS, COUNTY
  output$county_demographics <- renderDataTable(
    datatable({
      if (input$county_type == "Proportions") {
        county_proportions %>% 
          filter(Variable == input$county_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          set_names(c("County", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      } else if (input$county_type == "Totals") {
        county_totals %>% 
          filter(Variable == input$county_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          set_names(c("County District", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      }
    }, options = list(
      buttons = c("copy", "csv", "excel", "pdf"), 
      dom = "Bfrtip", scrollX = TRUE),
    extensions = "Buttons")
  )
  
  ## DEMOGRAPHICS, CONGRESSIONAL
  output$congress_demographics <- renderDataTable(
    datatable({
      if (input$congress_type == "Proportions") {
        congress_proportions %>% 
          filter(Variable == input$congress_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          set_names(c("Congressional District", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      } else if (input$congress_type == "Totals") {
        congress_totals %>% 
          filter(Variable == input$congress_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          set_names(c("Congressional District", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      }
    }, options = list(
      buttons = c("copy", "csv", "excel", "pdf"), 
      dom = "Bfrtip", scrollX = TRUE),
    extensions = "Buttons")
  )
  
  ## DEMOGRAPHICS, SLDU
  output$sldu_demographics <- renderDataTable(
    datatable({
      if (input$sldu_type == "Proportions") {
        sldu_proportions %>% 
          filter(Variable == input$sldu_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          set_names(c("SLDU", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      } else if (input$sldu_type == "Totals") {
        sldu_totals %>% 
          filter(Variable == input$sldu_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          set_names(c("SLDU", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      }
    }, options = list(
      buttons = c("copy", "csv", "excel", "pdf"), 
      dom = "Bfrtip", scrollX = TRUE),
    extensions = "Buttons")
  )
  
  ## DEMOGRAPHICS, SLDL
  output$sldl_demographics <- renderDataTable(
    datatable({
      if (input$sldl_type == "Proportions") {
        sldl_proportions %>% 
          filter(Variable == input$sldl_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          set_names(c("SLDL", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      } else if (input$sldl_type == "Totals") {
        sldl_totals %>% 
          filter(Variable == input$sldl_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          set_names(c("SLDL", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      }
    }, options = list(
      buttons = c("copy", "csv", "excel", "pdf"), 
      dom = "Bfrtip", scrollX = TRUE),
    extensions = "Buttons")
  )
  
}

shinyApp(ui = ui, server = server)
