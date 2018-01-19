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

county_long <- read_csv("Data/county_long.csv")
cg_long <- read_csv("Data/cg_long.csv")
ks_long <- read_csv("Data/ks_long.csv")
kr_long <- read_csv("Data/kr_long.csv")

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
  #            href = "http://127.0.0.1:5000/static/styles.css")
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
                             selected = "President / Vice President"),
                 p("Select the type of election (general or primary), year, and 
                   race by using the drop-down menus above. The figure 
                   shows the proportion of votes that the candidate received; 
                   the size of the rectangle refers to the size of the 
                   candidate's share of the vote. The table lists votes—and vote
                   proportions—for the figure. For primaries, all proportions
                   are within-party."),
                 em("R = Republican, D = Democrat,"),
                 br(),
                 em("L = Libertarian, O = Other")
                 ),
               mainPanel(
                 plotOutput("results_plot"),
                 dataTableOutput("results_table"),
                 em("Source: Kansas Secretary of State 
                   Office Election Statistics")
                 )
               )),
    
    navbarMenu("Registration Trends",
      tabPanel("County",
        sidebarLayout(
          sidebarPanel(
            selectInput("trends_county",
                        label = h4("County"),
                        choices = sort(unique(county_long$county)))
          ),
          mainPanel(
            plotOutput("registration_county")
          )
        )
      ),
      tabPanel("Congressional",
        sidebarLayout(
          sidebarPanel(
            selectInput("trends_cg_district",
                        label = h4("District"),
                        choices = sort(unique(cg_long$district_cg)))
          ),
          mainPanel(
            plotOutput("registration_cg")
          )
        )
      ),
      tabPanel("State Senate",
        sidebarLayout(
          sidebarPanel(
            selectInput("trends_ks_district",
                        label = h4("District"),
                        choices = sort(unique(ks_long$district_ks)))
          ),
          mainPanel(
            plotOutput("registration_ks")
          )
        )
      ),
      tabPanel("State House",
        sidebarLayout(
          sidebarPanel(
            selectInput("trends_kr_district",
                        label = h4("District"),
                        choices = sort(unique(kr_long$district_kr)))
          ),
          mainPanel(
            plotOutput("registration_kr")
          )
        )
      )
    ),
    
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
                             choices = turnout_variable_choices,
                             selected = "prop_turnout"),
                 p("Select the year, county, and variable of interest by using
                   the drop-down menus above. The table shows all the turnout
                   variables—not just the one you select."),
                 br(),
                 p("The figure is a density plot; it
                   shows us how common is each value of the variable of 
                   interest. The higher the curve, and thus more blue area under
                   the curve, the more common the value is. The dotted vertical
                   line is the county that you select above. This helps you see
                   how that county compares to other counties. If the vertical
                   line is in the tall area under the curve, it is similar to
                   other counties. If it is far to the right, then it has a much
                   higher value than most other counties.")
                 ),
               mainPanel(
                 dataTableOutput("turnout_table"),
                 em("Source: Kansas Secretary of State 
                   Office Election Statistics"),
                 plotOutput("density_turnout")
                 )
               )),
    
    tabPanel("Election History",
             mainPanel(
               dataTableOutput("candidate_history"),
               em("Source: Kansas Secretary of State 
                   Office Election Statistics"),
               width = 12
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
                            ),
                            p("Select the demographic variable that you are
                              interested in, as well as whether you would like
                              to see proportions or totals. For a look into
                              what the variables mean, see the Variable
                              Definitions tab under Demographics.")
                          ),
                          mainPanel(
                            dataTableOutput("state_demographics"),
                            em("Source: American Community Survey,
                               1-Year Estimates")
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
                            ),
                            p("Select the demographic variable that you are
                              interested in, as well as whether you would like
                              to see proportions or totals. For a look into
                              what the variables mean, see the Variable
                              Definitions tab under Demographics.")
                          ),
                          mainPanel(
                            dataTableOutput("county_demographics"),
                            em("Source: American Community Survey,
                               2011-2015 5-Year Estimates")
                          ))
                        ),
               tabPanel("Congressional",
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
                            ),
                            p("Select the demographic variable that you are
                              interested in, as well as whether you would like
                              to see proportions or totals. For a look into
                              what the variables mean, see the Variable
                              Definitions tab under Demographics.")
                          ),
                          mainPanel(
                            dataTableOutput("congress_demographics"),
                            em("Source: American Community Survey,
                               2011-2015 5-Year Estimates")
                          ))
                        ),
               tabPanel("State Senate",
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
                            ),
                            p("Select the demographic variable that you are
                              interested in, as well as whether you would like
                              to see proportions or totals. For a look into
                              what the variables mean, see the Variable
                              Definitions tab under Demographics.")
                          ),
                          mainPanel(
                            dataTableOutput("sldu_demographics"),
                            em("Source: American Community Survey,
                               2011-2015 5-Year Estimates")
                          ))
                        ),
               tabPanel("State House",
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
                            ),
                            p("Select the demographic variable that you are
                              interested in, as well as whether you would like
                              to see proportions or totals. For a look into
                              what the variables mean, see the Variable
                              Definitions tab under Demographics.")
                          ),
                          mainPanel(
                            dataTableOutput("sldl_demographics"),
                            em("Source: American Community Survey,
                               2011-2015 5-Year Estimates")
                          ))
                        ),
               tabPanel("Variable Definitions",
                        em("All variables are at the individual level (i.e., 
                           total numbers are refer to people), except where 
                           otherwise noted. Data come from the American 
                           Community Survey (ACS). Below are the definitions of 
                           the variables as well as the survey table name for 
                           each variable."),
                        br(),
                        br(),
                        em("If you would like to see more detailed numbers,
                           finer-grained cross-tabulations, or have any
                           questions, you can email us at this@email.com or 
                           visit factfinder.census.gov to explore the data,
                           based on the table names given below."),
                        h3("Cash Assistance/SNAP"),
                        p("Number of households (i.e., total numbers refer to 
                          households, not people) with cash public assistance or 
                          food stamps/SNAP in the past year. Table B19058."),
                        h3("Citizen"),
                        p("Citizenship and nativity. \"Not\" refers to not being
                          a citizen. If people are a citizen, nativity is broken
                          into: born in the United States, born in Peurto Rico 
                          or U.S. island Areas, born abroad of American 
                          parent(s), and citizen by naturalization. Table 
                          B05001."),
                        h3("Disability"),
                        p("Population with a disability. The ACS defines
                          disability with three questions that all aim to
                          identify limitations in basic reas of functioning and
                          independent living. It includes any physical, mental,
                          or emotional condition that makes it difficult to
                          concentrate, remember, make decisions, see, hear,
                          climb stairs, dress, bathe, or run errands. Table
                          B18101."),
                        h3("Education"),
                        p("Population at each education level. Table B15001."),
                        h3("Voting Age Population"),
                        p("Population that is 18 years of age or older. Gender
                          comes from table B01001, race from B01001B through 
                          B01001I, and citizenship comes from table B05003."),
                        h3("Health Insurance"),
                        p("Population with or without health insurance. Table
                          B27001."),
                        h3("Language at Home"),
                        p("Population of people 5 years or older who speak a
                          language—other than English—at homesometimes or often.
                          If people speak multiple at home, they were asked to
                          write down the language they speak most often. 
                          Table B16007."),
                        h3("Poverty"),
                        p("Population living below the poverty level (i.e.,
                          Below 100%), at the poverty level to 1.49 times the
                          poverty level (i.e., 100% to 149%), and at or above 
                          1.5 times the poverty level (i.e., 150%). Table 
                          B06012.")
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
                             aes(x = candidate, y = proportion, 
                                 fill = candidate)) +
        geom_bar(width = 1, stat = "identity") +
        theme_light() +
        theme(text = element_text(size = 18),
              panel.grid.major = element_blank()) +
        labs(y = "Proportion", x = NULL,
             title = input$results_race, 
             subtitle = paste(input$results_year, 
                              input$results_election,
                              "Election")) +
        scale_fill_discrete(name = "Candidate") +
        facet_wrap( ~ party, scales = "free")
    } else {
      results_plot <- ggplot(results_dat, 
                             aes(x = candidate, y = proportion, 
                                 fill = candidate)) +
        geom_bar(width = 1, stat = "identity") +
        theme_light() +
        theme(text = element_text(size = 18),
              panel.grid.major = element_blank()) +
        labs(y = "Proportion", x = NULL,
             title = input$results_race, 
             subtitle = paste(input$results_year, 
                           input$results_election,
                           "Election")) +
        scale_fill_discrete(name = "Candidate") +
        coord_flip()
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
        buttons = c("copy", "csv", "excel", "pdf"), dom = "Bfrtip",
        bInfo = FALSE
      ),
      rownames = NULL, extensions="Buttons", 
      colnames = c("Party", "Candidate", "Votes", "Proportion"))
    )
  
  ## REGISTRATION TRENDS
  # county
  output$registration_county <- renderPlot(
    ggplot(county_long[county_long$county == input$trends_county, ], 
         aes(x = date, y = count, color = desc_party)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = c(Democratic = "blue", Republican = "red", 
                                  Libertarian = "green", Unaffiliated = "grey"),
                       name = NULL) +
    labs(x = "Date", y = "Number of Registered Voters") +
    theme_minimal() +
    theme(legend.position = "top", text = element_text(size = 20))
  )
  
  # congressional
  output$registration_cg <- renderPlot(
    ggplot(cg_long[cg_long$district_cg == input$trends_cg_district, ], 
           aes(x = date, y = count, color = desc_party)) +
      geom_point() +
      geom_line() +
      scale_color_manual(values = c(Democratic = "blue", Republican = "red", 
                                    Libertarian = "green", Unaffiliated = "grey"),
                         name = NULL) +
      labs(x = "Date", y = "Number of Registered Voters") +
      theme_minimal() +
      theme(legend.position = "top", text = element_text(size = 20))
  )
  
  # kansas senate
  output$registration_ks <- renderPlot(
    ggplot(ks_long[ks_long$district_ks == input$trends_ks_district, ], 
           aes(x = date, y = count, color = desc_party)) +
      geom_point() +
      geom_line() +
      scale_color_manual(values = c(Democratic = "blue", Republican = "red", 
                                    Libertarian = "green", Unaffiliated = "grey"),
                         name = NULL) +
      labs(x = "Date", y = "Number of Registered Voters") +
      theme_minimal() +
      theme(legend.position = "top", text = element_text(size = 20))
  )
  
  # kansas house
  output$registration_kr <- renderPlot(
    ggplot(kr_long[kr_long$district_kr == input$trends_kr_district, ], 
           aes(x = date, y = count, color = desc_party)) +
      geom_point() +
      geom_line() +
      scale_color_manual(values = c(Democratic = "blue", Republican = "red", 
                                    Libertarian = "green", Unaffiliated = "grey"),
                         name = NULL) +
      labs(x = "Date", y = "Number of Registered Voters") +
      theme_minimal() +
      theme(legend.position = "top", text = element_text(size = 20))
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
    options = list(paging = FALSE, searching = FALSE, bInfo = FALSE,
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
  
  ## ELECTION HISTORY
  # election history table
  output$candidate_history <- renderDataTable(
    datatable(
      arrange(results[, c(3, 2, 7, 1, 6, 4, 5)], candidate), rownames = NULL,
      colnames = c("Candidate", "Party", "Year", 
                 "Race", "Election", "Votes", "Proportion"),
      options = list(
        buttons = c("copy", "csv", "excel", "pdf"), dom = "Bfrtip",
        pageLength = 2000, scrollY = "500px", paging = FALSE, bInfo = FALSE
      ),
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
      dom = "Bfrtip", scrollX = TRUE, paging = FALSE, 
      bInfo = FALSE, searching = FALSE
    ),
    extensions = "Buttons", rownames = FALSE)
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
      buttons = c("copy", "csv", "excel", "pdf"), paging = FALSE, 
      dom = "Bfrtip", scrollX = TRUE, bInfo = FALSE, scrollY = "450px"),
    extensions = "Buttons", rownames = FALSE)
  )
  
  ## DEMOGRAPHICS, CONGRESSIONAL
  output$congress_demographics <- renderDataTable(
    datatable({
      if (input$congress_type == "Proportions") {
        congress_proportions %>% 
          filter(Variable == input$congress_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          mutate(congressional = 
                   as.numeric(gsub("[^0-9]", "", congressional))) %>% 
          set_names(c("Congressional District", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      } else if (input$congress_type == "Totals") {
        congress_totals %>% 
          filter(Variable == input$congress_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          mutate(congressional = 
                   as.numeric(gsub("[^0-9]", "", congressional))) %>% 
          set_names(c("Congressional District", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      }
    }, options = list(
      buttons = c("copy", "csv", "excel", "pdf"), 
      dom = "Bfrtip", scrollX = TRUE, paging = FALSE, 
      bInfo = FALSE, searching = FALSE
    ),
    extensions = "Buttons", rownames = FALSE)
  )
  
  ## DEMOGRAPHICS, SLDU
  output$sldu_demographics <- renderDataTable(
    datatable({
      if (input$sldu_type == "Proportions") {
        sldu_proportions %>% 
          filter(Variable == input$sldu_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          mutate(sldu = sort(as.numeric(gsub("[^0-9]", "", sldu)))) %>% 
          set_names(c("State Senate District", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      } else if (input$sldu_type == "Totals") {
        sldu_totals %>% 
          filter(Variable == input$sldu_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          mutate(sldu = sort(as.numeric(gsub("[^0-9]", "", sldu)))) %>% 
          set_names(c("State Senate District", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      }
    }, options = list(
      buttons = c("copy", "csv", "excel", "pdf"), paging = FALSE,
      dom = "Bfrtip", scrollX = TRUE, bInfo = FALSE, scrollY = "450px"),
    extensions = "Buttons", rownames = FALSE)
  )
  
  ## DEMOGRAPHICS, SLDL
  output$sldl_demographics <- renderDataTable(
    datatable({
      if (input$sldl_type == "Proportions") {
        sldl_proportions %>% 
          filter(Variable == input$sldl_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          mutate(sldl = sort(as.numeric(gsub("[^0-9]", "", sldl)))) %>% 
          set_names(c("State House District", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      } else if (input$sldl_type == "Totals") {
        sldl_totals %>% 
          filter(Variable == input$sldl_variable) %>% 
          spread(col, Value) %>% 
          select(-Variable) %>% 
          mutate(sldl = sort(as.numeric(gsub("[^0-9]", "", sldl)))) %>% 
          set_names(c("State House District", colnames(.)[-1])) %>% 
          `[<-`(TRUE, -1, round(.[, -1], 3))
      }
    }, options = list(
      buttons = c("copy", "csv", "excel", "pdf"), paging = FALSE,
      dom = "Bfrtip", scrollX = TRUE, bInfo = FALSE, scrollY = "450px"),
    extensions = "Buttons", rownames = FALSE)
  )
  
}

shinyApp(ui = ui, server = server)
