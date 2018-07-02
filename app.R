#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(purrr)
library(reshape)

all_data <- readRDS("Data/2017_UN_votes.rds")
country_names <- unique(all_data$country_name)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "UN Votes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
      menuItem("States comparison", tabName = "states_comparison", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # 1st tab summary
      tabItem(tabName = "summary",
              fluidRow(
                box(
                  # output table
                  DT::dataTableOutput(outputId = "summaryTable")
                )
              ),
              fluidRow(
                box(
                  plotOutput(outputId = "resPerYear")
                ),
                box(
                  plotOutput(outputId = "votePercent")
                ),
                box(
                  plotOutput(outputId = "typePercent")
                ),
                box(
                  plotOutput(outputId = "unanimous")
                ),
                box(
                  selectInput(inputId = "resType", label = "Resolution Type", choices = c("me", "nu", "di", "hr", "co", "ec"), selected = "me"),
                  plotOutput(outputId = "unanimousPerType")
                )
              )
      ),
      # 2nd tab states comparison
      tabItem(tabName = "states_comparison",
              fluidRow(
                box(selectInput(inputId = "country1", label = "country1", choices = country_names, selected = country_names[1]),    
                    selectInput(inputId = "country2", label = "country2", choices = country_names, selected = country_names[2]), width = 12),
                  box(
                    plotOutput(outputId = "stateVotes")
                  ),
                  box(
                    selectInput(inputId = "issueCode", label = "Issue Code", choices = c("me", "nu", "di", "hr", "co", "ec"), selected = "me"),
                    plotOutput(outputId = "YesByYearsAndIssueCode")
                  ),
                  box(
                    plotOutput(outputId = "voteAgreement"), width = 12
                  )
              )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  getResType <- function(me, nu, di, hr, co ,ec){
    string <- " "
    if(me)
      string <- paste(string, "Palestinian", sep = " ")
    if(nu)
      string <- paste(string, "nuclear", sep = " ")
    if(di)
      string <- paste(string, "disarmament", sep = " ")
    if(hr)
      string <- paste(string, "human_rights", sep = " ")
    if(co)
      string <- paste(string, "colonialism", sep = " ")
    if(ec)
      string <- paste(string, "economic_development", sep = " ")
    return(string)
  }
  
  # table for each resolution (rcid)
  unique_res <- sort(unique(all_data$rcid))
  nr_res <- length(unique_res)
  DT_frame <- data.frame(rcid = numeric(nr_res), shortDescription = character(nr_res), year = numeric(nr_res), nr_votes = numeric(nr_res),
                         propYes = numeric(nr_res), resType = character(nr_res), stringsAsFactors = FALSE)
  #                  rcid,  code,  name, vote,  date, year, unres,short,   me,   nu,   di,   hr,   co,   ec
  columnsToKeep <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  res_data <- all_data[, columnsToKeep]
  res_data <- split(res_data, res_data$rcid)
  for (i in 1:length(res_data)) {
    DT_frame$rcid[i] <- res_data[[i]]$rcid[1]
    DT_frame$shortDescription[i] <- as.character(res_data[[i]]$short[1])
    DT_frame$year[i] <- res_data[[i]]$year[1]
    DT_frame$nr_votes[i] <- length(res_data[[i]]$vote)
    DT_frame$propYes[i] <- (res_data[[i]]$vote[res_data[[i]]$vote == "yes"] %>% length()) / DT_frame$nr_votes[i]
    DT_frame$resType[i] <- getResType(res_data[[i]]$me[1], res_data[[i]]$nu[1], res_data[[i]]$di[1], 
                                      res_data[[i]]$hr[1], res_data[[i]]$co[1], res_data[[i]]$ec[1])
  }
  output$summaryTable <- DT::renderDT(DT_frame)
  
  # plot for number of resolutions up for voting per year 1946 - 2017
  columnsToKeep <- c(TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
  year_data <- all_data[, columnsToKeep]
  year_data <- split(year_data, year_data$year)
  #View(year_data)
  print(length(year_data))
  nr_years <- length(year_data)
  df_plot <-  data.frame(year = numeric(nr_years), nr_res = numeric(nr_years))
  for (i in 1:nr_years) {
    df_plot$year[i] <- year_data[[i]]$year[1]
    df_plot$nr_res[i] <- length(unique(year_data[[i]]$rcid))
  }
  names(df_plot) <- c("Year","Number_of_resolutions")
  #print(df_plot)
  
  output$resPerYear <- renderPlot(
    ggplot(data = df_plot, mapping = aes(x = Year, y = Number_of_resolutions)) + geom_line() + geom_point() + ggtitle("Number of resolutions up for voting per year") +  theme(plot.title = element_text(hjust = 0.5))
  )
  
  # plot for average percentage of yes, no, abstain votes per year
  df_percentage <- data.frame(year = numeric(nr_years), nr_yes = numeric(nr_years), nr_no = numeric(nr_years), nr_abstain = numeric(nr_years))
  for (i in 1:nr_years) {
    votes <- table(year_data[[i]]$vote)
    total <- length(year_data[[i]]$vote)
    df_percentage$year[i] <- year_data[[i]]$year[1]
    df_percentage$nr_yes[i] <- votes[["yes"]] / total
    df_percentage$nr_no[i] <- votes[["no"]] / total
    df_percentage$nr_abstain[i] <- votes[["abstain"]] / total
  }
  df_percentage <- melt(df_percentage, id.vars = "year")
  
  output$votePercent <- renderPlot(
    ggplot(data = df_percentage, mapping = aes(x = year, y = value, fill = variable))
    + geom_bar(position = "fill",stat = "identity") +
      ggtitle("Average percentage of \"yes\", \"no\" and \"abstain\" votes per year") +  theme(plot.title = element_text(hjust = 0.5))
  )
  
  # plot for resolution types per year
  df_resType <- data.frame(year = numeric(nr_years), 
                           me = double(nr_years),
                           nu = double(nr_years), 
                           di = double(nr_years), 
                           hr = double(nr_years), 
                           co = double(nr_years), 
                           ec = double(nr_years))
  for (i in 1:nr_years) {
    df_resType$year[i] <- year_data[[i]]$year[1]
    df_resType$me[i] <- sum(year_data[[i]]$me)
    df_resType$nu[i] <- sum(year_data[[i]]$nu)
    df_resType$di[i] <- sum(year_data[[i]]$di)
    df_resType$hr[i] <- sum(year_data[[i]]$hr)
    df_resType$co[i] <- sum(year_data[[i]]$co)
    df_resType$ec[i] <- sum(year_data[[i]]$ec)
  }
  df_resType <- melt(df_resType, id.vars = "year")
  #print(df_resType)
  
  #TODO rename
  
  output$typePercent <- renderPlot(
    ggplot(data = df_resType, mapping = aes(x = year, y = value, fill = variable))
    + geom_bar(position = "fill",stat = "identity") +
      ggtitle("Proportion of resolution types per year") +  theme(plot.title = element_text(hjust = 0.5))
  )
  
  # plot for proportion of unanimous (2/3 yes) and non-unanimous votings per year
  df_unanimous <- data.frame(year = numeric(nr_years), 
                             unanimous = numeric(nr_years),
                             non_unanimous = numeric(nr_years)
  )
  for (i in 1:nr_years) {
    df_unanimous$year[i] <- year_data[[i]]$year[1]
    df_unanimous$unanimous[i] <- 0
    df_unanimous$non_unanimous[i] <- 0
    rcids <- unique(year_data[[i]]$rcid)
    for (j in rcids) {
      voting <- year_data[[i]][year_data[[i]]$rcid == j, ]
      votes <- table(voting$vote)
      yes_votes <- votes[["yes"]]
      no_votes <- votes[["no"]]
      if ((yes_votes / (yes_votes + no_votes)) > (0.667)) {
        df_unanimous$unanimous[i] <- df_unanimous$unanimous[i] + 1
      }else{
        df_unanimous$non_unanimous[i] <- df_unanimous$non_unanimous[i] + 1
      }
    }
  }
  df_unanimous <- melt(df_unanimous, id.vars = "year")
  
  output$unanimous <- renderPlot(
    ggplot(data = df_unanimous, mapping = aes(x = year, y = value, fill = variable))
    + geom_bar(position = "fill",stat = "identity") +
      ggtitle("Proportion of \"unanimous\" votings (at least 2/3 \"Yes\" votes) and non-\"unanimous\" votings per year") +  theme(plot.title = element_text(hjust = 0.5))
  )
  
  # plot for proportion of unanimous (2/3 yes) and non-unanimous votings per year and resolution type
  getDataByType <- reactive({
    df_unanimous_type <- data.frame(year = numeric(nr_years), 
                                    unanimous = numeric(nr_years),
                                    non_unanimous = numeric(nr_years)
    )
    
    for (i in 1:nr_years) {
      filtered_by_type <- NULL
      switch (input$resType,
              "me" = filtered_by_type <- year_data[[i]] %>% filter(me == 1),
              "nu" = filtered_by_type <- year_data[[i]] %>% filter(nu == 1),
              "di" = filtered_by_type <- year_data[[i]] %>% filter(di == 1),
              "hr" = filtered_by_type <- year_data[[i]] %>% filter(hr == 1),
              "co" = filtered_by_type <- year_data[[i]] %>% filter(co == 1),
              "ec" = filtered_by_type <- year_data[[i]] %>% filter(ec == 1)
      )
      df_unanimous_type$year[i] <- year_data[[i]]$year[1]
      df_unanimous_type$unanimous[i] <- 0
      df_unanimous_type$non_unanimous[i] <- 0
      if (length(filtered_by_type[[1]]) == 0) {
        next
      }
      rcids <- unique(filtered_by_type$rcid)
      for (j in rcids) {
        voting <- filtered_by_type[filtered_by_type$rcid == j, ]
        votes <- table(voting$vote)
        yes_votes <- votes[["yes"]]
        no_votes <- votes[["no"]]
        if ((yes_votes / (yes_votes + no_votes)) > (0.667)) {
          df_unanimous_type$unanimous[i] <- df_unanimous_type$unanimous[i] + 1
        }else{
          df_unanimous_type$non_unanimous[i] <- df_unanimous_type$non_unanimous[i] + 1
        }
      }
    }
    df_unanimous_type <- melt(df_unanimous_type, id.vars = "year")
  })
  
  output$unanimousPerType <- renderPlot(
    ggplot(data = getDataByType(), mapping = aes(x = year, y = value, fill = variable))
    + geom_bar(position = "fill",stat = "identity") +
      ggtitle("Proportion of \"unanimous\" votings (at least 2/3 \"Yes\" votes) and non-\"unanimous\" votings per year and resolution type") +  theme(plot.title = element_text(hjust = 0.5))
  )
  
  ### State comparison ###
  
  getYearDataForStates <- reactive({
    year_data_states <- all_data %>% filter(country_name == input$country1 | country_name == input$country2)
    #                  rcid,  code, name, vote,  date, year, unres, short,   me,   nu,   di,   hr,   co,   ec
    columnsToKeep <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
    year_data_states <- year_data_states[, columnsToKeep]
    year_data_states <- split(year_data_states, year_data_states$year)
    return(year_data_states)
  })
  
  getStateVotes <- reactive({
    year_data_states <- getYearDataForStates()
    voteState <- NULL
    for (i in 1:nr_years) {
      voteState$year[i] <- year_data_states[[i]]$year[1]
      country_vote <- year_data_states[[i]] %>% filter(country_name == input$country1) %>% select(vote)
      voteState$country1[i] <- country_vote %>% filter(vote == "yes") %>% unlist() %>% length() / country_vote %>% unlist() %>% length()
      
      country_vote <- year_data_states[[i]] %>% filter(country_name == input$country2) %>% select(vote)
      voteState$country2[i] <- country_vote %>% filter(vote == "yes") %>% unlist() %>% length() / country_vote %>% unlist() %>% length()
    } 
    names(voteState) <- c("year", input$country1, input$country2)
    voteState <-  melt(data.frame(voteState), id.vars = "year")
  })
  
  # plot for percentage of yes votes
  output$stateVotes <- renderPlot(
    ggplot(data = getStateVotes(), mapping = aes(x = year, y = value, fill = variable))
    + geom_bar(position = "dodge",stat = "identity") +
      ggtitle("Percentage of \"Yes\" votes by years for the selected states") +  theme(plot.title = element_text(hjust = 0.5))
  )
  
  # plot for percentage of yes votes by issue code
  getStateVotesByIssueCode <- reactive({
    
    year_data_states <- getYearDataForStates()
    voteState <- NULL
    for (i in 1:nr_years) {
      
      voteState$year[i] <- year_data_states[[i]]$year[1]
      
      switch (input$issueCode,
              "me" = year_data_states[[i]] <- year_data_states[[i]] %>% filter(me == 1),
              "nu" = year_data_states[[i]] <- year_data_states[[i]] %>% filter(nu == 1),
              "di" = year_data_states[[i]] <- year_data_states[[i]] %>% filter(di == 1),
              "hr" = year_data_states[[i]] <- year_data_states[[i]] %>% filter(hr == 1),
              "co" = year_data_states[[i]] <- year_data_states[[i]] %>% filter(co == 1),
              "ec" = year_data_states[[i]] <- year_data_states[[i]] %>% filter(ec == 1)
      )
      
      
      country_vote <- year_data_states[[i]] %>% filter(country_name == input$country1) %>% select(vote)
      voteState$country1[i] <- country_vote %>% filter(vote == "yes") %>% unlist() %>% length() / country_vote %>% unlist() %>% length()
      
      if(is.nan(voteState$country1[i])) #happens if no resolutions for an issue code exists in that year
      {
        voteState$country1[i] <- 0
      }
      
      country_vote <- year_data_states[[i]] %>% filter(country_name == input$country2) %>% select(vote)
      voteState$country2[i] <- country_vote %>% filter(vote == "yes") %>% unlist() %>% length() / country_vote %>% unlist() %>% length()
    
      if(is.nan(voteState$country2[i])) #happens if no resolutions for an issue code exists in that year
      {
        voteState$country2[i] <- 0
      }  
    } 
    
    names(voteState) <- c("year", input$country1, input$country2)
    voteState <-  melt(data.frame(voteState), id.vars = "year")
  })
  
  output$YesByYearsAndIssueCode <- renderPlot(
    ggplot(data = getStateVotesByIssueCode(), mapping = aes(x = year, y = value, fill = variable))
    + geom_bar(position = "dodge",stat = "identity") +
      ggtitle("Percentage of \"Yes\" votes by years and by issue code for the selected states") +  theme(plot.title = element_text(hjust = 0.5))
    )
  
  # plot for vote agreement between the selected states per year
  getVoteAgreement <- reactive({
    #this is how I calculate the vote agreement:
    # 1. compute percentage of yes, no and abstain votes for both countries seperately
    # 2. the sum of the minima of all three is the vote agreement
    
    year_data_states <- getYearDataForStates()
    voteState <- NULL
    for (i in 1:nr_years) {
      
      voteState$year[i] <- year_data_states[[i]]$year[1]
      
      country_vote <- year_data_states[[i]] %>% filter(country_name == input$country1) %>% select(vote)
      voteState$country1Yes[i] <- country_vote %>% filter(vote == "yes") %>% unlist() %>% length() / country_vote %>% unlist() %>% length()
      voteState$country1No[i] <- country_vote %>% filter(vote == "no") %>% unlist() %>% length() / country_vote %>% unlist() %>% length()
      voteState$country1Abstain[i] <- country_vote %>% filter(vote == "abstain") %>% unlist() %>% length() / country_vote %>% unlist() %>% length()
      
      country_vote <- year_data_states[[i]] %>% filter(country_name == input$country2) %>% select(vote)
      voteState$country2Yes[i] <- country_vote %>% filter(vote == "yes") %>% unlist() %>% length() / country_vote %>% unlist() %>% length()
      voteState$country2No[i] <- country_vote %>% filter(vote == "no") %>% unlist() %>% length() / country_vote %>% unlist() %>% length()
      voteState$country2Abstain[i] <- country_vote %>% filter(vote == "abstain") %>% unlist() %>% length() / country_vote %>% unlist() %>% length()
      
      minYes <- min(voteState$country1Yes[i], voteState$country2Yes[i])
      minNo <- min(voteState$country1No[i], voteState$country2No[i])
      minAbstain <- min(voteState$country1Abstain[i], voteState$country2Abstain[i])
      
      voteState$value[i] <- minYes + minNo + minAbstain
    } 
    voteState <- data.frame(year = voteState$year,
                            value = voteState$value*100)
  })
  
  output$voteAgreement <- renderPlot(
    ggplot(data = getVoteAgreement(), mapping = aes(x = year, y = value))
    + geom_bar(position = "dodge",stat = "identity") +
      geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, color = "blue") +
      ggtitle("Average Vote agreement for the selected states") +  theme(plot.title = element_text(hjust = 0.5))
      + geom_line(data = df_plot, aes(x = Year, y = Number_of_resolutions),color = "red", size=1.1)
      
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

