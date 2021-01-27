library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(DT)
library(data.table)
library(dplyr)
library(reticulate)



# 
source("global.R")
# source("helper_functions.R")
source("server.R")

# train <- read.csv("~/Downloads/train_data.csv")
# train <- subset(train, select = c("Job_title", 'Company', 'City' ,'avg_sal', 'Industry', 'Date_Posted', 'Valid_until', 'Job_Type' ))

ui <- dashboardPage(
  dashboardHeader(title = "Job Hunting"),
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem("Exploratory Data Analysis",
             menuSubItem("State Analysis", tabName = "state"),
             menuSubItem("Industry Analysis", tabName = "industry"),
             menuSubItem("Company Analysis", tabName = "company"),
             menuSubItem("Job Title Analysis", tabName = "jobtitle"),
             menuSubItem("Comparable Salary Analysis", tabName = "salary")),
    menuItem( "Model", tabName = "model"),
    convertMenuItem(menuItem("Job Recommendation", tabName = "recommendation",
             selectInput("education", "Education: ", c('Bachelors', 'Masters', 'PhD')),
             multiInput("skills", "Program skills: ", choices = c('Python', 'R', 'Hadoop', 'SQL', 'Tableau', 'Tensorflow', 'Agile', 'Power BI', 'SSaS', 'Algorithm', 'Java', 'Visualization'), options = list(enable_search = FALSE)),
             textInput("yof", "Experience (years): " ), 
             actionButton("goButton", "Go!")),
             "recommendation")
    
    
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "state",
                fluidPage(box(
                # title = "Select a State",
                selectInput("state", "State: ", c("NY" ,"NJ" ,"CA","TX" ,"VA" ,"MD", "DC")), # can variable 'states' instead of the list
                width = 24)),
              
              fluidPage(
                box(title = "Counties",
                    plotlyOutput("fig2"), width = 24),
                box(title = "Salary Range by States",
                    plotlyOutput("fig"), width = 24))
      ), 
      tabItem(tabName = "industry",
              fluidPage(
                box(title = "Industries Percentage",
                    plotlyOutput("fig6"), width = 24),
                box(title = 'Industries Annual Average Salary',
                    plotlyOutput("fig7"), width = 24)
              )),
      
      tabItem(tabName = "company",
              fluidPage(box(
                title = "Select a State",
                selectInput("states", "State: ", c("CA","TX" ,"VA" ,"MD", "DC")), # can variable 'states' instead of the list
                width = 24)),
              fluidPage(
                box(title = "Annual Average Salary by State Companies",
                    plotlyOutput("fig8"), width = 24),
                box(title = "Companies",
                    plotlyOutput("fig4"), width = 24))),
      
      tabItem(tabName = "jobtitle",
              fluidPage(
                box(title = "Job Titles Percentage",
                    plotlyOutput("fig3"), width = 24),
                box(title = "Job Titles Annual Average Salary",
                    plotlyOutput("fig5"), width = 24))),
      
      tabItem(tabName = "salary", plotlyOutput("fig9"), width = 24),
      tabItem(tabName = "recommendation", 
              fluidRow(
                DT::dataTableOutput("test")
              )
      ),
      tabItem(tabName = "model", 
              fluidPage(
            titlePanel("Logistic Regression for Job Recommender"),
            mainPanel(
            htmlOutput("inc"))
        )
      )
      
    ))
  
)

shinyApp(ui, server, options = list(height=1080))