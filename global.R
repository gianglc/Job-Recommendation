library(plotly)
library(dplyr)
library(stringr)
library(jsonlite)

train <- read.csv("train_data.csv", stringsAsFactors = FALSE)
com_sal <- read.csv("state_com_sal.csv", stringsAsFactors = FALSE)
job_listings_data <- readRDS("./job_listings_data.rds")
# train <- subset(train, select = c("Job_title", 'Company', 'City' ,'avg_sal', 'Industry', 'Date_Posted', 'Valid_until', 'Job_Type' ))
final_model <- readRDS("final_model.rds")

# Creating plot Job by States

states <- as.list(unique(train[c("State")]))

min_sal <- summarise(group_by(train, State), mymean = mean(Min_Salary))
max_sal <- summarise(group_by(train, State), mymean = mean(Max_Salary))

sal <- data.frame(states, min_sal$mymean, max_sal$mymean)

fig <- plot_ly(sal, x = ~State, y = ~min_sal$mymean, type = 'bar', name = 'Min Salary')
fig <- fig %>% add_trace(y = ~max_sal$mymean, name = 'Max Salary')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
fig

# Pie chart of States by County jobs
sub_df = train[train[, "State"] == "CA", ]
cities = as.data.frame(head(sort(table(sub_df[, "City"]), decreasing = TRUE), 5))
fig2 <- plot_ly(cities, labels = ~Var1, values = ~Freq, type = 'pie')
fig2 <- fig2 %>% layout(title = 'Job Percentages by State Counties',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig2

# pie chart and histogram by job_title 
#Top 8 job titles with max jobs
job_title_ranking <- as.data.frame(head(sort(table(train[, "Job_title"]), decreasing = TRUE), 8))
fig3 <- plot_ly(job_title_ranking, labels = ~Var1, values = ~Freq, type = 'pie')
fig3 <- fig3 %>% layout(title = 'Top 8 Job Titles with the Largest Percentage of Jobs',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),                      
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig3

# histogram chart by job_titles 
# Top 8 JobTitles woth Min and Max Salary
job_title_list <- job_title_ranking$Var1
sub_df_top8_job_title <- filter(train,Job_title %in% job_title_list)
min_sal_job_title <- summarise(group_by(sub_df_top8_job_title, Job_title), mymean = mean(Min_Salary))
max_sal_job_title <- summarise(group_by(sub_df_top8_job_title, Job_title), mymean = mean(Max_Salary))
job_title_sal <- data.frame(job_title_list, min_sal_job_title$mymean, max_sal_job_title$mymean)
fig5 <- plot_ly(job_title_sal, x = ~job_title_list, y = ~min_sal_job_title.mymean, type = 'bar', name = 'Min Salary')
fig5 <- fig5 %>% add_trace(y = ~max_sal_job_title.mymean, name = 'Max Salary')
fig5 <- fig5 %>% layout(yaxis = list(title = 'Mean Salary'),
                        xaxis = list(title = 'Job Titles'),
                        title = 'Annual Average Salaries of Top 8 Job Titles Above',
                        barmode = 'group')
fig5

# histogram chart by companies 
# Top 20 Companies which have most number of job postings
company_ranking = as.data.frame(head(sort(table(train[, "Company"]), decreasing = TRUE), 20))
fig4 <- plot_ly(company_ranking, x = ~Var1, y = ~Freq, type = 'bar', name = 'Company')
fig4 <- fig4 %>% layout(yaxis = list(title = 'Count'),
                        xaxis = list(title = 'Company Name', tickangle = -25),
                        barmode = 'group',
                        title = 'Top 20 Companies with the Most Job Postings')
fig4


# pie chart by industries
industries_ranking <- as.data.frame(head(sort(table(train[, "Industry"]), decreasing = TRUE), 8))
industries_ranking <- industries_ranking[-(5), ]
fig6 <- plot_ly(industries_ranking, labels = ~Var1, values = ~Freq, type = 'pie')
fig6 <- fig6 %>% layout(title = 'Top 8 Industries with the Largest Percentage of Jobs',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),                      
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig6

# histogram chart by industries
# Top 8 Industries with Min and Max Salary
industries_list <- industries_ranking$Var1
sub_df_top8_industries <- filter(train,Industry %in% industries_list)
min_sal_industries <- summarise(group_by(sub_df_top8_industries, Industry), mymean = mean(Min_Salary))
max_sal_industries <- summarise(group_by(sub_df_top8_industries, Industry), mymean = mean(Max_Salary))
industries_sal <- data.frame(industries_list, min_sal_industries$mymean, max_sal_industries$mymean)
fig7 <- plot_ly(industries_sal, x = ~industries_list, y = ~min_sal_industries.mymean, type = 'bar', name = 'Min Salary')
fig7 <- fig7 %>% add_trace(y = ~max_sal_industries.mymean, name = 'Max Salary')
fig7 <- fig7 %>% layout(yaxis = list(title = 'Mean Salary'),
                        xaxis = list(title = 'Industries'),
                        title = 'Annual Average Salaries of Top 8 Industries Above',
                        barmode = 'group')
fig7

# histogram chart by state companies salaries
sub_state_df = train[train[, "State"] == "TX", ]
company_df = as.data.frame(head(sort(table(sub_state_df[, "Company"]), decreasing = TRUE), 5))
company_lst = company_df$Var1
sub_df_top5_company <- filter(sub_state_df,Company %in% company_lst)
min_sal_company <- summarise(group_by(sub_df_top5_company, Company), mymean = mean(Min_Salary))
max_sal_company <- summarise(group_by(sub_df_top5_company, Company), mymean = mean(Max_Salary))
company_sal <- data.frame(company_lst, min_sal_company$mymean, max_sal_company$mymean)
fig8 <- plot_ly(company_sal, x = ~company_lst, y = ~min_sal_company$mymean, type = 'bar', name = 'Min Salary')
fig8 <- fig8 %>% add_trace(y = ~max_sal_company$mymean, name = 'Max Salary')
fig8 <- fig8 %>% layout(yaxis = list(title = 'Mean Salary'),
                        xaxis = list(title = 'Company'),
                        title = 'Minimal And Maximal Annual Average Salaries for top 5 companies',
                        barmode = 'group')
fig8

# Interactive Comparable Salary Map
com_sal$hover <- with(com_sal, paste(state, '<br>', "Grocery", grocery, "Housing", housing, "<br>",
                           "Utilities",utilities, "Transportation", transportation,
                           "<br>", "Healthcare", healthcare))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig9 <- plot_geo(com_sal, locationmode = 'USA-states')
fig9 <- fig9 %>% add_trace(
  z = ~index, text = ~hover, locations = ~state,
  color = ~index, colors = "RdYlGn"
)
fig9 <- fig9 %>% colorbar(title = "Million USD")
fig9 <- fig9 %>% layout(
  title = 'Comparable Salary in US States',
  geo = g
)

fig9






