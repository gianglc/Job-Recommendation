convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

data_processing <- function(input, data){
  #year of exp
  data$exp_signal <- as.numeric(input$yof) - as.numeric(data$experience_req)
  #skills
  input_skill <- as.vector(str_split(input$skills, " ", simplify = TRUE))
  #skill_ratio_signal
  for (idx in 1:dim(data)[1]){
    data$skill_signal[idx] = length(intersect(input_skill, data$programming_skills[[idx]]))
    data$skill_ratio_signal[idx] = data$skill_signal[idx]/length(data$programming_skills[[idx]])
  }
  #input for education
  if (input$education == "Bachelors") {
    data$input_edu = 0
  } else if (input$education == "Masters") {
    data$input_edu = 1
  } else {
    data$input_edu = 2
  }
  # calculate min and max difference in education
  for (idx in 1:dim(data)[1]){
    data$edu_signal_max[idx] = data$input_edu[idx] - max(as.numeric(data$Education[[idx]]))
    data$edu_signal_min[idx] = data$input_edu[idx] - min(as.numeric(data$Education[[idx]]))
  }
  return(data)
}  

return_recommendations <- function(data, model, match_threshold=0.5) {
  data$match_prob <- predict.glm(model, newdata=data, type="response", se.fit = FALSE)
  return(data[data$match_prob >= match_threshold,])
}

plot_piechart <- function(data) {
  cities = as.data.frame(head(sort(table(data[, "City"]), decreasing = TRUE), 5))
  fig <- plot_ly(cities, labels = ~Var1, values = ~Freq, type = 'pie')
  fig <- fig %>% layout(title = 'Job Percentages by State Counties',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(fig)
}

plot_hist <- function(data){
  company_df = as.data.frame(head(sort(table(data[, "Company"]), decreasing = TRUE), 5))
  company_lst = company_df$Var1
  sub_df_top5_company <- filter(data,Company %in% company_lst)
  min_sal_company <- summarise(group_by(sub_df_top5_company, Company), mymean = mean(Min_Salary))
  max_sal_company <- summarise(group_by(sub_df_top5_company, Company), mymean = mean(Max_Salary))
  company_sal <- data.frame(company_lst, min_sal_company$mymean, max_sal_company$mymean)
  fig8 <- plot_ly(company_sal, x = ~company_lst, y = ~min_sal_company$mymean, type = 'bar', name = 'Min Salary')
  fig8 <- fig8 %>% add_trace(y = ~max_sal_company$mymean, name = 'Max Salary')
  fig8 <- fig8 %>% layout(yaxis = list(title = 'Mean Salary'),
                          xaxis = list(title = 'Company'),
                          title = 'Minimal And Maximal Annual Average Salaries for top 5 companies',
                          barmode = 'group')
  return(fig8)
}
