source("global.R")
source("helper_functions.R")
require(tidyverse)
server <- function(input, output) { 
  output$fig <- renderPlotly({
    return(fig)
  })
  output$fig2 <- renderPlotly({
    fig2 = plot_piechart(data = train[train[, "State"] == input$state,])
    return(fig2)
  })
  output$fig3 <- renderPlotly({
    return(fig3)
  })
  
  output$fig4 <- renderPlotly({
    return(fig4)
  })
  
  output$fig5 <- renderPlotly({
    return(fig5)
  })
  output$fig6 <- renderPlotly({
    return(fig6)
  })
  output$fig7 <- renderPlotly({
    return(fig7)
  })
  output$fig8 <- renderPlotly({
    fig8 = plot_hist(data = train[train[, "State"] == input$states,])
    return(fig8)
  })
  output$fig9 <- renderPlotly({
    return(fig9)
  })
  process_recommendations <- eventReactive(input$goButton, {
    print("Start processing data")
    processed_input_data = data_processing(input, job_listings_data)
    print(head(processed_input_data))
    recommendations <- return_recommendations(processed_input_data, final_model)
   
    recommendations = recommendations[, c("job_title", "job_description", "salary" , "job_loc", "company", "experience_req",
                                               "programming_skills", "job_link" ,"match_prob")]
    print(names(recommendations))
    print(head(recommendations$job_link))
    
    recommendations$job_description <- str_trunc(recommendations$job_description, 100, "right")

    recommendations$job_link <- paste0("<a href='",recommendations$job_link,"'target='_blank'>","link","</a>")
  
    recommendations = recommendations[order(-recommendations$match_prob), ]
    DT::datatable(recommendations, filter = 'top', extensions = c('Buttons'), 
                  options = list(
                                 deferRender = TRUE,
                                 paging = TRUE,
                                 pageLength = 15,
                                 buttons = list('excel',
                                                list(extend = 'colvis', targets = 0, visible = FALSE)),
                                 dom = 'lBfrtip',
                                 fixedColumns = TRUE), 
                  rownames = FALSE, escape = FALSE)
  })
  output$test <- DT::renderDataTable({
      process_recommendations()
    
  }) 
  getPage<-function() {
    return(includeHTML("logistic.html"))
  }
  output$inc<-renderUI({getPage()})
    #process_recommendations()
  #})    
}

