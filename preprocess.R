# Preprocess data
job_listings_data <- read.csv("../final.csv", stringsAsFactors = FALSE)
job_listings_data <- job_listings_data %>% 
  mutate(Education = strsplit(Education, " ")) %>%
  mutate(programming_skills = strsplit(programming_skills, " "))
job_listings_data <- job_listings_data[,c(-1)]
saveRDS(job_listings_data, "job_listings_data.rds")
