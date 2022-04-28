library('DT')
library("NLP")
library('readr')
library("rvest")
library('udpipe')
library('shiny')
library("shinythemes")
library("shinyWidgets")
library("text2vec")
library('textrank')
library("textstem")
library('tidyverse')
library('tidytext')
library("tm")
library("xtable")
library("xml2")


#import occupation data
occupation <- read.csv(file = "Occupation_Data.csv")


#Function: TD-IDF text similarity analysis-------------------------------------------------------------------------


#function to clean the data
prep_func <- function(x) {
  x %>%
    #make text lower case
    str_to_lower() %>%
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>%
    # remove numbers
    {
      gsub(patter = "\\d", replace = " ", .)
    } %>%
    # remove stopwords
    removeWords(stopwords()) %>%
    # remove single character
    {
      gsub(patter = "\\b[A-z]\\b{1}", replace = " ", .)
    } %>%
    # collapse multiple spaces
    str_replace_all("\\s+", " ") %>%
    # lemmatization
    lemmatize_strings()
}

#function to compare syllabus and occupation database
scoreResult = function(input1, input2 = occupation) {
  syllabus <- read_file(input1)
  
  #make syllabus content as a dataframe, and combine with occupation by row
  syllabus_df <- tibble(Title = "User", Description = syllabus)
  
  #slbs_n_occ <- rbind(syllabus_df,occupation)
  slbs_n_occ <- rbind(syllabus_df,occupation)
  
  #clean the syllabus and occupation description, and save as a new column
  slbs_n_occ$description_clean = lemmatize_words(prep_func(slbs_n_occ$Description))
  
  #use vocabulary based vectorization
  itoken_syllabus <- itoken(slbs_n_occ$description_clean, progressbar = FALSE)
  v_syllabus <- create_vocabulary(itoken_syllabus)
  vectorizer_syllabus <- vocab_vectorizer(v_syllabus)
  
  #apply TF-IDF transformation
  dtm_syllabus <- create_dtm(itoken_syllabus, vectorizer_syllabus)
  tfidf <- TfIdf$new()
  dtm_tfidf_source <- fit_transform(dtm_syllabus, tfidf)
  
  #compute similarity-score against each row, and save the results as a new column
  syllabus_tfidf_cos_sim = sim2(x = dtm_tfidf_source, method = "cosine", norm = "l2")
  
  slbs_n_occ <- slbs_n_occ %>% mutate(similarity_score = round(syllabus_tfidf_cos_sim[1:nrow(syllabus_tfidf_cos_sim)],5))
  
  #sort the dataframe by descending similarity score 
  slbs_result <- slbs_n_occ %>%
    arrange(similarity_score %>% desc()) %>%
    select(c(Title,similarity_score)) %>%
    slice(2:21)
  
  
  #  return(slbs_result)
  return(slbs_result)  
}

#Function: Scrape jobs from Indeed.com-------------------------------------------------------------------------



page_result_start <- 0 # starting page 
page_result_end <- 5 # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 5)

full_df <- data.frame()

parse_job <- function(input_vector) {
  occu_list <- gsub(" ","%20",input_vector)
  for (occu in occu_list){
    #for (i in seq_along(page_results)){
    
    #url <- paste0("https://www.indeed.com/jobs?q=",occu,"&l","&start=",page_results[i],"&vjk=eca30ac0a79b0ddb")
    #url <- paste0("https://www.indeed.com/jobs?q=",occu,"&l","&start=","&vjk=eca30ac0a79b0ddb")
    url <- paste0("https://www.indeed.com/jobs?q=",occu)
    page <- xml2::read_html(url)
    
    # Sys.sleep pauses R for two seconds before it resumes
    # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
    Sys.sleep(2)
    
    #Get the job title
    job_title <- page %>%
      rvest::html_nodes("div") %>%
      rvest::html_nodes(xpath = '//*[@class = "jobTitle jobTitle-color-purple jobTitle-newJob"]')%>%
      rvest::html_nodes(xpath = '//span[@title]') %>%
      rvest::html_text() 
    
    
    #Get the company name 
    company_name <- page %>%
      rvest::html_nodes('span') %>%
      rvest::html_nodes(xpath = '//*[@class = "companyName"]') %>%
      rvest::html_text() 
    
    
    #Get the job location
    job_location <- page %>%
      rvest::html_nodes(xpath = '//div[@class = "companyLocation"]') %>%
      rvest::html_text() 
    
    
    #Get the snippet of job description
    job_snippet <- page %>%
      rvest::html_nodes('div') %>%
      rvest::html_nodes(xpath = '//*[@class = "job-snippet"]') %>%
      rvest::html_text()
    
    job_description_snippet <- paste(substr(job_snippet,1,nchar(job_snippet)-2 ),"...",sep="")
    
    job_occupation <- gsub("%20"," ",occu)
    
    job_link <- page %>%
      rvest::html_nodes('a') %>%
      rvest::html_nodes(xpath = '//*[contains(@data-hide-spinner,"true")]') %>%
      rvest::html_attr('href')
    
    link <- paste("https://www.indeed.com",job_link,sep = "")
    
    # df <- data.frame(job_title,company_name,job_location,job_occupation, job_description_snippet,link)
    # names(df)[5] <- "Description"
    # full_df <- rbind(full_df,df)
    
    if (length(job_title) != 0){
      df <- data.frame(job_title,company_name,job_location,job_occupation, job_description_snippet,link)
      names(df)[5] <- "Description"
      full_df <- rbind(full_df,df)
    } else {
      df <- data.frame(job_title = "No results found for this occupation", 
                       company_name = "N/A", 
                       job_location = "N/A", 
                       job_occupation,
                       job_description_snippet = "N/A",
                       link = "N/A")
      names(df)[5] <- "Description"
      full_df <- rbind(full_df,df)
    }
    
  }
  return(full_df) 
}
#Functions: Extract word frequency-------------------------------------------------------------------------
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

extractKeyword <- function(x) {
  
  syllabus <- read_file(x)
  
  syllabus_df <- tibble(Title = "User", Description = syllabus)
  
  #prepare the model for extracting keywords
  
  words <- udpipe_annotate(ud_model, x= syllabus_df$Description) 
  words <- as.data.frame(words)
  
  keywords <- textrank_keywords(words$lemma, relevant = words$upos %in% c("NOUN","ADJ"), ngram_max = 8, sep = " ")
  keywords <- subset(keywords$keywords,freq >= 5)
  return(head(keywords, n = 10))
}