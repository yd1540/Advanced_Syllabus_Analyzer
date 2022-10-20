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
library("httr")



#import occupation data
occupation <- read.csv(file = "Occupation_Data.csv")

if (.Platform$GUI == "AQUA") return(1L)

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

# headers <- c("User-Agent"="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36",
#               "Accept"= "*/*",
#              "Cookie"= 'CTK=1gfa1bn33h1i8800; _ga=GA1.2.2119129848.1665710549; _gid=GA1.2.831973622.1665710549; mobbcpc=1; LC="co=CN"; indeed_rcc="LV:CTK:UD"; __cf_bm=7QGYvCv5KbsuVvFGhS04k2O_zZldrpE__t_lzqva0ag-1665712575-0-AWxWgfAMNoR1ma6VAbwiWphS5LsJqYE3dk755zu64Znti6yGwqkzEoiBo5a4Ac2Q7LSBiVi0cQjvaEcDIUDKXYs=; cf_clearance=U4rmjfzZt7Btp2z0gnnebotf_jjMifOtOvCglC_SkrE-1665712590-0-150; gonetap=5; INDEED_CSRF_TOKEN=JrQnhD4ShGnmvzMY1gbH8P4TiT194EiN; _cfuvid=emRBiEdIrXV7iFwssxFh4fzBaILOKquxLZqGhMTLZS8-1665712770017-0-604800000; _gat=1; SHARED_INDEED_CSRF_TOKEN=JrQnhD4ShGnmvzMY1gbH8P4TiT194EiN; SURF=2IxSJThgA5KJu801iW0DYvkF2Ouxwed9; CSRF=lHaH5cn2lLIAXO9BarSFUqxDESvYhkIq; _gali=jobsearch; loctip=1; LV="LA=1665712785:CV=1665710546:TS=1665710546"; CO=US; LOCALE=en; MICRO_CONTENT_CSRF_TOKEN=BiTiQPjY350JiBMMGwpBMWHnETxguGHK; PPID=""; jaSerpCount=3; UD="LA=1665712794:CV=1665710546:TS=1665710546:SG=3b05303354af185de3924a34dc37aebd"; RQ="q=Business+intelligence+Analyst&l=&ts=1665712795026:q=Data+Entry+Keyer&l=&ts=1665712769716"; JSESSIONID=2273A89AC325D75E13C6F8D0E43D9677',
#              "accept-language"= "zh-CN,zh;q=0.9",
#              "referer"="https://www.indeed.com/jobs?q=Business+intelligence+Analyst&l=&vjk=1e17da0c9035fcf4",
#              "sec-fetch-mode"="cors")

page_result_start <- 0 # starting page 
page_result_end <- 5 # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 5)

full_df <- data.frame()

parse_job <- function(input_vector) {
  occu_list <- gsub(" ","+",input_vector)
  for (occu in occu_list){
    #for (i in seq_along(page_results)){
    
    #url <- paste0("https://www.indeed.com/jobs?q=",occu,"&l","&start=",page_results[i],"&vjk=eca30ac0a79b0ddb")
    #url <- paste0("https://www.indeed.com/jobs?q=",occu,"&l","&start=","&vjk=eca30ac0a79b0ddb")
    url <- paste0("https://www.indeed.com/jobs?q=",occu)
    #httr::set_config(httr::user_agent(headers))
    
    page <- url%>% GET(add_headers(headers))%>% content()
    #page <- read_html(httr::POST(url = url))
  
    # Sys.sleep pauses R for two seconds before it resumes
    # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
    Sys.sleep(2)
    
    #Get the job title
    job_title <- page %>%
      rvest::html_nodes("div") %>%
      rvest::html_nodes(xpath = '//*[contains(@class, "jobTitle")]')%>%
      rvest::html_nodes(xpath = '//span[@title]') %>%
      rvest::html_text()
    
    # job_title <- page %>%
    #   rvest::html_nodes("div") %>%
    #   rvest::html_nodes(xpath = '//*[contains(@class, "jobTitle")]')%>%
    #   rvest::html_nodes(xpath = '//span[@title]') %>%
    #   rvest::html_text()
    
    
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
    
    job_occupation <- gsub("+","",occu)
    #job_occupation <- occu
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