#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(magrittr)
library(babynames)
library(rio)

options(scipen = 999)

# frequency of surname frequencies: https://www2.census.gov/topics/genealogy
US_inhibitants_2010 <- 294979229
US_rare_surnames_2010 <- tribble(
  ~ name,                           ~ count_names, ~ count_people, ~ occurrences_min, ~ occurrences_max,
  "[random_rare_surname (50-99 occurrences)]", 114079,        7939724,     50,               99,
  "[random_rare_surname (25-49 occurrences)]", 454216,        6182636,     25,               49,
  "[random_rare_surname (10-24 occurrences)]", 354912,        5448514,     10,               24,
  "[random_rare_surname (5-9 occurrences)]",   424454,        2761552,     5,               9,
  "[random_rare_surname (2-4 occurrences)]",   1165587,       3079711,     2,                4,
  "[random_rare_surname (1 occurrence)]",      3899864,       3899864,     1,                1
) %>% mutate(prob = count_people / US_inhibitants_2010)

US_inhibitants_2000 <- 269762087
US_rare_surnames_2000 <- tribble(
  ~ name,                           ~ count_names, ~ count_people, ~ occurrences_min, ~ occurrences_max,
  "[random_rare_surname (50-99 occurrences)]", 105609,        7358924,     50,               99,
  "[random_rare_surname (25-49 occurrences)]", 166059,        5772510,     25,               49,
  "[random_rare_surname (10-24 occurrences)]", 331518,        5092320,     10,               24,
  "[random_rare_surname (5-9 occurrences)]",   395600,        2568209,     5,                9,
  "[random_rare_surname (2-4 occurrences)]",   1056992,       2808085,     2,                4,
  "[random_rare_surname (1 occurrence)]",      4040966,       4040966,     1,                1
) %>% mutate(prob = count_people / US_inhibitants_2000)

# what distribution to assume?


# functions ###############

search_name <- function(data_givennames, data_surnames, gender = 'all / no gender', givenname = "Simone", surname = "Raudszus", similar = F, country = "US"){
  
  gender <- case_when(gender == 'all / no gender' ~ "",
                      gender == '"female" names only' ~ "female",
                      gender == '"male" names only' ~ "male")
  
  name <- paste(givenname, surname)
  
  # sum of all names is not equal for both due to different years and form of data collection!  
  n_givennames <- data_givennames %>% summarize(n = sum(n)) %>% as_vector()
  n_surnames <- data_surnames %>% summarize(n = sum(count)) %>% as_vector()
  
  if (similar == F) {
    
    givennames_sum <- filter(data_givennames, name == givenname) %>% 
      summarise(prob = sum(prob), min = sum(occurrences_min), max = sum(occurrences_max))
    # draw randomly how many occurences a name has (in case of rare names):
    freq_given <- sample(givennames_sum$min:givennames_sum$max, 1) / n_givennames
      # summarise in case the name is unisex (even before the revolution)
      
    surnames_sum <- filter(data_surnames, name == surname) %>% 
      summarise(prob = sum(prob), min = sum(occurrences_min), max = sum(occurrences_max))
    # draw randomly how many occurences a name has (in case of rare names):
    freq_sur <- sample(surnames_sum$min:surnames_sum$max, 1) / n_surnames
  
    } else{
    # if similar == T
    
      # summarise across all similar names
  # ToDo: very broad filter... try other methods!
    similar_givennames <- data_givennames[agrep(pattern = givenname, x = data_givennames$name, max.distance = 0.0001), ]
    similar_givennames_sum <- similar_givennames %>% summarise(prob = sum(prob), min = sum(occurrences_min), max = sum(occurrences_max))
    # draw randomly how many occurrences a name has (in case of rare names):
      freq_given <- sample(similar_givennames_sum$min:similar_givennames_sum$max, 1) / n_givennames
    
    similar_surnames <- data_surnames[agrep(pattern = surname, x = data_surnames$name, max.distance = 0.251), ]
    # ToDo: adapt max.distance to minimum value that yields results
    similar_surnames_sum <- similar_surnames %>% 
      summarise(prob = sum(prob), min = sum(occurrences_min), max = sum(occurrences_max))  # in case the name is unisex (even before the revolution)
    
    # draw randomly how many occurences a name has (in case of rare names):
    freq_sur <- sample(similar_surnames_sum$min:similar_surnames_sum$max, 1) / n_surnames
  }
  
  freq <- freq_sur * freq_given # conditional on data selection (e.g. only male/female?)
  round_nonzero <- function(freq){round(x = 10^(log10(freq) - ceiling(log10(freq))), digits = 1) * 10 ^ ceiling(log10(freq))} # round to first non-zero decimal
  
  n <- freq * n_givennames # calculate based on birthyear and make gender specific! 
      # doing freq * US_inhibitants_2010 would in case of gender selection imply to prescribe only names of that gender to all citizens 
  # ToDo: make year-specific later!
  # ToDo: implement multiple names!
  
    if (similar == F) {
      output_lookup <- 
        if (!is.na(round_nonzero(freq))){
          paste0("<br>The probability for ", gender, " ", country, " citizens to be named ", name, " is ", round_nonzero(freq * 100), "%.",
              "<br><br>Assuming the inevitable implementation of the stupendously brilliant idea of names being randomly assigned by this revolutionary name generator, about 
              <b>" , round_nonzero(n), " ", gender, " ", country, ifelse(round_nonzero(n) == 1, " citizen ", " citizens "), "</b>would be called ", name, ".",
              "<br><br><b>¡Viva la revolución de los nombres!</b>")
        } else{paste("<br>The probability for", gender, " ", country, "citizens to be named ", name, "can not be calculated.", emo::ji("crying_cat_face"),
                   "<br>But hey: This means your name is really special! Congratulations!", emo::ji("tada"),
                   "<br><br><b>¡Viva la revolución de los nombres!</b>")}
    } else{ 
      # if similar == T
      
      if("try-error" %in% class(try(  # still necessary? 
        output_lookup <- 
          paste0("<br>The probability for ", gender, " ", country, " citizens to be named similar to ", name, " is ", round_nonzero(freq * 100), "%.", # ToDo: round to first non-zero
              "<br><br>Assuming the inevitable implementation of the stupendously brilliant idea of names being randomly assigned by this revolutionary name generator, about 
              <b>", round_nonzero(n), " ", gender, " ", country, ifelse(round_nonzero(n) == 1, " citizen ", " citizens "), "</b>would be called similar to ", name, ".",
              "<br>Plausible examples for similar names include:<br>",
              paste(
                sample(x = similar_givennames$name, size = 5, replace = T, prob = similar_givennames$prob), # replace = F for double-names
                sample(x = similar_surnames$name, size = 5, replace = T, prob = similar_surnames$prob)
                , collapse = ", "),
              
              "<br><br><b>¡Viva la revolución de los nombres!</b>")
        ))
      ){
        output_lookup <- 
          paste("<br>The probability for", gender, country, "citizens to be named similar to", name, "can not be calculated.",
              "<br>Are you really sure this is indeed an actual name?!?",
              "<br><br><b>¡Viva la revolución de los nombres!</b>")
       }
    }
  return(output_lookup)
}


# Define UI for app ####################
ui <- fluidPage(

    # Application title
    titlePanel("The Revolutionary Name Generator"),

    # Sidebar:
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year of Birth:",
                        min = 1880,
                        max = 2017,
                        ticks = F,
                        step = 1, 
                        sep = "",
                        value = 1997),
            selectInput("gender",
                        "gender-specific names?",
                        c("all / no gender", '"female" names only', '"male" names only')),
            checkboxInput("similar", 
                          "include similar names?", 
                          value = FALSE),
            selectInput("country",
                        label = "Country", 
                        choices = c("US")
                        ),
            textInput("name", 
                      label = "Name Lookup",
                      placeholder = "name"),
            
            htmlOutput("version")
        ),

        # Output:
        mainPanel(
           htmlOutput("generate"),
           htmlOutput("lookup")
        )
          
    ),
    tags$head(tags$style("#generate{color: violet;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")),
              tags$head(tags$style("#lookup{color: purple;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")),
    tags$head(tags$style("#version{color: lightgrey;
                                 font-size: 15px;
                                 font-style: italic;
                                 }"))
)

# Define server logic required ######################
server <- function(input, output, session) {
  
  output$version <- renderText({'
                                <footer> 
                                  <a href="https://github.com/milanschroeder/RandomNames">
                                    <img src="https://tse4.mm.bing.net/th?id=OIP.Q7V27pjgyelBqH0iwvAUEAD6D6&pid=Api" alt="View on GitHub" height="40px" width="40px">
                                  </a>
                                  Version 1.0.1 (2023/04/05)
                                </footer>
                              '})
  
  get_data_givennames <- reactive({
    
    if (input$country == "US") {
      
      gender <- case_when(input$gender == 'all / no gender' ~ c("F", "M"),
                          input$gender == '"female" names only' ~ "F",
                          input$gender == '"male" names only' ~ "M")
      
      births <- if(input$year < 1909){
        # ToDo: find better method later... probably ts-prediction
        bind_rows(tibble(year = input$year, births = babynames::births$births[babynames::births$year == min(babynames::births$year)]),
                  babynames::births)
          } else{babynames::births}
      births %<>% filter(year == input$year) %>% select(births) %>% as_vector()
        
      data_givennames <- babynames::babynames %>% filter(year == input$year) %>% rename(prob = prop) %>% 
        mutate(occurrences_min = n,
               occurrences_max = n)
      
      coverage <- data_givennames %>% 
        group_by(sex) %>% 
        summarise(perc = sum(prob), freq = sum(n)) %>%  
        # includes only names that occur with n < 4 (between 77301 and 309199 names -> what distribution to assume?)
        mutate(sex_ratio = perc/sum(perc),
               rare_names_n = (births - sum(freq)) * sex_ratio, # sth wyld here...
               rare_names_perc = rare_names_n / freq,
               name = paste0("[random_rare_name(", sex, ")]"),
               year = input$year,
               occurrences_min = 1,
               occurrences_max = 4) 
      
      data_givennames %<>% 
        bind_rows(., coverage %>% select(year, 
                                         sex, 
                                         name, 
                                         n = rare_names_n, 
                                         prob = rare_names_perc, 
                                         occurrences_min, 
                                         occurrences_max)) %>% 
    # filter if gender specific:
        filter(sex %in% gender)
    }
    return(data_givennames)
  })
  
  
  
  # surnames 2010 (occuring > 99 times):
  get_data_surnames <- reactive({
    if (input$country == "US") {
      # data source: https://www2.census.gov/topics/genealogy
     
      surname_source <- case_when(input$year > 2009 ~ "./US_surnames/Names_2010Census.csv",
                                  input$year < 2010 ~ "./US_surnames/Names_2000Census.csv"
                                  )
      US_rare_surnames <- case_when(input$year > 2009 ~ US_rare_surnames_2010,
                                    input$year < 2010 ~ US_rare_surnames_2000
                                    )
      
       US_surnames <- rio::import(surname_source) %>% 
        mutate(name = stringr::str_to_title(name),
               prob = count / US_inhibitants_2010,
               occurrences_min = count,
               occurrences_max = count,
               year = input$year) %>% 
        select(name, count, prob, occurrences_min, occurrences_max) %>%             # ignoring ethnical differences
        filter(name != "All Other Names") %>%     # more precise data instead
        bind_rows(., US_rare_surnames %>% select(name, count = count_people, prob, occurrences_min, occurrences_max))
      # does not include any double-names -> frequency data?!
      # for US:
      data_surnames <- US_surnames
    } 
    return(data_surnames)
  })  
  
  
  # creating temp vars here to filter:

  generate_givenname <- reactive({
      random_givenname <- sample(x = get_data_givennames()$name, size = 1, replace = T, prob = get_data_givennames()$prob)  # replace = F for double-names
    return(random_givenname)
  })

  generate_surname <- reactive({
      random_surname <- sample(x = get_data_surnames()$name, size = 1, replace = T, prob = get_data_surnames()$prob)
    return(random_surname)
  })
  
  
  generate_name <- reactive({
    random_name <- paste(
      generate_givenname(),
      generate_surname()
    ) 
    return(random_name)
  })
  
    output$generate <- renderText({
      paste(
        "<br>Your new, randomly assigned name is now:",
        "<br><b><font size='20'>",
        generate_name(),
        "</font></b>"
        )
      })
    
    
    
    
    output$lookup <- renderText({
        paste(
          search_name(get_data_givennames(), 
                      get_data_surnames(),
                      gender = input$gender, 
                      givenname = ifelse(length(str_split_1(input$name, " ")) == 2, str_split_1(input$name, " ")[1], generate_givenname()),
                      surname = ifelse(length(str_split_1(input$name, " ")) == 2, str_split_1(input$name, " ")[-1], generate_surname()),
                      similar = input$similar, 
                      country = input$country)
        )
    })
}

# Run the app:
shinyApp(ui = ui, 
         server = server)
