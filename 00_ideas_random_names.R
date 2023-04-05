# Name Distribution data: 
  # in Germany 2020 (source:  https://gfds.de/ausfuehrliche-auswertung-vornamen-2021/)
    # by: (Nord/Süd/Ost/West), gender, erst/folgename
    # Schreibweisen kombiniert
  # Nachnamen (1995): 
    # https://gfds.de/vornamen/familiennamen/ / Konrad Kunze: »dtv-Atlas Namenkunde. Vor- und Familiennamen im deutschen Sprachgebiet.« München: Deutscher Taschenbuch Verlag 1999. Zusammengestellt auf Grundlage von 28.448.352 Einträgen in Telefonverzeichnissen aus dem Jahr 1995.


  # austria: http://www.statistik.gv.at/web_de/statistiken/menschen_und_gesellschaft/bevoelkerung/geborene/vornamen/index.html
  # switzerland: https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung/geburten-todesfaelle/vornamen-neugeborene.html
  # lichtenstein: https://www.llv.li/inhalt/11334/amtsstellen/vornamenstatistik


library(tidyverse)
library(magrittr)

# probably do operations inside db!

# US #####
library(babynames)



# just 1997 for now -> as function
yeareg = 1905

US_givennames <- babynames::babynames %>% filter(year == yeareg)

US_births <- if(year < 1909){
                    # ToDo: find beter method later... probably ts-prediction
         bind_rows(tibble(year = yeareg, births = babynames::births$births[babynames::births$year == min(babynames::births$year)]
                            
                          # this gives only inhibitant count: births = US_givennames %>% summarise(sum(n)) %>% as_vector()
                            ), 
                   babynames::births)
  } else{babynames::births}
US_births <-  US_births %>% filter(year == yeareg) %>% select(births) %>% as_vector()

(coverage <- US_givennames %>% 
  group_by(sex) %>% 
  summarise(perc = sum(prop), freq = sum(n)) %>%  
    # includes only names that occur with n < 4 (between 77301 and 309199 names -> what distribution to assume?)
  mutate(sex_ratio = perc/sum(perc),
         rare_names_n = (US_births - sum(freq)) * sex_ratio, # sth wyld here...
         rare_names_perc = rare_names_n / freq,
         name = paste0("[random_rare_name(", sex, ")]"),
         year = yeareg,
         occurrences_min = 1,
         occurrences_max = 4) 
)

US_givennames %<>% bind_rows(., coverage %>% select(year, sex, name, n = rare_names_n, prop = rare_names_perc)) %>% rename(prob = prop)


# regional data: https://www.ssa.gov/OACT/babynames/limits.html - > not necessary for now

# lastnames 2010 (occuring > 99 times):
  # frequency of surname frequencies: https://www2.census.gov/topics/genealogy/2010surnames/surnames.pdf
US_inhibitants_2010 <- 294979229
US_rare_surnames_2010 <- tribble(
  ~ name,                           ~ count_names, ~ count_people, ~ occurences_min, ~ occurences_max,
  "[random_rare_surname (50-99 occurences)]", 114079,        7939724,     50,               99,
  "[random_rare_surname (25-49 occurences)]", 454216,        6182636,     25,               49,
  "[random_rare_surname (10-24 occurences)]", 354912,        5448514,     10,               24,
  "[random_rare_surname (5-9 occurences)]",   424454,        2761552,     5,               9,
  "[random_rare_surname (2-4 occurences)]",   1165587,       3079711,     2,                4,
  "[random_rare_surname (1 occurence)]",      3899864,       3899864,     1,                1
) %>% mutate(prob = count_people / US_inhibitants_2010)

US_inhibitants_2000 <- 269762087
US_rare_surnames_2000 <- tribble(
  ~ name,                           ~ count_names, ~ count_people, ~ occurences_min, ~ occurences_max,
  "[random_rare_surname (50-99 occurences)]", 105609,        7358924,     50,               99,
  "[random_rare_surname (25-49 occurences)]", 166059,        5772510,     25,               49,
  "[random_rare_surname (10-24 occurences)]", 331518,        5092320,     10,               24,
  "[random_rare_surname (5-9 occurences)]",   395600,        2568209,     5,                9,
  "[random_rare_surname (2-4 occurences)]",   1056992,       2808085,     2,                4,
  "[random_rare_surname (1 occurence)]",      4040966,       4040966,     1,                1
) %>% mutate(prob = count_people / US_inhibitants_2000)

# More work & data needed here:
US_rare_surnames_1990 <- read.table("https://www2.census.gov/topics/genealogy/1990surnames/dist.all.last", 
                                    col.names = c("name", "prob_perc", "cum_prob_perc", "rank")) 

# what distribution to assume?

# data source: https://www2.census.gov/topics/genealogy/2010surnames/names.zip
US_surnames <- rio::import("./US_surnames/Names_2010Census.csv") %>% 
  mutate(name = stringr::str_to_title(name),
         prob = count / US_inhibitants) %>% 
  select(name, count, prob) %>%             # ignoring ethnical differences
  filter(name != "All Other Names") %>%     # more precise data instead
  bind_rows(., US_rare_surnames %>% select(name, count = count_people, prob))
# does not include any double-names -> frequency data?!

# draw:
# assume just one name:
print(paste(
  sample(x = US_givennames$name, size = 100, replace = T, prob = US_givennames$prob), # replace = F for double-names
  sample(x = US_surnames$name, size = 100, replace = T, prob = US_surnames$prob)
))

# calculate likelihood of specific name:
search_name <- function(name = "Simone Raudszus", similar = F){
  options(scipen = 999)
  
  lookup_name <- str_split_1(name, " ")
  
  if (similar == F) {
  freq_given <- filter(US_givennames, name == lookup_name[1]) %>% summarise(prob = sum(prob)) %>%  # in case the name is unisex (even before the revolution)
    select(prob) %>% as_vector()
  freq_sur <- filter(US_surnames, name == lookup_name[2]) %>% select(prob) %>% as_vector()
  } else{
  
# include similar names (as option)? -> US_givennames[agrep(pattern = lookup_name[1], x = US_givennames$name), ] instead of filter
  # very broad!
  similar_givennames <- US_givennames[agrep(pattern = lookup_name[1], x = US_givennames$name, max.distance = 0.0001), ] 
  freq_given <- similar_givennames %>% summarise(prob = sum(prob)) %>%  # in case the name is unisex (even before the revolution)
    select(prob) %>% as_vector()
  similar_surnames <- US_surnames[agrep(pattern = lookup_name[2], x = US_surnames$name, max.distance = 0.251), ]
# ToDo: adapt max.distance to minimum value that yields results
  freq_sur <- similar_surnames %>% summarise(prob = sum(prob)) %>%  # in case the name is unisex (even before the revolution)
    select(prob) %>% as_vector()
  }
  
  freq <- freq_sur * freq_given
  n <- freq * US_inhibitants
  # ToDo: implement multiple names!
  
if (similar == F) {
  if (!is_empty(freq)){
    cat("The frequency for US citizens to be named ", name, " is ", round(freq * 100, 2), "%. \n\n",
      "Assuming the stupendous idea of names being randomly assigned by this revolutionary name generator, about ", round(n, 1), " Americans would be called, ", name, ".",
      "\n\n?Viva la revoluci?n de los nombres!", sep = "")
} else{cat("The frequency for US citizens to be named ", name, "can not be calculated. Sorry :(\n",
           "But hey: This means your name is really special! Congratulations!",
           "\n\n?Viva la revoluci?n de los nombres!")}
} else{
  
  if("try-error" %in% class(try(
    cat("The frequency for US citizens to be named similar to ", name, " is ", round(freq * 100, 7), "%. \n\n", # ToDo: round to first non-zero
    "Assuming the stupendous idea of names being randomly assigned by this revolutionary name generator, about ", round(n, 1), " Americans would be called similar to, ", name, ".",
    "\n\nPlausible examples for similar names include: \n",
    paste(
      sample(x = similar_givennames$name, size = 5, replace = F, prob = similar_givennames$prob), # replace = F for double-names
      sample(x = similar_surnames$name, size = 5, replace = F, prob = similar_surnames$prob)
      , collapse = ", "),
                                        
    "\n\n?Viva la revoluci?n de los nombres!", sep = "")
    ))
    ){
    cat("The frequency for US citizens to be named similar to ", name, "can not be calculated.\n",
             "Are you really sure this is an actual name?!?",
             "\n\n?Viva la revoluci?n de los nombres!")
    }
  }
}
search_name(similar = T)






# BRD #####
n_kids = 1045751 # BRD 2020

n_names_prob <- 
  tibble(
    n = c(1L, 2L, 3L, 4L, 5L),
    freq = c(.615, .349, .033, NA, NA),
    abs = freq * n_kids
  )

# library(zoo)
na.approx(linear_name_freq, n_names_prob)
# Hmisc::approxExtrap(n_names_prob$n, n_names_prob$abs , n_names_prob, method = "linear", n = 2, rule = 2, f = 0, ties = "ordered", na.rm = FALSE)

# plot frequencies:
par(mfrow = c(2, 1))
plot(n_names_prob[1:2], type = "b")
ggplot(n_names_prob, # %>% filter(n < 4), 
       aes(n, freq)) +
  geom_smooth(formula = y ~ poly(x, 2), se = F) +
  geom_point()

# linear_name_freq <- lm(freq ~ n, data = n_names_prob %>% filter(n < 4))
nonlinear_name_freq <- glm(freq ~ poly(n, 2), data = n_names_prob %>% filter(n < 4))
# predict(linear_name_freq, newdata = data.frame(n = c(4,5)))
(pre_freq <- predict(nonlinear_name_freq, newdata = data.frame(n = c(1,2,3,4,5))))
(pre_freq = pre_freq + abs(min(pre_freq)) + 1)

approx_n_names <- tibble(testwert = seq(0.0001, 0.002, 0.00001),
tester = pre_freq[4] * testwert + testwert,
closeness = abs(tester - 1- sum(n_names_prob$freq, na.rm = T)))
ggplot(approx_n_names, aes(x = testwert, y = closeness)) +
  geom_point()

closest = min(approx_n_names$closeness)
multiplier = pre_freq[4]
# this converts to list for some reason...
n_names_prob$freq[c(4,5)] <- c(approx_n_names$testwert[approx_n_names$closeness == closest] * multiplier, 
                               approx_n_names$testwert[approx_n_names$closeness == closest]
)

sum(n_names_prob$freq)

n_names_prob %>%  mutate(abs = freq * n_kids)

# (pre_freq = pre_freq / sum (pre_freq))
# sum(pre_freq)
# plot(pre_freq)

# margins::marginal_effects(linear_name_freq, data = n_names_prob)

##################
# Data Maske:
##################

library(magrittr)

population_wb_original <- wbstats::wb_data(indicator = c("SP.POP.TOTL.MA.IN", "SP.POP.TOTL.FE.IN"), mrnev = 1, return_wide = F) %>% 
  select(-c(last_updated, obs_status, indicator_id, iso2c))

population_wb_nb <- wbstats::wb_data(indicator = c("SP.POP.TOTL.MA.IN", "SP.POP.TOTL.FE.IN", "SP.POP.TOTL"), mrnev = 1, return_wide = T) 

gender_balance = 0.5
  
population_wb_nb %<>% 
    filter(is.na(SP.POP.TOTL.FE.IN)) %>% 
  mutate(SP.POP.TOTL.FE.IN = round(SP.POP.TOTL * gender_balance),
         SP.POP.TOTL.MA.IN = floor(SP.POP.TOTL * (1 - gender_balance)), 
         footnote = paste("gender distribution manufactured using gender_balance =", gender_balance)
  ) %>% 
  select (-iso2c, -SP.POP.TOTL) %>% 
  pivot_longer(cols = -c(country, iso3c, date, footnote), names_to = "indicator", values_to = "value") %>% 
  mutate(indicator = ifelse(indicator == "SP.POP.TOTL.FE.IN", "Population, female", "Population, male")) %>% 
  select(names(population_wb_original))
         
population_wb <- bind_rows(population_wb_original, population_wb_nb) %>% mutate(indicator = str_sub(indicator, 13, 13))


# very data-intense approach... better base it on (conditional) probability draws?

# loop over world population:
everyone_is_data <- tibble()
for (country in 1:length(population_wb$country)) {
  for (person in 1:population_wb$value[country]) {
    everyone_is_data %<>% 
      bind_rows(., tibble(
        gender = population_wb$indicator[country],
        iso3c = population_wb$iso3c[country],
        country = population_wb$country[country],
        id = paste0(population_wb$iso3c[country], sprintf("%09d", person), population_wb$indicator[country]),
        country_gender_pop = population_wb$value[country],
        date = population_wb$date[country],
        footnote = population_wb$footnote[country],
        firstname = NA,
        secondname = NA,
        thirdname = NA,
        fourthname = NA,
        lastname = NA,
        n_names = NA
        )
      )    
    # show progress:
    print(paste0(population_wb$iso3c[country], sprintf("%09d", person), population_wb$indicator[country]))
  }
}

# just repeat:
population_wb %>% nest(., -country)
  slice(rep(1:n(), each = population_wb$value/1000))
 
# %>% group_by(country, gender) %>% 
#  mutate(id = paste0(iso3c, sprintf("%09d", ), population_wb$indicator[country]))
