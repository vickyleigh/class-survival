
library(shiny)
library(tidyverse) #for data wrangling
library(forecast) #for cumulative probability of surviving projection 

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("How many people will be at my class reunion?"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      numericInput("n_male",
                   "Number of Men",
                   min = 0,
                   max = 500,
                   value = 50),
      numericInput("n_female",
                   "Number of Women",
                   min = 0,
                   max = 500,
                   value = 50),
      numericInput("age_grad",
                   "Age at Graduation",
                   min = 18,
                   max = 70,
                   value = 24),
      numericInput("year_grad",
                   "Year of Graduation",
                   min = 1940,
                   max = 2000,
                   value = 1979),
    
    hr(),
    helpText("Data from the ONS (2017), deaths rates after 2016 are estimates")
    ), 
   
   # Show a plot
    mainPanel(
      br(),
      textOutput("number"),
      br(),
      br(), 
      plotOutput("distPlot")
      )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$number <- renderText({
    
    current_year <- lubridate::year(Sys.Date())
    current_age <-  current_year - input$year_grad + input$age_grad
    years_passed <- current_age - input$age_grad
    YoB_input <- input$year_grad - input$age_grad
    
    #Input and tidy populations from (ONS, 2017) https://tinyurl.com/y7v5nvvf 
    male_pop <- read_csv("male-pop.csv", col_types = cols(X58 = col_skip(), 
                                                          X59 = col_skip(),
                                                          X60 = col_skip(),
                                                          X61 = col_skip(),
                                                          X62 = col_skip(),
                                                          X63 = col_skip(),
                                                          X64 = col_skip()), 
                         skip = 1) %>%
      head(-6) %>%
      gather(year, pop, 2:57)
    
    #Deaths
    male_deaths <- read_csv("male-deaths.csv", skip =1) %>%
      head(-3) %>%
      gather(year, deaths, 2:85) %>%
      na.omit()
    
    #Join tables
    male <- inner_join(male_pop, male_deaths, by = c("age", "year")) %>%
      filter(age != "105+") %>%
      mutate(year = as.numeric(year),
             age = as.numeric(age),
             YoB = year- age) %>% #Create Year of Birth column
      filter(YoB == YoB_input, age>= input$age_grad) %>% #Keep ony the data from graduation onwards
      mutate(qt = deaths/ pop,  #probability of dying during the time interval 
             pt = 1 - qt,  #probability of surviving in the time interval 
             Psur = 1) #cumulative probability of surviving at the beginning of the time interval 
  
    
    #Psur2 = pt * Psur1 starting from 100 survival 
    for(i in 2:nrow(male)) {
      male$Psur[i] <- male$pt[i - 1] * male$Psur[i - 1] 
    }
    
    #Same again for the female data 
    female_pop <- read_csv("female-pop.csv", skip =1) %>%
      head(-6) %>%
      gather(year, pop, 2:62) %>%
      na.omit()
    
    female_deaths <- read_csv("female-deaths.csv", skip =1) %>%
      head(-3) %>%
      gather(year, deaths, 2:78) %>%
      na.omit() %>%
      head(-11)
    
    female <- inner_join(female_pop, female_deaths, by = c("age", "year")) %>%
      filter(age != "105+") %>%
      mutate(year = as.numeric(year),
             age = as.numeric(age),
             YoB = year- age) %>%
      filter(YoB == YoB_input, age >= input$age_grad) %>%
      mutate(qt = deaths/ pop, 
             pt = 1 - qt, 
             Psur = 1)
    
    for(i in 2:nrow(female)) {
      female$Psur[i] <- female$pt[i - 1] * female$Psur[i - 1]
    }
    
    #The data only goes up to 2016 so to find the cumulative probabilty for survival we need to forecast the data
    last_age <- last(male$age)
    last_year <- last(male$year)
    n_missing <- current_age - last_age
    
    #forecast function projects the rates forward
    m_pred <- as_tibble(forecast(male$Psur, h = n_missing)) %>%
      mutate(Psur = `Point Forecast`,
             age = seq((last_age + 1), current_age, by = 1)) %>%
      select(age, Psur)
    
    f_pred <- as_tibble(forecast(female$Psur, h = n_missing)) %>%
      mutate(Psur = `Point Forecast`,
             age = seq((last_age + 1), current_age, by = 1)) %>%
      select(age, Psur)
    
    
    #select the current years probaility of survival and mutiply by number of people in class
    
    #Number of men who survived
    m_pred_value <- m_pred$Psur[n_missing] * input$n_male
    
    #Number of women who survived
    f_pred_value <- f_pred$Psur[n_missing] * input$n_female 
    
    #Total survived
    total_pred <- round(m_pred_value + f_pred_value, 0)
    
    paste0(total_pred, " people from the class of ", input$year_grad, " are expected to have survived to the age of ", current_age)
    
  })
  
  output$distPlot <- renderPlot({
    
    current_year <- lubridate::year(Sys.Date())
    current_age <-  current_year - input$year_grad + input$age_grad
    years_passed <- current_age - input$age_grad
    YoB_input <- input$year_grad - input$age_grad
    
    #Input and tidy populations from (ONS, 2017) https://tinyurl.com/y7v5nvvf 
    male_pop <- read_csv("male-pop.csv", col_types = cols(X58 = col_skip(), 
                                                          X59 = col_skip(),
                                                          X60 = col_skip(),
                                                          X61 = col_skip(),
                                                          X62 = col_skip(),
                                                          X63 = col_skip(),
                                                          X64 = col_skip()), 
                         skip = 1) %>%
      head(-6) %>%
      gather(year, pop, 2:57)
    
    #Deaths
    male_deaths <- read_csv("male-deaths.csv", skip =1) %>%
      head(-3) %>%
      gather(year, deaths, 2:85) %>%
      na.omit()
    
    #Join tables
    male <- inner_join(male_pop, male_deaths, by = c("age", "year")) %>%
      filter(age != "105+") %>%
      mutate(year = as.numeric(year),
             age = as.numeric(age),
             YoB = year- age) %>% #Create Year of Birth column
      filter(YoB == YoB_input, age>= input$age_grad) %>% #Keep ony the data from graduation onwards
      mutate(qt = deaths/ pop,  #probability of dying during the time interval 
             pt = 1 - qt,  #probability of surviving in the time interval 
             Psur = 1) #cumulative probability of surviving at the beginning of the time interval 
    
    
    #Psur2 = pt * Psur1 starting from 100 survival 
    for(i in 2:nrow(male)) {
      male$Psur[i] <- male$pt[i - 1] * male$Psur[i - 1] 
    }
    
    #Same again for the female data 
    female_pop <- read_csv("female-pop.csv", skip =1) %>%
      head(-6) %>%
      gather(year, pop, 2:62) %>%
      na.omit()
    
    female_deaths <- read_csv("female-deaths.csv", skip =1) %>%
      head(-3) %>%
      gather(year, deaths, 2:78) %>%
      na.omit() %>%
      head(-11)
    
    female <- inner_join(female_pop, female_deaths, by = c("age", "year")) %>%
      filter(age != "105+") %>%
      mutate(year = as.numeric(year),
             age = as.numeric(age),
             YoB = year- age) %>%
      filter(YoB == YoB_input, age >= input$age_grad) %>%
      mutate(qt = deaths/ pop, 
             pt = 1 - qt, 
             Psur = 1)
    
    for(i in 2:nrow(female)) {
      female$Psur[i] <- female$pt[i - 1] * female$Psur[i - 1]
    }
    
    #The data only goes up to 2016 so to find the cumulative probabilty for survival we need to forecast the data
    last_age <- last(male$age)
    last_year <- last(male$year)
    n_missing <- current_age - last_age
    
    #forecast function projects the rates forward
    m_pred <- as_tibble(forecast(male$Psur, h = n_missing)) %>%
      mutate(Psur = `Point Forecast`,
             year = seq((last_year +1), current_year, by = 1)) %>%
      select(year, Psur)
    
    f_pred <- as_tibble(forecast(female$Psur, h = n_missing)) %>%
      mutate(Psur = `Point Forecast`,
             year = seq((last_year +1), current_year, by = 1)) %>%
      select(year, Psur)
    
    
    #select the current years probaility of survival and mutiply by number of people in class
    
    #Number of men who survived
    m_pred_value <- m_pred$Psur[n_missing] * input$n_male
    
    #Number of women who survived
    f_pred_value <- f_pred$Psur[n_missing] * input$n_female 
    
    
    #Total survived
    total_pred <- round(m_pred_value + f_pred_value, 0)
    
    m_to_join <- male %>%
      select(year, Psur)
    
    f_to_join <- female %>%
      select(year, Psur)
    
    #join together calculated probabilities to predicted probailities 
    m_plot <- union(m_to_join, m_pred) %>%
      mutate(gender = "Male")
    
    f_plot <- union(f_to_join, f_pred) %>%
      mutate(gender = "Female")
    
    plot_data <- union(f_plot, m_plot)
    
    thePlot <- ggplot(data = plot_data, aes(x = year, y= Psur * 100, colour = gender)) +
      geom_smooth() +
      scale_colour_manual("Gender", labels= c("Female", "Male"), values = c("gold", "darkgreen")) +
      theme_classic() +
      labs(title = "Percentage of expected survival since graduation", 
           x = "Year", y = "% Survival") +
      theme(legend.position = "bottom") +
      guides(color=guide_legend(override.aes=list(fill=NA))) 
    
    thePlot
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

