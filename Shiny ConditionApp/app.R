## Condition App

library(shiny)
library(dplyr)
getwd()
setwd("C:/Users/kenny/OneDrive/Programming/Personal Projects/Shiny ConditionApp")
# temp <- read.csv("data/Sample.csv")
# temp <- temp %>% select(-1)
# temp$Date <- as.Date(temp$Date)

# Define UI ----
ui <- fluidPage(
  
  ## CSS
  tags$head(
    tags$style(
      HTML('body{background-image: url("https://images.pexels.com/photos/977080/pexels-photo-977080.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=750&w=1260");}')
    )),
  
  tags$style(HTML(".irs-bar,.irs-bar-edge,.irs-single,.irs-grid-pol {background: navy;border-color: navy;}")),
  
  ## TITLE
  titlePanel(
    h1(span("CONDITION",
       style = "font-family: 'Century Gothic'; color: navy; text-align: center; letter-spacing: 5px; padding: 10px; border: 3px solid navy; display:table; margin:0 auto;"))   # --CSS--
             ), 
  
  sidebarLayout(
    position = "left",
    sidebarPanel(    # Sidebar panel
      h2("INPUT",
         style = "text-align: center; color: orange"), #--CSS--
      
      hr(style="border-top: 3px solid orange;"), # --CSS--
      
      # Date
      dateInput("Date",
                #h5("Date")
                br()
                ),
      # STATES
      hr(style="border-top: 2px dashed #bbb;"), # --CSS--
      h3("STATES"),
      sliderInput("SlideMindfulness",
                  h5("Mindfulness"),
                  min = 0,
                  max = 10,
                  value = 5
                  ),
      sliderInput("SlideJoy",
                  h5("Joy"),
                  min = 0,
                  max = 10,
                  value = 5
                  ),
      sliderInput("SlideAnxiety",
                  h5("Anxiety"),
                  min = 0,
                  max = 10,
                  value = 5
                  ),
      sliderInput("SlideProductivity",
                  h5("Productivity"),
                  min = 0,
                  max = 10,
                  value = 5
                  ),
      hr(style="border-top: 2px dashed #bbb; padding: 0px; margin: 0px;"), # --CSS--
      # VALUES
      checkboxGroupInput("CheckValues",
                         h3("VALUES"),
                         choices = list("Spiritual","Family","Social","Health","Learn")
                                  #list("Family" = 1,"Social" = 2,"Health" = 3,"Education" = 4)
                         ),
      hr(style="border-top: 2px dashed #bbb; padding: 0px; margin: 0px;"), # --CSS--
      # ACTIONS
      checkboxGroupInput("CheckActions",
                         h3("ACTIONS"),
                         choices = list("Read","Draw","Workout","Muay Thai","Skateboard","Diet","Video Games","Meditation")
                         #choices = list("Read" = 1,"Draw" = 2,"Workout" = 3,"Muay Thai" = 4,"Skateboard" = 5,"Video Games" = 6,"Meditation" = 7)
                         ),
      hr(style="border-top: 2px dashed #bbb; padding: 0px; margin: 0px;"), # --CSS--
      # NOTES
      textInput("TextNotes",
                h3("Notes"),
                value=""
                ),
      hr(style="border-top: 2px dashed #bbb; "), # --CSS--
      # SUBMIT
      actionButton("ButtonSubmit", "Submit",
                   style="background-color: orange; border: 2px solid orange; color: white;"), # --CSS--
      # Save
      actionButton("ButtonSave", "Save",
                   style="background-color: purple; border: 2px solid purple; color: white; "), # --CSS--
      
      #submit message
      h6(span(uiOutput("isSaved"), style ="color: purple;"))
      
      
      ), 
    
    mainPanel(      #main panel
      br(),
      
      h2("OUTPUT",
         style = "text-align: center; color: purple;"), #--CSS--

      
      hr(style="border-top: 3px solid purple;"), # --CSS--

      # textOutput("Date"),
      # h3("States"),
      # textOutput("Mindfulness"),
      # textOutput("Joy"),
      # textOutput("Anxiety"),
      # textOutput("Productivity"),
      # h3("Values"),
      # textOutput("Values"),
      # h3("Actions"),
      # textOutput("Actions"),
      
      uiOutput("Table",
               style="background-color: white; opacity: 0.8;"
               )
      
  ) # end of mainpanel
  ) # end of sidebarlayout

)

# Define server logic ----
server <- function(input, output) {

  # # Date
  # output$Date <- renderText({
  #   paste("Date: ", input$Date)
  # })
  # # STATES
  # output$Mindfulness <- renderText({
  #   paste("Mindfulness: ", input$SlideMindfulness)
  # })
  # output$Joy <- renderText({
  #   paste("Joy: ", input$SlideJoy)
  # })
  # output$Anxiety <- renderText({
  #   paste("Anxiety: ", input$SlideAnxiety)
  # })
  # output$Productivity <- renderText({
  #   paste("Productivity: ", input$SlideProductivity)
  # })
  # # Values
  # output$Values <- renderText({
  #   toString(input$CheckValues)
  # })
  # # Actions
  # output$Actions <- renderText({
  #   toString(input$CheckActions)
  # })
  
  # Generate Data
  Data <- reactive({
    if (input$ButtonSubmit >0){ # if submit is true wonder if == True works here too
      df <- data.frame(Date=input$Date, 
                       State.Mindfulness=input$SlideMindfulness, 
                       State.Joy=input$SlideJoy, 
                       State.Anxiety=input$SlideAnxiety, 
                       State.Productivity=input$SlideProductivity, 
                       Values=toString(input$CheckValues), 
                       Actions=toString(input$CheckActions),
                       Notes=toString(input$TextNotes)
                       )
      
      return(list(df=df))
    }
  })
   
  ## Create list of dataframes for existing data, set to null
  PrevData <- list(df=NULL)
  
  ## Import previous data if it exists in the wd
   if(file.exists("data/ConditionData.csv") == T){
     appdata <- read.csv("data/ConditionData.csv")
     appdata <- appdata %>% select(-1)
     appdata$Date <- as.Date(appdata$Date)
     # update list of dataframes with appdata if it exists
     PrevData$df <- appdata                  
   }

  
  ## Output new data appended with previous data
  output$Table <- renderTable({
    if(is.null(Data())){
      return()
    } else if(is.null(PrevData$df)){
        temp <- Data()$df
        temp$Date <- as.character(temp$Date)
        print(tail(temp))
        #print(Data()$df)
    } else {
        temp <- rbind(PrevData$df, Data()$df)
        temp$Date <- as.character(temp$Date)
        print(tail(temp))
        #print(rbind(PrevData$df, Data()$df))
      }
  })
  
  output$isSaved <- renderText({"NOT SAVED"})
  
  
  ## Save file
  observeEvent(input$ButtonSave,{
    if(is.null(Data())){
      return()
    } else if(is.null(PrevData$df)){
      #write.csv(Data()$df, paste(getwd(),"/data/sample.csv", sep=""))
      write.csv(Data()$df, paste(getwd(),"/data/ConditionData.csv", sep=""))
      output$isSaved <- renderText({"SAVED"})
    } else {
      #write.csv(rbind(PrevData$df, Data()$df), paste(getwd(),"/data/sample.csv", sep=""))
      write.csv(rbind(PrevData$df, Data()$df), paste(getwd(),"/data/ConditionData.csv", sep=""))
      output$isSaved <- renderText({"SAVED"})
    }
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
