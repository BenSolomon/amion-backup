library(shiny); library(tidyverse); library(DT)
library(lubridate); library(shinyBS); library(shinythemes)
library(gsheet)

source("helper.R")
# # Data
# amion_dir <- "/home/ben/amion"
# 
# available_schedules <- tibble(files = list.files(sprintf("%s/schedules", amion_dir), full.names = T)) %>%
#   arrange(desc(files)) %>% 
#   separate(files, into = c("schedules"), sep = "\\.", extra = "drop", remove = F) %>% 
#   separate(schedules, into = c(NA, NA, "date", "time"), sep = "_") %>% 
#   mutate(datetime = as.character(ymd_hms(paste(date, time, sep = "_"), tz = "America/Los_Angeles"))) %>% 
#   mutate(datetime_string = paste(date, time, sep = "_")) %>% 
#   arrange(datetime)
#   
# first_schedule <- available_schedules %>% head(1) %>% pull(datetime)
# current_schedule <- available_schedules %>% tail(1) %>% pull(datetime)
# all_schedules <- available_schedules %>% pull(datetime)
# 
# names <- read_rds(list.files(sprintf("%s/schedules", amion_dir), full.names = T)[1]) %>% 
#   pull(Staff_Name) %>% 
#   unique() %>% 
#   sort()
# 
# change_points <- read_csv(sprintf("%s/changes/stpeds_changes.csv", amion_dir)) %>% 
#   mutate(change_detected = as.character(change_detected)) %>% 
#   pull(change_detected) %>% 
#   unique()
# 
# name_key <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1U-oSLmTJ2m0nGTCLrmNaq6X8H18sLoVorT1JrpKGOwU') %>% 
#   select(names = `Name (don't change format)`, 
#          email = `Stanford email`, 
#          res_id = `Submit ID`,
#          send_email = `Do you want email alerts of schedule changes?`) %>% 
#   mutate(res_id = toupper(res_id)) %>% 
#   right_join(tibble(names = names))

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  tags$head(includeHTML("google-analytics.js")),
  
  titlePanel("Amion Schedule Changes"),
  
  sidebarLayout(
    sidebarPanel(width = 2,
                # # Resident selection widget
                # # Not used when resident is selected by code rather than name
                #  selectInput("resident", label = "Resident",
                #              choices = c("All", names)),
                
                 # Resident code input widget
                 tags$style(".form-group {margin-bottom:5px;"),
                 div(style="margin:0px;",textInput("res_code",
                           label = "Resident ID code",
                           placeholder = "Enter text...")),
                 
                # Resident code submission button
                 tags$style("#submitButton {padding:5px;"),
                 actionButton("submitButton", "Submit", width = "100%"),                
                
                # JS to allow button click with return-key
                  tags$script(HTML(
                    "$(document).keyup(function(event) {
                      if ($('#res_code').is(':focus') && (event.key == 'Enter')) {
                        $('#submitButton').click();
                      }
                    });"
                  )), 
                
                
                 # Collapsible resident code reminder instructions
                 tags$style("#panel {margin-bottom:15px;padding:5px;"),
                 bsCollapse(id = "panel",
                   bsCollapsePanel(
                     title = div(icon("bars"),HTML('&nbsp;'), "ID code reminder"),
                     div(
                       tags$u("ALL CAPS"),
                       tags$ol(type="1", style="padding-left:15px;",
                         tags$li("First/Last initial"),
                         tags$li("First 2 digits of childhood street number"),
                         tags$li("First 2 letters of childhood street name")),
                       "E.g. Barack Obama 1600 Pennsylvania Ave ==>",
                       "BO16PE"
                       )
                   )
                 ),
                 

                 
                 # Change display selection widget
                 # Changes which schedule update period is displayed
                 selectInput("changes", label = "Display changes from",
                             choices = c("All", rev(change_points))),
                 
                 # Collapsable schedule download widget
                 p(style="font-weight:bold;", "Downloads"),
                 bsCollapse(
                   bsCollapsePanel(
                     title = div(icon("bars"),HTML('&nbsp;'), "Download past schedules"),
                     value = NULL, 
                     selectInput("schedule_date", label = "Schedule Date",
                                 choices = c(all_schedules)),
                     downloadButton('downloadData', 'Download')
                   )
                 )
    ),
    mainPanel(
      
      # Render table
      # fluidRow(column(12, DT::dataTableOutput("mytable"))),
      fluidRow(column(12, h3(textOutput("res_text")))),
      fluidRow(column(12, uiOutput("ui"))),
      
      # Render update stats
      fluidRow(column(12, div(style="
                                        border:solid;
                                        background-color:#f5f7fa;
                                        width: -moz-fit-content;
                                        width:fit-content;
                                        float:left;
                                        border-radius: 15px",
                              div(style = "text-align:left; padding:5px 10px 5px 10px;",
                                        "First schedule archived at: ", first_schedule, br(), "Most recent schedule archived at: ", current_schedule))))
      
    )
  )
)


server <- function(input, output) {
  source("helper.R")
  # Set res_code with button
  res_id <- eventReactive(input$submitButton, {
    toupper(input$res_code)
  })
  
  # Find res_name using res_code
  res_name <- eventReactive(input$submitButton, {
    if(res_id() == master_code){
      "All"
    } else {
      name_key %>% 
        filter(res_id == res_id()) %>% 
        pull(names)
    }
  })
  
  # Render text to display which resident is active
  output$res_text <- renderText({
    sprintf("Schedule changes for: %s", res_name())
    })
  
  # Read change data table and filter for resident
  # Triggered by submitButton
  resident_table <- eventReactive(input$submitButton, {
    df <- read_csv(changes_path) %>% 
      mutate(timestamp = as.character(timestamp), change_detected = as.character(change_detected)) %>% 
      select(Resident=Staff_Name, Shift_Date=Date, Start_Time, End_Time, Shift_Name, Time_Archived=timestamp, Edit_Status=edited, change_detected) %>% 
      arrange(desc(change_detected), Resident, Shift_Date, Edit_Status) 
    if(res_name() == "All"){
      df <- df
    } else {
      df <- df %>% filter(Resident == res_name())
    }
    df
  })
  
  # Filter change data table based on selected change point
  # Regardless of submitButton
  resident_time_table <- reactive({
    if(input$changes == "All"){
      df <- resident_table()
    } else {
      df <- resident_table() %>% filter(change_detected == input$changes)
    }
    df
  })
  
  # Create formated datatable for output
  output$mytable = DT::renderDataTable({
    datatable(
      resident_time_table(),
      colnames = c("Resident", "Shift Date", "Start Time", "End Time", "Shift Name", "Time Archived", "Edit Status", "Change Detected"),
      rownames = FALSE,
      class="cell-border stripe",
      # class = "cell-border",
      extensions = "RowGroup",
      options = list(dom = "tlip",
                     ordering = F,
                     rowGroup = list(dataSrc=c(7)),
                     columnDefs = list(
                       list(className = 'dt-center', targets = 0:7),
                       list(targets = 7, visible = F)),
                     oLanguage = list(sEmptyTable = "No changes since first archived schedule")
      )
    )
  })
  
  # Create df for download
  download_df <- reactive({
    read_rds(available_schedules %>% 
               filter(datetime == input$schedule_date) %>% 
               pull(files)
    ) %>% 
      select(Resident=Staff_Name, Shift_Date=Date, Start_Time, End_Time, Shift_Name, Time_Archived=timestamp) %>%
      filter(Resident == res_name())
  })
  
  # Create filename for df download
  download_name <- reactive({
    resident <- gsub(" |, ", "_", res_name())
    timestamp <- available_schedules %>% filter(datetime == input$schedule_date) %>% pull(datetime_string)
    sprintf("amion_%s_%s.csv", resident, timestamp)
  })
  
  # Download command
  output$downloadData <- downloadHandler(
    filename = function(){download_name()},
    content = function(file) {write.csv(download_df(), file, row.names = FALSE)})
  
  # Reactive UI element
  # Hides table until a valid resident code is entered
  # Displays error if invalid code
  output$ui <- renderUI({
    if (res_id() %in% c(name_key$res_id, master_code)){
      DT::dataTableOutput("mytable")
    } else {
      h3(style="color:red;","Please submit a valid Resident ID")
    }
  })

}

shinyApp(ui = ui, server = server)
