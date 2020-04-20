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
library(OECD)
library (DT)
library(rlang)
library(shinyWidgets)
library(shinydashboard)


#datasets 
tb_oecd_ds =        search_dataset("*") %>%
                            arrange(title)


font_family = "font-family: \"Trebuchet MS\",  Helvetica, sans-serif;"

# Define UI for application that draws a histogram
ui <- fluidPage(

    tags$head(
            tags$style(str_replace_all("body {#font_family# background-color: #F4F4F4;}"
                              , "#font_family#" , font_family )
            )
    ) ,
    titlePanel("OECD Data Browser") ,
    div ("Data Source: " , 
         #style = str_c ( font_family , "font-size: 10px;line-height:150%;") ,
         tags$a(href="https://data.oecd.org/", "https://data.oecd.org/" , target="_blank") ) ,
    
    useShinydashboard(),
    # Sidebar with a slider input for number of bins 
    fluidRow(   
       column( width = 4 ,
          
          box(width = 12 , 
              title = "Select Data Set" ,
              
              selectInput("oecd_ds", label = "Dataset",  choices =tb_oecd_ds$title) ) ,
          
          box (width = 12 , 
               title = "Available Inputs" ,
               uiOutput("oecd_params") ,
               tags$label(class = "control-label" ,
                            "R Command") ,
               verbatimTextOutput ("oecd_get_data_command" ) ,
               actionButton("goButton", "Get Data!")            
               )
          
          
          ),

        # Show a plot of the generated distribution
       column( width = 8 ,
                    box(width = 12 ,
                     title = "Browse data structure" ,
                     #verbatimTextOutput("oecd_ds_code") ,
                     selectInput ("oecd_ds_str", label = "Dataset Structure", choices =c ("")) ,
                      dataTableOutput("tb_oecd_ds_str" ) 
                     ) ,
                    
                    box(width = 12 ,
                          title = "OECD data" ,
                          dataTableOutput("tb_oecd_data") 
                    
                    )
       )
       
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    #reactives
    tb_selected_oecd_ds = reactive(
        tb_oecd_ds %>%
            filter (title %in% c (input$oecd_ds))
    )
    
    tb_selected_ds_str = reactive(
        get_data_structure (tb_selected_oecd_ds () %>%
                              select (id) %>%
                              pull)
    )
    
    tb_params = reactive( {
      
              ds = tb_selected_ds_str ()
            
              ds$VAR_DESC%>% 
              mutate(rn = row_number() ,
                     rn_time =   max (case_when( 
                       description %in% c ("Year", "Time" , "Time period") ~ rn ,
                       TRUE ~ as.integer(0)
                       
                     ) 
                     ) )%>% 
              filter (rn < rn_time)
            
      
    })
    
    str_data_command = reactive(  {
      
      ds =  tb_selected_oecd_ds () %>%
        select (id) %>%
        pull%>%
        as.character() 
      
      location =        input$oecd_ds_location %>% 
        str_c(collapse = "','") %>% 
        str_c ("'" , . , "'") %>% 
        str_c ("c (" , . ,")")
      
      trans =  input$oecd_ds_transaction %>% 
        str_c(collapse = "','") %>% 
        str_c ("'" , . , "'") %>% 
        str_c ("c (" , . ,")") 
      
      
      str_c( "get_dataset(" , "'" , ds, "'" , 
             ",filter = list (" , location , "," , trans, ")"  ,
             ", start_time = 2012 , end_time = 2020)")
      
    })
    
    str_data_command_selected = reactive(  {
      
      ds = tb_selected_ds_str ()
      
      ds_code =  tb_selected_oecd_ds () %>%
        select (id) %>%
        pull%>%
        as.character() 
      
      # recuperate filter for each input in tb_params ()
      filter = "filter = list ("
      
      tb_params () %>% 
        select (id , description)%>% 
        mutate (is_last = ifelse(row_number() == n() , T , F))  %>% 
        pmap( function (id , description , is_last) {
          
              selected_labels = input[[id]]
              
              selected_codes =  ds[[id]] %>% 
                                filter(label %in% selected_labels)%>% 
                                select (id) %>% 
                                pull 
                
              param =  selected_codes %>% 
                str_c(collapse = "','") %>% 
                str_c ("'" , . , "'") %>% 
                str_c ("c (" , . ,")")
                filter <<- str_c(filter ,param , ifelse(is_last ,"" , ","))
        })
      
      filter =  str_c(filter , ")")
      
      str_c( "get_dataset(" , "'" , ds_code, "'" , "," , filter , ")")
      
    })   
    
    str_command_exec = eventReactive( input$goButton , {
      str_data_command_selected ()
    })
    
    observeEvent(input$oecd_ds , {
      
        code = tb_selected_oecd_ds () %>%
                select (id) %>%
                pull%>%
                as.character()
      
        updateSelectInput(session , "oecd_ds_str" ,
                          choices = names(tb_selected_ds_str ()) ,
                          label = str_c ("Dataset " , code , " structure")
                          )
    })
    

    
    output$oecd_ds_code = renderText(
      str_c( "Data set code: " ,  tb_selected_oecd_ds () %>%
                                select (id) %>%
                                pull%>%
                                as.character() )
    )
    
    output$tb_oecd_ds_str = renderDataTable( {
        
      datatable (
                  tb_selected_ds_str ()[[input$oecd_ds_str]] ,
                  options = list(pageLength = 5) ,
                  rownames= F
      )
    })
    
    output$oecd_get_data_command = renderText( { 
      
      str_c("library (OECD)" , "\n" ,
                str_data_command_selected () )
      #print ( tb_params () )
    })
    
    output$tb_oecd_data = renderDataTable( {
      
       str = str_command_exec ()
      
       tb_expr = rlang::parse_expr(str)
       
       datatable(
                      rlang::eval_tidy(tb_expr) ,
                      extensions=c('Scroller') ,
                      options = list(
                                  
                                     scrollX=TRUE
                      ),
                      rownames= F
                      
                      )
  
    
      })
    
    output$oecd_params = renderUI ( {
      
      #depends on order of params . generates inputs for n params where n >= 1 to n = max time param
      
      # get params till time row is reached
      ds =  tb_selected_ds_str ()
      
      tb_params () %>% 
        select (id , description)%>% 
        pmap( function (id , description) {
          
            choices = ds[[id]] %>% 
                        select (label)  %>% 
                        pull
            
            selectInput (id , label = description, choices =choices , multiple = T)
        
          })
      

      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
