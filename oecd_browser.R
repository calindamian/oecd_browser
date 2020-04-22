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
library(shinycssloaders)

# helper functions
'%ni%' <- Negate('%in%')

oecd_recode_data = function (ds , tb , colname = "id") {
  
  #recode values
  cols = colnames(tb) %>%
    intersect( ds$VAR_DESC[[colname]])
  map ( cols , function (col) {
    dict =c ()
    dict[ds[[col]]$id] = ds[[col]]$label
    
    tb <<- tb %>%
      mutate_at( col , function (x ) {
        recode(x , !!!dict)}  )
  }
  )
  
  #rename columns
  cols_rename = ds$VAR_DESC %>% 
    filter (id %in% colnames(tb))
  tb %>% 
    rename_at(vars(matches (cols_rename$id) ) , funs (cols_rename$description ) )
}


#datasets 
tb_oecd_ds =        search_dataset("*") %>%
                            arrange(title)
                
#ds codes

ds_codes = c ("KEI" , "CSPCUBE" , "HIGH_AGLINK_2019" , "SHA" , "HEALTH_STAT" ,
              "GOV_DEBT" , "MEI_FIN" , "GOV_2019" , "RS_GBL" , "SNA_TABLE1")

tb_oecd_ds = tb_oecd_ds %>%
              filter (id %in%  ds_codes)

font_family = "font-family: \"Trebuchet MS\",  Helvetica, sans-serif;"

# Define UI for application that draws a histogram
ui <- fluidPage(

    tags$head(
            tags$style(str_replace_all("body {#font_family# background-color: #F4F4F4;}
                                       .selectize-input {font-size:12px}
                                       .shiny-input-container { margin-top:-10px}" # controls spacing beteen controls
                                        , "#font_family#" , font_family )
            )
    ) ,
   
  
    
    useShinydashboard(),
    fluidRow( column( width =11 ,
                  box (width = 12 ,
                      div ( style = "background-color:#4292c6;color:white;" ,
                     titlePanel("OECD Data Browser") ,
                     "Data Source: " , 
                     #style = str_c ( font_family , "font-size: 10px;line-height:150%;") ,
                     tags$a(style = "color:white;", href="https://data.oecd.org/", "https://data.oecd.org/" , target="_blank") ) ) ) ) ,
    # Sidebar with a slider input for number of bins 
    fluidRow(   
 
       column( width = 3 ,
          
          box(width = 12 , 
              title = "Select Data Set" ,
              solidHeader = TRUE,
              status = "primary" ,
              selectInput("oecd_ds", label = "Dataset",  choices =tb_oecd_ds$title ) ) ,
          
          tabBox(
             width = 12 ,
             tabPanel(h5 ("Browse structure"),  
                      selectInput ("oecd_ds_str", label = "Dataset Structure", choices =c ("")) ,
                      dataTableOutput("tb_oecd_ds_str" ) %>% withSpinner()
                      
                      ) ,
             
             tabPanel(h5 ("Inputs") , 
                      uiOutput("oecd_params") ,
                      tags$label(class = "control-label" ,
                                 "R Command") ,
                      verbatimTextOutput ("oecd_get_data_command" ) ,
                      #verbatimTextOutput ("time_slider" ) ,
                      actionButton("goButton", "Get Data!")      
                      
                    )                
              )
          ),

        # Show a plot of the generated distribution
       column( width = 8,

                    box(width = 12 ,
                          title = "OECD data" ,
                          solidHeader = TRUE,
                          status = "primary" ,
                         fluidRow(
                          column(width = 3 , downloadButton("download1","Download data as csv" ) ) ,
                          column (width = 3 , offset = 0, checkboxInput("last_tb_oecd_data", "Latest data available", FALSE) )
                          ),
                          dataTableOutput("tb_oecd_data") %>% withSpinner()
                    
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
      
      command = str_c( "get_dataset(" , "'" , ds_code, "'" , "," , filter ,  ")")
      
      if (!is.null( input$time_slider ) ) {
        time_filter = str_c ("start_time = " ,input$time_slider[1] ,", end_time= ", input$time_slider[2]) 
        command = str_c( "get_dataset(" , "'" , ds_code, "'" , "," , filter , "," , time_filter , ")")
      }

      command
      
      
    })   
    
    tb_oecd_data = eventReactive( input$goButton , {

      ds = tb_selected_ds_str ()
      
      tb_expr = rlang::parse_expr(str_command_exec ())
      
      tb = tryCatch ( { 
                        rlang::eval_tidy(tb_expr)
                        },  
                        error=function(theError) {
                          print (  str_c ("get_dataset : " , theError$message) )
                          return(tibble()) 
                        }
                        )
      
      #print (str_c ("tb_oecd_data :" , now() ))
      oecd_recode_data(ds , tb)
      
    }
    )
    
    tb_display = eventReactive( c (tb_oecd_data (), input$last_tb_oecd_data) ,{
      
      tb = tb_oecd_data ()
      #get latest available datapoints
      if (input$last_tb_oecd_data) {
        
        group_cols = colnames(tb) %>%
          setdiff( c("obsTime" , "obsValue" , "Observation Status") )
        
        tb = tb %>%
          arrange(desc (obsTime))  %>%
          group_by_at(group_cols) %>%
          filter ( row_number() == 1)%>%
          arrange(Country)
        
      }
      
      #print (str_c ("tb_display :" , now() ))
      
      tb
      
    } )
    
    str_command_exec = eventReactive( input$goButton , {
      str_data_command_selected ()
    
    })
    
    
    observeEvent(input$oecd_ds , {
      
        code = tb_selected_oecd_ds () %>%
                select (id) %>%
                pull%>%
                as.character()
      
        ds = tb_selected_ds_str ()
        choices  = ds$VAR_DESC %>%
                      filter ( id != "OBS_VALUE") %>%
                      select (description) %>%
                      pull
        
        updateSelectInput(session , "oecd_ds_str" ,
                          choices = choices,
                          label = str_c ("Dataset Code: " , code )
                          )
    })
    

    
    output$oecd_ds_code = renderText(
      str_c( "Data set code: " ,  tb_selected_oecd_ds () %>%
                                select (id) %>%
                                pull%>%
                                as.character() )
    )
    
    output$tb_oecd_ds_str = renderDataTable( {
      
      ds = tb_selected_ds_str ()
      
      selected_var = ds$VAR_DESC %>%
                      filter (description %in% input$oecd_ds_str )%>%
                      select (id) %>%
                      pull
        
      datatable (
                  ds [[selected_var]] ,
                  options = list(pageLength = 5 , 
                                 autoWidth = T ,
                                 initComplete = htmlwidgets::JS(
                                   "function(settings, json) {",
                                   paste0("$(this.api().table().container()).css({'font-size': '10px'});"),
                                   "}") ) ,
                  rownames= F
      ) 
    })
    
    output$oecd_get_data_command = renderText( { 
      
      str_c("library (OECD)" , "\n" ,
                str_data_command_selected () )
      #print ( tb_params () )
    })
    
    output$tb_oecd_data = renderDataTable( {
      

      
      
       datatable(
                      #tb_oecd_data (),
                      tb_display (),
                      extensions = c('Scroller'),
                      callback = JS("$('div.dwnld').append($('#download1'));"),
                      options = list( 
                        #dom = 'B<"dwnld">frtip', #'B<"dwnld">frtip'
                        initComplete = htmlwidgets::JS(
                        "function(settings, json) {",
                        paste0("$(this.api().table().container()).css({'font-size': '12px'});"),
                        "}") ,
                                  
                        scrollX=TRUE
                      ),
                      rownames= F
                      
                      )
  
    
      })
    
    output$oecd_params = renderUI ( {
      
      #depends on order of params . generates inputs for n params where n >= 1 to n = max time param
      
      # get params till time row is reached
      ds =  tb_selected_ds_str ()
      
      inputs = tb_params () %>% 
              select (id , description)%>% 
              pmap( function (id , description) {
                
                  choices = ds[[id]] %>% 
                              select (label)  %>% 
                              pull
                  
                  selectInput (id , label = description, choices =choices , multiple = T)
              
                })
            

      #add time slider if time dimension is available
      slider =  ds$VAR_DESC%>% 
         filter (description %in% c ("Year", "Time" , "Time period") )%>% 
          pmap( function (id , description) {
            sliderTextInput ("time_slider",label = description,
                             choices = ds[[id]]$id ,
                             selected = range (ds[[id]]$id) )
            
          })
                 
      
      inputs %>% 
        append ( slider)
      
    })
    
    output$download1 <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(tb_oecd_data (), file)
      }
    )
    
    output$time_slider = renderPrint( {
      print( nrow( tb_display () ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
