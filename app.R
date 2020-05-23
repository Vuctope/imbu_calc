options(scipen = 9999)

library(shiny)
library(data.table)
source("item_list.R")
source("functions.R")
imbu_levels <- c("Basic", "Intricate", "Powerful")

imbu_tab <- prep_imbu_tab()
imbu_tab[category %in% c("crit", "paral", "cap", "speed"), category := "other"]

imbu_categories <- unique(imbu_tab[,.(name, category)])

imbu_select_list <- split(x = imbu_categories$name, imbu_categories$category )

# Define UI for application that draws a histogram
ui <- fluidPage(theme ="bootstrap.css",
    
    titlePanel("Imbuement price"),
    
    fluidRow(width = 12,
             tags$head( tags$style(HTML(" .selectize-input {  min-width: 150px; padding-top: 5px;} ")) ),
             fluidRow(
                 column(12, 
                        numericInput(inputId = "price_token",
                                     label = "Gold Token Price:", 
                                     value = 30000,
                                     step = 1),
                 ),
                 column(12,
                        selectizeInput("s_level",
                                       label = "Select level:",
                                       choices = imbu_levels,
                                       selected = imbu_levels[length(imbu_levels)])
                 ),
                 column(12,
                        #         tags$head(
                        #             tags$style(type="text/css", "label{ display: table-cell; text-align: left; vertical-align: middle; margin-top: 25px;} 
                        # .form-group { display: table-row;}")
                        #         ),
                        selectizeInput(
                            inputId = "s_imbu",
                            label = "Select imbuent:",
                            choices = imbu_select_list)
                 )
                 
             ),
             fluidRow(
                 column(12,
                        uiOutput(outputId = "ui_items")
                 )
             ),
             fluidRow(
                 column(4,
                        actionButton(inputId = "but_submit", label = "Oblicz", icon = icon("calculator")))
             ),
             
             fluidRow(
                 column(12,
                        textOutput(outputId = "result")
                 )
             )
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    rows.selected <- eventReactive(input$s_imbu, {
        imbu_tab[name == input$s_imbu]
    })
    
    cost.list <- reactive({
        list(Basic_option_0t = input$quant_1 * input$price_1,
             Basic_option_2t = 2 * input$price_token,
             Intricate_option_0t = input$quant_1 * input$price_1 + 
                 input$quant_2 * input$price_2,
             Intricate_option_2t = 2 * input$price_token + 
                 input$quant_2 * input$price_2,
             Intricate_option_4t = 4 * input$price_token,
             Powerful_option_0t = input$quant_1 * input$price_1 +
                 input$quant_2 * input$price_2 + 
                 input$quant_3 * input$price_3,
             Powerful_option_2t = 2 * input$price_token +
                 input$quant_2 * input$price_2 +
                 input$quant_3 * input$price_3,
             Powerful_option_4t = 4 * input$price_token +
                 input$quant_3 * input$price_3,
             Powerful_option_6t = 6 * input$price_token
        )
        
    })
    
    
    output$result <- eventReactive(input$but_submit, {
        lvl <- input$s_level
        
        list_of_results <- names(which.min(cost.list()[names(cost.list()) %like% lvl]))
        
        if(lvl == "Basic"){
            if(list_of_results == "Basic_option_0t"){
                sprintf("Kup %dx %s",
                        input$quant_1, rows.selected()[stage == 1]$item)                
            }else if(list_of_results == "Basic_option_2t"){
                sprintf("Kup 2x Gold Tokens")
            }
        }else if(lvl == "Intricate"){
            if(list_of_results == "Intricate_option_0t"){
                sprintf("Kup %dx %s i %dx %s zamiast 4x Gold Tokens",
                        input$quant_1, rows.selected()[stage == 1]$item,
                        input$quant_2, rows.selected()[stage == 2]$item)                
            }else if(list_of_results == "Intricate_option_2t"){
                sprintf("Kup %dx %s i 2x Gold Tokens",
                        input$quant_2, rows.selected()[stage == 2]$item)
            }else if(list_of_results == "Intricate_option_4t"){
                sprintf("Kup 4x Gold Tokens")
            }
        }else{
            if(list_of_results == "Powerful_option_0t"){
                sprintf("Kup %dx %s,  %dx %s,  %dx %s zamiast 6x Gold Tokens",
                        input$quant_1, rows.selected()[stage == 1]$item,
                        input$quant_2, rows.selected()[stage == 2]$item,                
                        input$quant_3, rows.selected()[stage == 3]$item)                
            }else if(list_of_results == "Powerful_option_2t"){
                sprintf("Kup %dx %s i %dx %s i 2x Gold Tokens",
                        input$quant_2, rows.selected()[stage == 2]$item,
                        input$quant_3, rows.selected()[stage == 3]$item)
            }else if(list_of_results == "Powerful_option_4t"){
                sprintf("Kup %dx %s i 4x Gold Tokens",
                        input$quant_3, rows.selected()[stage == 3]$item)
            }else if(list_of_results == "Powerful_option_6t"){
                sprintf("Kup 6x Gold Tokens")
            }
        }
        
    })
    
    output$ui_items <- renderUI({
        
        fluidPage(
            tags$head( tags$style(HTML(" .numeric-input {  margin-top: 150px;} ")) ),
            # h1(sprintf("Items for %s",  input$s_imbu)),
            fluidRow(
                column(4,
                       HTML(paste0("<b>",rows.selected()[stage == 1]$item,"</b>")),
                       fluidRow(
                           column(5,
                                  numericInput(inputId = "quant_1", label = "Quantity", value = rows.selected()[stage == 1]$number_of),
                           ),
                           column(1,HTML("X")),
                           column(5,
                                  numericInput(inputId = "price_1", label = "Price", value = rows.selected()[stage == 1]$price, step = 1) 
                           )
                       )
                       
                ),
                column(4, 
                       numericInput(inputId = "quant_2", label = rows.selected()[stage == 2]$item, value = rows.selected()[stage == 2]$number_of),
                       numericInput(inputId = "price_2", label = "Price:", value = 1000, step = 1)
                ),
                column(4,
                       numericInput(inputId = "quant_3", label = rows.selected()[stage == 3]$item, value = rows.selected()[stage == 3]$number_of),
                       numericInput(inputId = "price_3", label = "Price:", value = 1000, step = 1)
                )
                
            ),
        )
        
    })
    prices <- reactiveValues(price_1 = 0, price_2 = 0, price_3 = 0)
    
    observeEvent(input$but_submit, {

        update_price(tab = imbu_tab, imbu = input$s_imbu, imbu_stage = 1, new_price = input$price_1)
        
        # updateNumericInput(inputId = "price_1", value = prices$price_1)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
