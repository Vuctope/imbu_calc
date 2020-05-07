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
ui <- fluidPage(
    
    titlePanel("Imbuement price"),
    
    fluidRow(width = 12,
             tags$head( tags$style(HTML(" .selectize-input {  min-width: 150px; padding-top: 5px;} ")) ),
             fluidRow(
                 column(6,
                        tags$head(
                            tags$style(type="text/css", "label{ display: table-cell; text-align: left; vertical-align: middle; margin-top: 25px;} 
                .form-group { display: table-row;}")
                        ),
                        selectizeInput(
                            inputId = "s_imbu",
                            label = "Select imbuent:",
                            choices = imbu_select_list),
                        selectizeInput("s_level",
                                       label = "Select level:",
                                       choices = imbu_levels,
                                       selected = imbu_levels[length(imbu_levels)])
                 ),
                 column(6, 
                        h3("Gold Token Price:"),
                        fluidRow(column(6,numericInput(inputId = "price_token", label = "Price:", value = 30000, step = 1)))
                 )
             ),
             hr(),
             fluidRow(
                 column(12,
                        uiOutput(outputId = "ui_items")
                 )
             )
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    
    output$ui_items <- renderUI({
        lvl <- switch(input$s_level,
                      Basic=1,
                      Intricate=2,
                      Powerful=3)
        
        
        
        rows.selected <- imbu_tab[name == input$s_imbu]
        
        # Musze to ogarnac - jak zrobic porownanie w prostyi przejrzysty sposob?
        
        # Zle rozwiazanie - aktualizuje sie dopiero po wyborze poziomu :(. Serio. Nie powinienem wybierac poziomu, tylko wyswietlac dla kazdego uzupelnionego (price > 0)
        # dt <- eventReactive(input$s_level,{
        #     dt_out_choice <- data.table(`Lp.` = 1,
        #                                 sum_price = input$quant_1 * input$price_1,
        #                                 token_price = input$price_token * 1 * 2)
        #     if(lvl > 1){
        #         for(i in 2:lvl){
        #             dt_out_choice <- rbindlist(l = list(dt_out_choice,
        #                                                 eval(
        #                                                     parse(
        #                                                         text = stringr::str_interp("data.table(`Lp.` = ${i},
        #                                                                                      sum_price = input$quant_${i} * input$price_${i},
        #                                                                                      token_price = input$price_token * ${i} * 2)")
        #                                                     )
        #                                                 )
        #             )
        #             )
        #             
        #         }
        #     }
        #     dt_out_choice[, cum_price := cumsum(sum_price)]
        #     dt_out_choice
        #     
        # })
        dt <- reactive({
            dt_out_choice <- data.table(`Lp.` = c(1:3),
                                        sum_price = c(input$quant_1 * input$price_1,
                                                      input$quant_2 * input$price_2,
                                                      input$quant_3 * input$price_3
                                        ),
                                        token_price = c(input$price_token * 1 * 2,
                                                        input$price_token * 2 * 2,
                                                        input$price_token * 3 * 2))
            dt_out_choice[, cum_price := cumsum(sum_price)]
            dt_out_choice
            
        })
        
        # Basic
        output$choice_1 <- renderText({
            if(dt()$cum_price[1] >= dt()$token_price[1]){
                sprintf("Kup 2x Gold Tokens")
            }else{
                sprintf("Kup %dx %s zamiast 2x Gold Tokens",  input$quant_1, rows.selected[stage == 1]$item)
            }
            
            
        })
        # Intricate
        output$choice_2 <- renderText({
            if(dt()$cum_price[2] >= dt()$token_price[2]){
                if(dt()$sum_price[2] < dt()$token_price[2]/2){
                    sprintf("Kup 2x Gold Tokens i %dx %s",
                            input$quant_2, rows.selected[stage == 2]$item)
                }else{
                    sprintf("Kup 4x Gold Tokens")
                }
            }else{
                if(dt()$sum_price[1] > dt()$token_price[1]){
                    sprintf("Kup 2x Gold Tokens i %dx %s",
                            input$quant_2, rows.selected[stage == 2]$item)
                }else{
                    sprintf("Kup %dx %s i %dx %s zamiast 4x Gold Tokens",
                            input$quant_1, rows.selected[stage == 1]$item,
                            input$quant_2, rows.selected[stage == 2]$item)
                }
            }
        })
        
      
        
        fluidPage(
            tags$head( tags$style(HTML(" .numeric-input {  margin-top: 150px;} ")) ),
            h1(sprintf("Items for %s %s", input$s_level, input$s_imbu)),
            
            fluidRow(
                column(4,offset = 1, 
                       h3(rows.selected[stage == 1]$item),
                       numericInput(inputId = "quant_1", label = "Quantity", value = rows.selected[stage == 1]$number_of),
                       numericInput(inputId = "price_1", label = "Price:", value = 0, step = 1)
                ),
                column(6, offset = 1,
                       textOutput(outputId = "choice_1")
                       
                )
                
                
                
            ),
            hr(),
            conditionalPanel("input.s_level != 'Basic'",
                             fluidRow(
                                 column(4, offset = 1,
                                        h3(rows.selected[stage == 2]$item),
                                        numericInput(inputId = "quant_2", label = "Quantity:", value = rows.selected[stage == 2]$number_of),
                                        numericInput(inputId = "price_2", label = "Price:", value = 1000, step = 1)
                                 ),
                                 column(6, offset = 1,
                                        textOutput(outputId = "choice_2")
                                 )
                             ),
            ),
            conditionalPanel("input.s_level == 'Powerful'",
                             fluidRow(
                                 column(4, offset = 1,
                                        h3(rows.selected[stage == 3]$item),
                                        numericInput(inputId = "quant_3", label = "Quantity:", value = rows.selected[stage == 3]$number_of),
                                        numericInput(inputId = "price_3", label = "Price:", value = 1000, step = 1)
                                 ),
                                 column(6, offset = 1,
                                        textOutput(outputId = "choice_3")
                                 )
                             ),
            )
        )
    })
    # prices <- reactiveValues(price_1 = 0, price_2 = 0, price_3 = 0)
    # 
    # 
    # if(isolate(prices$price_1) != isolate(input$price_1)){
    #     prices$price_1 <- isolate(input$price_1)
    #     update_price(tab = imbu_tab, imbu = input$s_imbu, imbu_item = input$s_level, new_price = isolate(prices$price_1))
    # }
    # updateNumericInput(inputId = "price_1", value = isolate(input$price))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
