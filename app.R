library(shiny)
library(httr)
library(stringr)

ui <- fluidPage(
  div(
    titlePanel("R Shiny Chatbot backend by ChatGPT"),
    style = "color: white; background-color: #45f3ae"
  ),
  sidebarLayout(
    sidebarPanel(
      p("This app allows you to chat with an OpenAI GPT model. Key in your OpenAI API key below."),
      textInput("api_key", "API Key", "sk-INSERT_YOUR_OWN_OPENAI_API_KEY_HERE"),
      tags$p("Main Reference:", 
             tags$a(href = "https://github.com/tolgakurtuluss/shinychatgpt", target="_blank", "shinychatgpt")
      ),
      p("Main Change: Conversation history is added to the model so that it has a more interactive behaviour."),
      tags$hr(),
      selectInput("model_name", "Model Name",
                  choices = c("gpt-4", "gpt-3.5-turbo"), selected = "gpt-3.5-turbo"),
      tags$hr(),
      sliderInput("temperature", "Temperature", min = 0.1, max = 1.0, value = 0.7, step = 0.1),
      sliderInput("max_length", "Maximum Length", min = 1, max = 4096, value = 1024, step = 1),
      tags$hr(),
      textAreaInput(inputId = "sysprompt", label = "SYSTEM PROMPT",height = "200px", placeholder = "You are Jane, an AI helper.")
      
    )
    ,
    mainPanel(
      tags$style(type = "text/css", ".shiny-output-error {visibility: hidden;}"),
      tags$style(type = "text/css", ".shiny-output-error:before {content: ' Check your inputs or API key';}"),
      tags$style(type = "text/css", "label {font-weight: bold;}"),
      fluidRow(
        column(12,tags$h3("Let Us Chat"),tags$hr(),uiOutput("chat_history"),tags$hr())
      ),
      fluidRow(
        column(9,textAreaInput(inputId = "user_message", placeholder = "Enter your message:", label="USER PROMPT", width = "100%")),
        column(3,actionButton("send_message", "Send",icon = icon("refresh"),height = "350px"))
      ),style = "background-color: #00A67E")
  ),style = "background-color: #3d2e5f")

server <- function(input, output, session) {
  
  chat_data <- reactiveVal(data.frame())
  history_log <- reactiveVal(list())
  observeEvent(input$send_message, {
    
    if (input$user_message != "") {
      new_data <- data.frame(source = "User", message = input$user_message, stringsAsFactors = FALSE)
      chat_data(rbind(chat_data(), new_data))
  
      history_log( list(history_log(),list(
        role = "user",
        time = date(),
        content =input$user_message)))
      print(history_log())
    
      gpt_res <- call_gpt_api(input$api_key, history_log(), input$model_name, input$temperature, input$max_length, input$sysprompt)
      # gpt_res <- call_gpt_api(input$api_key, chat_data(), input$model_name, input$temperature, input$max_length, input$sysprompt)
      
      if (!identical(gpt_res,character(0))) {
        gpt_data <- data.frame(source = "ChatGPT", message = gpt_res, stringsAsFactors = FALSE)
        
        chat_data(rbind(chat_data(), gpt_data))
        
      }
      
      
      else {
        gpt_data <- data.frame(source = "ChatGPT", message = "Cannot connect to OpenAI. Did you key in a valid API key? ", stringsAsFactors = FALSE)
        chat_data(rbind(chat_data(), gpt_data))
      }
      
      updateTextInput(session, "user_message", value = "")
    }
  })
  
  call_gpt_api <- function(api_key, prompt, model_name, temperature, max_length, sysprompt) {
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type("application/json"),
      encode = "json",
      body = list(
        model = model_name,
        messages = list(
          list(role = "user", content = paste(unlist(lapply(prompt, paste, collapse=" ")), collapse = " ")),
          list(role = "system", content = sysprompt)
        ),
        temperature = temperature,
        max_tokens = max_length
      )
    )

    history_log( list(history_log(),list(
      role = "chatbot",
      time = date(),
      content =str_trim(content(response)$choices[[1]]$message$content))))
 
    return(str_trim(content(response)$choices[[1]]$message$content))
  }
  
  output$chat_history <- renderUI({
    chatBox <- lapply(1:nrow(chat_data()), function(i) {
      tags$div(class = ifelse(chat_data()[i, "source"] == "User", "alert alert-secondary", "alert alert-success"),
               HTML(paste0("<b>", chat_data()[i, "source"], ":</b> ", chat_data()[i, "message"])))
    })
    do.call(tagList, chatBox)
  })
  
  # observeEvent(input$download_button, {
  #   if (nrow(chat_data()) > 0) {
  #     session$sendCustomMessage(type = "downloadData", message = "download_data")
  #   }
  # })
}
shinyApp(ui = ui, server = server)
