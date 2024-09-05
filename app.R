library(shiny)
library(dplyr)
library(httr)
library(jsonlite)
library(shinyWidgets)

# Load the RDS file at the start
dfPPT <- readRDS("G:/DSZ/SA2016/Datasets/Output/main/20. Test/WILLMA hackathon/dfOutput_per_vak.rds")


data <- dfPPT %>% filter(!is.na(extracted_text))

if (exists("api_key")) {
  headers <- add_headers(`X-API-KEY` = api_key, `Content-Type` = "application/json")  # Define R requests headers
  willma_base_url <- "https://willma.soil.surf.nl/"

  response <- GET(paste0(willma_base_url, "api/models"), headers)
  models <- fromJSON(content(response, "text"))

  model <- models %>%
    filter(id == 82) %>%
    slice(1)  # Get the first match
}

# Load the RDS file at the start
# data <- readRDS("G:/DSZ/SA2016/Datasets/Output/main/20. Test/CAN_Powerpoint_text.rds")

# Define the function to handle PowerPoint text
generate_request <- function(ppt_content, model, input_type, api_key, willma_base_url) {
  print(ppt_content)
  print(model)
  print(input_type)
  # Validate input_type
  valid_input_types <- c("summary", "flashcard", "multiplechoice")

  if (!(input_type %in% valid_input_types)) {
    stop("Invalid input_type. Choose from 'summary', 'flashcard', or 'multiplechoice'.")
  }

  selected_model <- model

  if (nrow(selected_model) > 0) {
    model_name <- selected_model$name
    cat(sprintf("Sending request to %s\n", model_name))

    # Prepare the data for the POST request based on input_type
    input_text <- switch(input_type,
                         summary = sprintf("Antwoord mij enkel in het nederlands. Geef een samenvatting van deze slides van powerpoint %s", ppt_content),
                         flashcard = sprintf("Maak een diepgaand flashcard setje vragen over de slides die ik jou nu stuur, respondeer in het nederlands. Content is van de slides hier: %s", ppt_content),
                         multiplechoice = sprintf("Maak een diepgaand flashcard setje vragen over de slides die ik jou nu stuur, respondeer in het nederlands maak multiple choice met 4 keuzes per vraag. Content is van de slides hier: %s", ppt_content))

    request_data <- toJSON(list(
      sequence_id = selected_model$id,
      input = input_text
    ), auto_unbox = TRUE)  # Convert to JSON format

    # Send the POST request
    response <- POST(
      paste0(willma_base_url, "api/chat/completions"),
      body = request_data,
      encode = "json",
      add_headers(`X-API-KEY` = api_key, `Content-Type` = "application/json")
    )

    # Return the response
    llm_antwoord <- content(response, "text")
    llm_json <- jsonlite::fromJSON(llm_antwoord)
    return(paste0(llm_json[["message"]]))

  } else {
    cat("No model found with ID 82.\n")
  }
}

# Define UI
ui <- fluidPage(

  titlePanel("AMIRA | Create content with a click!"),

  sidebarLayout(
    sidebarPanel(

      selectInput("courseCode", "Select Course Code:",
                  choices = NULL,
                  selected = NULL),  # Initialize as NULL

      prettyRadioButtons(
        inputId = "type",
        label = "Choose activity form:",
        choices = c("flashcard", "summary", "multiplechoice"),
        icon = icon("check"),
        bigger = TRUE,
        status = "info",
        animation = "jelly"
      ),
      selectInput("mimeClass", "Select MIME Class:",
                  choices = c("All", unique(data$mime_class)),
                  selected = "All"),  # Pre-select "All" for MIME class

      pickerInput(
        inputId = "filename",
        label = "Select file:",
        choices = NULL,
        selected = NULL,
        options = list(
          style = "btn-primary",
          `actions-box` = TRUE),
        multiple = TRUE
      ),

      actionButton("generate", "Generate Response", class = "btn-success"),
      actionButton("toggle-text", "Toggle Info", class = "btn-warning"),
      actionButton("toggle", "Toggle Table", class = "btn-info")
    ),

    mainPanel(
      textAreaInput("output_text", "Bewerk:", "", rows = 10, width = "4000px", height = "375px"),
      actionBttn(
        inputId = "Id108",
        label = "Download as markdown",
        style = "fill",
        color = "royal"
      ),
      actionBttn(
        inputId = "Id108",
        label = "Download as docx",
        style = "fill",
        color = "primary"
      ),
      actionBttn(
        inputId = "Id108",
        label = "Upload to Canvas",
        style = "fill",
        color = "success"
      ),
      textOutput("filteredText"),

      #verbatimTextOutput("output_text"),
      uiOutput("filteredData")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  text_visible <- reactiveVal(FALSE)
  table_visible <- reactiveVal(FALSE)
  # Reactive expression to update course code options
  updateCourseCodeOptions <- reactive({
    req(input$mimeClass)

    if (input$mimeClass == "All") {
      unique_ids <- unique(data$name)
    } else {
      unique_ids <- unique(data %>%
                             filter(mime_class == input$mimeClass) %>%
                             pull(name))
    }

    updateSelectInput(session, "courseCode",
                      choices = unique_ids,
                      selected = unique_ids[1])
  })

  # Reactive expression to update filename options
  updateFilenameOptions <- reactive({
    req(input$courseCode)

    filtered_data <- data %>%
      filter(name == input$courseCode,
             filename != "Powerpoint+presentatie.pptx")

    if (input$mimeClass != "All") {
      filtered_data <- filtered_data %>%
        filter(mime_class == input$mimeClass)
    }

    unique_filenames <- unique(filtered_data$filename)

    updatePickerInput(session, "filename",
                      choices = unique_filenames,
                      selected = unique_filenames[1])

  })

  # Observe changes to update dropdown options
  observe({
    updateCourseCodeOptions()
    updateFilenameOptions()
  })

  # Filter data based on selected values
  filteredData <- reactive({
    req(input$courseCode, input$filename)

    filtered_data <- data %>%
      filter(name == input$courseCode,
             filename == input$filename)

    if (input$mimeClass != "All") {
      filtered_data <- filtered_data %>%
        filter(mime_class == input$mimeClass)
    }

    return(filtered_data)
  })

  filteredText <- reactive({
    req(input$courseCode, input$filename)
    paste("Type:", input$type,
          "| Course Code:", input$courseCode,
          "| MIME Class:", input$mimeClass,
          "| Filename:", input$filename)

  })

  # Display selected information
  output$filteredText <- renderText({
    filteredText()

  })

  # Display filtered data
  output$filteredData <- renderTable({
    filteredData()
  })

  # Generate response when button is clicked
  observeEvent(input$generate, {
    req(input$courseCode, input$filename)  # Ensure inputs are available

    # Get the selected PowerPoint content

    # Combine the output from filteredData into a single string

    if (input$type == "summary") {
      response_text <- paste(filteredData()$summary, collapse = " ")
      ##' *INFO*  crop the text to 1000 characters for display purposes
      response_text <- substr(response_text, 1, 1000)
    } else if (input$type == "flashcard") {
      response_text <- paste(filteredData()$flashcard, collapse = "\n")
      ##' *INFO*  crop the text to 1000 characters for display purposes
      response_text <- substr(response_text, 1, 1000)
    } else if (input$type == "multiplechoice") {
      response_text <- paste(filteredData()$multiplechoice, collapse = "\n")
      ##' *INFO*  crop the text to 1000 characters for display purposes
      response_text <- substr(response_text, 1, 1000)
    } else {
      response_text <- "Invalid input type"
    }

    updateTextInput(session, "output_text", value = response_text)

    # Toggle table visibility
    observeEvent(input$toggle_text, {
      text_visible(!text_visible())  # Toggle the boolean value
    })

    # Render the filtered data table conditionAlly
    output$filteredText <- renderUI({
      if (text_visible()) {
        textOutput("filteredText")

        #tableOutput("filteredDataTable")  # Show the tableOutput when visible
      }
    })

    # Render the actual table data
    output$filteredText <- renderText({
      filteredText()
    })

    # Toggle table visibility
    observeEvent(input$toggle, {
      table_visible(!table_visible())  # Toggle the boolean value
    })

    # Render the filtered data table conditionAlly
    output$filteredData <- renderUI({
      if (table_visible()) {
        tableOutput("filteredDataTable")  # Show the tableOutput when visible
      }
    })

    # Render the actual table data
    output$filteredDataTable <- renderTable({
      filteredData()
    })


  })
}

# Run the application
shinyApp(ui = ui, server = server)


