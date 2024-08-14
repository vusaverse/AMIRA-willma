library(shiny)
library(dplyr)


# Load the RDS file at the start
dfPPT <- readRDS("G:/DSZ/SA2016/Datasets/Output/main/20. Test/WILLMA hackathon/dfOutput_per_vak.rds")


data <- dfPPT %>% filter(!is.na(extracted_text))



headers <- add_headers(`X-API-KEY` = api_key, `Content-Type` = "application/json")  # Define R requests headers
willma_base_url <- "https://willma.soil.surf.nl/"

response <- GET(paste0(willma_base_url, "api/models"), headers)
models <- fromJSON(content(response, "text"))

model <- models %>%
  filter(id == 82) %>%
  slice(1)  # Get the first match


library(shiny)
library(dplyr)
library(httr)
library(jsonlite)

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

  titlePanel("AMIRA | Canvas automating with AI"),

  sidebarLayout(
    sidebarPanel(

      selectInput("courseCode", "Select Course Code:",
                  choices = NULL,
                  selected = NULL),  # Initialize as NULL

      radioButtons("type", "Choose Input Type:",
                   choices = c("flashcard", "summary", "multiplechoice")),
      selectInput("mimeClass", "Select MIME Class:",
                  choices = c("ALL", unique(data$mime_class)),
                  selected = "ALL"),  # Pre-select "ALL" for MIME class

      selectInput("filename", "Select Filename:",
                  choices = NULL,
                  selected = NULL),  # Initialize as NULL

      actionButton("generate", "Generate Response"),
      actionButton("toggle", "Toggle Table")
    ),

    mainPanel(
      textOutput("selectedInfo"),
      verbatimTextOutput("output_text"),
      uiOutput("filteredData")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  table_visible <- reactiveVal(FALSE)
  # Reactive expression to update course code options
  updateCourseCodeOptions <- reactive({
    req(input$mimeClass)

    if (input$mimeClass == "ALL") {
      unique_ids <- unique(data$course_code)
    } else {
      unique_ids <- unique(data %>%
                             filter(mime_class == input$mimeClass) %>%
                             pull(course_code))
    }

    updateSelectInput(session, "courseCode",
                      choices = unique_ids,
                      selected = unique_ids[1])
  })

  # Reactive expression to update filename options
  updateFilenameOptions <- reactive({
    req(input$courseCode)

    filtered_data <- data %>%
      filter(course_code == input$courseCode)

    if (input$mimeClass != "ALL") {
      filtered_data <- filtered_data %>%
        filter(mime_class == input$mimeClass)
    }

    unique_filenames <- unique(filtered_data$filename)

    updateSelectInput(session, "filename",
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
      filter(course_code == input$courseCode,
             filename == input$filename)

    if (input$mimeClass != "ALL") {
      filtered_data <- filtered_data %>%
        filter(mime_class == input$mimeClass)
    }

    return(filtered_data)
  })

  # Display selected information
  output$selectedInfo <- renderText({
    paste("Type:", input$type,
          "| Course Code:", input$courseCode,
          "| MIME Class:", input$mimeClass,
          "| Filename:", input$filename)
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


    # Display the generated response in the output text box
    output$output_text <- renderText({
      response_text
    })

    # Toggle table visibility
    observeEvent(input$toggle, {
      table_visible(!table_visible())  # Toggle the boolean value
    })

    # Render the filtered data table conditionally
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


