library(shiny)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
      .title-panel {
        background-color: #6eb5ff; /* Slightly darker blue */
        color: white; /* White text */
        padding-top: 18px; /* Increased padding */
        padding-bottom: 18px; /* Increased padding */
        overflow: hidden; /* Hide overflow */
      }
      .title-text {
        font-size: 48px; /* Increased font size */
        margin-left: 15px; /* Moved title to the right */
      }
      .sidebar {
        height: calc(100vh - 100px); /* Adjusting sidebar height to fit available space */
        display: flex; 
        flex-direction: column; 
        justify-content: center;
        overflow-y: auto; /* Allowing sidebar content to scroll if needed */
      }
      .main-panel {
        height: calc(100vh - 100px); /* Adjusting main panel height to fit available space */
        justify-content: center; 
        align-items: center;
        overflow-y: auto; /* Allowing main panel content to scroll if needed */
      }
      #download_output { display: block; } /* Initially show download button */
      #url_field, #milestone2_btn, #milestone3_btn, #milestone4_btn { cursor: pointer; } /* Add cursor pointer to URL field and milestone buttons */
      .enter-button {
        padding: 10px;
        font-size: 16px;
        width: 100%;
        background-color: #fff; /* Default color */
        border: 1px solid #ccc;
        color: #333;
      }
      .enter-button:hover {
        background-color: #28a745 !important; /* Green color when hovered */
        color: #fff !important;
      }
      .enter-button:active {
        background-color: #28a745 !important; /* Green color when clicked */
        color: #fff !important;
      }
      /* Loading overlay styles */
      #overlay {
        position: fixed;
        width: 100%;
        height: 100%;
        top: 0;
        left: 0;
        background-color: rgba(0, 0, 0, 0.5); /* Semi-transparent black */
        z-index: 1000; /* Ensure it's above other content */
        display: none; /* Initially hidden */
        justify-content: center;
        align-items: center;
      }
      #loading-icon {
        font-size: 36px;
        color: white;
      }
    "))
  ),
  
  div(class = "title-panel", 
      titlePanel(HTML("<span class='title-text'>POSSIBLE FINANCE</span>"))
  ),
  
  # Sidebar with milestone buttons
  sidebarLayout(
    sidebarPanel(
      class = "sidebar", 
      actionButton("milestone2_btn", "Fraudster Detection", style="padding:20px; font-size:24px; margin-bottom: 10px"),
      actionButton("milestone3_btn", "Account Verification", style="padding:20px; font-size:24px; margin-bottom: 10px"),
      actionButton("milestone4_btn", "Account Visualization", style="padding:20px; font-size:24px; margin-bottom: 10px")
    ),
    
    # Main panel with URL input field and error message
    mainPanel(
      class = "main-panel",
      fluidRow(
        column(12,
               style = "height: 320px;"  # Adding a gap between buttons
        )
      ),
      fluidRow(
        column(12, 
               uiOutput("url_field_ui")
        )
      ),
      fluidRow(
        column(4), # Empty column to the left
        column(4, offset = 0, align = "center",
               uiOutput("enter_btn_ui")), # Centered download button
        column(4) # Empty column to the right
      ),
      fluidRow(
        column(12,
               style = "height: 20px;"  # Adding a gap between buttons
        )
      ),
      fluidRow(
        column(4), # Empty column to the left
        column(4, offset = 0, align = "center",
               uiOutput("download_button_ui")), # Centered download button
        column(4) # Empty column to the right
      ),
      # Loading overlay
      div(id = "overlay",
          div(id = "loading-icon", "Loading...")  # Loading icon
      ),
      uiOutput("error_message_ui")  # Error message below the URL field
    )
  )
)

server <- function(input, output, session) {
  # Initialize milestone selection
  selected_milestone <- reactiveVal(NULL)
  
  # Initialize error message
  error_message <- reactiveVal(NULL)
  
  # Initialize storedText reactiveVal
  storedText <- reactiveVal(NULL)
  
  # Initialize fileReady reactiveVal
  fileReady <- reactiveVal(FALSE)
  
  # Initialize enter button color
  enterButtonColor <- reactiveVal("#fff")  # Default color
  
  # Reactive value to track URL field click
  urlFieldClicked <- reactiveVal(FALSE)
  
  # Reactive value to track milestone button click
  milestoneButtonClicked <- reactiveVal(FALSE)
  
  observeEvent(input$milestone2_btn, {
    selected_milestone("Fraudster Detection")
    error_message(NULL) # Clear error message
    milestoneButtonClicked(TRUE)  # Update milestone button click status
  })
  
  observeEvent(input$milestone3_btn, {
    selected_milestone("Account Verification")
    error_message(NULL) # Clear error message
    milestoneButtonClicked(TRUE)  # Update milestone button click status
  })
  
  observeEvent(input$milestone4_btn, {
    selected_milestone("Account Visualization")
    error_message(NULL) # Clear error message
    milestoneButtonClicked(TRUE)  # Update milestone button click status
  })
  
  output$url_field_ui <- renderUI({
    if(is.null(selected_milestone())) return(NULL)
    
    tagList(
      tags$div(class = "form-group",
               tags$label(paste("Provide the", selected_milestone(), "input URL"),style = "font-size: 16px;"),
               tags$input(id = "url_field", type = "text", class = "form-control", placeholder = "Enter URL here...",
                          style = "height: 50px;"),  # Increase the height of the URL field
               onclick = "Shiny.setInputValue('urlFieldClicked', true);"
      )
    )
  })
  
  output$error_message_ui <- renderUI({
    error_msg <- error_message()
    if(is.null(error_msg)) return(NULL)
    div(class = "error-message", style = "color: red; text-align: center; font-size:16px;", error_msg)
  })
  
  output$enter_btn_ui <- renderUI({
    if (!is.null(selected_milestone())) {
      fluidRow(
        column(12, 
               actionButton("enter_btn", "Submit", 
                            style="padding:10px; font-size:16px; width:100%; background-color: #fff; border: 1px solid #ccc; color: #333;", 
                            class = "enter-button"),
               align = "center"
        )
      )
    }
  })
  
  output$download_button_ui <- renderUI({
    if (fileReady() && !urlFieldClicked() && !milestoneButtonClicked()) {  # Check if file ready and no URL field or milestone button click
      fluidRow(
        column(12, 
               downloadButton("download_output", "Download",
                              style="padding:10px; font-size:16px; width:100%; background-color: #007bff; border: none; color: #fff;", 
                              class = "download-button"),
               align = "center"
        )
      )
    } else {
      div(id = "download_output")  # Empty div to maintain layout
    }
  })
  
  observeEvent(input$enter_btn, {
    if (is.null(selected_milestone())) {
      error_message("Please select a milestone first.")
      return(NULL)
    }
    
    entered_url <- isolate(input$url_field)
    
    if (!grepl("^https?://", entered_url)) {
      error_message("Entered URL is not valid.")
      return(NULL)
    } else {
      error_message(NULL) # Clear error message
    }
    
    showLoadingOverlay(TRUE)
    
    if (selected_milestone() == "Fraudster Detection") {
      storedText(entered_url)
      source("fraudsterDetection_1.R", local = TRUE, chdir = TRUE)
      fileReady(TRUE)
    }
    
    if (selected_milestone() == "Account Verification") {
      storedText(entered_url)
      source("accountVerification_1.R", local = TRUE, chdir = TRUE)
      fileReady(TRUE)
    }
    
    if (selected_milestone() == "Account Visualization") {
      storedText(entered_url)
      source("bankAcctViz_1.R", local = TRUE, chdir = TRUE)
      fileReady(TRUE)
    }
    
    # Reset URL field click and milestone button click
    urlFieldClicked(FALSE)
    milestoneButtonClicked(FALSE)
    
    showLoadingOverlay(FALSE)
  })
  
  output$download_output <- downloadHandler(
    filename = function() {
      req(storedText())  # Ensure storedText is not NULL
      testFileURL <- storedText()
      outputFileName = sub(".*/", "", testFileURL)  # Remove everything before the last '/'
      outputFileName = sub("\\?.*", "", outputFileName)  # Remove everything after '?'
      outputFileName = sub("\\..*", "", outputFileName)
      
      # Define file extension based on selected milestone
      if (selected_milestone() %in% c("Fraudster Detection", "Account Verification")) {
        outputFileName = paste0(outputFileName, ".csv")
      } else if (selected_milestone() == "Account Visualization") {
        outputFileName = paste0(outputFileName, ".pdf")
      }
      
      outputFileName
    },
    content = function(file) {
      req(storedText())  # Ensure storedText is not NULL
      testFileURL <- storedText()
      outputFileName = sub(".*/", "", testFileURL)  # Remove everything before the last '/'
      outputFileName = sub("\\?.*", "", outputFileName)  # Remove everything after '?'
      outputFileName = sub("\\..*", "", outputFileName)
      
      # Define file extension based on selected milestone
      if (selected_milestone() %in% c("Fraudster Detection", "Account Verification")) {
        outputFileName = paste0(outputFileName, ".csv")
      } else if (selected_milestone() == "Account Visualization") {
        outputFileName = paste0(outputFileName, ".pdf")
      }
      
      file.copy(file.path(outputFileName), file)
    }
)
  showLoadingOverlay <- function(show) {
    if (show) {
      runjs(
        '
        $("#overlay").show();  // Show loading overlay
        '
      )
    } else {
      runjs(
        '
        $("#overlay").hide();  // Hide loading overlay
        '
      )
    }
  }
  
}

# Run the application
shinyApp(ui = ui, server = server)
