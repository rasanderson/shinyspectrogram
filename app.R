library(shiny)
library(bslib)
library(tidyverse)
library(tuneR)
library(signal)
library(DT)

ui <- page_sidebar(
    title = "Spectrogram Annotation Viewer",
    sidebar = sidebar(
        fileInput("wav_file", "Upload WAV File",
                  accept = c(".wav")),
        fileInput("selection_table", "Upload Raven Selection Table (TSV)",
                  accept = c(".txt", ".tsv")),
        numericInput("img_height", "Plot Height (pixels)", value = 480),
        # Spectrogram parameters
        numericInput("window_length", "Window Length (samples)", value = 512),
        numericInput("overlap", "Overlap (%)", value = 50, min = 0, max = 100),
        sliderInput("contrast", "Contrast", min = 0, max = 1, value = 0.3),
        checkboxInput("use_color", "Use Color", value = TRUE)
    ),
    
    card(
        card_header("Annotated Spectrogram"),
        plotOutput("annotated_plot", 
                   height = "auto",
                   hover = hoverOpts("plot_hover", delay = 100, nullOutside = TRUE))
    ),
    
    card(
        card_header("Selection Data"),
        div(
            style = "margin-bottom: 10px;",
            actionButton("play_selection", "Play Selected Segment", 
                         icon = icon("play"),
                         class = "btn-primary")
        ),
        DTOutput("selection_table_display"),
        
        # Add custom playback controls
        card(
            card_header("Custom Playback"),
            layout_columns(
                col_widths = c(4, 4, 4),
                numericInput("custom_start", "Start Time (s)", value = 0, step = 0.1),
                numericInput("custom_end", "End Time (s)", value = 1, step = 0.1),
                actionButton("play_custom", "Play Custom Segment",
                             icon = icon("play"),
                             class = "btn-secondary")
            )
        )
    )
)

server <- function(input, output, session) {
    # Create reactive values to track playback state and plot bounds
    rv <- reactiveValues(
        is_playing = FALSE,
        start_time = NULL,
        current_time = NULL,
        end_time = NULL,
        start_sys_time = NULL,
        plot_bounds = NULL,
        selection_data = NULL,
        duration = NULL
    )
    
    # Read and process the WAV file
    wav_data <- reactive({
        req(input$wav_file)
        wav <- readWave(input$wav_file$datapath)
        rv$duration <- length(wav@left) / wav@samp.rate  # Store duration
        wav
    })
    
    # Calculate spectrogram
    spectrogram <- reactive({
        req(wav_data())
        
        audio <- wav_data()
        window <- input$window_length
        overlap_samples <- floor(window * input$overlap / 100)
        
        spec <- specgram(x = audio@left,
                         n = window,
                         Fs = audio@samp.rate,
                         overlap = overlap_samples)
        
        P <- abs(spec$S)
        P <- log10(P)
        P <- (P - min(P)) / (max(P) - min(P))
        P <- (P - 0.5) * (input$contrast * 2) + 0.5
        P[P < 0] <- 0
        P[P > 1] <- 1
        
        list(
            P = P,
            time = spec$t,
            freq = spec$f,
            duration = length(audio@left) / audio@samp.rate
        )
    })
    
    # Initialize or update selection data when file is uploaded
    observeEvent(input$selection_table, {
        data <- read_tsv(input$selection_table$datapath) %>%
            select(contains(c("Begin Time", "End Time", "Low Freq", "High Freq"))) %>%
            mutate(across(where(is.numeric), \(x) round(x, 3)))
        
        colnames(data) <- c("Begin Time (s)", "End Time (s)", 
                            "Low Freq (Hz)", "High Freq (Hz)")
        
        rv$selection_data <- data
    })
    
    # Update custom end time when wav file is loaded
    observeEvent(rv$duration, {
        if (!is.null(rv$duration)) {
            updateNumericInput(session, "custom_end", value = min(1, rv$duration))
            updateNumericInput(session, "custom_end", max = rv$duration)
            updateNumericInput(session, "custom_start", max = rv$duration)
        }
    })
    
    # Animation observer
    observe({
        if (rv$is_playing) {
            invalidateLater(50)
            elapsed <- as.numeric(Sys.time() - rv$start_sys_time)
            current <- rv$start_time + elapsed
            
            if (current <= rv$end_time) {
                rv$current_time <- current
            } else {
                rv$is_playing <- FALSE
            }
        }
    })
    
    # Function to play audio segment
    play_segment <- function(start_time, end_time) {
        req(wav_data())
        
        audio <- wav_data()
        start_sample <- round(start_time * audio@samp.rate) + 1
        end_sample <- round(end_time * audio@samp.rate)
        
        segment <- audio
        segment@left <- audio@left[start_sample:end_sample]
        if(audio@stereo) {
            segment@right <- audio@right[start_sample:end_sample]
        }
        
        temp_file <- tempfile(fileext = ".wav")
        writeWave(segment, temp_file)
        
        rv$is_playing <- TRUE
        rv$start_time <- start_time
        rv$current_time <- start_time
        rv$end_time <- end_time
        rv$start_sys_time <- Sys.time()
        
        system2("play", temp_file, wait = FALSE)
        
        later::later(function() {
            if(file.exists(temp_file)) {
                unlink(temp_file)
            }
        }, 5)
    }
    
    # Create the annotated plot
    output$annotated_plot <- renderPlot({
        req(spectrogram())
        
        spec <- spectrogram()
        
        mar <- c(4, 4, 1, 1)
        par(mar = mar)
        
        color_palette <- if(input$use_color) {
            hcl.colors(50, "YlOrRd", rev = TRUE)
        } else {
            grey.colors(50, start = 1, end = 0)
        }
        
        image(x = spec$time,
              y = spec$freq,
              z = t(spec$P),
              col = color_palette,
              xlab = "Time (seconds)",
              ylab = "Frequency (Hz)",
              useRaster = TRUE)
        
        # Add rectangles for selections if we have data
        if (!is.null(rv$selection_data)) {
            selections <- rv$selection_data
            selected_row <- input$selection_table_display_rows_selected
            
            for(i in 1:nrow(selections)) {
                border_color <- if(!is.null(selected_row) && i == selected_row) "yellow" else "red"
                rect(selections[[1]][i],
                     selections[[3]][i],
                     selections[[2]][i],
                     selections[[4]][i],
                     border = border_color,
                     lwd = if(border_color == "yellow") 3 else 2)
            }
        }
        
        usr <- par("usr")
        rv$plot_bounds <- list(
            xlim = usr[1:2],
            ylim = usr[3:4],
            mar = mar
        )
        
        if (rv$is_playing && !is.null(rv$current_time)) {
            abline(v = rv$current_time, col = "cyan", lwd = 2)
        }
    }, height = function() input$img_height)
    
    # Create the selection table output
    output$selection_table_display <- renderDT({
        req(rv$selection_data)
        datatable(rv$selection_data,
                  options = list(
                      pageLength = 5,
                      scrollX = TRUE
                  ),
                  selection = "single",
                  rownames = FALSE)
    })
    
    # Play selected segment when button is clicked
    observeEvent(input$play_selection, {
        req(wav_data(), input$selection_table_display_rows_selected)
        
        selected_row <- input$selection_table_display_rows_selected
        selections <- rv$selection_data
        
        if(length(selected_row) > 0) {
            play_segment(selections[[1]][selected_row], selections[[2]][selected_row])
        }
    })
    
    # Play custom segment when button is clicked
    observeEvent(input$play_custom, {
        req(wav_data())
        
        # Validate input times
        start_time <- max(0, input$custom_start)
        end_time <- min(input$custom_end, rv$duration)
        
        if(end_time > start_time) {
            play_segment(start_time, end_time)
        }
    })
}

shinyApp(ui, server)
