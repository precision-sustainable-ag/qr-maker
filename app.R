library(shiny)
library(purrr)
library(dplyr)
library(stringr)
library(scales)
library(glue)
library(googlesheets)

# HTTP2 Chrome fix
httr::set_config(httr::config(http_version = 0))


source("secret.R")
source("barcode_for_weeds.R")

# auth_token <- gs_auth()
# saveRDS(auth_token, "auth_token.rds")

gs_auth("auth_token.rds")


underliner <- function(strings, colors, size) {
  purrr::map2(
    strings, colors,
    ~span(
      span(.x, style = glue("border-bottom: {0.9*size}px solid {.y};")),
      style = glue("font-weight: bold; font-family: monospace; font-size: {size*2}px")
    )
  ) %>%
    tagList()
}



anno <- scales::brewer_pal("qual", "Set1")(5)

CROWN_format <- underliner(
  stringr::str_split("W XYZ B0 T1", " ", simplify = T),
  anno[1:4],
  12
)

nonCROWN_format <- underliner(
  stringr::str_split("1 XYZ B0 T1", " ", simplify = T),
  anno[c(5, 2:4)],
  12
)

CROWN_format_s <- underliner(
  stringr::str_split("W XYZ B0 T1", " ", simplify = T),
  anno[1:4],
  7
)

nonCROWN_format_s <- underliner(
  stringr::str_split("1 XYZ B0 T1", " ", simplify = T),
  anno[c(5, 2:4)],
  7
)



ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
iframe {
height: 90vh !important;
width: 100%;
}
"
      )
    )
  ),
  titlePanel("QR ID sheet generator"),
  sidebarLayout(
    sidebarPanel(
      "The CROWN pages are formatted as", br(),
      CROWN_format, br(), br(),
      tags$ol(
        tags$li(CROWN_format_s[[1]][[1]], "this sample is a Weeds image."),
        tags$li(CROWN_format_s[[1]][[2]], "the field ID code."),
        tags$li(CROWN_format_s[[1]][[3]], "treatment (Bare or Cover) and quadrat number, 0-9."),
        tags$li(CROWN_format_s[[1]][[4]], "image Timing, 1-3.")
      ),
      hr(),
      "The non-CROWN weed images should be formatted as", br(), 
      nonCROWN_format, br(), br(),
      "We're expecting that you'll use fewer individual fields on-station, but",
      "there are likely to be multiple replicate plots used in each field.",
      tags$ol(
        tags$li(nonCROWN_format_s[[1]][[1]], "replicate block number."),
        tags$li(nonCROWN_format_s[[1]][[2]], "the field ID code, assigned to you by Sarah."),
        tags$li(nonCROWN_format_s[[1]][[3]], "treatments and quadrat number, 0-9."),
        tags$li(nonCROWN_format_s[[1]][[4]], "image Timing, 0-9, up to 10 total timings.")
      ),
      "If you are using additional treatments (e.g. Early, Intermediate, Late killed covers)",
      "you can edit the box below. Each treatment code should be a single character.",
      hr(),
      "To generate a PDF of barcodes, fill out these fields:",
      fluidRow(
        column(6, numericInput("rep", label = "Total replicates", value = 4, min = 1, max = 9)),
        column(6, textInput("fld", label = "Field ID", placeholder = "XYZ"))
      ),
      fluidRow(
        column(6, textInput("trt", label = "Treatments", value = "BC", placeholder = "BC")),
        column(6, numericInput("qrt", label = "Quadrats per plot", value = 5, min = 1, max = 10))
      ),
      numericInput("tim", label = "Timing", value = 0, min = 0, max = 9),
      actionButton("preview", "Preview codes", icon = icon("fas fa-receipt")),
      actionButton("generate", "Generate file", icon = icon("far fa-file-pdf")),
      downloadButton("pdf_file", "Download PDF")
    ),
    mainPanel(
      fluidRow(
        column(7, dataTableOutput("table")), 
        column(5, uiOutput("pdf_preview"))
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    l <- str_length(input$fld)
    if (l > 3) {
      updateTextInput(
        session, "fld", 
        value = "", 
        placeholder = "Field ID must be 3 letters!"
      )
    }
  })
  
  labs <- reactive({
    x <- expand.grid(
      Site = str_to_upper(input$fld),
      Quadrat = c(0, seq_len(input$qrt-1)),
      Treatment = unique(str_to_upper(str_extract_all(input$trt, "[:alnum:]", simplify = T))),
      Block = seq_len(input$rep),
      Timing = input$tim
    ) %>% 
      mutate(
        label = glue("{Block} {Site} {Treatment}{Quadrat} T{Timing}")
      ) %>% 
      select(Block, Site, Treatment, Quadrat, Timing, label)
    
    if (any(str_length(x$label) != 11) | nrow(x) == 0) {
      x <- data.frame(Error = "All fields must be provided!")
    }
    x
  })
  
  observeEvent(
    input$preview,
    {
      output$table <- renderDataTable(
        labs(), options = list(
          ordering = F,
          searching = F,
          scrollY = "85vh",
          scrollCollapse = T,
          paging = F
        )
      )
    })
  
  observeEvent(
    input$generate,
    {
      if (is.character(labs()$label)) {
        dir.create("www", showWarnings = F)
        
        addResourcePath("www", "www")
        
        fn <- glue("www/{str_to_upper(input$fld)} time {input$tim}.pdf")
        
        weed_sheets(labs()$label, fn)
        
        output$pdf_preview <- renderUI({
          tags$iframe(
            style = "width: 100%; height: 100%;", 
            src = fn
            )
        })
        
        if (file.exists(fn)) {
          output$pdf_file <- downloadHandler(
            basename(fn), content = function(file) file.copy(from = fn, to = file)
            )
        }
        
        new_row <- labs() %>% select(Site, Block, Treatment, Quadrat, Timing) %>% 
          map(unique) %>% map(paste, collapse = "/") %>% unlist()
        
        gs_add_row(
          gs_key(gs_key_for_qr, lookup = F, visibility = "private"),
          input = new_row
          )
        
      }
    }
  )
  
}

shinyApp(ui, server)