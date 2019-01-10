#' Synonym and Antonym Replacement Addin
#'
#' Gets currently selected word, searches for synonym or antonym using [syn],
#' allows user to pick replacement.
#' @param synonym Show synonym (`TRUE`) or antonym (`FALSE`)
syn_addin <- function(synonym = TRUE) {
  context <- rstudioapi::getSourceEditorContext()
  if (length(context$selection) != 1) {
    stop("Please select a single word to look up", call. = FALSE)
  }
  res <- syn_gadget(context$selection[[1]]$text, synonym)
  if (length(res)) {
    rstudioapi::modifyRange(context$selection[[1]]$range, text = res, id = context$id)
  }
}

#' @rdname syn_addin
ant_addin <- function() syn_addin(synonym = FALSE)

#' Synonym Antonym Gadget
#'
#' Gadget that powers synonym/antonym replacement.
#' @param word A single word to look up
#' @inheritParams syn_addin
#' @export
syn_gadget <- function(word, synonym = TRUE) {
  library(shiny)
  library(miniUI)

  is_first_capitalized <- grepl("^[A-Z]", word)
  word <- tolower(word)
  results <- find_synonym(word, synonym)

  ui <- miniPage(
    tags$style(
      "h5 {",
      "  display: inline-block;",
      "  margin-top: 0;",
      "  max-width: 100%;",
      "  margin-bottom: 10px;",
      "  font-weight: 700; }",
      "#syn_word_next { color: #dc3545; }",
      "#syn_word_next:hover { text-decoration: none; border-bottom: 1px dotted }"
    ),
    miniContentPanel(
      padding = 25,
      tags$h5(
        paste0(syn_type(synonym), "s of"),
        uiOutput("ui_syn_word", inline = TRUE)
      ),
      selectizeInput("syn_result", label = NULL, choices = results[[1]], multiple = FALSE),
      uiOutput("ui_syn_replacement"),
      tags$div(
        class = "btn-group btn-group-sm", role = "group",
        actionButton("return", "Replace", icon = icon("exchange"), class = "btn-primary"),
        actionButton("random", "Random", icon = icon("random")),
        actionButton("cancel", "Cancel", icon = icon("times-circle"))
      )
    )
  )

  server <- function(input, output, session) {
    syn_word_selector <- reactiveVal(1L)
    observeEvent(input$syn_word_next, {
      # Update currently selected syn hit
      s_syn_idx <- syn_word_selector()
      next_syn_idx <- s_syn_idx + 1L
      if (next_syn_idx > length(results)) {
        next_syn_idx <- 1L
      }
      syn_word_selector(next_syn_idx)
    })

    output$ui_syn_word <- renderUI({
      if (length(results) < 2) {
        names(results)[1]
      } else {
        actionLink("syn_word_next", names(results)[syn_word_selector()],
                   alt = paste(names(results), collapse = "\n"))
      }
    })

    observe({
      updateSelectizeInput(
        session, "syn_result",
        choices = results[[syn_word_selector()]])
    })

    observeEvent(input$random, {
      updateSelectizeInput(
        session, "syn_result",
        selected = sample(results[[syn_word_selector()]], 1)
      )
    })

    output$ui_syn_replacement <- renderUI({
      ret <- input$syn_result
      if (is_first_capitalized) {
        ret <- paste0(toupper(substr(ret, 1, 1)), substring(ret, 2))
      }
      textInput("syn_replacement", paste0('Replace "', word, '" with'), value = ret)
    })
    observeEvent(input$cancel, {
      stopApp()
    })
    observeEvent(input$return, {
      stopApp(input$syn_replacement)
    })
  }

  runGadget(
    ui, server,
    viewer = shiny::dialogViewer("Replace Word", width = 300, height = 300)
  )
}

syn_type <- function(synonym = TRUE) ifelse(synonym, "Synonym", "Antonym")

find_synonym <- function(word, synonym = TRUE) {
  if (nchar(word) < 1 || length(strsplit(word, " ")[[1]]) != 1) {
    stop("Please select a single word.", call. = FALSE)
  }

  # Generate possible root variants of given `word` by removing endings
  suffixes <- c("ed", "d", "ing", "s", "es", "i?es", "i?er", "i?est")
  search_words <- vapply(
    suffixes,
    function(suffix) sub(paste0(suffix, "$"), "", word),
    character(1)
  )

  search_words <- c(word, search_words,
                    paste0(search_words, "e"), paste0(search_words, "y"))
  search_words <- unique(search_words)

  # Search for snys/ants for all variants of `word`
  results <- if (synonym) syn::syns(search_words) else syn::ants(search_words)

  # Return the first result in the list (if any)
  hits <- vapply(results, length, 0L) > 0
  if (!any(hits)) {
    stop(paste0("No ", tolower(syn_type(synonym)), "s found."), call. = FALSE)
  }
  results[hits]
}
