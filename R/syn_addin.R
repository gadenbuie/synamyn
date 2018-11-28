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
    miniContentPanel(
      padding = 25,
      selectizeInput("syn_result",
                     paste0(syn_type(synonym), "s of ", names(results)[1]),
                     choices = results[[1]],
                     multiple = FALSE),
      uiOutput("ui_syn_replacement"),
      actionButton("return", "Replace", icon = icon("exchange"), class = "btn-primary"),
      actionButton("cancel", "Cancel", icon = icon("times-circle"))
    )
  )

  server <- function(input, output, session) {
    output$ui_syn_replacement <- renderUI({
      ret <- input$syn_result
      if (is_first_capitalized) {
        ret <- paste0(
          toupper(substr(ret, 1, 1)),
          substring(ret, 2)
        )
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

  suffixes <- c("ed", "d", "ing", "s", "es", "i?es", "i?er", "i?est")
  search_words <- c(word,
    vapply(suffixes, function(suffix) sub(paste0(suffix, "$"), "", word), "")
  )
  search_words <- c(search_words, paste0(search_words, "e"), paste0(search_words, "y"))

  results <- if (synonym) {
    syn::syns(search_words)
  } else {
    syn::ants(search_words)
  }

  ret <- list()
  for (i in names(results)) {
    if (!length(ret) && length(results[[i]])) ret <- results[i]
  }

  if (!length(ret)) {
    stop(paste0("No ", tolower(syn_type(synonym)), "s found."), call. = FALSE)
  }

  ret
}
