library("ggplot2")
library("shiny")

# get RStudio hex stickers urls from GitHub (to use as profile pics)
avatars <- readLines("https://github.com/rstudio/hex-stickers/tree/master/PNG")
avatars <- avatars[
  grep('" href="/rstudio/hex-stickers/blob/master/PNG/.*\\.png', avatars)
]
avatars <- sort(sub(
  "\\.png.*", "",
  sub(".*/rstudio/hex-stickers/blob/master/PNG/", "", avatars)
))
avatars_url <-
  "https://raw.githubusercontent.com/rstudio/hex-stickers/master/PNG/"

# connected users
users <- reactiveVal()
# currently selected object by the viewer user
act_object <- reactiveVal("none")

# audience questions
aud_qs <- reactiveVal()

# users that voted for each answer
poll_1_ans <- reactiveVal(list(
  "Yes" = NULL,
  "No" = NULL,
  "Won't answer!" = NULL
))
poll_1_show_correct <- reactiveVal(FALSE)

library("wordcloud")

# words that each user wrote
wordcloud_1_ans <- reactiveVal(list())

# rating that each user gave
rating_1_ans <- reactiveVal(list())

# each user questions
question_1_ans <- reactiveVal()

ui <- fluidPage(
  title = "interactingan: shiny_contest",
  theme = shinythemes::shinytheme("spacelab"),

  # admin panel
  conditionalPanel(
    "(output.is_viewer==true) && (output.is_admin==true)",
    br(),
    wellPanel(
      verbatimTextOutput("admin_panel")
    )
  ),

  # if it is the viewer user, then show the interactive object selector
  conditionalPanel(
    "(output.is_viewer==true) && false",
    selectInput(inputId = "act_obj", label = "", choices = c(
      "Select object" = "empty",
      "Shiny Contest 2020 winner" = "wordcloud_1",
      "Already seen heyshiny?" = "poll_1",
      "Rate interactingan:" = "rating_1",
      "Feedback:" = "question_1",
      "Audience Questions" = "aud_qs"
    )),
    align = "center"
  ),

  # button for audience to ask questions
  conditionalPanel(
    "(output.is_viewer==false)",
    h2(actionLink(
      "aud_qs",
      label = "",
      icon = icon("question-circle"),
      title = "Ask the speaker"
    )),
    align = "center"
  ),

  # questions viewer pane
  conditionalPanel(
    "(output.is_viewer==true) && (output.act_object=='aud_qs')",
    uiOutput("aud_qs_viewer")
  ),

  # poll voting selections
  conditionalPanel(
    "(output.is_viewer==false) && (output.act_object=='poll_1') && (output.done_poll_1==false)",
    h3("Already seen heyshiny?"),
    selectInput(
      inputId = "poll_1_sel",
      label = "",
      choices = c("Yes", "No", "Won't answer!"),
      multiple = FALSE,
      selectize = FALSE,
      size = 3
    ),
    actionButton("poll_1_send", label = "Send"),
    align = "center"
  ),

  # poll results plot
  conditionalPanel(
    "((output.is_viewer==true) || (output.done_poll_1==true)) && (output.act_object=='poll_1')",
    plotOutput("poll_1", click = "poll_1_click")
  ),

  # wordcloud input
  conditionalPanel(
    "(output.is_viewer==false) && (output.act_object=='wordcloud_1') && (output.done_wordcloud_1==false)",
    h3("Shiny Contest 2020 winner"),
    textAreaInput(
      "wordcloud_1_words",
      label = "Words (up to 1 words)",
      placeholder = "Type your words (separated by comma) ..."
    ),
    actionButton("wordcloud_1_send", "Send"),
    align = "center"
  ),

  # wordcloud results plot
  conditionalPanel(
    "((output.is_viewer==true) || (output.done_wordcloud_1==true)) && (output.act_object=='wordcloud_1')",
    wellPanel(plotOutput("wordcloud_1"))
  ),

  # rating slider
  conditionalPanel(
    "(output.is_viewer==false) && (output.act_object=='rating_1') && (output.done_rating_1==false)",
    h3("Rate interactingan:"),
    sliderInput(
      inputId = "rating_1_sel",
      label = "",
      step = 1,
      min = 1,
      max = 5,
      value = 3
    ),
    actionButton("rating_1_send", label = "Send"),
    align = "center"
  ),

  # rating results plot
  conditionalPanel(
    "((output.is_viewer==true) || (output.done_rating_1==true)) && (output.act_object=='rating_1')",
    plotOutput("rating_1")
  ),

  # question input
  conditionalPanel(
    "(output.is_viewer==false) && (output.act_object=='question_1')",
    h3("Feedback:"),
    checkboxInput("question_1_anonymous", "Anonymous"),
    textAreaInput(
      "question_1_question",
      "",
      placeholder = "Type your question (max of 160 characters)"
    ),
    actionButton("question_1_send", "Send"),
    align = "center"
  ),

  # questions viewer pane
  conditionalPanel(
    "(output.is_viewer==true) && (output.act_object=='question_1')",
    uiOutput("question_1_viewer")
  ),

)

server <- function(input, output, session) {
  # set user info, with random name and avatar
  rand_prof <- sample(avatars, 1)
  curr_user <-
    reactiveVal(data.frame(
      id = paste0(
        session$request$REMOTE_ADDR, ":", session$request$REMOTE_PORT
      ),
      name = rand_prof,
      avatar = rand_prof,
      stringsAsFactors = FALSE
    ))

  # if it is an audience user, then let it select avatar and name
  observeEvent(getQueryString(), {
    if (!is.null(getQueryString()$viewer) && getQueryString()$viewer == "IACC") {
      return()
    }

    # if the user previously logged in, then assign previous profile
    if (curr_user()$id %in% users()$id) {
      users <- users()
      curr_user(users[users$id == curr_user()$id, , drop = FALSE])
      return()
    }

    showModal(modalDialog(
      title = "My profile",
      fluidRow(
        column(6, selectInput(
          "profile_avatar", "",
          choices = avatars, selected = rand_prof
        )),
        column(6, htmlOutput("profile_avatar_show"))
      ),
      textInput("profile_name", "Your name", rand_prof),
      footer = fluidRow(
        actionButton("profile_submit", label = "Save")
      ),
      easyClose = FALSE,
      size = "s",
      align = "center"
    ))
    output$profile_avatar_show <- renderText(paste0(
      '<img src="',
      avatars_url, input$profile_avatar, '.png" height="120" width="120">'
    ))

    observeEvent(input$profile_submit, {
      user_name <- trimws(input$profile_name)
      if (user_name %in% users()$name) {
        showNotification("User name already taken", type = "error")
        return()
      }
      user <- curr_user()
      user$name <- user_name
      user$avatar <- input$profile_avatar
      users(rbind(users(), user))
      curr_user(user)
      removeModal()
    })
  })

  # is_viewer checks if it is the slides viewer user
  is_viewer <- reactiveVal(FALSE)
  output$is_viewer <- reactive({
    is_viewer(
      !is.null(getQueryString()$viewer) && getQueryString()$viewer == "IACC"
    )
  })
  outputOptions(output, "is_viewer", suspendWhenHidden = FALSE)

  output$is_admin <- reactive({
    is_viewer() && !is.null(getQueryString()$admin)
  })
  outputOptions(output, "is_admin", suspendWhenHidden = FALSE)

  output$admin_panel <- renderText({
    res <- ""
    if (!is.null(users())) {
      res <- paste0(
        res,
        paste0(
          "Users:\n",
          paste(apply(users(), 1, paste, collapse = " || "), collapse = "\n")
        ),
        "\n\n"
      )
    }
    if (!is.null(aud_qs())) {
      res <- paste0(
        res,
        paste(
          "Audience questions:",
          paste(
            apply(aud_qs(), 1, paste, collapse = " || "),
            collapse = "\n"
          ),
          sep = "\n"
        ),
        "\n\n"
      )
    }
    if (length(wordcloud_1_ans()) > 0) {
      res <- paste0(
        res,
        paste(
          "Shiny Contest 2020 winner",
          paste(names(wordcloud_1_ans()), wordcloud_1_ans(), collapse = "\n"),
          sep = "\n"
        ),
        "\n\n"
      )
    }
    if (length(unlist(poll_1_ans())) > 0) {
      poll_ans <- unlist(poll_1_ans())
      res <- paste0(
        res,
        paste(
          "Already seen heyshiny?",
          paste(names(poll_1_ans()), poll_1_ans(), collapse = "\n"),
          sep = "\n"
        ),
        "\n\n"
      )
    }
    if (length(rating_1_ans()) > 0) {
      rating_res <- rating_1_ans()
      rating_res <- by(names(rating_res), unlist(rating_res), as.character)
      res <- paste0(
        res,
        paste(
          "Rate interactingan:",
          paste(names(rating_res), rating_res, collapse = "\n"),
          sep = "\n"
        ),
        "\n\n"
      )
    }
    if (!is.null(question_1_ans())) {
      res <- paste0(
        res,
        paste(
          "Feedback:",
          paste(
            apply(question_1_ans(), 1, paste, collapse = " || "),
            collapse = "\n"
          ),
          sep = "\n"
        ),
        "\n\n"
      )
    }
    res
  })

  # show the selected object
  observeEvent(
    {
      getQueryString()
      input$act_obj
    },
    {
      if (!is.null(getQueryString()$viewer) && getQueryString()$viewer == "IACC") {
        act_object(input$act_obj)
      }
    }
  )
  output$act_object <- reactive({
    act_object()
  })
  outputOptions(output, "act_object", suspendWhenHidden = FALSE)

  # allow object selector through url
  observeEvent(getQueryString(), {
    if (
      !is.null(getQueryString()$viewer) &&
        getQueryString()$viewer == "IACC" &&
        !is.null(getQueryString()$object)
    ) {
      updateSelectInput(session, "act_obj", selected = getQueryString()$object)
    }
  })

  # audience question form (max of 160 chars per question)
  max_aud_q_chars <- 160
  observeEvent(input$aud_qs, {
    showModal(modalDialog(
      title = "Ask the speaker",
      HTML(paste0(
        '<img src="',
        avatars_url,
        curr_user()$avatar,
        '.png" height="40" width="40">'
      )),
      curr_user()$name,
      checkboxInput("q_anonymous", "Anonymous"),
      textAreaInput(
        "q_question",
        "",
        placeholder = paste0(
          "Type your question (max of ",
          max_aud_q_chars,
          " characters)"
        )
      ),
      footer = fluidRow(
        actionButton("send_q", label = "Send"),
        modalButton("Dismiss"),
        align = "center"
      ),
      easyClose = TRUE,
      size = "s",
      align = "center"
    ))
  })
  observeEvent(input$send_q, {
    question <- trimws(input$q_question)
    if (question == "") {
      showNotification("Please enter your question", type = "error")
      return()
    }
    if (nchar(question) > max_aud_q_chars) {
      showNotification(
        paste0(
          "Too many characters: ",
          nchar(question),
          " (max of ",
          max_aud_q_chars,
          ")"
        ),
        type = "error"
      )
      return()
    }
    question <- data.frame(
      user = curr_user()$id,
      name = ifelse(input$q_anonymous, "Anonymous", curr_user()$name),
      avatar = ifelse(input$q_anonymous, "", curr_user()$avatar),
      time = format(Sys.time(), "%H:%M"),
      question = question,
      stringsAsFactors = FALSE
    )
    if (is.null(aud_qs()) || !any(apply(aud_qs(), 1, function(x) all(x == question)))) {
      # solves when "send" is hit very fast
      aud_qs(rbind(aud_qs(), question))
      removeModal()
      showNotification("Question sent", type = "message")
    }
  })
  # show the questions, nicely printed in the pane
  output$aud_qs_viewer <- renderUI({
    aud_qs <- aud_qs()
    if (is.null(aud_qs) || nrow(aud_qs) == 0) {
      return(wellPanel())
    }
    wellPanel(
      apply(aud_qs, 1, function(x) {
        wellPanel(
          HTML(paste0(
            '<img src="',
            avatars_url, x["avatar"], '.png" ',
            'title="', ifelse(x["avatar"] == "", "", x["user"]),
            '" height="40" width="40">'
          )),
          h4(x["name"]),
          x["time"],
          HTML(gsub(" ", "&nbsp;", gsub("\n", br(), paste0("\n", x["question"]))))
        )
      })
    )
  })

  # check if the current user has already voted this poll
  output$done_poll_1 <- reactive({
    curr_user()$id %in% unlist(poll_1_ans())
  })
  outputOptions(output, "done_poll_1", suspendWhenHidden = FALSE)

  # for each answer, save the voters ids
  observeEvent(input$poll_1_send, {
    act_sels <- input$poll_1_sel
    act_ans <- poll_1_ans()
    for (act_sel in act_sels) {
      act_ans[[act_sel]] <- unique(c(act_ans[[act_sel]], curr_user()$id))
    }
    poll_1_ans(act_ans)
  })
  observeEvent(input$poll_1_click, {
    if (is_viewer()) {
      poll_1_show_correct(TRUE)
    }
  })
  # create the poll answers plot
  output$poll_1 <- renderPlot({
    act_ans <- poll_1_ans()
    opts <- names(act_ans)
    correct_opts <- c("")
    act_ans <- data.frame(
      Option = factor(opts, levels = opts),
      Correct = as.numeric(opts %in% correct_opts),
      N = unlist(lapply(act_ans, length))
    )
    act_ans$Votes <- 100 * act_ans$N / max(1, sum(act_ans$N))
    ggplot(act_ans) +
      (if (!poll_1_show_correct()) {
        geom_col(aes(x = Option, y = Votes, fill = Option))
      } else {
        geom_col(aes(x = Option, y = Votes, fill = Option, alpha = Correct))
      }) +
      geom_text(aes(x = Option, y = Votes, label = N), size = 12) +
      theme(
        legend.position = "none",
        text = element_text(size = 30)
      ) +
      (if (!poll_1_show_correct()) {
        ggtitle("Show correct")
      } else {
        ggtitle("")
      }) +
      scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))
  })

  # check if the current user has submited to this wordcloud
  output$done_wordcloud_1 <- reactive({
    curr_user()$id %in% names(wordcloud_1_ans())
  })
  outputOptions(output, "done_wordcloud_1", suspendWhenHidden = FALSE)

  # for each answer, save the voters id and words
  observeEvent(input$wordcloud_1_send, {
    words <- input$wordcloud_1_words
    words <- trimws(strsplit(words, ",")[[1]])
    if (length(words) == 0) {
      showNotification("Please enter your words", type = "error")
      return()
    }
    words <- words[seq_len(min(length(words), 1))]
    act_ans <- wordcloud_1_ans()
    act_ans[[curr_user()$id]] <- words
    wordcloud_1_ans(act_ans)
  })
  # create the wordcloud answers plot
  output$wordcloud_1 <- renderPlot({
    act_ans <- table(unlist(wordcloud_1_ans()))
    if (length(act_ans) == 0) {
      return()
    }
    set.seed(8818) # so everyone gets the same cloud
    wordcloud(
      names(act_ans),
      as.vector(act_ans),
      min.freq = 1,
      random.order = FALSE,
      colors = brewer.pal(8, "Dark2")
    )
  })

  # check if the current user has submited to this rating
  output$done_rating_1 <- reactive({
    curr_user()$id %in% names(rating_1_ans())
  })
  outputOptions(output, "done_rating_1", suspendWhenHidden = FALSE)

  # for each answer, save the voters ids and rating
  observeEvent(input$rating_1_send, {
    rating <- input$rating_1_sel
    act_ans <- rating_1_ans()
    act_ans[[curr_user()$id]] <- rating
    rating_1_ans(act_ans)
  })
  # create the rating answers plot
  output$rating_1 <- renderPlot({
    act_ans <- unlist(rating_1_ans())
    opts <- as.character(seq_len(5))
    ratings <- table(act_ans)
    ratings[opts[!opts %in% names(ratings)]] <- 0
    ratings <- ratings[order(as.numeric(names(ratings)))]
    score <- 0
    if (!is.null(act_ans)) {
      score <- round(mean(act_ans), 2)
    }
    act_ans <- data.frame(
      Rating = factor(opts, levels = opts),
      N = as.vector(ratings)
    )
    act_ans$Votes <- 100 * act_ans$N / max(1, sum(act_ans$N))
    ggplot(act_ans) +
      geom_col(aes(x = Rating, y = Votes, fill = Rating)) +
      geom_text(aes(x = Rating, y = Votes, label = N), size = 12) +
      ggtitle(paste0("Score: ", score)) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 30)
      ) +
      scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))
  })

  # for each question, save the user info
  observeEvent(input$question_1_send, {
    question <- trimws(input$question_1_question)
    if (question == "") {
      showNotification("Please enter your question", type = "error")
      return()
    }
    if (nchar(question) > 160) {
      showNotification(
        paste0(
          "Too many characters: ",
          nchar(question),
          " (max of ",
          160,
          ")"
        ),
        type = "error"
      )
      return()
    }
    question <- data.frame(
      user = curr_user()$id,
      name = ifelse(input$question_1_anonymous, "Anonymous", curr_user()$name),
      avatar = ifelse(input$question_1_anonymous, "", curr_user()$avatar),
      time = format(Sys.time(), "%H:%M"),
      question = question,
      stringsAsFactors = FALSE
    )
    if (is.null(question_1_ans()) || !any(apply(question_1_ans(), 1, function(x) all(x == question)))) {
      # solves when "send" is hit very fast
      question_1_ans(rbind(question_1_ans(), question))
      showNotification("Question sent", type = "message")
      updateTextAreaInput(session, "question_1_question", value = "")
    }
  })

  # show the questions, nicely printed in the pane
  output$question_1_viewer <- renderUI({
    act_qs <- question_1_ans()
    if (is.null(act_qs) || nrow(act_qs) == 0) {
      return(wellPanel())
    }
    wellPanel(
      apply(act_qs, 1, function(x) {
        wellPanel(
          HTML(paste0(
            '<img src="',
            avatars_url, x["avatar"], '.png" ',
            'title="', ifelse(x["avatar"] == "", "", x["user"]),
            '" height="40" width="40">'
          )),
          h4(x["name"]),
          x["time"],
          HTML(gsub(" ", "&nbsp;", gsub("\n", br(), paste0("\n", x["question"]))))
        )
      }),
    )
  })

}

shinyApp(ui = ui, server = server)
