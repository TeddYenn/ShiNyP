AI_Bot_Server <- function(input, output, session) {
  
  observeEvent(input$ai_chat_send, {
    user_msg <- input$ai_chat_input
    if (!is.null(user_msg) && nzchar(user_msg)) {
      # 1. Show user's message
      session$sendCustomMessage("addMsg", list(who = "user", msg = user_msg))
      updateTextInput(session, "ai_chat_input", value = "")
      
      # 2. Immediately show AI thinking animation
      session$sendCustomMessage("addThinking", list())
      
      # 3. RAG retrieval (LlamaCloud)
      llama_prompt <- NULL
      llama_error <- NULL
      tryCatch({
        llama_prompt <- llama_retrieve_answer(user_msg)
      }, error = function(e) {
        llama_error <<- paste("LlamaCloud retrieval failed:", e$message)
        llama_prompt <<- NULL
      })
      
      # 4. Compose system prompt
      if (!is.null(llama_prompt) &&
          is.character(llama_prompt) &&
          length(llama_prompt) > 0 &&
          !is.na(llama_prompt) &&
          !startsWith(llama_prompt, "Query failed")) {
        system_prompt <- c("You are a helpful technical assistant for ShiNyP platform users. Please refer to the following information and provide a detailed, easy-to-understand response using bullet points or numbered lists. Conclude your answer with a brief summary sentence in bold text, without using conjunctions.",
                           llama_prompt)
      } else {
        system_prompt <- "You're a helpful assistant."
      }
      
      # 5. Start Gemini chat (remove thinking animation, show typing bubble)
      session$sendCustomMessage("removeThinking", list())
      session$sendCustomMessage("addMsg", list(who = "ai_typing", msg = ""))
      
      chat_instance <- chat_google_gemini(
        system_prompt = system_prompt,
        base_url = "https://generativelanguage.googleapis.com/v1beta/",
        api_key = API_KEY_GOOGLE_Gemini,
        model = "gemini-2.0-flash-lite",
        echo = "none"
      )
      
      ai_reply <- ""
      
      # 6. Streaming Gemini reply
      coro::loop(for (chunk in chat_instance$stream(user_msg)) {
        ai_reply <- paste0(ai_reply, as.character(chunk))
        session$sendCustomMessage("updateTyping", list(msg = ai_reply))
      })
      
      # 7. Finalize AI reply
      session$sendCustomMessage("finalizeTyping", list(msg = ai_reply))
      
      # 8. Optional: Show RAG retrieval warning if failed
      if (!is.null(llama_error)) {
        session$sendCustomMessage("addMsg", list(
          who = "system",
          msg = paste0(
            "<span style='color:#C00;'>Notice: The knowledge search system failed to connect. Gemini will answer using general AI knowledge only.<br>Error: ",
            llama_error, "</span>")
        ))
      }
    }
  })
}