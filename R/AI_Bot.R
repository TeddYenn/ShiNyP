AI_Bot_Server <- function(input, output, session) {
  
  observeEvent(input$ai_chat_send, {
    user_msg <- input$ai_chat_input
    if (!is.null(user_msg) && nzchar(user_msg)) {
      # 1. Show user's message
      session$sendCustomMessage("addMsg", list(who = "user", msg = user_msg))
      
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
        system_prompt <- c(
          "You are a technical assistant for ShiNyP platform users. Please follow these principles when responding:\n",
          
          "1. Address the user's question directly and provide a logical, relevant answer based on the provided information.\n",
          "2. If the reference information is not helpful for answering the user's question, do not use it.\n",
          "3. Use bullet points or numbered lists to organize your response clearly.\n",
          "4. Conclude your answer with a brief summary in bold text, if suitable. Do not use conjunctions in the summary.\n",
          "5.Reference information is provided by the developer of ShiNyP.\n",
          "Reference information: ",
          llama_prompt
        )
      } else {
        system_prompt <- "You are a helpful assistant."
      }
      
      # 5. Start Gemini chat (remove thinking animation, show typing bubble)
      session$sendCustomMessage("removeThinking", list())
      session$sendCustomMessage("addMsg", list(who = "ai_typing", msg = ""))
      
      ai_reply <- ""
      gemini_error <- NULL
      
      # 6. Streaming Gemini reply (with tryCatch)
      tryCatch({
        chat_instance <- chat_google_gemini(
          system_prompt = system_prompt,
          base_url = "https://generativelanguage.googleapis.com/v1beta/",
          api_key = KEY,
          model = "gemini-2.5-flash-lite-preview-06-17",
          echo = "none"
        )
        coro::loop(for (chunk in chat_instance$stream(user_msg)) {
          ai_reply <- paste0(ai_reply, as.character(chunk))
          session$sendCustomMessage("updateTyping", list(msg = ai_reply))
        })
      }, error = function(e) {
        gemini_error <<- paste(
          "Error:", e$message
        )
        ai_reply <<- NULL
      })
      
      # 7. Finalize AI reply
      if (!is.null(ai_reply)) {
        session$sendCustomMessage("finalizeTyping", list(msg = ai_reply))
      } else {
        session$sendCustomMessage("finalizeTyping", list(
          msg = "<span style='color:#C00;'>Sorry, the ShiNyP AI failed to generate a response at this time.</span>"
        ))
      }
      
      # 8. Optional: Show RAG retrieval warning if failed
      if (!is.null(llama_error)) {
        session$sendCustomMessage("addMsg", list(
          who = "system",
          msg = paste0("Error: ", llama_error)
        ))
      }
      
      # 9. Optional: Show Gemini error if occurred
      if (!is.null(gemini_error)) {
        session$sendCustomMessage("addMsg", list(
          who = "system",
          msg = gemini_error))
      }
    }
  })
}
