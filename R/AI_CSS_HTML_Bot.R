CSS_UI_Bot = function() {
  tags$head(
    tags$style(HTML("
      /* Floating Action Button */
      #ai-fab {
        position: fixed;
        right: 30px;
        bottom: 30px;
        z-index: 1050;
        background: #34495e;
        color: white;
        width: 60px; height: 60px;
        border-radius: 50%;
        border: none;
        font-size: 32px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.2);
        cursor: pointer;
        display: flex; align-items: center; justify-content: center;
      }

      /* Main Chat Panel */
      #ai-chat-panel {
        position: fixed;
        right: 100px;
        bottom: 30px;
        width: 340px;
        height: 60vh;
        background: #f3f1e5;
        border-radius: 20px;
        box-shadow: 0 8px 32px rgba(0,0,0,0.20);
        z-index: 1051;
        display: flex;
        flex-direction: column;
        overflow: visible;
        border: 1px solid #dedede;
        resize: none;
        min-width: 260px;
        min-height: 200px;
        max-width: 90vw;
        max-height: 90vh;
      }
      
      #ai-chat-panel * {
        border-radius: 8px;
      }

      /* Resizer Bars (for all four sides) */
      .ai-resizer {
        position: absolute;
        z-index: 10;
        background: transparent;
      }
      .ai-resizer.top    { top: -4px; left: 0; right: 0; height: 8px;  cursor: ns-resize;}
      .ai-resizer.right  { top: 0; right: -4px; bottom: 0; width: 8px; cursor: ew-resize;}
      .ai-resizer.bottom { left: 0; right: 0; bottom: -4px; height: 8px; cursor: ns-resize;}
      .ai-resizer.left   { top: 0; left: -4px; bottom: 0; width: 8px; cursor: ew-resize;}

      /* Chat Header */
      #ai-chat-header {
        flex: 0 0 auto;
        height: 48px;
        background: #34495e;
        color: #fff;
        padding: 10px 15px;
        font-size: 18px;
        font-weight: bold;
        display: flex; justify-content: space-between; align-items: center;
      }
      
      #ai-header-actions {
        display: flex;
        align-items: center;
        gap: 10px;
      }

      .ai-header-btn {
        width: 28px;
        height: 28px;
        background: transparent;
        border: none;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: background 0.15s;
        cursor: pointer;
        padding: 0;
      }

      .ai-header-btn:hover {
        background: #e5ecf4;
      }

      .ai-header-btn i {
        font-size: 16px;
        color: #0077b6;
      }

      /* Message Display Area */
      #ai-chat-messages {
        flex: 1 1 auto;
        padding: 15px;
        overflow-y: auto;
        font-size: 16px;
        background: #f9f7ef;
        max-height: none;      /* Max message area height */
        min-height: 60px;
        border-bottom: 1px solid #eee;
        /* Scrollbar styling */
        scrollbar-width: thin;
        scrollbar-color: #888 #e6e6e6;
      }
      #ai-chat-messages::-webkit-scrollbar {
        width: 8px;
        background: #e6e6e6;
      }
      #ai-chat-messages::-webkit-scrollbar-thumb {
        background: #b0b0b0;
        border-radius: 4px;
      }

      /* Input Row */
      #ai_chat_input-row {
        flex-shrink: 0;
        display: flex;
        border-top: 1px solid #dedede;
        background: #f8f8f8;
        padding: 8px;
      }

      /* Text Input */
      #ai_chat_input {
        flex: 1;
        border: none;
        border-radius: 8px;
        padding: 2px 8px;
        font-size: 15px;
        outline: none;
      }

      /* Send Button */
      #ai_chat_send {
        margin-left: 10px;
        padding: 2px 10px;
        border-radius: 6px;
        border: none;
        background: #34495e;
        color: #fff;
        cursor: pointer;
        font-weight: bold;
      }
      
      #ai-chat-messages h1, #ai-chat-messages h2 { margin: 10px 0 4px 0; font-weight: bold;}
      #ai-chat-messages strong, #ai-chat-messages b { font-weight: bold;}
      #ai-chat-messages em, #ai-chat-messages i { font-style: italic;}
      #ai-chat-messages pre, #ai-chat-messages code { background: #f4f4f4; border-radius: 5px; padding: 2px 4px;}

    "))
  )
}

# Main jQuery events for AI Chat UI
HTML1_UI_Bot = function() {
  tags$script(HTML(
    '
      $(document).ready(function(){
        // Toggle chat panel when FAB is clicked (open if closed, close if open)
        $("#ai-fab").on("click", function(){
          var $panel = $("#ai-chat-panel");
          if ($panel.is(":visible")) {
            $panel.fadeOut();
          } else {
            $panel.fadeIn(function(){
              // Auto-scroll to the latest message when opened
              var m = document.getElementById("ai-chat-messages");
              m.scrollTop = m.scrollHeight;
            });
          }
        });

        // Close chat panel when close button is clicked
        $("#ai-chat-close").on("click", function(){
          $("#ai-chat-panel").fadeOut();
        });

        $(function () {
          $(\'[data-toggle="tooltip"]\').tooltip();
        });

        // Send message on Enter key in input box
        $(document).on("keydown", "#ai_chat_input", function(e) {
          if(e.key === "Enter" && !e.shiftKey) {
            e.preventDefault();
            $("#ai_chat_send").click();
            return false;
          }
        });
        
        // Send message to Shiny and clear input on button click
        $("#ai_chat_send").on("click", function(){
          var msg = $("#ai_chat_input").val();
          Shiny.setInputValue("ai_chat_input", msg, {priority: "event"});
          $("#ai_chat_input").val("");
        });

        // Auto-scroll on panel resize
        $("#ai-chat-panel").on("resize", function(){
          var m = document.getElementById("ai-chat-messages");
          m.scrollTop = m.scrollHeight;
        });
      });
    '
  ))
}

# Custom resizable logic for AI chat panel
HTML2_UI_Bot = function() {
  tags$script(HTML("
    $(function() {
      var $panel = $('#ai-chat-panel');
      var resizing = false, dir = '', startX = 0, startY = 0, startW = 0, startH = 0, startT = 0, startL = 0;
      
      // Start resizing when user presses mouse on any resizer
      $('.ai-resizer').on('mousedown', function(e){
        e.preventDefault();
        e.stopPropagation();
        resizing = true;
        // Detect which side is being resized
        dir = $(this).hasClass('top')    ? 'top' :
              $(this).hasClass('right')  ? 'right' :
              $(this).hasClass('bottom') ? 'bottom' :
              $(this).hasClass('left')   ? 'left' : '';
        startX = e.clientX;
        startY = e.clientY;
        startW = $panel.width();
        startH = $panel.height();
        startT = $panel.offset().top;
        startL = $panel.offset().left;
        $('body').css('user-select','none'); // Prevent text selection during resize
      });
      
      // Resize the panel on mouse move
      $(document).on('mousemove', function(e){
        if (!resizing) return;
        var dx = e.clientX - startX;
        var dy = e.clientY - startY;
        var minW = $panel.css('min-width').replace('px','')*1 || 200;
        var minH = $panel.css('min-height').replace('px','')*1 || 100;
        var maxW = $(window).width() * 0.9;
        var maxH = $(window).height() * 0.9;

        // Right border resizing
        if (dir == 'right') {
          var newW = Math.max(minW, Math.min(maxW, startW + dx));
          $panel.width(newW);
        }
        // Bottom border resizing
        if (dir == 'bottom') {
          var newH = Math.max(minH, Math.min(maxH, startH + dy));
          $panel.height(newH);
        }
        // Left border resizing (also moves panel horizontally)
        if (dir == 'left') {
          var newW = Math.max(minW, Math.min(maxW, startW - dx));
          if (newW !== startW) {
            $panel.width(newW);
            $panel.css('left', startL + dx);
            $panel.css('right', 'auto');
          }
        }
        // Top border resizing (also moves panel vertically)
        if (dir == 'top') {
          var newH = Math.max(minH, Math.min(maxH, startH - dy));
          if (newH !== startH) {
            $panel.height(newH);
            $panel.css('top', startT + dy);
            $panel.css('bottom', 'auto');
          }
        }
      // End resizing and restore user select
      }).on('mouseup', function(){
        resizing = false;
        dir = '';
        $('body').css('user-select','auto');
        var winW = $(window).width();
        var winH = $(window).height();
        var offset = $panel.offset();
        var panelW = $panel.outerWidth();
        var panelH = $panel.outerHeight();
        if (offset.left + panelW > winW) {
          $panel.css('left', winW - panelW);
        }
        if (offset.top + panelH > winH) {
          $panel.css('top', winH - panelH);
        }
        if (offset.left < 0) $panel.css('left', 0);
        if (offset.top < 0) $panel.css('top', 0);
        });
    });
    "))
}

# Make the AI FAB button draggable by the user
HTML3_UI_Bot = function() {
  tags$script(HTML("
    $(function() {
      var isDragging = false, offset = {x:0, y:0};

      // Start dragging on mouse or touch start
      $('#ai-fab').on('mousedown touchstart', function(e) {
        isDragging = true;
        var evt = e.type.startsWith('touch') ? e.originalEvent.touches[0] : e;
        var $this = $(this);
        offset.x = evt.clientX - $this.offset().left;
        offset.y = evt.clientY - $this.offset().top;
        $('body').css('user-select', 'none'); // Prevent text selection during drag
      });

      // Move the button as the mouse or finger moves
      $(document).on('mousemove touchmove', function(e) {
        if(!isDragging) return;
        var evt = e.type.startsWith('touch') ? e.originalEvent.touches[0] : e;
        var x = evt.clientX - offset.x;
        var y = evt.clientY - offset.y;
        // Restrict movement within the window
        var winW = $(window).width() - $('#ai-fab').outerWidth();
        var winH = $(window).height() - $('#ai-fab').outerHeight();
        x = Math.max(0, Math.min(winW, x));
        y = Math.max(0, Math.min(winH, y));
        $('#ai-fab').css({left: x, top: y, right: 'auto', bottom: 'auto', position: 'fixed'});
      });

      // Stop dragging on mouse or touch end
      $(document).on('mouseup touchend', function() {
        isDragging = false;
        $('body').css('user-select', ''); // Restore text selection
      });
    });
  "))
}

# Shiny custom handlers
HTML4_UI_Bot = function() {
  tags$head(
    tags$style(HTML(
      '
      .ai-thinking-dots span {
        opacity: 0.2;
        animation: ai-blink 1.4s infinite both;
        font-size: 1.2em;
        transition: opacity 0.2s;
        padding: 0 2px;
      }
      .ai-thinking-dots span:nth-child(1) { animation-delay: 0s; }
      .ai-thinking-dots span:nth-child(2) { animation-delay: 0.2s; }
      .ai-thinking-dots span:nth-child(3) { animation-delay: 0.4s; }

      @keyframes ai-blink {
        0%, 80%, 100% { opacity: 0.2; }
        40% { opacity: 1; }
      }
      '
    )),
    tags$script(HTML(
      '
      // AI Thinking
      Shiny.addCustomMessageHandler("addThinking", function(msg) {
        var m = document.getElementById("ai-chat-messages");
        var thinkingDiv = document.createElement("div");
        thinkingDiv.id = "ai-thinking-msg";
        thinkingDiv.style.margin = "3px 0";
        thinkingDiv.style.padding = "4px 8px";
        thinkingDiv.style.borderRadius = "8px";
        thinkingDiv.style.background = "#eee7f3";
        thinkingDiv.style.textAlign = "left";
        thinkingDiv.innerHTML = `
        <span class="ai-thinking-dots">
        <span>•</span><span>•</span><span>•</span>
        </span>
        `;
        m.appendChild(thinkingDiv);
        m.scrollTop = m.scrollHeight;
      });
      
      Shiny.addCustomMessageHandler("removeThinking", function(msg) {
        var tDiv = document.getElementById("ai-thinking-msg");
        if (tDiv) tDiv.remove();
      });

      // Shiny custom handler: add new message to chat window
      Shiny.addCustomMessageHandler("addMsg", function(msg) {
        // Handle typing/streaming message (single, updatable message)
        if(msg.who === "ai_typing") {
          var m = document.getElementById("ai-chat-messages");
          var typingDiv = document.createElement("div");
          typingDiv.id = "ai-typing-msg";
          typingDiv.style.margin = "3px 0";
          typingDiv.style.padding = "4px 8px";
          typingDiv.style.borderRadius = "8px";
          typingDiv.style.background = "#eee7f3";
          typingDiv.style.textAlign = "left";
          typingDiv.innerText = "";
          m.appendChild(typingDiv);
          m.scrollTop = m.scrollHeight;
          return;
        }
        // Handle normal (user or final AI) messages
        var d = document.createElement("div");
        d.style.margin = "3px 0";
        d.style.padding = "4px 8px";
        d.style.borderRadius = "8px";
        d.style.background = (msg.who == "user") ? "#e1e8f5" : "#eee7f3";
        d.style.textAlign = (msg.who == "user") ? "right" : "left";
        d.innerText = msg.msg;
        var m = document.getElementById("ai-chat-messages");
        m.appendChild(d);
        // Always scroll to latest message
        m.scrollTop = m.scrollHeight;
      });

      // Streaming: update typing message (keep content in a single bubble)
      Shiny.addCustomMessageHandler("updateTyping", function(msg) {
        var typingDiv = document.getElementById("ai-typing-msg");
        if (typingDiv) {
          typingDiv.innerHTML = marked.parse(msg.msg);
          var m = document.getElementById("ai-chat-messages");
          m.scrollTop = m.scrollHeight;
        }
      });

      // Streaming finished: finalize typing message as static
      Shiny.addCustomMessageHandler("finalizeTyping", function(msg) {
        var typingDiv = document.getElementById("ai-typing-msg");
        if (typingDiv) {
          typingDiv.innerHTML = marked.parse(msg.msg);
          typingDiv.id = ""; // Remove the id so a new stream can start next time
        }
      });
      '
    ))
  )
}
