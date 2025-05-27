CSS_UI = function() {
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Sans Serif Collection', sans-serif;
        letter-spacing: 0.01em;
        word-spacing: 0.12em;
        line-height: 1.6;
      }
    
      body.modal-open {
        overflow: auto !important;
      }

      .title-text-style {
        font-size: 20px;
        font-weight: bold;
        color: #853717;
      }

      .run-action-button {
        color: #fff !important;
        background-color: #007ACC !important;
      }

      .run-action-button:hover {
        color: #fff !important;
        background-color: #025e9c !important;
      }

      .S-action-button {
        background-color: #b9d8ed !important;
      }

      .S-action-button:hover {
        color: #fff !important;
        background-color: #007ACC !important;
      }

      .AI1-action-button {
        color: #fff !important;
        background-color: #00a595 !important;
      }

      .AI1-action-button:hover {
        color: #fff !important;
        background-color: #006a60 !important;
      }

      .AI2-action-button {
        background-color: #f7f7ff !important;
      }

      .AI2-action-button:hover {
        background-color: #eaeaed !important;
      }

      .web-button {
        background-color: #99866a !important;
        color: #efefef !important;
        margin-top: 10px;
        margin-bottom: 10px;
        font-size: 15px;
        border-radius: 5px;
        padding: 8px 12px;
      }

      .web-button:hover {
        background-color: #c9bfb0 !important;
        color: #544939 !important;
      }

      .guide-text-block {
        white-space: pre-wrap;
        font-size: 16px;
        color: #333333;
        background: linear-gradient(145deg, #f0f4f8, #e0e6ed);
        padding: 15px;
        border: 1px solid #d0d9e3;
        border-radius: 8px;
        box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
      }

      .progress-bar {
        background-color: #b68d4d;
      }

      .dashed-hr {
        border: none;
        border-top: 1px dashed #000;
        margin: 20px 0;
      }

      .AIReport-tab {
        background-color: #e6e6f0;
        color: #081142;
        border: 1px solid #b0b0cc;
        box-shadow: 0 0 10px rgba(0, 122, 204, 0.4);
        padding: 20px;
        border-radius: 10px;
      }

      .AIReport-tab .form-control {
        background-color: #f7f7ff;
        color: #081142;
        border: 1px solid #b0b0cc;
        border-radius: 6px;
      }

      .AIReport-tab .form-control:focus {
        border-color: #007ACC;
        box-shadow: 0 0 8px rgba(0, 122, 204, 0.6);
      }

      .AIReport-tab .custom-h4 {
        color: #112288;
        font-weight: bold;
        font-size: 24px;
      }

      .AIReport-tab #AI_response1 {
        background-color: #ffffff;
        border: 1px solid #cccccc;
        padding: 20px;
        margin-top: 20px;
        border-radius: 5px;
        box-shadow: 0 0 5px #cccccc;
        white-space: pre-wrap;
      }

      .AIReport-tab #AI_response2 {
        background-color: #ffffff;
        border: 1px solid #cccccc;
        padding: 20px;
        margin-top: 20px;
        border-radius: 5px;
        box-shadow: 0 0 5px #cccccc;
        white-space: pre-wrap;
      }
    "))
  )
}