library(shiny)
library(httr)
library(jsonlite)

# =========================================================
# Config
# =========================================================
# Inside Docker: API_BASE_URL=http://api:8000
# Local (no Docker network): API_BASE_URL=http://127.0.0.1:8000
API_BASE_URL   <- Sys.getenv("API_BASE_URL", "http://api:8000")

PREDICT_PATH <- "/predict"
HEALTH_PATH  <- "/health"

api_url <- function(path) paste0(API_BASE_URL, path)

# =========================================================
# Small helpers
# =========================================================
fmt_pct <- function(x, digits = 1) {
  if (is.null(x) || is.na(x)) return("N/A")
  paste0(round(100 * as.numeric(x), digits), "%")
}

risk_bucket <- function(p) {
  # Simple presentation-only buckets (not a clinical threshold).
  if (is.null(p) || is.na(p)) return(list(label = "N/A", cls = "risk-na"))
  p <- as.numeric(p)
  if (p < 0.20) return(list(label = "Low",      cls = "risk-low"))
  if (p < 0.50) return(list(label = "Moderate", cls = "risk-mod"))
  list(label = "High", cls = "risk-high")
}

clamp01 <- function(x) max(0, min(1, as.numeric(x)))

# =========================================================
# UI
# =========================================================
ui <- navbarPage(
  title = div(class = "app-title", "Heart Failure Mortality Prediction"),
  id = "nav",

  header = tags$head(
    tags$style(HTML("
      :root{
        --bg:#0b1220;
        --card:#111b2e;
        --card2:#0f172a;
        --text:#e5e7eb;
        --muted:#94a3b8;
        --accent:#38bdf8;
        --ok:#22c55e;
        --warn:#f59e0b;
        --bad:#ef4444;
        --border:rgba(148,163,184,.18);
      }
      body{ background: linear-gradient(180deg, var(--bg), #070b14 60%); color:var(--text); }
      .navbar{ background: rgba(17,27,46,.92) !important; border-bottom: 1px solid var(--border); }
      .navbar a, .navbar-brand{ color: var(--text) !important; }
      .app-title{ font-weight: 700; letter-spacing: .2px; }
      .container-fluid{ max-width: 1150px; }
      h2,h3,h4{ color: var(--text); }
      .muted{ color: var(--muted); }
      .card{
        background: rgba(17,27,46,.72);
        border: 1px solid var(--border);
        border-radius: 14px;
        padding: 16px 18px;
        box-shadow: 0 10px 30px rgba(0,0,0,.25);
        margin-bottom: 16px;
      }
      .card-title{ font-size: 16px; font-weight: 700; margin-bottom: 10px; }
      .hr{ height:1px; background: var(--border); margin: 12px 0; }
      .badge{
        display:inline-block; padding:6px 10px; border-radius: 999px;
        font-size: 12px; font-weight: 700; border: 1px solid var(--border);
        background: rgba(15,23,42,.6);
      }
      .badge-ok{ border-color: rgba(34,197,94,.35); color: var(--ok); }
      .badge-warn{ border-color: rgba(245,158,11,.35); color: var(--warn); }
      .badge-bad{ border-color: rgba(239,68,68,.35); color: var(--bad); }
      .btn-primary{
        background: linear-gradient(90deg, #0ea5e9, #22d3ee);
        border: none;
        font-weight: 800;
      }
      .btn-default, .btn-secondary{
        background: rgba(15,23,42,.55) !important;
        color: var(--text) !important;
        border: 1px solid var(--border) !important;
      }
      .progress{
        background: rgba(148,163,184,.16);
        border-radius: 999px;
        height: 12px;
        overflow: hidden;
        border: 1px solid var(--border);
      }
      .progress-bar{
        height: 100%;
        width: 0%;
        background: linear-gradient(90deg, #22c55e, #f59e0b, #ef4444);
      }
      .risk-pill{
        display:inline-block;
        padding:6px 10px;
        border-radius: 999px;
        font-weight: 800;
        font-size: 12px;
        border: 1px solid var(--border);
        background: rgba(15,23,42,.6);
      }
      .risk-low{ color: var(--ok); border-color: rgba(34,197,94,.35); }
      .risk-mod{ color: var(--warn); border-color: rgba(245,158,11,.35); }
      .risk-high{ color: var(--bad); border-color: rgba(239,68,68,.35); }
      .risk-na{ color: var(--muted); }
      .small-note{ font-size: 12px; color: var(--muted); line-height: 1.35; }
      .kv{ display:flex; gap:10px; flex-wrap: wrap; }
      .kv .k{ color: var(--muted); }
      .kv .v{ font-weight: 800; }
      .mono{
        background: rgba(15,23,42,.55);
        border: 1px solid var(--border);
        border-radius: 10px;
        padding: 10px 12px;
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;
        font-size: 12px;
        white-space: pre-wrap;
        overflow-x: auto;
      }
      /* Keep normal links readable */
      .navbar-default .navbar-nav > li > a{
        color: var(--text) !important;
      }

      /* Hover/focus background (optional) */
      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-nav > li > a:focus{
        background: rgba(56,189,248,.10) !important;
        color: var(--text) !important;
      }

      /* Active tab: avoid white background + give visible text */
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus{
        background: rgba(56,189,248,.18) !important;  /* dark-ish, not white */
        color: var(--accent) !important;              /* visible text */
        border-radius: 10px;
      }

    "))
  ),

  # ----------------------------
  # TAB 1: Predict
  # ----------------------------
  tabPanel(
    "Predict",
    fluidPage(
      br(),
      fluidRow(
        column(
          5,
          div(
            class = "card",
            div(class = "card-title", "Patient Inputs"),
            p(class = "small-note",
              "Inputs are validated locally and then sent to the Plumber API. ",
              "Ranges follow the API contract."
            ),
            div(class = "hr"),

            numericInput("age", "Age (years)", value = 60, min = 0, max = 120),
            numericInput("ejection_fraction", "Ejection fraction (%)", value = 40, min = 0, max = 100),
            numericInput("serum_creatinine", "Serum creatinine (mg/dL)", value = 1.2, min = 0.01, max = 20, step = 0.1),
            numericInput("serum_sodium", "Serum sodium (mEq/L)", value = 135, min = 90, max = 200, step = 1),

            div(class = "hr"),

            fluidRow(
              column(6, actionButton("btn_predict", "Run Prediction", class = "btn-primary", width = "100%")),
              column(3, actionButton("btn_demo", "Demo", class = "btn-default", width = "100%")),
              column(3, actionButton("btn_reset", "Reset", class = "btn-default", width = "100%"))
            ),

            div(class = "hr"),
            strong("API status: "),
            uiOutput("api_badge"),
            br(), br()
          )
        ),

        column(
          7,
          div(
            class = "card",
            div(class = "card-title", "Prediction Result"),
            div(class = "kv",
                span(class = "k", "Predicted probability of death:"),
                span(class = "v", textOutput("prob_percent", inline = TRUE))
            ),
            br(),
            div(class = "progress", div(class = "progress-bar", style = "width:0%", id = "risk_bar")),
            br(),
            strong("Risk category: "),
            uiOutput("risk_pill"),
            div(class = "hr"),
            strong("Input summary:"),
            br(),
            uiOutput("input_summary"),
            br(),
            checkboxInput("show_raw", "Show raw JSON response (debug)", value = FALSE),
            conditionalPanel(
              condition = "input.show_raw === true",
              div(class = "mono", verbatimTextOutput("raw_json"))
            )
          ),

          div(
            class = "card",
            div(class = "card-title", "Notes"),
            tags$ul(
              class = "small-note",
              tags$li("This is a university project demo. The risk bucket (Low/Moderate/High) is for presentation only."),
              tags$li("The API is a logistic regression model trained on the Heart Failure Clinical Records dataset."),
              tags$li("If the API is unreachable, check Docker Compose / port forwarding (Codespaces).")
            ),
            downloadButton("download_json", "Download prediction JSON", class = "btn-default")
          )
        )
      )
    )
  ),

  # ----------------------------
  # TAB 2: Model & Dataset
  # ----------------------------
  tabPanel(
    "Model & Dataset",
    fluidPage(
      br(),
      div(
        class = "card",
        div(class = "card-title", "Model (Phase 1)"),
        p(class = "muted",
          "Logistic regression. Final predictors used by the trained model:"
        ),
        tags$ul(
          class = "muted",
          tags$li("Age"),
          tags$li("Ejection fraction"),
          tags$li("Serum creatinine"),
          tags$li("Serum sodium"),
          tags$li("Age^2 term (I(age^2))")
        ),
        p(class = "small-note",
          "The Shiny app treats the model as a black box and calls the API endpoint /predict."
        )
      ),
      div(
        class = "card",
        div(class = "card-title", "API Contract"),
        p(class = "muted", "POST /predict expects JSON with: age, ejection_fraction, serum_creatinine, serum_sodium."),
        p(class = "muted", "GET /health returns a simple status for readiness checks.")
      )
    )
  ),

  # ----------------------------
  # TAB 3: Deployment
  # ----------------------------
  tabPanel(
    "Deployment",
    fluidPage(
      br(),
      div(
        class = "card",
        div(class = "card-title", "Run (Docker Compose)"),
        div(
          class = "mono",
          "docker compose up -d --build\n\n",
          "# Test\n",
          "curl http://localhost:8000/health\n",
          "curl -i -X POST http://localhost:8000/predict -H \"Content-Type: application/json\" -d '{\"age\":60,\"ejection_fraction\":40,\"serum_creatinine\":1.2,\"serum_sodium\":135}'\n\n",
          "# Stop\n",
          "docker compose down"
        ),
        p(class = "small-note",
          "In Codespaces, forward ports 8000 (API) and 3838 (Shiny) from the Ports tab."
        )
      )
    )
  )
)

# =========================================================
# Server
# =========================================================
server <- function(input, output, session) {

  # Store the last API response
  last_response <- reactiveVal(NULL)
  last_error    <- reactiveVal(NULL)

  # -----------------------------------------
  # API status badge (refresh every ~5 seconds)
  # -----------------------------------------
  output$api_badge <- renderUI({
    invalidateLater(5000, session)

    txt <- tryCatch({
      r <- GET(api_url(HEALTH_PATH), timeout(2))
      if (status_code(r) == 200) "OK" else paste("ERROR", status_code(r))
    }, error = function(e) "UNREACHABLE")

    cls <- if (identical(txt, "OK")) "badge badge-ok" else if (grepl("^ERROR", txt)) "badge badge-warn" else "badge badge-bad"
    span(class = cls, txt)
  })

  # -----------------------------------------
  # Buttons: demo/reset
  # -----------------------------------------
  observeEvent(input$btn_demo, {
    updateNumericInput(session, "age", value = 72)
    updateNumericInput(session, "ejection_fraction", value = 28)
    updateNumericInput(session, "serum_creatinine", value = 2.1)
    updateNumericInput(session, "serum_sodium", value = 130)
  })

  observeEvent(input$btn_reset, {
    updateNumericInput(session, "age", value = 60)
    updateNumericInput(session, "ejection_fraction", value = 40)
    updateNumericInput(session, "serum_creatinine", value = 1.2)
    updateNumericInput(session, "serum_sodium", value = 135)
    last_response(NULL)
    last_error(NULL)
  })

  # -----------------------------------------
  # Input summary
  # -----------------------------------------
  output$input_summary <- renderUI({
    div(
      class = "mono",
      paste0(
        "age = ", input$age, "\n",
        "ejection_fraction = ", input$ejection_fraction, "\n",
        "serum_creatinine = ", input$serum_creatinine, "\n",
        "serum_sodium = ", input$serum_sodium
      )
    )
  })

  # -----------------------------------------
  # Run prediction
  # -----------------------------------------
  observeEvent(input$btn_predict, {

    # UI-side validation (prevents obvious bad requests)
    # These ranges should mirror plumber.R validation.
    if (is.na(input$age) || input$age < 0 || input$age > 120) {
      showNotification("Invalid age (0–120).", type = "error")
      return()
    }
    if (is.na(input$ejection_fraction) || input$ejection_fraction < 0 || input$ejection_fraction > 100) {
      showNotification("Invalid ejection fraction (0–100).", type = "error")
      return()
    }
    if (is.na(input$serum_creatinine) || input$serum_creatinine <= 0 || input$serum_creatinine > 20) {
      showNotification("Invalid serum creatinine (>0–20).", type = "error")
      return()
    }
    if (is.na(input$serum_sodium) || input$serum_sodium < 90 || input$serum_sodium > 200) {
      showNotification("Invalid serum sodium (90–200).", type = "error")
      return()
    }

    payload <- list(
      age = input$age,
      ejection_fraction = input$ejection_fraction,
      serum_creatinine = input$serum_creatinine,
      serum_sodium = input$serum_sodium
    )

    last_error(NULL)

    withProgress(message = "Contacting API...", value = 0.3, {
      res <- tryCatch({
        POST(
          url = api_url(PREDICT_PATH),
          body = payload,
          encode = "json",
          timeout(10)
        )
      }, error = function(e) e)

      incProgress(0.5)

      if (inherits(res, "error")) {
        last_response(NULL)
        last_error(paste("API call failed:", res$message))
        showNotification("API call failed. Check API container/port forwarding.", type = "error")
        return()
      }

      if (status_code(res) != 200) {
        body_txt <- tryCatch(content(res, as = "text", encoding = "UTF-8"), error = function(e) "")
        last_response(NULL)
        last_error(paste0("API error ", status_code(res), "\n", body_txt))
        showNotification(paste("API error", status_code(res)), type = "error")
        return()
      }

      out <- tryCatch(content(res, as = "parsed", type = "application/json"), error = function(e) NULL)
      if (is.null(out)) {
        last_response(NULL)
        last_error("API response could not be parsed as JSON.")
        showNotification("Bad API response format (not JSON).", type = "error")
        return()
      }

      last_response(out)
      incProgress(0.2)
    })
  })

  # -----------------------------------------
  # Outputs based on last_response / last_error
  # -----------------------------------------
  output$prob_percent <- renderText({
    if (!is.null(last_error())) return("N/A")
    out <- last_response()
    if (is.null(out) || is.null(out$probability)) return("N/A")
    fmt_pct(out$probability, digits = 1)
  })

  output$risk_pill <- renderUI({
    if (!is.null(last_error())) {
      return(span(class = "risk-pill risk-na", "N/A"))
    }
    out <- last_response()
    if (is.null(out) || is.null(out$probability)) {
      return(span(class = "risk-pill risk-na", "N/A"))
    }
    rb <- risk_bucket(out$probability)
    span(class = paste("risk-pill", rb$cls), rb$label)
  })

  output$raw_json <- renderPrint({
    if (!is.null(last_error())) {
      cat(last_error())
      return(invisible(NULL))
    }
    out <- last_response()
    if (is.null(out)) {
      cat("No prediction yet.")
      return(invisible(NULL))
    }
    cat(toJSON(out, auto_unbox = TRUE, pretty = TRUE))
  })

  output$download_json <- downloadHandler(
    filename = function() {
      paste0("prediction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      if (!is.null(last_error())) {
        writeLines(toJSON(list(error = last_error()), auto_unbox = TRUE, pretty = TRUE), con = file)
        return()
      }
      out <- last_response()
      if (is.null(out)) out <- list(message = "No prediction yet.")
      writeLines(toJSON(out, auto_unbox = TRUE, pretty = TRUE), con = file)
    }
  )

  # -----------------------------------------
  # Update risk bar width via JS
  # -----------------------------------------
  observe({
    out <- last_response()
    err <- last_error()

    width_pct <- 0
    if (is.null(err) && !is.null(out) && !is.null(out$probability)) {
      width_pct <- round(100 * clamp01(out$probability))
    }
    session$sendCustomMessage("setRiskBar", list(width = paste0(width_pct, "%")))
  })

  session$onFlushed(function() {
    session$sendCustomMessage("setRiskBar", list(width = "0%"))
  }, once = TRUE)
}

# JS message handler
js <- "
Shiny.addCustomMessageHandler('setRiskBar', function(message) {
  var el = document.getElementById('risk_bar');
  if (el) el.style.width = message.width;
});
"

ui <- tagList(
  tags$head(tags$script(HTML(js))),
  ui
)

shinyApp(ui, server)
