# app.R

library(shiny)
library(shinyjs)
library(ggplot2)
library(ggResidpanel)


# ============================================================
# DATA GENERATION FUNCTIONS
# ============================================================

# ------------------------------------------------------------
# Generate non-normal errors
# ------------------------------------------------------------

generate_errors <- function(n, type = NULL, sd = 1) {
  
  if (is.null(type)) {
    type <- sample(
      c(
        "heavy_tails",
        "positive_skew",
        "strong_skew",
        "mixture",
        "light_tails"
      ),
      1
    )
  }
  
  errors <- switch(
    type,
    
    # Heavy tails
    heavy_tails = {
      df <- sample(c(2, 3, 4, 5), 1)
      rt(n, df = df)
    },
    
    # Moderately positively skewed
    positive_skew = {
      df <- sample(c(3, 5, 8), 1)
      rchisq(n, df = df)
    },
    
    # Strong positive skew
    strong_skew = {
      rlnorm(
        n,
        meanlog = 0,
        sdlog = sample(c(0.6, 0.8, 1.0), 1)
      )
    },
    
    # Mostly normal observations with a small number
    # having much larger variance
    mixture = {
      ifelse(
        runif(n) < 0.9,
        rnorm(n, 0, 1),
        rnorm(n, 0, sample(c(4, 6, 8), 1))
      )
    },
    
    # Light-tailed distribution
    light_tails = {
      runif(n, -sqrt(3), sqrt(3))
    }
  )
  
  # Centre and scale so that the different distributions
  # have approximately comparable overall variance.
  errors <- errors - mean(errors)
  
  if (sd(errors) > 0) {
    errors <- errors / sd(errors) * sd
  }
  
  errors
}


# ------------------------------------------------------------
# Generate variance structures
# ------------------------------------------------------------

generate_variance <- function(x, type = NULL) {
  
  if (is.null(type)) {
    type <- sample(
      c(
        "increasing",
        "decreasing",
        "u_shape",
        "inverted_u",
        "wave"
      ),
      1
    )
  }
  
  z <- (x - min(x)) / (max(x) - min(x))
  
  switch(
    type,
    
    # Variance gets progressively larger
    increasing =
      0.25 + 2.0 * z,
    
    # Variance gets progressively smaller
    decreasing =
      2.25 - 2.0 * z,
    
    # Small at both ends, large in the middle
    u_shape =
      0.25 + 2.0 * abs(z - 0.5),
    
    # Large in the middle, small at the ends
    inverted_u =
      0.3 + 2.0 * (1 - abs(2 * z - 1)),
    
    # Several changes in variance
    wave =
      0.5 + 1.5 * sin(2 * pi * z)^2
  )
}


# ------------------------------------------------------------
# Generate nonlinear relationships
# ------------------------------------------------------------

generate_mean <- function(x, type = NULL) {
  
  if (is.null(type)) {
    type <- sample(
      c(
        "quadratic",
        "logarithmic",
        "sine",
        "threshold",
        "saturating"
      ),
      1
    )
  }
  
  switch(
    type,
    
    # Curvature
    quadratic =
      2 + 1.5 * x - 0.15 * x^2,
    
    # Diminishing returns
    logarithmic =
      2 + 4 * log(x + 1),
    
    # Oscillating relationship
    sine =
      5 + 2 * sin(x),
    
    # Change in slope
    threshold =
      2 + 0.5 * x + 4 * (x > median(x)),
    
    # Rapid rise followed by flattening
    saturating =
      2 + 12 * (1 - exp(-x / 3))
  )
}


# ============================================================
# SIMULATE DATA
# ============================================================

simulate_data <- function(
    n,
    normal = TRUE,
    equal_variance = TRUE,
    linear = TRUE,
    no_influential = TRUE,
    predictor_type = "Both with interaction"
) {
  
  # ----------------------------------------------------------
  # Predictors
  # ----------------------------------------------------------
  
  x <- runif(n, 0, 10)
  
  group <- factor(
    sample(
      c("A", "B"),
      n,
      replace = TRUE
    )
  )
  
  
  # ----------------------------------------------------------
  # Mean structure
  # ----------------------------------------------------------
  
  if (linear) {
    
    if (predictor_type == "Continuous only") {
      
      mu <- 2 + 1.5 * x
      
    } else if (predictor_type == "Categorical only") {
      
      mu <- 2 +
        ifelse(group == "B", 3, 0)
      
    } else if (predictor_type == "Both") {
      
      mu <- 2 +
        1.5 * x +
        ifelse(group == "B", 3, 0)
      
    } else {
      
      # Both predictors + interaction
      mu <- 2 +
        1.5 * x +
        ifelse(group == "B", 3, 0) +
        ifelse(group == "B", 0.8 * x, 0)
    }
    
  } else {
    
    # Randomly choose the type of non-linearity
    nonlinear_type <- sample(
      c(
        "quadratic",
        "logarithmic",
        "sine",
        "threshold",
        "saturating"
      ),
      1
    )
    
    nonlinear_x <- generate_mean(
      x,
      nonlinear_type
    )
    
    if (predictor_type == "Continuous only") {
      
      mu <- nonlinear_x
      
    } else if (predictor_type == "Categorical only") {
      
      mu <- 2 +
        ifelse(group == "B", 3, 0)
      
    } else if (predictor_type == "Both") {
      
      mu <- nonlinear_x +
        ifelse(group == "B", 3, 0)
      
    } else {
      
      # Nonlinear continuous effect + interaction
      mu <- nonlinear_x +
        ifelse(
          group == "B",
          3 + 0.8 * x,
          0
        )
    }
  }
  
  
  # ----------------------------------------------------------
  # Error distribution
  # ----------------------------------------------------------
  
  if (normal) {
    
    errors <- rnorm(
      n,
      mean = 0,
      sd = 2
    )
    
  } else {
    
    errors <- generate_errors(
      n,
      type = NULL,
      sd = 2
    )
  }
  
  
  # ----------------------------------------------------------
  # Equality of variance
  # ----------------------------------------------------------
  
  if (!equal_variance) {
    
    variance_type <- sample(
      c(
        "increasing",
        "decreasing",
        "u_shape",
        "inverted_u",
        "wave"
      ),
      1
    )
    
    variance_multiplier <- generate_variance(
      x,
      variance_type
    )
    
    errors <- errors * variance_multiplier
  }
  
  
  # ----------------------------------------------------------
  # Response
  # ----------------------------------------------------------
  
  y <- mu + errors
  
  
  # ----------------------------------------------------------
  # Influential point
  # ----------------------------------------------------------
  
  if (!no_influential) {
    
    # Select an observation towards the edge of the
    # predictor distribution.
    candidate_points <- which(
      x > quantile(x, 0.80)
    )
    
    i <- sample(
      candidate_points,
      1
    )
    
    # High leverage + unusual response
    x[i] <- max(x) + runif(1, 5, 10)
    
    y[i] <- max(y) +
      runif(1, 10, 25)
  }
  
  
  # ----------------------------------------------------------
  # Return data
  # ----------------------------------------------------------
  
  data.frame(
    y = y,
    x = x,
    group = group
  )
}


# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  # ----------------------------------------------------------
  # Global CSS
  # ----------------------------------------------------------
  
  tags$head(
    
    tags$style(HTML("

      body {
        font-family:
          'Segoe UI',
          Arial,
          sans-serif;
        background-color: #f7f8fa;
        color: #222222;
      }

      /* Banner */
      .app-banner {
        background-color: #0072CF;
        color: white;
        padding: 22px 30px;
        margin: -15px -15px 25px -15px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.15);
      }

      .app-banner h1 {
        font-family:
          'Segoe UI',
          Arial,
          sans-serif;
        font-size: 30px;
        font-weight: 600;
        margin: 0;
      }

      .app-banner p {
        font-size: 15px;
        margin: 5px 0 0 0;
        opacity: 0.92;
      }

      /* Sidebar */
      .well {
        background-color: white;
        border: 1px solid #e0e3e7;
        border-radius: 8px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.06);
      }

      /* Section headings */
      h3 {
        font-weight: 600;
      }

      h4 {
        font-weight: 600;
        color: #333333;
      }

      /* Primary button */
      .btn-primary {
        background-color: #0072CF;
        border-color: #0072CF;
        font-weight: 600;
      }

      .btn-primary:hover {
        background-color: #005fae;
        border-color: #005fae;
      }

      /* Diagnostic plot area */
      .shiny-plot-output {
        background-color: white;
        border-radius: 8px;
      }

      /* Checkbox spacing */
      .checkbox {
        margin-top: 12px;
        margin-bottom: 12px;
      }

      /* Slider */
      .irs-bar,
      .irs-bar-edge {
        background: #0072CF;
        border-top-color: #0072CF;
        border-bottom-color: #0072CF;
      }

    "))
    
  ),
  
  
  # ----------------------------------------------------------
  # Banner
  # ----------------------------------------------------------
  
  div(
    class = "app-banner",
    
    h1(
      "Linear Regression Diagnostics Practice"
    ),
    
    p(
      "Explore how violations of regression assumptions affect diagnostic plots."
    )
  ),
  
  
  # ----------------------------------------------------------
  # Main layout
  # ----------------------------------------------------------
  
  sidebarLayout(
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      h4("Simulation settings"),
      
      sliderInput(
        inputId = "n",
        label = "Sample size",
        min = 10,
        max = 100,
        value = 30,
        step = 5
      ),
      
      selectInput(
        inputId = "predictor_type",
        label = "Predictor structure",
        
        choices = c(
          "Continuous only",
          "Categorical only",
          "Both",
          "Both with interaction"
        ),
        
        selected = "Both with interaction"
      ),
      
      hr(),
      
      h4("Regression assumptions"),
      
      
      checkboxInput(
        inputId = "linear",
        label = "Linearity",
        value = TRUE
      ),
      
      checkboxInput(
        inputId = "normal",
        label = "Normality of residuals",
        value = TRUE
      ),
      
      checkboxInput(
        inputId = "equal_variance",
        label = "Equality of variance",
        value = TRUE
      ),
      
      checkboxInput(
        inputId = "no_influential",
        label = "Lack of influential points",
        value = TRUE
      ),
      
      hr(),
      
      h4("Diagnostic plots"),
      
      checkboxInput(
        inputId = "smoother",
        label = "Show smoother",
        value = TRUE
      ),
      
      hr(),
      
      actionButton(
        inputId = "simulate",
        label = "Simulate new dataset",
        class = "btn-primary",
        width = "100%"
      ),
      
      br(),
      br(),
      
      helpText(
        "Untick an assumption to deliberately introduce a "
        ,"randomly selected violation into the simulated data."
      )
    ),
    
    
    # ========================================================
    # MAIN PANEL
    # ========================================================
    
    mainPanel( 
      
      tabsetPanel( 
        
        tabPanel( "Diagnostic plots", 
      
                  h3("Diagnostic plots"), 
                  
                  plotOutput( outputId = "diagnostics", 
                              height = "850px" 
                              ) 
                  ), 
        
        tabPanel( "Simulated data", 
                  
                  h3("Simulated data"), 
                  
                  plotOutput( 
                    outputId = "data_plot", 
                    height = "500px" 
                    ) 
                  ) 
        ) 
      )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Simulate data
  # ----------------------------------------------------------
  
  dat <- eventReactive(
    input$simulate,
    
    {
      
      simulate_data(
        n = input$n,
        
        normal =
          input$normal,
        
        equal_variance =
          input$equal_variance,
        
        linear =
          input$linear,
        
        no_influential =
          input$no_influential,
        
        predictor_type =
          input$predictor_type
      )
      
    },
    
    ignoreNULL = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Fit model
  # ----------------------------------------------------------
  
  model <- reactive({
    
    d <- dat()
    
    if (
      input$predictor_type ==
      "Continuous only"
    ) {
      
      lm(
        y ~ x,
        data = d
      )
      
    } else if (
      input$predictor_type ==
      "Categorical only"
    ) {
      
      lm(
        y ~ group,
        data = d
      )
      
    } else if (
      input$predictor_type ==
      "Both"
    ) {
      
      lm(
        y ~ x + group,
        data = d
      )
      
    } else {
      
      lm(
        y ~ x * group,
        data = d
      )
    }
  })
  
  
  # ----------------------------------------------------------
  # ggResidpanel diagnostic plots
  # ----------------------------------------------------------
  
  output$diagnostics <- renderPlot({
    
    ggResidpanel::resid_panel(
      
      model(),
      
      plots = c(
        "resid",
        "qq",
        "ls",
        "cookd"
      ),
      
      smoother = input$smoother
    )
  })
  
  
  # ----------------------------------------------------------
  # Plot simulated data
  # ----------------------------------------------------------
  
  output$data_plot <- renderPlot({
    
    d <- dat()
    
    if (
      input$predictor_type ==
      "Continuous only"
    ) {
      
      ggplot(
        d,
        aes(
          x = x,
          y = y
        )
      ) +
        
        geom_point(
          size = 2
        ) +
        
        geom_smooth(
          method = "lm",
          se = FALSE
        ) +
        
        theme_minimal(
          base_family = "Segoe UI"
        ) +
        
        labs(
          x = "Continuous predictor",
          y = "Response"
        )
      
    } else if (
      input$predictor_type ==
      "Categorical only"
    ) {
      
      ggplot(
        d,
        aes(
          x = group,
          y = y
        )
      ) +
        
        geom_jitter(
          width = 0.1,
          height = 0,
          size = 2
        ) +
        
        geom_boxplot(
          alpha = 0.2
        ) +
        
        theme_minimal(
          base_family = "Segoe UI"
        ) +
        
        labs(
          x = "Categorical predictor",
          y = "Response"
        )
      
    } else {
      
      ggplot(
        d,
        aes(
          x = x,
          y = y,
          colour = group
        )
      ) +
        
        geom_point(
          size = 2
        ) +
        
        geom_smooth(
          method = "lm",
          se = FALSE
        ) +
        
        theme_minimal(
          base_family = "Segoe UI"
        ) +
        
        labs(
          x = "Continuous predictor",
          y = "Response",
          colour = "Group"
        )
    }
  })
}


# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)

