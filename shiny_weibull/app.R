library("shiny")
library("ggplot2")
library("dplyr")
library("patchwork")

# --------------------------------------------------
# 1. User Interface (UI) Definition
# --------------------------------------------------
ui <- fluidPage(
  # titlePanel("Interactive Weibull Distribution Comparison"),
  
  withMathJax(), # Enable LaTeX rendering for math notation
  
  # Input Sliders arranged in a fluidRow
  fluidRow(
    # Distribution 1 Inputs (Solid Line)
    column(4,
           wellPanel(
             div(style = "color: black; font-weight: bold;", "Distribution 1: solid line"),
             sliderInput("shape1", withMathJax("Shape"), min = 0.5, max = 5.0, value = 1.8, step = 0.1),
             sliderInput("scale1", withMathJax("Scale"), min = 0.1, max = 2.0, value = 1.0, step = 0.1)
           )
    ),
    
    # Distribution 2 Inputs (Dashed Line)
    column(4,
           wellPanel(
             div(style = "color: black; font-weight: bold;", "Distribution 2: dashed line"),
             sliderInput("shape2", withMathJax("Shape"), min = 0.5, max = 5.0, value = 2.2, step = 0.1),
             sliderInput("scale2", withMathJax("Scale"), min = 0.1, max = 2.0, value = 0.8, step = 0.1)
           )
    ),
    
    # Plot Limits Input
    column(4,
           wellPanel(
             div(style = "color: black; font-weight: bold;", "Plot Limits"),
             sliderInput("xmax_limit", "Time axis (max)", min = 1, max = 5, value = 3, step = 0.5)
           )
    )
  ),
  
  tags$hr(),
  
  # Output Plot
  fluidRow(
    column(12,
           plotOutput("weibull_plots", height = "320px")
    )
  )
)

# --------------------------------------------------
# 2. Server Logic
# --------------------------------------------------
server <- function(input, output) {
  
  # Set the custom theme for all plots within the server
  custom_theme <- theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(linewidth = 0.5, color = "black"),
      axis.line.y = element_line(linewidth = 0.5, color = "black"),
      legend.position = "none",
      text = element_text(size = 16)
    )
  
  output$weibull_plots <- renderPlot({
    
    # Data generation sequence based on t max slider
    x_seq <- seq(0, input$xmax_limit, length.out = 500)
    
    # Dist 1 (Solid) calculations
    alpha1 <- input$shape1
    beta1 <- input$scale1
    y_pdf <- dweibull(x_seq, alpha1, beta1)
    y_cdf <- pweibull(x_seq, alpha1, beta1)
    y_surv <- 1 - y_cdf
    y_haz <- y_pdf / y_surv
    
    # Dist 2 (Dashed) calculations
    alpha2 <- input$shape2
    beta2 <- input$scale2
    z_pdf <- dweibull(x_seq, alpha2, beta2)
    z_cdf <- pweibull(x_seq, alpha2, beta2)
    z_surv <- 1 - z_cdf
    z_haz <- z_pdf / z_surv
    
    # Long Format Data for all three plots
    df_pdf_long  <- data.frame(t = rep(x_seq, 2), ft = c(y_pdf, z_pdf),   Distribution = factor(rep(c("Dist1", "Dist2"), each = 500), levels = c("Dist1", "Dist2")))
    df_surv_long <- data.frame(t = rep(x_seq, 2), St = c(y_surv, z_surv), Distribution = factor(rep(c("Dist1", "Dist2"), each = 500), levels = c("Dist1", "Dist2")))
    df_haz_long  <- data.frame(t = rep(x_seq, 2), ht = c(y_haz, z_haz),   Distribution = factor(rep(c("Dist1", "Dist2"), each = 500), levels = c("Dist1", "Dist2")))
    
    # Determine Y limits dynamically for robust plotting
    y_max_pdf <- max(df_pdf_long$ft[is.finite(df_pdf_long$ft)]) * 1.05
    y_max_haz <- max(df_haz_long$ht[is.finite(df_haz_long$ht) & df_haz_long$t > 0.01]) * 1.05
    
    # --- Shared Aesthetic Setup ---
    shared_aesthetics <- list(
      geom_line(linewidth = 0.8, color = "black"),
      scale_linetype_manual(values = c("Dist1" = "solid", "Dist2" = "dashed")),
      scale_x_continuous(name = "Time (t)", expand = expansion(mult = c(0, 0.05)))
    )
    
    # --- Plot 1: PDF (f(t)) ---
    p1 <- ggplot(df_pdf_long, aes(x = t, y = ft, linetype = Distribution)) +
      shared_aesthetics +
      labs(y = expression(f(t))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, y_max_pdf)) +
      custom_theme
    
    
    # --- Plot 2: Survival (S(t)) ---
    p2 <- ggplot(df_surv_long, aes(x = t, y = St, linetype = Distribution)) +
      shared_aesthetics +
      labs(y = expression(S(t))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 1.05)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      custom_theme
    
    # --- Plot 3: Hazard (h(t)) ---
    p3 <- ggplot(df_haz_long, aes(x = t, y = ht, linetype = Distribution)) +
      shared_aesthetics +
      labs(y = expression(h(t))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, y_max_haz)) +
      custom_theme
    
    # Combine plots side-by-side using patchwork
    p1 + p2 + p3 + plot_layout(ncol = 3)
  }, res = 96) # Set resolution for better image quality
}

# --------------------------------------------------
# 3. Run the application
# --------------------------------------------------
shinyApp(ui, server)
