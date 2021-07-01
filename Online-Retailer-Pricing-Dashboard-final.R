rm(list = ls())

list.of.packages <- c("shiny", "shinyWidgets", "shinyjs", "ggplot2", "minpack.lm", "sf", "rgeos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) { install.packages(new.packages, dependencies = T); print("Packages updated.") } else { print("All packages OK!") }

### Libraries to include
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(minpack.lm)
library(sf)
library(rgeos)

set.seed(123)

### User Interface Design
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  h2("Online Retailer Pricing Model Optimization", style = "color: darkblue; font-size: 20px; font-weight: bold; text-align: center;"),
  
  fluidRow(
    column(10, 
           plotOutput("mpgPlot")
    ),
    column(2,
           verticalLayout(
             actionButton("plot_btn", "Plot!", width = 100),
             actionButton("optimize_btn", "Optimize", width = 100),
             actionButton("export_btn", "Export!", width = 100),
             actionButton("clear_btn", "Clear!", width = 100),
             actionButton("exit_btn", "Exit!", width = 100),
             numericInput("real_prod_price", "Price [$]:", 50, min = 0.01, max = 99999, step = 1),
            
             tags$h5("Demand", style = "font-weight: bold; "),
             verbatimTextOutput("dem"),
             tags$head(tags$style(HTML("#dem { width: 100px; height: 40px; } "))),
             tags$h5("Profit", style = "font-weight: bold; "),
             verbatimTextOutput("pr"),
             tags$head(tags$style(HTML("#pr { width: 100px; height: 40px; } ")))
           )
    )
  ),
  
  fluidRow(
    column(2, style='padding-left:5px; padding-right:2px;',
           tags$h5("Online order inconvenience:", style = "font-weight: bold; font-size: 90%;"),
           numericInput("inconvenience_online", NA, 0.20, min = 0, max = 0.5, step = 0.01),
           tags$h5("Mitigation factor:", style = "font-weight: bold; font-size: 90%;"),
           sliderInput("inconvenience_factor", NA, "min" = 0, "value" = 0.25, "max" = 1, "step" = 0.01)
    ),
    column(2, style='padding-right:1px;',
           tags$h5("Priority cost for retailer [$]:", style = "font-weight: bold; font-size: 90%;"),
           numericInput("shipment_real_prc", NA, 24.99, min = 0.00, max = 30.00, step = 0.01),
           tags$h5("Expedited cost for retailer [$]:", style = "font-weight: bold; font-size: 90%;"),
           numericInput("shipment_real_exp", NA, 5.82, min = 0.00, max = 30.00, step = 0.01),
           tags$h5("Standard cost for retailer [$]:", style = "font-weight: bold; font-size: 90%;"),
           numericInput("shipment_real_std", NA,  3.32, min = 0.00, max = 30.00, step = 0.01)
    ),
    column(2, style='padding-right:1px;',
           tags$h5("Priority quoted to customer[$]:", style = "font-weight: bold; font-size: 90%;"),
           numericInput("shipment_quoted_prc", NA, 29.99, min = 0.00, max = 40.00, step = 0.01),
           tags$h5("Expedited quoted to customer[$]:", style = "font-weight: bold; font-size: 90%;"),
           numericInput("shipment_quoted_exp", NA, 6.99, min = 0.00, max = 40.00, step = 0.01),
           tags$h5("Standard quoted to customer[$]:", style = "font-weight: bold; font-size: 90%;"),
           numericInput("shipment_quoted_std", NA, 3.99, min = 0.00, max = 40.00, step = 0.01)
    ),
    column(2, style='padding-right:1px;',
           tags$h5("Surcharge for priority:", style = "font-weight: bold; font-size: 90%;"),
           verbatimTextOutput("surch_prc"),
           tags$head(tags$style(HTML("#surch_prc { width: 100px; height: 34px; } "))),
           tags$h5("Surcharge for expedited:", style = "font-weight: bold; font-size: 90%; padding-top: 5px;"),
           verbatimTextOutput("surch_exp"),
           tags$head(tags$style(HTML("#surch_exp { width: 100px; height: 34px; } "))),
           tags$h5("Surcharge for standard:", style = "font-weight: bold; font-size: 90%; padding-top: 5px;"),
           verbatimTextOutput("surch_std"),
           tags$head(tags$style(HTML("#surch_std { width: 100px; height: 34px; } ")))
    ),
    column(2, style='padding-right:1px; padding-left:0px;',
           tags$h5("Extra sourcing & handling costs:", style = "font-weight: bold; font-size: 90%;"),
           sliderInput("handling", NA, "min" = 0, "value" = 0.25, "max" = 1, "step" = 0.01),
           tags$h5("Monetary value of 1 day [$]:", style = "font-weight: bold; font-size: 90%;"),
           sliderInput("value_of_one_day", NA, "min" = 0.05, "value" = 0.05, "max" = 25, "step" = 0.05),
           checkboxInput("opp", "Provide pickup delivery?", value = T),
           tags$head(tags$style(HTML("#opp { font-weight: bold; font-size: 90%; } ")))
    ),
    column(2, style='padding-right:2px;',
           tags$h5("Priority lead time [days]:", style = "font-weight: bold; font-size: 90%;"),
           uiOutput("aux_prc"),
           tags$h5("Expedited lead time [days]:", style = "font-weight: bold; font-size: 90%;"),
           uiOutput("aux_exp"),
           tags$h5("Standard lead time [days]:", style = "font-weight: bold; font-size: 90%;"),
           numericInput("leadtime_std", NA, 44, min = 1, max = 100, step = 1)
    )
  )
)


### Server functionality 
server <- function(input, output, session) {
  
  what_was_aux_prc <- reactiveVal()
  what_was_aux_exp <- reactiveVal()
  
  output$aux_prc <- renderUI({
    x_b <- req(input$leadtime_exp)
    numericInput("leadtime_prc", NA, value = max(1, what_was_aux_prc()), min = 1, max = x_b, step = 1)
  })
  
  output$aux_exp <- renderUI({
    x_c <- req(input$leadtime_std)
    numericInput("leadtime_exp", NA, value = max(1, what_was_aux_exp()), min = 1, max = x_c, step = 1)
  })
  
  observe({
    what_was_aux_prc(5)
  })
  
  observe({
    what_was_aux_exp(22)
  })
  
  #PRC_MIN     <- reactive({ 1 })
  PRC_MAX     <- reactive({ input$leadtime_prc })
  #PRC_LENGTH  <- reactive({ input$leadtime_prc })
  #EXP_MIN     <- reactive({ input$leadtime_prc + 1 })
  EXP_MAX     <- reactive({ input$leadtime_exp })
  #EXP_LENGTH  <- reactive({ input$leadtime_exp-input$leadtime_prc })
  #STD_MIN     <- reactive({ input$leadtime_exp + 1 })
  STD_MAX     <- reactive({ input$leadtime_std })
  #STD_LENGTH  <- reactive({ input$leadtime_std - input$leadtime_exp })
  attempt_max <- 500
  
  ### Disabling the two buttons that can only be used after optimization has been done
  #shinyjs::disable("export_btn")
  shinyjs::disable("clear_btn")
  shinyjs::disable("optimize_btn")

  ### Basic information message before plotting
  output$mpgPlot <- renderPlot({
    ggplot() + 
      coord_cartesian(xlim = c(0, STD_MAX()), ylim = c(0, 1) ) +
      xlab("Days of delivery") +
      ylab("Aggregated Relative Cost [-]") +
      theme(axis.line = element_line()) +
      scale_x_continuous("Delivery lead time [days]", seq(0,ceiling(STD_MAX()/5)*5,5)) +
      scale_y_continuous("Aggregated Relative Cost [-]", seq(0,1,0.1)) +
      geom_label(aes(x = STD_MAX()/2, y = 0.5, label = "In order to see the plotted results, please set the input \n parameters on the tab below and then press the plot button!"), fill = "red", size = 6)
    })
  
  ### Product price curve fit
  real_prod_price    <- reactive({ input$real_prod_price })

  ## Amount paid by the retailer: real cost
  shipment_real_prc  <- reactive({ input$shipment_real_prc })
  shipment_real_exp  <- reactive({ input$shipment_real_exp })
  shipment_real_std  <- reactive({ input$shipment_real_std })

  ## Amount quoted to the customer
  shipment_quoted_prc<- reactive({ input$shipment_quoted_prc })
  shipment_quoted_exp<- reactive({ input$shipment_quoted_exp })
  shipment_quoted_std<- reactive({ input$shipment_quoted_std })
  
  ## Surcharge factors to verbatimOutputs
  output$surch_prc      <- renderText({ round(shipment_quoted_prc()/shipment_real_prc(),4) }) 
  output$surch_exp      <- renderText({ round(shipment_quoted_exp()/shipment_real_exp(),4) })
  output$surch_std      <- renderText({ round(shipment_quoted_std()/shipment_real_std(),4) })
  
  ds <- data.frame("price" = c(rep(NULL, 4)), "shipmt" = c(rep(NULL,4)))
  
  # See notes on 3rd October 2019
  ShipmentCurve <- function(s, p = real_prod_price(), nlmInitial = list(a = -0.5, power1 = -1, power2 = -1) ) {
    
    ds <- data.frame("price" = c(0.67, max(p, 0.68), 999, 1000), "shipmt" = c(1, s*0.5/p, s*0.5/999, s*0.5/1000))
    
    #nlmInitial <- list(a = 0.5, power1 = 1, power2 = -0.5)
    
    m <- nlsLM(shipmt ~ a*I(price^power1) + I(price^power2), 
          data = ds,
          start = nlmInitial,
          trace = FALSE,
          control = nls.lm.control(maxiter = 1024))

    summary(m)$coefficients[,1]
  }
  
  ## Cost of one single day for the customer segment
  one_day_cost <- reactive({ input$value_of_one_day })
  
  ## Inconvenience
  rel_inconv_price   <- reactive({ input$inconvenience_online })
  inconv_mit_factor  <- reactive({ input$inconvenience_factor })
  
  ## Handling cost
  rel_handling       <- reactive({ input$handling })

  ## Provide OPP channel
  opp_checked        <- reactive({ input$opp })
  
  
  curve_fit <- reactiveValues(init_price = NA,
                              coeffs = NA,
                              Convert = function(x) return(NA))
  
  PriceCurveFit <- function(pp, nlmInitial = list(a = 0.5, power1 = -1, power2 = 0.5)) {
    ds <- data.frame("real" = c(0.01, max(pp,0.02), 999, 1000), "relative" = c(0, 0.5, 0.999, 1))
    
    m <- nlsLM(relative ~ a*I(real^power1) + I(real^power2),
          data = ds,
          start = nlmInitial,
          trace = FALSE,
          control = nls.lm.control(maxiter = 1024))
    
    summary(m)$coefficients[,1]
  }
  
  observeEvent(input$plot_btn, {
    curve_fit[['init_price']] <- real_prod_price()
    curve_fit[['coeffs']] <- PriceCurveFit(pp = real_prod_price())
    shinyjs::enable("clear_btn")
  })
  
  observeEvent(curve_fit[['coeffs']], {
    curve_fit[['Convert']] <- function(x) {
      (curve_fit[['coeffs']][1])*x^(curve_fit[['coeffs']][2]) + x^(curve_fit[['coeffs']][3])
    }
  }) 

  ### Aggregated costs
  AggregatedRelativeCost <- function(m_price, m_inc_price, m_inc_fact, coeff, type = c("online", "opp")) {
    if(type == "online") {
      return(curve_fit$Convert(m_price) + m_inc_price + (coeff[1])*(m_price)^(coeff[2]) + (m_price)^(coeff[3]))
    } else {
      return(curve_fit$Convert(m_price) + m_inc_price*(1-m_inc_fact) + (coeff[1])*(m_price)^(coeff[2]) + (m_price)^(coeff[3]))
    }
  }
  
  discount_curve <- function(x, pp = curve_fit[['init_price']], dp = one_day_cost()) {  1 - ( dp/(2*pp) ) * x }
  
  inv_discount_curve <- function(y, pp = curve_fit[['init_price']], dp = one_day_cost()) { (1 - y) / ( dp/(2*pp) ) }
  
  maxim <- function(x) { min(1,x) }


  ### Defining profit (and demand) calculating function
  ProfitCalc <- function(m_price = real_prod_price(),
                         m_shipment_quoted_prc = shipment_quoted_prc(),
                         m_shipment_quoted_exp = shipment_quoted_exp(),
                         m_shipment_quoted_std = shipment_quoted_std(),
                         m_shipment_cost_prc = shipment_real_prc(), 
                         m_shipment_cost_exp = shipment_real_exp(),
                         m_shipment_cost_std = shipment_real_std(), 
                         m_handling = rel_handling(),
                         m_day_price = one_day_cost(),
                         m_curve_fit = curve_fit[['coeffs']],
                         m_prc_max = PRC_MAX(), 
                         m_exp_max = EXP_MAX(),
                         m_std_max = STD_MAX()) {
    
    
    m_coeff_prc <- NULL
    attempt <- 0
    while( is.null(m_coeff_prc) && attempt <= attempt_max ) {
      attempt <- attempt + 1
      try({
        if(attempt == 1) { 
          m_coeff_prc <- ShipmentCurve(s = m_shipment_quoted_prc, p = curve_fit[['init_price']])
        } else {
          rnd = runif(1); print(attempt)
          m_coeff_prc <- ShipmentCurve(s = m_shipment_quoted_prc, p = curve_fit[['init_price']], nlmInitial = list(a = -0.5*runif(1), power1 = -1*rnd, power2 = -1*rnd))
        }
      })
    }
    
    m_coeff_exp <- NULL
    attempt <- 0
    while( is.null(m_coeff_exp) && attempt <= attempt_max ) {
      attempt <- attempt + 1
      try({
        if(attempt == 1) { 
          m_coeff_exp <- ShipmentCurve(s = m_shipment_quoted_exp, p = curve_fit[['init_price']])
        } else {
          rnd = runif(1); print(attempt)
          m_coeff_exp <- ShipmentCurve(s = m_shipment_quoted_exp, p = curve_fit[['init_price']], nlmInitial = list(a = -0.5*runif(1), power1 = -1*rnd, power2 = -1*rnd))
        }
      })
    }
    
    m_coeff_std <- NULL
    attempt <- 0
    while( is.null(m_coeff_std) && attempt <= attempt_max ) {
      attempt <- attempt + 1
      try({
        if(attempt == 1) { 
          m_coeff_std <- ShipmentCurve(s = m_shipment_quoted_std, p = curve_fit[['init_price']])
        } else {
          rnd = runif(1); print(attempt)
          m_coeff_std <- ShipmentCurve(s = m_shipment_quoted_std, p = curve_fit[['init_price']], nlmInitial = list(a = -0.5*runif(1), power1 = -1*rnd, power2 = -1*rnd))
        }
      })
    }
    
    m_AC_prc_online    <- AggregatedRelativeCost(m_price = m_price, m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = m_coeff_prc, type = "online")
    m_AC_exp_online    <- AggregatedRelativeCost(m_price = m_price, m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = m_coeff_exp, type = "online")
    m_AC_std_online    <- AggregatedRelativeCost(m_price = m_price, m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = m_coeff_std, type = "online")
    m_AC_prc_opp       <- AggregatedRelativeCost(m_price = m_price, m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = m_coeff_prc, type = "opp")
    m_AC_exp_opp       <- AggregatedRelativeCost(m_price = m_price, m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = m_coeff_exp, type = "opp")
    m_AC_std_opp       <- AggregatedRelativeCost(m_price = m_price, m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = m_coeff_std, type = "opp")
    
    m_disc_curve <- function(x, pp = curve_fit[['init_price']], dp = m_day_price) { 1 - ( dp/(2*pp) ) * x }

    m_inverse_disc <- function(y, pp = curve_fit[['init_price']], dp = m_day_price) { (1-y)/(dp/(2*pp)) }
    
    
    ### Function to output polygon coordinates corresponding to acquired demand
    polyDrawer <- function(type = c("prc", "exp", "std")) {
      
      if(type == "prc") {

        if((inv_discount_curve(m_AC_prc_online) < (m_prc_max+0.5)) && (inv_discount_curve(m_AC_prc_opp) < (m_prc_max+0.5))) {
          coords <- data.frame(x = c(0,0,0,0), y = c(0,0,0,0))
        } else if((inv_discount_curve(m_AC_prc_online) > (m_prc_max+0.5)) && (inv_discount_curve(m_AC_prc_opp) > (m_prc_max+0.5))) {
          coords <- data.frame(x = c(m_prc_max+0.5, inv_discount_curve(m_AC_prc_opp), inv_discount_curve(m_AC_prc_online), m_prc_max+0.5, m_prc_max+0.5),
                               y = c(m_AC_prc_opp, m_AC_prc_opp, m_AC_prc_online, m_AC_prc_online, m_AC_prc_opp))
        } else {
          coords <- data.frame(x = c(m_prc_max+0.5, inv_discount_curve(m_AC_prc_opp), m_prc_max+0.5, m_prc_max+0.5),
                               y = c(m_AC_prc_opp, m_AC_prc_opp, discount_curve(m_prc_max+0.5), m_AC_prc_opp))
        }
      } else if(type == "exp") {
        if((inv_discount_curve(m_AC_exp_online) < (m_exp_max+0.5)) && (inv_discount_curve(m_AC_exp_opp) < (m_exp_max+0.5))) {
          coords <- data.frame(x = c(0,0,0,0), y = c(0,0,0,0))
        } else if((inv_discount_curve(m_AC_exp_online) > (m_exp_max+0.5)) && (inv_discount_curve(m_AC_exp_opp) > (m_exp_max+0.5))) {
          coords <- data.frame(x = c(m_exp_max+0.5, inv_discount_curve(m_AC_exp_opp), inv_discount_curve(m_AC_exp_online), m_exp_max+0.5, m_exp_max+0.5),
                               y = c(m_AC_exp_opp, m_AC_exp_opp, m_AC_exp_online, m_AC_exp_online, m_AC_exp_opp))
        } else {
          coords <- data.frame(x = c(m_exp_max+0.5, inv_discount_curve(m_AC_exp_opp), m_exp_max+0.5, m_exp_max+0.5),
                               y = c(m_AC_exp_opp, m_AC_exp_opp, discount_curve(m_exp_max+0.5), m_AC_exp_opp))
        }
      } else if(type == "std") {
        if((inv_discount_curve(m_AC_std_online) < (m_std_max+0.5)) && (inv_discount_curve(m_AC_std_opp) < (m_std_max+0.5))) {
          coords <- data.frame(x = c(0,0,0,0), y = c(0,0,0,0))
        } else if((inv_discount_curve(m_AC_std_online) > (m_std_max+0.5)) && (inv_discount_curve(m_AC_std_opp) > (m_std_max+0.5))) {
          coords <- data.frame(x = c(m_std_max+0.5, inv_discount_curve(m_AC_std_opp), inv_discount_curve(m_AC_std_online), m_std_max+0.5, m_std_max+0.5),
                               y = c(m_AC_std_opp, m_AC_std_opp, m_AC_std_online, m_AC_std_online, m_AC_std_opp))
        } else {
          coords <- data.frame(x = c(m_std_max+0.5, inv_discount_curve(m_AC_std_opp), m_std_max+0.5, m_std_max+0.5),
                               y = c(m_AC_std_opp, m_AC_std_opp, discount_curve(m_std_max+0.5), m_AC_std_opp))
        }
      } else { coords <- data.frame(x = c(0,0,0,0), y = c(0,0,0,0)) }
      
      coords
    }
    
    # Unit profit values
    unitpr_prc      <- curve_fit$Convert(m_price) + m_shipment_quoted_prc - m_shipment_cost_prc - m_handling 
    unitpr_exp      <- curve_fit$Convert(m_price) + m_shipment_quoted_exp - m_shipment_cost_exp - m_handling 
    unitpr_std      <- curve_fit$Convert(m_price) + m_shipment_quoted_std - m_shipment_cost_std - m_handling
    
    ## Demand areas appearing from OPP channel
    prc_pol <- SpatialPolygons(list(Polygons(list(Polygon(polyDrawer("prc"))), "prc")))
    exp_pol <- SpatialPolygons(list(Polygons(list(Polygon(polyDrawer("exp"))), "exp")))
    std_pol <- SpatialPolygons(list(Polygons(list(Polygon(polyDrawer("std"))), "std")))

    ## Polygons corresponding to the Lead Time limits
    prcmaxpol <- SpatialPolygons(list(Polygons(list(Polygon(data.frame(x = c(m_prc_max+0.5,m_prc_max+0.5, 99999, 99999), y = c(0, 1, 1, 0)))), "prcmax")))
    expmaxpol <- SpatialPolygons(list(Polygons(list(Polygon(data.frame(x = c(m_exp_max+0.5,m_exp_max+0.5, 99999, 99999), y = c(0, 1, 1, 0)))), "expmax")))
    stdmaxpol <- SpatialPolygons(list(Polygons(list(Polygon(data.frame(x = c(m_std_max+0.5,m_std_max+0.5, 99999, 99999), y = c(0, 1, 1, 0)))), "stdmax")))
    
    ## Demand areas originally here from Online
    if(m_AC_std_online <= discount_curve(m_std_max)) {
      coord_onl_dem_std <- data.frame(x = c(m_std_max, m_std_max,inv_discount_curve(m_AC_std_online), m_std_max),
                                      y = c(m_AC_std_online, discount_curve(m_std_max), m_AC_std_online, m_AC_std_online))
      std_onl_dem_pol <- SpatialPolygons(list(Polygons(list(Polygon(coord_onl_dem_std)), "demand_online_standard")))
    } else {
      coord_onl_dem_std <- data.frame(x = c(0,0,0,0), y = c(0,0,0,0))
      std_onl_dem_pol <- SpatialPolygons(list(Polygons(list(Polygon(coord_onl_dem_std)), "demand_online_standard")))
    }
    
    if(m_AC_exp_online <= discount_curve(m_exp_max)) {
      coord_onl_dem_exp <- data.frame(x = c(m_exp_max, m_exp_max,inv_discount_curve(m_AC_exp_online), m_exp_max),
                                      y = c(m_AC_exp_online, discount_curve(m_exp_max), m_AC_exp_online, m_AC_exp_online))
      exp_online_dem_pol <- SpatialPolygons(list(Polygons(list(Polygon(coord_onl_dem_exp)), "demand_online_expedite")))
    } else {
      coord_onl_dem_exp <- data.frame(x = c(0,0,0,0), y = c(0,0,0,0))
      exp_online_dem_pol <- SpatialPolygons(list(Polygons(list(Polygon(coord_onl_dem_exp)), "demand_online_expedite")))
    }
    
    if(m_AC_prc_online <= discount_curve(m_prc_max)) {
      coord_onl_dem_prc <- data.frame(x = c(m_prc_max, m_prc_max,inv_discount_curve(m_AC_prc_online), m_prc_max),
                                      y = c(m_AC_prc_online, discount_curve(m_prc_max), m_AC_prc_online, m_AC_prc_online))
      prc_online_dem_pol <- SpatialPolygons(list(Polygons(list(Polygon(coord_onl_dem_prc)), "demand_online_priority")))
    } else {
      coord_onl_dem_prc <- data.frame(x = c(0,0,0,0), y = c(0,0,0,0))
      prc_online_dem_pol <- SpatialPolygons(list(Polygons(list(Polygon(coord_onl_dem_prc)), "demand_online_priority")))
    }
    
    ## Check for overlaps for demand from Online
    demand_online_standard <- gArea(std_onl_dem_pol)
    if(is.null(gIntersection(exp_online_dem_pol, stdmaxpol)) == TRUE) { demand_online_expedite <- gArea(exp_online_dem_pol) }
    else        { demand_online_expedite <- gArea(exp_online_dem_pol) - gArea(gIntersection(exp_online_dem_pol, stdmaxpol)) }
    if(is.null(gIntersection(prc_online_dem_pol, expmaxpol)) == TRUE) { demand_online_priority <- gArea(prc_online_dem_pol) }
    else        { demand_online_priority <- gArea(prc_online_dem_pol) - gArea(gIntersection(prc_online_dem_pol, expmaxpol)) }

    ## Check for overlaps for demand from OPP
    dem_std <- gArea(std_pol)
    if(is.null(gIntersection(exp_pol, stdmaxpol)) == TRUE) { dem_exp <- gArea(exp_pol) }
    else        { dem_exp <- gArea(exp_pol) - gArea(gIntersection(exp_pol, stdmaxpol)) }
    if(is.null(gIntersection(prc_pol, expmaxpol)) == TRUE) { dem_prc <- gArea(prc_pol) }
    else        { dem_prc <- gArea(prc_pol) - gArea(gIntersection(prc_pol, expmaxpol)) }
    
    ## Profits from Online
    profit_online_standard <- unitpr_std*demand_online_standard
    profit_online_expedite <- unitpr_exp*demand_online_expedite
    profit_online_priority <- unitpr_prc*demand_online_priority
    
    ## Profits from OPP
    pr_std  <- unitpr_std*dem_std
    pr_exp  <- unitpr_exp*dem_exp
    pr_prc  <- unitpr_prc*dem_prc
    
    demand  <- dem_std+dem_exp+dem_prc+demand_online_standard+demand_online_expedite+demand_online_priority
    profit  <- pr_std+pr_exp+pr_prc+profit_online_standard+profit_online_expedite+profit_online_priority

    c(demand, profit)
  }
  
  
  observeEvent(input$plot_btn, {
   
    if(real_prod_price() <= 1000) {
    
      coeff_prc <- NULL
      attempt <- 0
      while( is.null(coeff_prc) && attempt <= attempt_max ) {
        attempt <- attempt + 1
        try({
          if(attempt == 1) { 
            coeff_prc <- ShipmentCurve(s = shipment_quoted_prc(), p = curve_fit[['init_price']])
          } else {
            rnd = runif(1); print(attempt)
            coeff_prc <- ShipmentCurve(s = shipment_quoted_prc(), p = curve_fit[['init_price']], nlmInitial = list(a = -0.5*runif(1), power1 = -1*rnd, power2 = -1*rnd))
          }
        })
      }
      
      coeff_exp <- NULL
      attempt <- 0
      while( is.null(coeff_exp) && attempt <= attempt_max ) {
        attempt <- attempt + 1
        try({
          if(attempt == 1) { 
            coeff_exp <- ShipmentCurve(s = shipment_quoted_exp(), p = curve_fit[['init_price']])
          } else {
            rnd = runif(1); print(attempt)
            coeff_exp <- ShipmentCurve(s = shipment_quoted_exp(), p = curve_fit[['init_price']], nlmInitial = list(a = -0.5*runif(1), power1 = -1*rnd, power2 = -1*rnd))
          }
        })
      }
               
      coeff_std <- NULL
      attempt <- 0
      while( is.null(coeff_std) && attempt <= attempt_max ) {
        attempt <- attempt + 1
        try({
          if(attempt == 1) { 
            coeff_std <- ShipmentCurve(s = shipment_quoted_std(), p = curve_fit[['init_price']])
          } else {
            rnd = runif(1); print(attempt)
            coeff_std <- ShipmentCurve(s = shipment_quoted_std(), p = curve_fit[['init_price']], nlmInitial = list(a = -0.5*runif(1), power1 = -1*rnd, power2 = -1*rnd))
          }
        })
      }
      
      
      
      AC_prc_online <- reactive({ AggregatedRelativeCost(m_price = real_prod_price(), m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = coeff_prc, type = "online") })
      AC_exp_online <- reactive({ AggregatedRelativeCost(m_price = real_prod_price(), m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = coeff_exp, type = "online") })
      AC_std_online <- reactive({ AggregatedRelativeCost(m_price = real_prod_price(), m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = coeff_std, type = "online") })
      AC_prc_opp    <- reactive({ AggregatedRelativeCost(m_price = real_prod_price(), m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = coeff_prc, type = "opp") })
      AC_exp_opp    <- reactive({ AggregatedRelativeCost(m_price = real_prod_price(), m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = coeff_exp, type = "opp") })
      AC_std_opp    <- reactive({ AggregatedRelativeCost(m_price = real_prod_price(), m_inc_price = rel_inconv_price(), m_inc_fact = inconv_mit_factor(), coeff = coeff_std, type = "opp") })
      
      discount_curve <- function(x, pp = curve_fit[['init_price']], dp = one_day_cost()) { 1 - ( dp/(2*pp) ) * x }
      
      inv_discount_curve <- function(y, pp = curve_fit[['init_price']], dp = one_day_cost()) { (1 - y) / ( dp/(2*pp) ) }
      
      output$mpgPlot <- renderPlot({
        
        x1 <- 0.5:(STD_MAX()+0.5)
        dat <- data.frame(
          channel = rep(c("online", "opp"), each = length(x1)),
          shipment = rep(c(rep("prc", (PRC_MAX()+1)), rep("exp", (EXP_MAX()-PRC_MAX())), rep("std", (STD_MAX()-EXP_MAX())))),
          leadtime = rep(0.5:(STD_MAX()+0.5), 2),
          aggregatecost = c(
            # first half are the online (no opp) options
            c(rep(AC_prc_online(), (PRC_MAX()+1)), rep(AC_exp_online(), (EXP_MAX()-PRC_MAX())), rep(AC_std_online(), (STD_MAX()-EXP_MAX()))),
            # second half are the opp options
            c(rep(AC_prc_opp(), (PRC_MAX()+1)), rep(AC_exp_opp(), (EXP_MAX()-PRC_MAX())), rep(AC_std_opp(), (STD_MAX()-EXP_MAX())))
          ),
          discountvalue = rep(discount_curve(x1), 2)
        )
        
        dat_disc <- data.frame(
          x_val = seq(0,STD_MAX()+1,0.01),
          discountvalue = discount_curve(seq(0,STD_MAX()+1,0.01))
        )
        
        dat_back <- data.frame(
          x_val = seq(0,STD_MAX()+1,0.01),
          y_val = discount_curve(seq(0,STD_MAX()+1,0.01))
        )
        
        dat_prcmax <- data.frame(
          y_val = seq(0,AC_prc_online(),0.01),
          x_val = rep(PRC_MAX()+1, length(seq(0,AC_prc_online(),0.01)))
        )
        
        dat_expmax <- data.frame(
          y_val = seq(0,AC_exp_online(),0.01),
          x_val = rep(EXP_MAX()+1, length(seq(0,AC_exp_online(),0.01)))
        )
        
        dat_stdmax <- data.frame(
          y_val = seq(0,AC_std_online(),0.01),
          x_val = rep(STD_MAX()+1, length(seq(0,AC_std_online(),0.01)))
        )
        
        maxim <- function(x) {
          min(1,x)
        }
        
        polyDrawer <- function(type = c("prc", "exp", "std")) {
          if(type == "prc") {
            if((inv_discount_curve(AC_prc_online()) < (PRC_MAX()+0.5)) && (inv_discount_curve(AC_prc_opp()) < (PRC_MAX()+0.5))) {
              x_coord = c(0,0,0,0)
              y_coord = c(0,0,0,0)
            } else if((inv_discount_curve(AC_prc_online()) > (PRC_MAX()+0.5)) && (inv_discount_curve(AC_prc_opp()) > (PRC_MAX()+0.5))) {
              x_coord = c(PRC_MAX()+1, inv_discount_curve(AC_prc_opp()), inv_discount_curve(AC_prc_online()), PRC_MAX()+1)
              y_coord = c(AC_prc_opp(), AC_prc_opp(), AC_prc_online(), AC_prc_online())
            } else {
              x_coord = c(PRC_MAX()+1, inv_discount_curve(AC_prc_opp()), PRC_MAX()+1)
              y_coord = c(AC_prc_opp(), AC_prc_opp(), discount_curve(PRC_MAX()+1))
            }
          } else if(type == "exp") {
            if((inv_discount_curve(AC_exp_online()) < (EXP_MAX()+0.5)) && (inv_discount_curve(AC_exp_opp()) < (EXP_MAX()+0.5))) {
              x_coord = c(0,0,0,0)
              y_coord = c(0,0,0,0)
            } else if((inv_discount_curve(AC_exp_online()) > (EXP_MAX()+0.5)) && (inv_discount_curve(AC_exp_opp()) > (EXP_MAX()+0.5))) {
              x_coord = c(EXP_MAX()+1, inv_discount_curve(AC_exp_opp()), inv_discount_curve(AC_exp_online()), EXP_MAX()+1)
              y_coord = c(AC_exp_opp(), AC_exp_opp(), AC_exp_online(), AC_exp_online())
            } else {
              x_coord = c(EXP_MAX()+1, inv_discount_curve(AC_exp_opp()), EXP_MAX()+1)
              y_coord = c(AC_exp_opp(), AC_exp_opp(), discount_curve(EXP_MAX()+1))
            }
          } else if(type == "std") {
            if((inv_discount_curve(AC_std_online()) < (STD_MAX()+0.5)) && (inv_discount_curve(AC_std_opp()) < (STD_MAX()+0.5))) {
              x_coord = c(0,0,0,0)
              y_coord = c(0,0,0,0)
            } else if((inv_discount_curve(AC_std_online()) > (STD_MAX()+0.5)) && (inv_discount_curve(AC_std_opp()) > (STD_MAX()+0.5))) {
              x_coord = c(STD_MAX()+1, inv_discount_curve(AC_std_opp()), inv_discount_curve(AC_std_online()), STD_MAX()+1)
              y_coord = c(AC_std_opp(), AC_std_opp(), AC_std_online(), AC_std_online())
            } else {
              x_coord = c(STD_MAX()+1, inv_discount_curve(AC_std_opp()), STD_MAX()+1)
              y_coord = c(AC_std_opp(), AC_std_opp(), discount_curve(STD_MAX()+1))
            }
          } else { }
  
          c(x_coord, y_coord)
        }
  
        #geom_bar(data = dat_back, aes(x=unlist(dat_back["x_val"]), y=unlist(dat_back["y_val"])), fill = "darkgrey", width = 0.01, stat = "identity") +
        #geom_bar(data = dat_back, aes(x=unlist(dat_back["x_val"]), y=unlist(dat_back["y_val"])), fill = "darkgrey", width = 0.01, stat = "identity") +
    
        if(opp_checked()==T) {
          ggplot() +
            geom_bar(data = dat[1:(STD_MAX()+1),], 
                     aes(x=dat["leadtime"]$leadtime[1:(STD_MAX()+1)], y=unlist(lapply(dat["aggregatecost"]$aggregatecost[1:(STD_MAX()+1)], maxim))), 
                     width = 1, fill = "yellow", stat = "identity") +
            geom_bar(data = data.frame(cbind(x=dat["leadtime"]$leadtime[(STD_MAX()+2):nrow(dat)], dat["aggregatecost"]$aggregatecost[(STD_MAX()+2):nrow(dat)])), 
                     aes(x=dat["leadtime"]$leadtime[(STD_MAX()+2):nrow(dat)], y=unlist(lapply(dat["aggregatecost"]$aggregatecost[(STD_MAX()+2):nrow(dat)], maxim))), 
                     width = 1, fill = "lightblue", stat = "identity") +
            geom_line(data = data.frame(cbind(dat_disc["x_val"][dat_disc["discountvalue"] >= 0], dat_disc["discountvalue"][dat_disc["discountvalue"] >= 0])), 
                                  aes(x=dat_disc["x_val"][dat_disc["discountvalue"] >= 0], y=dat_disc["discountvalue"][dat_disc["discountvalue"] >= 0]),
                                  colour = "red", size = 1) +
            geom_line(data = dat_prcmax, aes(x=unlist(dat_prcmax["x_val"]), y=unlist(dat_prcmax["y_val"])), size = 0.5, linetype = "dashed", colour = "#4ea2b5") +
            geom_line(data = dat_expmax, aes(x=unlist(dat_expmax["x_val"]), y=unlist(dat_expmax["y_val"])), size = 0.5, linetype = "dashed", colour = "#4ea2b5") +
            geom_line(data = dat_stdmax, aes(x=unlist(dat_stdmax["x_val"]), y=unlist(dat_stdmax["y_val"])), size = 0.5, linetype = "dashed", colour = "#4ea2b5") +
            coord_cartesian(xlim = c(0, ceiling(STD_MAX()/5)*5), ylim=c(0,1)) +
            theme(axis.line = element_line()) +
            scale_x_continuous("Delivery lead time [days]", seq(0,ceiling(STD_MAX()/5)*5,5)) +
            scale_y_continuous("Aggregated Relative Cost [-]", seq(0,1,0.1)) +
            geom_polygon(aes(x = polyDrawer(type = "prc")[1:(length(polyDrawer(type = "prc"))/2)], 
                             y = polyDrawer(type = "prc")[(1 + length(polyDrawer(type = "prc"))/2):length(polyDrawer(type = "prc"))]), fill = "purple", alpha = 0.5, show.legend = FALSE) +
            geom_polygon(aes(x = polyDrawer(type = "exp")[1:(length(polyDrawer(type = "exp"))/2)], 
                             y = polyDrawer(type = "exp")[(1 + length(polyDrawer(type = "exp"))/2):length(polyDrawer(type = "exp"))]), fill = "purple", alpha = 0.5, show.legend = FALSE) +
            geom_polygon(aes(x = polyDrawer(type = "std")[1:(length(polyDrawer(type = "std"))/2)], 
                             y = polyDrawer(type = "std")[(1 + length(polyDrawer(type = "std"))/2):length(polyDrawer(type = "std"))]), fill = "purple", alpha = 0.5, show.legend = FALSE)
  
        }
        else {
          ggplot() +
            geom_bar(data = dat[1:(STD_MAX()+1),], 
                     aes(x=dat["leadtime"]$leadtime[1:(STD_MAX()+1)], y=dat["aggregatecost"]$aggregatecost[1:(STD_MAX()+1)]), 
                     width = 1, fill = "lightblue", stat = "identity") +
            geom_line(data = data.frame(cbind(dat_disc["x_val"][dat_disc["discountvalue"] >= 0], dat_disc["discountvalue"][dat_disc["discountvalue"] >= 0])), 
                      aes(x=dat_disc["x_val"][dat_disc["discountvalue"] >= 0], y=dat_disc["discountvalue"][dat_disc["discountvalue"] >= 0]),
                      colour = "red", size = 1) +
            geom_line(data = dat_prcmax, aes(x=unlist(dat_prcmax["x_val"]), y=unlist(dat_prcmax["y_val"])), size = 0.5, linetype = "dashed", colour = "#4ea2b5") +
            geom_line(data = dat_expmax, aes(x=unlist(dat_expmax["x_val"]), y=unlist(dat_expmax["y_val"])), size = 0.5, linetype = "dashed", colour = "#4ea2b5") +
            geom_line(data = dat_stdmax, aes(x=unlist(dat_stdmax["x_val"]), y=unlist(dat_stdmax["y_val"])), size = 0.5, linetype = "dashed", colour = "#4ea2b5") +
            coord_cartesian(ylim=c(0,1)) +
            theme(axis.line = element_line()) +
            scale_x_continuous("Delivery lead time [days]", seq(0,ceiling(STD_MAX()/5)*5,5)) +
            scale_y_continuous("Aggregated Relative Cost [-]", seq(0,1,0.1))
          }
      })
    
      shinyjs::enable("optimize_btn")
    }
    else{
      output$mpgPlot <- renderPlot({
        ggplot() + 
          coord_cartesian(xlim = c(0, STD_MAX()), ylim = c(0, 1) ) +
          xlab("Days of delivery") +
          ylab("Aggregated Relative Cost [-]") +
          theme(axis.line = element_line()) +
          scale_x_continuous("Delivery lead time [days]", seq(0,ceiling(STD_MAX()/5)*5,5)) +
          scale_y_continuous("Aggregated Relative Cost [-]", seq(0,1,0.1)) +
          geom_label(aes(x = STD_MAX()/2, y = 0.5, label = "Please choose an valid initial product price of at most 1000$."), fill = "red", size = 6)
      })
      output$dem     <- renderText({ })
      output$pr      <- renderText({ })
      shinyjs::disable("clear_btn")
    }
  })
 
  profit <- reactiveVal()
  demand <- reactiveVal()
  
  observeEvent(input$plot_btn, {
    
    out <- reactive({ ProfitCalc(m_price = real_prod_price(), 
                                 m_shipment_cost_prc = shipment_real_prc(), 
                                 m_shipment_cost_exp = shipment_real_exp(),
                                 m_shipment_cost_std = shipment_real_std(),
                                 m_shipment_quoted_prc = shipment_quoted_prc(),
                                 m_shipment_quoted_exp = shipment_quoted_exp(),
                                 m_shipment_quoted_std = shipment_quoted_std(),
                                 m_handling = rel_handling(),
                                 m_day_price = one_day_cost(),
                                 m_curve_fit = curve_fit[['coeffs']],
                                 m_prc_max = PRC_MAX(), 
                                 m_exp_max = EXP_MAX(),
                                 m_std_max = STD_MAX()) })

    output$dem <- renderText({ round(out()[1],4) })
    output$pr  <- renderText({  round(out()[2],4) })
    profit(out()[2])
    demand(out()[1])
  })
  
    
  ## Optimization
  observeEvent(input$optimize_btn, {
    x0 <- c(curve_fit[['init_price']], shipment_quoted_prc(), shipment_quoted_exp(), shipment_quoted_std())
    lower_boundary <- c(curve_fit[['init_price']]-0.05*curve_fit[['init_price']], 0.8*shipment_quoted_prc(), 0.8*shipment_quoted_exp(), 0.8*shipment_quoted_std())
    upper_boundary <- c(curve_fit[['init_price']]+0.05*curve_fit[['init_price']], 1.2*shipment_quoted_prc(), 1.2*shipment_quoted_exp(), 1.2*shipment_quoted_std())


    FunctionToOptimize <- function(x,y,coeff) {
      (-1)*ProfitCalc(m_price = x[1], m_shipment_quoted_prc = x[2], m_shipment_quoted_exp = x[3], m_shipment_quoted_std = x[4],
                        m_shipment_cost_prc = y[1], m_shipment_cost_exp = y[2], m_shipment_cost_std = y[3], m_handling = y[4],
                        m_day_price = y[5], m_curve_fit = coeff)[2]
      }

    require(optimx)

    m <- optimx(par = x0,
           fn = FunctionToOptimize,
           method = "L-BFGS-B",
           lower = lower_boundary, upper = upper_boundary,
           y=c(shipment_real_prc(), shipment_real_exp(), shipment_quoted_std(), rel_handling(), one_day_cost()),
           coeff = curve_fit[['coeffs']])

    #shinyjs::enable("export_btn")


    # Write optimized parameters to the inputs to visualize their effect

    # conn <- file(paste0("Profit_optim_output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
    # writeLines(c("============= SPECIFICATIONS OF THE SELECTED CUSTOMER SEGMENT =============",
    #              paste("Inconvenience factor corresponding to online order:", "\t\t ",   rel_inconv_price()),
    #              paste("Inconvenience mitigating factor:",                    "\t\t\t\t ", inconv_mit_factor()),
    #              "\n================== SPECIFICATIONS OF THE PRODUCT SEGMENT ==================",
    #              paste("Valuation of the product type (lead time valuation):", "\t\t",  one_day_cost(), "$/day"),
    #              "\n================== SPECIFICATIONS OF THE RETAILER SEGMENT =================",
    #              paste("Extra sourcing and handling costs:",                   "\t\t\t\t ", rel_handling()),
    #              "\n====================== PROFIT OPTIMISATION RESULTS ========================",
    #              paste("Initial product price:",                               "\t\t\t\t\t\t", curve_fit[['init_price']] ),
    #              paste("Optimal product price:",                               "\t\t\t\t\t\t", round(m$p1,2) ),
    #              paste("Initial priority courier shipment price:",             "\t\t\t", round(shipment_quoted_prc(),2) ),
    #              paste("Optimal priority courier shipment price:",             "\t\t\t", round(m$p2,2) ),
    #              paste("Initial expedited shipment price:",                    "\t\t\t\t ",  round(shipment_quoted_exp(),2) ),
    #              paste("Optimal expedited shipment price:",                    "\t\t\t\t", round(m$p3,2) ),
    #              paste("Initial standard shipment price:",                     "\t\t\t\t ",  round(shipment_quoted_std(),2) ),
    #              paste("Optimal standard shipment price:",                     "\t\t\t\t ",  round(m$p4,2) ),
    #              paste("\nOriginal demand:",                                   "\t\t\t\t\t\t", round(demand(),4)),
    #              paste("Post-optimization demand:",                            "\t\t\t\t\t", round(ProfitCalc(m_price=m$p1,m_shipment_cost_prc=shipment_real_prc(),m_shipment_cost_exp=shipment_real_exp(),m_shipment_cost_std=shipment_real_std(),m_shipment_quoted_prc=m$p2,m_shipment_quoted_exp=m$p3,m_shipment_quoted_std=m$p4,m_handling=rel_handling(),m_day_price=one_day_cost(),m_curve_fit=curve_fit[['coeffs']])[1],4)),
    #              paste("Original profit:",                                     "\t\t\t\t\t\t", round(profit(),4)),
    #              paste("Maximal profit reached:",                              "\t\t\t\t\t", round((-1)*m$value,4))
    #              ),
    #            conn)
    # close(conn)

    #winDialog("ok", "Optimization finished succesfully.\nOptimization results are exported to a txt file.")
    
    #§winDialog("ok", paste(rel_inconv_price(), inconv_mit_factor(), curve_fit[['init_price']], one_day_cost(), rel_handling()))
    winDialog("ok", paste(round(demand(),4), round(ProfitCalc(m_price=m$p1,m_shipment_cost_prc=shipment_real_prc(),m_shipment_cost_exp=shipment_real_exp(),m_shipment_cost_std=shipment_real_std(),m_shipment_quoted_prc=m$p2,m_shipment_quoted_exp=m$p3,m_shipment_quoted_std=m$p4,m_handling=rel_handling(),m_day_price=one_day_cost(),m_curve_fit=curve_fit[['coeffs']])[1],4),
                      round(profit(),4), round(ProfitCalc(m_price=m$p1,m_shipment_cost_prc=shipment_real_prc(),m_shipment_cost_exp=shipment_real_exp(),m_shipment_cost_std=shipment_real_std(),m_shipment_quoted_prc=m$p2,m_shipment_quoted_exp=m$p3,m_shipment_quoted_std=m$p4,m_handling=rel_handling(),m_day_price=one_day_cost(),m_curve_fit=curve_fit[['coeffs']])[2],4)))
    winDialog("ok", paste(round(m$p1,2), round(m$p2,2), round(m$p3,2), round(m$p4,2)))
    
    
    

    
  })
  # # 
  # observeEvent(input$optimize_btn, {
  #   
  #   # # price: 10, 20, 50, 100, 200, 500
    # # h : 0.50
    # # w : 0.01 - 0.50 @ steps of 0.01
    # # r : 0.01, 0.25, 0.50, 0.75, 1.00
    # 
    # df = data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    # names(df) <- c("w", "r", "p", "dp", "h", "d0", "d1", "pr0", "pr1", "ppp*", "prc*", "exp*", "std*")
    # 
    # #w_vector <- seq(0.01, 0.50, 0.01)
    # #r_vector <- c(0.01, 0.25, 0.50, 0.75, 1.00)
    # #p_vector <- c(10, 20, 50, 100, 200, 500)
    # 
    # p_vector <- 500
    # w_vector <- 0.40
    # r_vector <- 0.01
    # for (ppp in p_vector) {
    #   #dp_vector <- c(0.05*ppp, 0.02*ppp, 0.005*ppp)
    #   dp_vector <- 0.02*ppp
    #   ppp_coeffs <- PriceCurveFit(pp = ppp)
    #   for (dppp in dp_vector) {
    #     for (www in w_vector) {
    #       for (rrr in r_vector) {
    #         
    #         out <- ProfitCalc(m_price = ppp, m_shipment_cost_prc = shipment_real_prc(), m_shipment_cost_exp = shipment_real_exp(), m_shipment_cost_std = shipment_real_std(),
    #                                      m_shipment_quoted_prc = shipment_quoted_prc(), m_shipment_quoted_exp = shipment_quoted_exp(), m_shipment_quoted_std = shipment_quoted_std(),
    #                                      m_handling = rel_handling(), m_day_price = dppp, m_curve_fit = ppp_coeffs, 
    #                                      m_prc_max = PRC_MAX(), m_exp_max = EXP_MAX(), m_std_max = STD_MAX())
    #         
    #         
    #         x0 <- c(ppp, shipment_quoted_prc(), shipment_quoted_exp(), shipment_quoted_std())
    #         lower_boundary <- c(ppp-0.05*ppp, 0.8*shipment_quoted_prc(), 0.8*shipment_quoted_exp(), 0.8*shipment_quoted_std())
    #         upper_boundary <- c(ppp+0.05*ppp, 1.2*shipment_quoted_prc(), 1.2*shipment_quoted_exp(), 1.2*shipment_quoted_std())
    #         
    #         
    #         FunctionToOptimize <- function(x,y,coeff) {
    #           (-1)*ProfitCalc(m_price = x[1], m_shipment_quoted_prc = x[2], m_shipment_quoted_exp = x[3], m_shipment_quoted_std = x[4],
    #                           m_shipment_cost_prc = y[1], m_shipment_cost_exp = y[2], m_shipment_cost_std = y[3], m_handling = y[4],
    #                           m_day_price = y[5], m_curve_fit = coeff)[2]  
    #         }
    #         
    #         require(optimx)
    #         
    #         m <- optimx(par = x0, 
    #                     fn = FunctionToOptimize, 
    #                     method = "L-BFGS-B",
    #                     lower = lower_boundary, upper = upper_boundary, 
    #                     y=c(shipment_real_prc(), shipment_real_exp(), shipment_quoted_std(), rel_handling(), one_day_cost()),
    #                     coeff = ppp_coeffs)
    #         
    #         de <- data.frame(www, rrr, ppp, dppp, rel_handling(), 
    #           out[1], round(ProfitCalc(m_price=m$p1,m_shipment_cost_prc=shipment_real_prc(),m_shipment_cost_exp=shipment_real_exp(),m_shipment_cost_std=shipment_real_std(),m_shipment_quoted_prc=m$p2,m_shipment_quoted_exp=m$p3,m_shipment_quoted_std=m$p4,m_handling=rel_handling(),m_day_price=dppp,m_curve_fit=ppp_coeffs)[1],4),
    #           out[2], round((-1)*m$value,4),
    #           round(m$p1,2), round(m$p2,2), round(m$p3,2), round(m$p4,2))
    #         names(de) <- c("w", "r", "p", "dp", "h", "d0", "d1", "pr0", "pr1", "ppp*", "prc*", "exp*", "std*")
    #         df <- rbind(df, de)
    #       }
    #     }
    #   }
    # }
    # 
    # print(df)
  # })
  
  
  observeEvent(input$export_btn, { 

  })
  
  ### Clear
  observeEvent(input$clear_btn, {
    output$mpgPlot <- renderPlot({
      ggplot() + 
        coord_cartesian(xlim = c(0, STD_MAX()), ylim = c(0, 1) ) +
        xlab("Days of delivery") +
        ylab("Aggregated Relative Cost [-]") +
        theme(axis.line = element_line()) +
        scale_x_continuous("Delivery lead time [days]", seq(0,ceiling(STD_MAX()/5)*5,5)) +
        scale_y_continuous("Aggregated Relative Cost [-]", seq(0,1,0.1)) +
        geom_label(aes(x = STD_MAX()/2, y = 0.5, label = "In order to see the plotted results, please set the input \n parameters on the tab below and then press the plot button!"), fill = "red", size = 6)
    })
    output$dem     <- renderText({ })
    output$pr      <- renderText({ })
    shinyjs::disable("clear_btn")
  })
  
  
  ### Exit
  observeEvent(input$exit_btn, {
    stopApp()
  })
  
}

shinyApp(ui, server)