# Load required packages
library(shiny)  # To build interactive application
library(data.tree)  # To manipulate tree structures used to present price processes
library(DiagrammeR)  # Generate trees 


# Function takes in parameters for a stock: intial price, movement factors and time to maturity
# Builds a trinomial stock price tree (undiscounted)
build_trinomial_tree <- function(S0, u, m, d, N) {
  # Starting node for initial price
  tree <- Node$new(paste0("S_0: ", S0))
  tree$price <- S0
  
  # Function that extends each node with up, middle and down states
  build_node <- function(node, time, maturity) {
    # When maturity is reached
    if (time >= maturity) return()
    # When maturity is not reached
    for (state in c("down", "middle", "up")) {
      move_factor <- switch(state, up = u, middle = m, down = d)
      new_price <- node$price * move_factor
      # Add child node to current node
      child <- node$AddChild(paste0("S_", time + 1, ": ", round(new_price, 2)))
      child$price <- new_price
      # Recursively apply build_node function to child
      build_node(child, time + 1, maturity)
    }
  }
  
  build_node(tree, 0, N)
  return(tree)
}

# Function that takes in an existing tree, strike price, option type and discounting bond factor
# Uses backwards propagation to find the value of an European option
# Since the risk neutral probabilities of a trinomial model is not unique
# Here we assume the risk neutral probability for an up / middle / down movement is q = 1/3
add_option_values_discounted <- function(tree, K, type = c("Call", "Put"), bond_factor = 1) {
  # At leaf nodes (no children), i.e. maturity payoffs
  tree$Do(function(node) {
    if (node$isLeaf) {
      node$option <- switch(type,
                            Call = max(node$price - K, 0), # Payoff for call
                            Put  = max(K - node$price, 0)) # Payoff for put
      # Since at t=0, it is level 1, so to discount from time t, we have to discount (level - 1) number of years
      node$option <- node$option / (bond_factor ^ (node$level - 1))  
      node$name <- paste0(node$name, "\n", type, ": ", round(node$option, 2))
    }
  })
  
  # For non-leaf nodes
  tree$Do(function(node) {
    if (!node$isLeaf) {
      # Extract option values of the children
      child_option <- sapply(node$children, function(x) x$option)
      # Since we assumed q = 1/3. the option value for the parent is their expectation
      node$option <- mean(child_option) / bond_factor
      node$name <- paste0(node$name, "\n", type, ": ", round(node$option, 2))
    }
    # We use backwards propagation, children should be processed before the parent
    # This is specified as "post-order"
  }, traversal = "post-order")
  
  return(tree)
}


# The structure of a Shiny App has tree components:
# 1. User interface, 2. Server function and 3. Call to the shinyapp function


# 1. Define user interface object
ui <- navbarPage("Arbitrage Explorer",
                 # Tab 1: About page
                 tabPanel("About",
                          fluidPage(
                            withMathJax(),  # Allows latex notations
                            h2("Welcome to Arbitrage Explorer!"),
                            p("In this market, there is one risky asset \\( S \\) and one risk-free bond \\( B \\). Their prices evolve in discrete time steps. We use \\( S_0 \\) and \\( B_0 \\) to notate the current prices of stock and bond at time \\(t=0\\) respectively. Prices one year later are represented using \\( S_1 \\) and \\( B_1 \\), continuing up to \\( S_N \\) and \\( B_N \\), where \\( N \\) is the time to maturity. Using the two provided tabs, your challenge is to tune market parameters such that arbitrage exists, and contruct a portfolio to exploit it."),
                            
                            br(),
                            
                            h4(tags$u("Tab 1 – Trinomial Tree Visualisation:")),
                            p("Use this tab to build and display a trinomial tree of stock prices, call, or put option values. You can input the following parameters to model the movements of \\(S\\) and \\(B\\):"),
                            p("- Initial (time \\( t=0 \\)) stock price (\\( S_0 \\))"),
                            p("- Movement factors \\( U,M,D \\) of stock \\( S \\) across one time period. From one time period to the next one, the stock can move to one of three states: the (U)p state, the (M)iddle state, and the (D)own state, hence the name trinomial model. This movement is given by:"),
                            p("$$ S_t = S_{t-1} \\cdot \\text{Factor}, \\quad \\text{where } \\text{Factor} \\in \\{ \\text{U}, \\text{M}, \\text{D} \\}. $$"),
                            p("- Number of time periods / time to maturity \\( (N) \\). Here, due to space constraints for the visuals, \\( N \\) is only allowed to take values in \\( {1,2,3} \\)"),
                            p("- Strike price for the European call / put option \\( (K) \\). Reminder that a European call option is the right (but not the obligation) at maturity to buy the stock at strike price \\(K\\). This is only be exercised when it is cheaper to buy the stock at \\( K \\) rather than paying the market price of \\( S_N \\). This yields a payoff of \\(  S_N - K \\) when exercised. Hence, the value of a call option at maturity year \\( N \\) is given by"),
                            p(""),
                            p("$$\\text{Call: } \\max(S_N - K, 0).$$"),
                            p("Similarly for a put option, it is the right to sell the stock at strike price \\( K \\) at maturity, which will only be exercised if selling at \\( K \\) is more profitable than selling at \\( S_T \\). This yields a payoff of \\( K - S_N \\) when exercised. Hence, the value of a call option at maturity year \\( N \\) is given by,"),
                            p("$$\\text{Put: } \\max(K - S_N, 0).$$"),
                            p("- Movement factor \\( R \\) of the bond \\( B \\) across one time period. Bond price at time \\( t=0 \\) is defined to be \\( 1 \\), and unlike the unpredictability of stock price, it follows a deterministic evolution given by:"),
                            p("$$ B_t = R^t. $$"),
                            p("Following the above parameter inputs, the corresponding stock tree (undiscounted) will be generated. You can also choose to display the discounted European call / put option price process, which obey the discounting method:"),
                            p("$$\\text{Discounted option value }=  \\frac{\\text{Undiscounted option value at time } t}{B_t}. $$"),
                            
                            br(),
                            
                            h4(tags$u("Tab 2 - Build Your Portfolio:")),
                            p("This tab lets you build your own portfolio consisting of stock \\( S \\) and bond \\( B \\) that follows the dynamics defined by the parameters above. Your task is to choose the number of shares \\(W_S\\) and number of bonds \\(W_B\\) you want to hold at time \\(t=0\\), which form a portfolio with zero initial value. The value of the portfolio at maturity will then be simulated many times (here 1000 times is used), and the arbitrage conditions below are checked:"),
                            tags$ul(
                              tags$li("Zero initial portfolio value (satisfied by construction)"),
                              tags$li("No risk of loss"),
                              tags$li("Possibility of profit")
                            ),
                            p("If all three conditions are met, we say there is arbitrage in the market. An empirical cumulative distribution function (CDF) of portfolio values will also be plotted."),
                            
                            br(),
                            
                            h4(tags$u("Hints:")),
                            p("To create arbitrage portfolios, hold the variable values in the \"Trinomial Tree Visualisation\" tab fixed except for bond price factor \\(R\\), and vary \\(R\\) to compare the two cases below:"),
                            p("$$ R \\in [D,U] \\quad vs \\quad R \\notin [D,U].$$"),
                            p("Simulate again values of portfolio at maturity using the two cases of \\( R \\). Your task is to figure out in which case there is arbitrage, and in which case there is not."),
                            p("Please also note that if the portfolio has possibility of loss and never has profit at maturity, this is essentially an arbitrage that garantees losing money. To invert portfolio position, signs of portfolio weights of stock and bond can be flipped to create an arbitrage."),
                            p("If you are still struggling to create an arbitrage opportunity, try picking \\( R > U \\) and any negative \\( W_S \\), or \\( R < D \\) and any positive \\( W_S \\)."),
                            
                            br(),
                            br(),
                            br()
                          )
                 ),
                 
                 # Tab 2: Trinomial Tree Visualisation
                 tabPanel("Trinomial Tree Visualisation",
                          sidebarLayout(
                            sidebarPanel(
                              h4(tags$u("Input the following parameter values of our trinomial model:")),
                              numericInput("S0", "Initial Stock Price (S0)", value = 100, min = 1),
                              numericInput("U", "Up Factor (U)", value = 1.1, min = 0),
                              numericInput("M", "Middle Factor (M)", value = 1, min = 0),
                              numericInput("D", "Down Factor (D)", value = 0.9, min = 0),
                              numericInput("N", "Number of Steps / Year to Maturity (N)", value = 2, min = 1, max = 3),
                              numericInput("K", "Strike Price (K)", value = 100, min = 0),
                              numericInput("bondFactor", "Bond Price Factor (R)", value = 1, min = 0),
                              radioButtons("view", "Select Tree to Display:",
                                           choices = c("Stock Price Tree" = "stock",
                                                       "Discounted European Call Option Price Tree" = "Call",
                                                       "Discounted European Put Option Price Tree"  = "Put"))
                            ),
                            # Tree in main display area
                            mainPanel(
                              div(style = "overflow-x: scroll; white-space: nowrap;",  # To add scrolling
                                  DiagrammeR::grVizOutput("treePlot", width = "1500px", height = "600px")),
                              textOutput("treeSummary")
                            )
                          )
                 ),
                 
                 # Tab 3: Create your portfolio
                 tabPanel("Build Your Portfolio",
                          sidebarLayout(
                            sidebarPanel(
                              withMathJax(),
                              h4(tags$u("Construct a zero initial valued portfolio")),
                              p("Create your portfolio weighted by \\( W_S \\) number of stock \\( S \\) and \\( W_B \\) number of bond \\( B \\). The value of the portfolio \\( V_t\\) at time \\( t \\) is:"),
                              p("$$V_t = W_S \\cdot S_t + W_B \\cdot B_t.$$"),
                              p("In order for the portfolio to have zero initial value, and using the fact that \\( B_0=1 \\), the following condition has to be fulfilled:"),
                              p("$$V_0 = W_S \\cdot S_0 + W_B \\cdot 1 = 0,$$"),
                              p("where \\( S_0 \\) was defined in the previous tab. Note that weights \\( W_S \\) and \\( W_B \\) are allowed to be negative. Positive weight means you are holding the asset, while negative weight means you short sell the asset."),
                              numericInput("w_stock", "Number of Stock / Stock Weight \\( (W_S)\\)", value = 1),
                              numericInput("w_bond", "Number of Bond / Bond Weight \\( (W_B)\\)", value = -100),
                              p("Click on the \"Calibrate Bond Weight\" button if you wish to calibrate to the appropriate bond weight given the stock weight you entered, such that the zero intial value condition is met."),
                              actionButton("calibrateBtn", "Calibrate Bond Weight"),
                              p("Click on the \"Simulate\" button to simulate the value of the portfolio at maturity 1000 times"),
                              actionButton("simulateBtn", "Simulate")
                            ),
                            mainPanel(
                              plotOutput("cdfPlot"),
                              textOutput("cdfSummary"),
                              tableOutput("arbitrageTable"),
                              textOutput("arbSummary"),
                              verbatimTextOutput("portfolioMsg")
                            )
                          )
                 )
)



# 2. Define server function
server <- function(input, output, session) {
  
  # Tab 2
  output$treePlot <- DiagrammeR::renderGrViz({
    
    # Making sure the user inputs are valid
    if (input$S0 <= 0) { 
      showNotification("Error: Please input a positive initial stock price", type = "error")
      return(NULL)
    }
    if (input$U <= 0) { 
      showNotification("Error: Please input a positive upwards movement factor", type = "error")
      return(NULL)
    }
    if (input$M <= 0) { 
      showNotification("Error: Please input a positive middle movement factor", type = "error")
      return(NULL)
    }
    if (input$D <= 0) { 
      showNotification("Error: Please input a positive downwards movement factor", type = "error")
      return(NULL)
    }
    if (!(input$M >= input$D && input$U >= input$M)) { 
      showNotification("Error: Please adjust the factors such that U > M > D", type = "error")
      return(NULL)
    }
    if (!(input$N == 1 | input$N == 2 | input$N == 3)) { 
      showNotification("Error: Year to maturity can only take values 1 or 2 or 3", type = "error")
      return(NULL)
    }
    if (input$K <= 0) { 
      showNotification("Error: Please input a positive strike price", type = "error")
      return(NULL)
    }
    if (input$bondFactor <= 0) { 
      showNotification("Error: Please input a positive bond factor", type = "error")
      return(NULL)
    }
    
    # From user's input, generate price tree
    tree <- build_trinomial_tree(input$S0, input$U, input$M, input$D, input$N)
    # If the call / put option price is selected
    if (input$view == "Call") {
      tree <- add_option_values_discounted(tree, input$K, "Call", input$bondFactor)
    } else if (input$view == "Put") {
      tree <- add_option_values_discounted(tree, input$K, "Put", input$bondFactor)
    }
    
    g <- ToDiagrammeRGraph(tree)
    
    # Set global graph style
    g$graph_attrs <- c(g$graph_attrs, 
                       "rankdir = LR",  # horizontal layout
                       "bgcolor = 'transparent'")
    
    # Customise the DiagrammeR tree
    # Node style
    g$nodes_df$style <- "filled, rounded"
    g$nodes_df$shape <- "box"
    g$nodes_df$fillcolor <- "LightBlue"
    g$nodes_df$penwidth <- 1
    # Edge style
    g$edges_df$color <- "grey30"
    g$edges_df$penwidth <- 2
    
    # Render the graph
    DiagrammeR::render_graph(g)
    
  })
  
  # Summary of the tree
  output$treeSummary  <- renderText({"The tree plot above visualises the evolution of stock and option prices over time. Each node represents a possible price at a given time, and the edges to represent possible movements from one state to another. The tree begins at the top at t = 0, and evolves downwards through time. When a parent node is split into three children states, the child to its left corresponds to a down move, directly underneath for the middle move, and to its right for an up move. Hence at maturity, the left region of the tree which has low stock prices encourages put option to the excercised. Therefore it has higher put values and lower call values when compared to the right side of the tree."})
  
  
  # Tab 3
  # Calibrate button
  observeEvent(input$calibrateBtn, {
    S0 <- input$S0
    w_stock <- input$w_stock
    if (is.na(w_stock)) {  # The stock weight cannot be empty
      showNotification("Error: Do not leave the stock weight empty", type = "error")
      return(NULL)
    }
    # Calculate corresponding bond weight
    w_bond <- -w_stock * S0
    
    updateNumericInput(session, "w_bond", value = w_bond)
    showNotification("Bond weight calibrated for zero-cost portfolio.", type = "message")
  })
  
  # Simulate button
  observeEvent(input$simulateBtn, {
    S0 <- input$S0
    u <- input$U
    m <- input$M
    d <- input$D
    N <- input$N
    w_stock <- input$w_stock
    w_bond <- input$w_bond
    bond_factor <- input$bondFactor
    
    # Check initial cost = 0 condition
    V0_zero <- w_stock * S0 + w_bond
    if (V0_zero != 0) { 
      showNotification("Error: Make sure the initial portofolio value is 0", type = "error")
      return(NULL)
    }
    
    # Simulate final portfolio values
    simulate_path <- function() {
      stock_price <- S0
      bond_price <- 1
      for (i in 1:N) {
        move <- sample(c(u, m, d), 1)
        stock_price <- stock_price * move
        bond_price <- bond_price * bond_factor
      }
      return(w_stock * stock_price + w_bond * bond_price)
    }
    
    # Run Monte Carlo sampling 
    num_MCsamples <- 1000
    final_vals <- replicate(num_MCsamples, simulate_path())
    
    # Create empirical CDF
    output$cdfPlot <- renderPlot({
      plot(ecdf(final_vals),
           main = "Empirical CDF of Portfolio Value at Maturity",
           xlab = "Portfolio Value", ylab = "Cumulative Probability",
           col = "blue", lwd = 2)
      # Red vertical line indicating zero value
      abline(v = 0, col = "red", lty = 2)
    })
    
    # Summary of the CDF plot
    output$cdfSummary <- renderText({"The plots shows the empirical CDF of the simulated portfolio value at maturity, which is an estimate of the true CDF. The vertical line passing throgh V = 0 (the red dotted line if visible) serves as the break-even point. Samples to the left corresponds to portfolio losses and everything to the right represents profit. The y-intercept of the line segment that crosses V = 0 can be interpreted as the probability that the portfolio loses money. We now assess the arbitrage conditions using the table below:"})
    
    # Arbitrage conditions
    # Condition 1 automatically satisfied
    no_loss <- all(final_vals >= 0)  # condition 2
    some_profit <- any(final_vals > 0)  # condition 3
    
    
    # Summarise the results in a dataframe
    results <- data.frame(
      Quantity = c("V_0 = 0", "P(V_N ≥ 0) = 1", "P(V_N > 0) > 0"),
      Financial_Implication = c("No initial wealth", "No possibility of loss", "Possibility of profit"),
      True_False = as.character(c(TRUE, no_loss, some_profit))
    )
    
    # Display the dataframe
    output$arbitrageTable <- renderTable({
      results
    }, striped = TRUE, bordered = TRUE)
    
    # Summary of whether arbitrage exists
    output$arbSummary <- renderText({
      if (no_loss && some_profit) {
        "All conditions are met, there is arbitrage, which can be exploited by holding the constructed portfolio at time 0. Arbitrage exists since you either chose R > U or R < D. For the R > U case, the bond dominates the stock as the bond price growth factor is even higher than the best case scenario for the stock. This is reflected in the signs of portfolio weights. We want to hold positive weights for bond (the better asset), and hold negative weights to short-sell the stock (the worse asset). Similar logic applies to the R < D case."
      } else {
        "At least one condition failed, there is no arbitrage. Now try adjust the bond price factor R or flip the signs of portfolio weights to create arbitrage. Refer to \"Hints\" on the About tab if you need help."
      }
    })
  })
}

# 3. Call to the shinyapp function
shinyApp(ui = ui, server = server)
