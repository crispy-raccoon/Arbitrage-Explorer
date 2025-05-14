Project name:
Arbitrage Explorer


Project description:
Create an interactive dashboard using R Shiny, build and display trinomial trees for stock prices, call, and put option value based on the parameter values inputted by the user. The user is challenged to tune the parameters such that arbitrage exists, and contruct a portfolio to exploit it.


Running the application:
Open "Code" in RStudio, press Ctrl + A to select all the code, followed by Ctrl + Enter to execute it.


Data source and processing:
Data is obtained through users' input and simulation. Processing is not required.


Desciption of each variable:
- Initial stock price S_0: The price of stock at time t=0
- Movement factors U,M,D of stock S: The multiplicative constants applied to stock price from one time period to the next one
- Time to maturity N: The number of years before an European option expires
- Strike price: The price to be compared with stock price at maturity to decide whether an option is exercised
- Movement factor R of bond B: THe multiplicative constant applied to bond price from one time period to the next one
- Weight W_S: Number of stocks held at t=0 in the constructed portfolio
- Weight W_B: Number of bonds held at t=0 in the constructed portfolio


Packages used:
- shiny: To build interactive application
- data.tree: To manipulate tree structures used to present price processes
- DiagrammeR: Supports tree generation


Output and analysis performed:
- Plotted trinomial trees for stock prices and analysed the trend in option prices based on the position of node in the tree
- Plotted an empirical CDF for the simulated portfolio values at maturity, explaining how the vertical line V = 0 can be helpful in finding the probability of losing money
- Displayed the truth values for arbitrage conditions using a table, explaining why arbitrage exists / does not exist


Reference:
- Shiny guidelines: https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/
- data.tree package: https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
- Typeset Math expressions: https://shiny.posit.co/r/reference/shiny/0.11/withmathjax.html

Author:
CID: 06011902


Support:
Please report issues to 06011902@ic.ac.uk

