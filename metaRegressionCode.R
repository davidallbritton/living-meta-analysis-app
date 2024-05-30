# code for meta-regression tab; include in server

### plan: 1. make a new tab in the left panel to choose mediators, and
#         have a "yes/no" selector for mediation like the one for 
#         Bayesian robustness analysis
#         2. make a new results tab for meta-regression, display only if "yes"
#         3. to make the output for the meta-regression tab, calculate a new
#         frequentist model (fmaReg, a reactive like fma) that has 
#         moderators in the equation.  Should depend on MA.
#         4. put the server code for the meta-regression tab in a separate file
#         5. "source" the code in the server function in server.R

#### Code to insert into the server function for meta-regression
