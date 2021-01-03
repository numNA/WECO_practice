# WECO_practice
This code was used for finding outliers and those patterns to validate the 'Predictive Maintenance' solution for specific equipment. There were sensor systems for gathering electric related data(current, voltage, thermal index, etc.) every second. In the dataset there are some patterns decreasing current and voltage to zero rarely, however, we couldn't know exactly when and why it happened and how many times was patterns occurred. So I made 3 R-based functions searching outliers(decreasing points) and drawing plots at time units including before and after each outlier.

### WECO_A
Analyze Dataset (Finding is more appropriate)
### WECO_P
Drawing plots of outliers
### WECO_T
Making Tables about each outlier's starting and endpoints
