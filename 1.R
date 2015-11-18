## Q5
# Suppose that we have created a machine learning algorithm that predicts whether a link will
# be clicked with 99% sensitivity and 99% specificity. The rate the link is clicked is 1/1000 of visits
# to a website. If we predict the link will be clicked on a specific visit, what is the probability
# it will actually be clicked?

sensitivity <- 0.99
specificity <- 0.99
prevalence <- 1/1000


true.positive <- prevalence * sensitivity
false.positive <- 1 - specificity
positive.predictive.value <- true.positive / (true.positive + false.positive)
positive.predictive.value