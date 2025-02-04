# Spatial Iterated Prisoner's Dilemma.
Parameterized Netlogo model of the [Spatial Iterated Prisoner’s Dilemma](https://faculty.washington.edu/majeski/asymmetric.isq.pdf). It contains all the basic features and boasts a few extra features.

![Model](https://github.com/maartenlb/spatial-iterated-prisoners-dilemma/blob/main/Capture.JPG)

## WHAT IS IT?

This is a Netlogo model of the Iterated Prisoner's Dilemma. It contains all the basic features and boasts a few extra features, these extra features are:

- Option to visualize the patches by payoff. Lighter shades visualize higher payoffs and darker shades visualize lower payoffs.

- Option to visualize the patches by strategy changes. A patch will darken its color by 5% whenever it changes strategy, so darker patches are the ones that have changed the most.

- A button called Carpet which will visualize a 'Persian carpet' dynamic which implies the model updates in a synchronous manner.

Note that world wrapping is on and the world size is 121x121.

## HOW TO USE IT

While most components of the GUI should be pretty self-explanatory. A few parts could use some explaining.

- I chose to make visualization a chooser rather than 3 seperate buttons. The upside is it can be used while running go and you can actively see the changes happen. The downside is that go needs to run once before the visuals change.

- I made a output-rank-list switch because I noticed waiting for all the numbers to be written down caused a bit of a delay (since go doesn't want to loop before all the numbers are written down) so I put it on a switch to turn off.

- print-score-table is a button that well... prints the score table. Don't use it while running go since go clears the output and you'd have about a third of a second to see the score table.
