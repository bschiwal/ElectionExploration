# ElectionExploration
Using Election Data to learn more about R and R Studio


I am starting this project both to learn more about R studio, Dyplyr and other
statistical coding that I need practice with, but also to learn how to publish
what I am working on in GitHub. This is the first project to a hopefully varying
future portfolio. 

in 'PresidentialResults08-20.R' I am testing various ways at analyzing county data in the past presidential elections. 

MappingTest.R is a test project I found to help me learn how to generate maps in R.


## Results so far
Most importantly I wanted to be able to show that, regardless of the polarized nature of our political discourse, most of America is pretty evenly split. To do this I need to be able to show a map with two variables as well as weight. I started with a diverging axis, where the results split away from zero. However, I realized that focusing only on the winners in any race is also forgetting the people that vote for the other party. 

So I need a map that can show two variables at the same time. Looking in to this I found a tutorial on making a bivarate map that, rather than using a single scale, uses a grid for colorization. This allowed me to show quantity of votes for both parties at the same time on the graph in a more visual way. 

Here is the result

![2020 US voting Map](./2020USvotemap.jpg)