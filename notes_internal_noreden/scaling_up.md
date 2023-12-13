# Experiment plan

## Food groups

1-28 (some are not really important)

No need to test ALL combinations

At this point, no need to do an extensive search. More important to know the runtime. 

Start from 5, 10, 12, 14, ..., 20

Also there will be groups that are absolutely necessary to keep





## GHGE reduction

95%

90%

85%

80%

75%



## foods

(Remove)

potatoes

Sugars 

-----

Other grains

butter

juice

white meat

cakes 

-----

legume

nuts 

creams



### constraints

drop sugar

add Sat fat 

vitamin c

calcium 



# Notes

N foods, N constraints, ghge coefficient, Solution ok? time, comments

#### Bread, vegetables, red meat

3, 5, 1, ok, 5.36s

3, 5, 0.95, ok, 5.16s, too much reduction in vegetable

3, 5, 0.90, marginally ok, 5.13s, further reduction in vegetable



#### (+) Milk, Fish, Cheese

6, 5, 0.95, ok, 5.22s

6, 5, 0.90, ok, 5.29s, reduce red meat, add cheese

6, 5, 0.85, ok, 5.23, even more cheese

6, 5, 0.80, ok, more cheese and fish, 75% reduction of red meat

6, 5, 0.75, ok, same 

#### (+) fruit

7, 5, 0.95, ok, 5.34s,  less red meat, more cheese

7, 5, 0.90, ok, 5.23s, 

7, 5, 0.85, ok, 5.53s, less red meat, more cheese and fish

#### (+) egg, potatoes

9, 5, 0.95, ok, 5.9s, more eggs, less red meat and cheese

9, 5, 0.90, ok, 6.79s, more eggs, less red meat

9, 5, 0.75, ok, 6.37s, almost twice eggs, 36% more cheese, 95% less red meat

#### (+) sugar, other grains

11, 5, 0.95, ok, 6.8s, more sweets

11, 5, 0.75, ok, 5.3s, lots more eggs, more fish, 83% less red meat

#### (+) butter, juice, white meat 

14, 5, 0.95, ok, 6.6s, more eggs, less red meat

14, 5, 0.75, ok, 6.4s

#### (+) cakes, legumes

16, 5, 0.95, ok, 7s, more legumes, less red meat and cheese and sugar

16, 5, 0.75, ok, 6.5s, much more legumes (8 times), 73% less red meat, 90% less sugar

#### (+) nuts, cream

18, 5, 0.95, ok, 8.2s, 

18, 5, 0.75, ok, 7.1s, much more nuts and legumes

#### (+) sauces, snacks

20, 5, 0.95, ok, 7.4s, less snacks

20, 5, 0.75, ok, 7.4s, much more nuts and legumes

#### (+) spices, soda

22, 5, 0.95, ok, 7.8s, more snacks. Possible highly correlated with something else

22, 5, 0.75, ok, 8.4s, more legumes and nuts

#### (+) non-dairy milk, vegetarian, other

25, 5, 0.95, ok, 8.4s, more nuts, 

25, 5, 0.75, ok, 8.6s



## Add constraint: vitamin C

25, 6, 0.95, ok, 10s

25, 6, 0.75, ok, 10s

## Add constraint: calcium

25, 7, 0.95, 11s, ghge can not be satisfied, 11s

possibly due to calcium constraint within animal product?



















