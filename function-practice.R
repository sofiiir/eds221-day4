# adds up the number of birds and dogs

#defined function 
birddog_sum <- function(bird,dog){ #order defined here is the order you call when inputting values
  pets = bird + dog
  return(pets) #return allows us to use the value
}

#use the function!
total_pets <- birddog_sum(bird = 2, dog = 5) #specifiying our argument: better syntax
total_pets <- birddog_sum(2, 5) #same as above

#create a function to double values
double_it <-function(x){
  print(2*x)
}

double_it(9875889)

#splits for quarter miles
quarter_splits <- c(1.0, 1.1, 1.2, 1.1, 1.4, 1.5, 1.6, 1.4)

#adding two consecutive quarter mile splits 
#instead of seq_along can also use 1:length()
for (i in seq_along(quarter_splits)){
    sum_splits <- quarter_splits[i] + quarter_splits [i+1]
    print(sum_splits)
}


#write a function with conditionals 
#example of converting animals' ages

animal_age <- function(animal, age){
if (animal == "dog"){
  print(age * 7)
}else if(animal == "goat"){
  print(age * 4.7)
}
}

#try using for an 8 year old dog
animal_age(animal = "dog", age = 8)

#try using for a cow
animal_age(animal = "cow", age = 8)

# write an updated version of the animal age function with error messages
#%in% operator checks if variable is in the vector
#!variable means not that variable
animal_age_stop <- function(animal, age){
  
  if(!animal %in% c("dog", "goat")){
    stop("Oops! Animal must be a dog or goat.")
  }
  
  if(is.numeric(age) == FALSE){
    stop("The age must be a number.")
  }
 
  if (age <= 0 | age > 50){
    warning("Are you sure about your animal's age?")
  }
  if (animal == "dog"){
    print(age * 7)
  }else if(animal == "goat"){
    print(age * 4.7)
  }
}

animal_age_stop("dog", 1000) 

