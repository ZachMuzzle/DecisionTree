# Decsion Tree Program Using Lisp

## What This Program Does
* Takes an input of data that includes animal features such as hair, feathers, eggs, milk, etc.
* Based on those features a type will be given to it. From the numbers 1-7.

## What is Outputed
* We have a function called **Testcases** and the class will be returned the class that the animal is linked to, based on the data.
* Cross-validation is then used with different n-folds and then the error rate is returned.

## Output Example:
```
(test-9)

"The next testcases call classify" 

"The class of OCTOPUS is  " 
7 


"The class of SLOWWORM is  " 
3 


"The class of STARFISH is  " 
7 


"The class of SPARROW is  " 
2 


"The class of SEASNAKE is  " 
3 


"The class of TERMITE is  " 
6 

******************************************************
"The next test cases are calls to cross-validate" 


"The  error rate with 10-fold cross validation is  " 
0.060000002 


"The  error rate with 5-fold cross validation is   " 
0.060000002 


"The  error rate with 4-fold cross validation is   " 
0.1 


"The error rate with 2-fold cross validation is   " 
0.120000005

```