---
sidebar_position: 1
---

# Design Document Part 1

**Class Diagram**
![UML Class Diagram](https://github.com/cis3296s23/MacroCalc704/blob/main/UML_Diagram.png 'Class Diagram')
This is the UML Diagram as of 4/18/23. Classes are separated to show how different problems were handled. Most code is within one file (app.R) and it pulls from various other files for help to function properly. Some of these include various .rda files, daily_values.txt, and some .json files. The .json and .db files handle the user login and storage, the .txt and .rda files are for information about the food, the .py files handle the server side/online capability, and most of the hard work to display the app and allow for things to work are within the app.R.


**Sequence Diagram 1**

![Sequence Diagram 1](https://github.com/cis3296s23/MacroCalc704/blob/main/UML_Sequence1.png 'Sequence Diagram 1')

The user wants to search for a specific ingredient and add it to their intake for the day. They type the item into the search bar, which prompts the app to list the item or things close to it. Then buttons appear where one, which the user clicks, adds the item to the intake for the day. Once it is clicked the program saves data from the ingredient and stores it into that day's database following the server() function.


**Sequence Diagram 2**

![Sequence Diagram 1](https://github.com/cis3296s23/MacroCalc704/blob/main/UML_Sequence2.png 'Sequence Diagram 1')

The user stopped at McDonalds on the way home from work today and thought of ordering a BigMac. Wanting to see how this would impact their daily values, they search BigMac into the search bar which then prompts the user with options. The user selects view nutrient information and decided the BigMac would put them over some of their daily goals and decides against getting it.

**Database**
A database is used to store user's account information. This is done through google sign-in. The user's email and name are stored so that the data can be associated with their account. This should allow users to sign-in and save their recipes, progress, etc.
