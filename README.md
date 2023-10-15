Project 3. Mental Health in Tech. 
This dataset is from a 2014 survey that measures attitudes towards mental health and frequency of mental health disorders in the tech workplace. 
Mental Health Survey (note, there are two sheets in the workbook). 

This dataset was spread on two sheets and required cleaning to be usable. 
The orignal dataset has 27 columns. Some columns did not have issues. For example, the 'coworker', 'leave', and 'mental_health_consequence' columns did not have invalid data.
Other columns had invalid data type formats, incorrect data, or typos. For example, some entries had ages with negative number or dates from the year 1905. 
If the values could be interpreted then they were replaced with the corresponding values. For 'gender', some entires used M, this was replaced with Male. 
If the values could not be interpreted, then they were replaced with NA values. 

For the most part, rows with NA values were not removed from the dataset. Most entries that had NA values also had a lot of other useful data.


However, rows missing a lot of data were removed. Some rows were missing more than 20 variables. I thought that they did not have enough data to be considered useful and removed them from the dataset.
