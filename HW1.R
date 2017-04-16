exam <-read.table("c:/Users/tsunh/Desktop/exam_results.txt", header=TRUE)
View(exam)
exam$scores[5] # the 5th element in the original list of correct grades
sort(exam$scores)[5] #the fifth lowest grade
sort(exam$scores)[1:5] #Extract the five lowest grades together
sort(exam$scores,decreasing = T)[1:5] #the five highest scores by first sorting exam$scores in decreasing order
sd(exam$scores) # the standard deviation of scores
scores_diff
