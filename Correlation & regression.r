
Name: Niranjan Vinod Patil.
Batch: DA Lab-1.		
Roll No: SCETTYDSA15.
Course: Descriptive Analytics – SDL. 
Use Simple correlation, Simple Regression and Multiple regressions in R programming
Code:

Simple Correlation


#Values of height
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
#Values of weight
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
#find correlation between height and weight
cor(x,y)



plot(y,x,col = "blue",main = "Height & Weight Regression",
     abline(lm(x~y)),cex = 1.3,,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")


Simple Regression

#Fit the linear regression model
relation <- lm(y~x)
print(relation)



# Find weight of a person with height 170.
a <- data.frame(x = 170)
result <-  predict(relation,a)
print(result)


 Multiple regression 

input <- mtcars[,c("mpg","disp","hp","wt")]
print(head(input))


# Create the relationship model.
model <- lm(mpg~disp+hp+wt, data = input)

# Show the model.
print(model)


a <- coef(model)[1]
print(a)


Xdisp <- coef(model)[2]
Xhp <- coef(model)[3]
Xwt <- coef(model)[4]

print(Xdisp)
print(Xhp)
print(Xwt)
x1 = 221
x2 = 102 
x3 = 2.91
Y = a+Xdisp*x1+Xhp*x2+Xwt*x3
print(Y)
