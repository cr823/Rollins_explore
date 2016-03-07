#Homework 6
#Part 2
#Carmen Rollins

#1) Figure out how to generically extract all numeric columns from any data frame 
#so the method you develop can be used on any data.

require(ggplot2) #this is just for the test dataframe diamonds i use!
x <- data.frame(diamonds) #insert dastaframe you want to use, define it by x
keep_numeric<-x[sapply(x,is.numeric)] #this will eliminate all columns that are not numeric
delete_numeric<-x[!sapply(x,is.numeric)] #this will eliminate all numeric columns

#2) Create a dataframe that contains each pair of variable names in the first column 
#in a single string separated by a -, e.g. for the variables x and y, you should form
#the string “x-y” (HINT: Look at the help provided for the paste function) and their 
#corresponding Pearson correlation coefficient in the second column. (HINT: There is
#a function that calculates correlation coefficients – look carefully at what is 
#returned and optimize how you extract the correlation coefficients). Do not repeat
#any pairs.

#For this you will only need to update the dataframe (in this case it is "keep_numeric" in the first below command for this function to work)


#creating the pearson correlation for each variable:
pearson_cor<-cor(keep_numeric, use = "everything", method = c("pearson")) #use all observations in 'keep_numeric' (above) to find pearson corrleations 
#this will create a matrix of all variables (pearson) correlations we see that x and x and y and y, etc are on the matrix (with value 1)
#also that the valules repeat for xy and yx, etc. We will need to clean this up..
###NOTE: switch out "keep_numeric" for other datasets to use on others.

#to put in the correct order, I will first make a function: This fuction will remain the same for any data
#function to put all variable pairs in a column and have a second column of the pearson correlation coefficents:
var_columns <- function(data) {
  uptri <- upper.tri(data)
  data.frame( var1 = rownames(data)[row(data)[uptri]], var2 = rownames(data)[col(data)[uptri]], pearson  =(data)[uptri])}
#above is creating function(data) using the upper triangle section of the data from correlation (because the correlation data from
#pearson matrix above will give us a matrix of MxM results for pearson correlation (as explained above, we get repeats)
#Taking only the upper part of the triangle will eliminate the repeated variables from both the upper and lower part of the triangle. 

a<-var_columns(pearson_cor) #putting the pearson correlations from dataset into the function
a #looking at x we see what var1 and var2 and pearson are. We now want to merge var1 and var2 with a "-":

#To merge the two variabes we use the paste function:
variables<-(paste(a$var1,a$var2, sep= "-", collapse=NULL))  #paste var1 and var2 from x together with separation of "-"
pearson1<-a$pearson
#Now we want to create the columns:
final<-cbind(variables, pearson1) #this will create two columns. column1 is the two merged variables and column 2 is the pearson coefficents
final #this is the result we want

#3) Create and label a scatter plot for every pair of numeric variables. Add a title 
#to the plot that contains the calculated Pearson correlation coefficient of variables
#contained in the plot. (HINT: You should figure out how to extract all numeric columns 
#from a data frame so your method can be used on any data frame.)

pearson_var<-as.matrix(a$pearson) #putting the pearson coefficent data as a matrix so we can plot
plot(pearson_var, main="Pearson Correlation Coefficents", col="blue") #plotting coefficents, adding title and changing color of scatter dots

##Prof G: You got the first part and began correctly on the second part. The third part is to create
##Prof G: a scatterplot for each unique pair (expcluding a column paried with itself). You started
##Prof G: to put the column names correctly in part 2 but only created one pair name.
##Prof G: See my code below:

####################################################
# Function to generate R-square values and correlation
# values. All R-square values are returned in one dataframe.
# Only correlation (R) values that exceed the parameter 
# threshold are returned in a separate dataframe.
# The function also creates labels by combining the
# names of the columns pairs into a single string
# like "Carat-Price"
####################################################
get_rsq_corr <- function(Num_Frame, threshold) {
   #Create correlation matrix using cor function
   #Specify pearson correlation
   corr_a <- cor(Num_Frame, method="pearson")
   #Set up a threshold and null vectors before entering loop
   r_square <- NULL
   rsq_names <- NULL
   corr_list <- NULL
   corr_names <- NULL
   
   #Get the length of the one dimension of the square matrix
   len <- length(corr_a[1,])
   
   #Only loop through the upper right triangle
   for (i in (1:(len-1))) {
      for (j in ((i+1):len)) {
         #Form the name pair and add to the named pair vector
         pair_name <- paste(names(corr_a[,1])[[i]],names(corr_a[1,])[[j]],sep="-")
         rsq_names <- c(rsq_names, pair_name)
         
         #Add the r_square value to the value vector
         r_square <- c(r_square, corr_a[i,j]^2)
         
         #if the threshold is exceeded, add the name and value to the
         #respective correlation vectors
         if (abs(corr_a[i,j]) > threshold) {
            corr_names <- c(corr_names, pair_name)
            corr_list <- c(corr_list, corr_a[i,j]) 
         }
      }
   }
   
   #create the dataframes and label the columns
   rsq_df <- data.frame(cbind(rsq_names, r_square))
   names(rsq_df)[1] <- "Pair"
   names(rsq_df)[2] <- "Value"
   corr_df <- data.frame(cbind(corr_names, corr_list))
   names(corr_df)[1] <- "Pair"
   names(corr_df)[2] <- "Value"
   return(list("rsquare"=rsq_df, "correlation"=corr_df))
}

