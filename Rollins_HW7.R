#Homework 7
#Carmen Rollins

#Write an R function named explore that takes a data frame, a vector of bin sizes, and
#a correlation threshold as input parameters:

explore<-function(dframe,vector,thresh){ #starting to write the function explore...
  #dframe= data frame; 
  #vector= vector of bin sizes; 
  #thresh= correlation threshold
  require(grid)
  require(ggplot2) 
  require(grid)
  require(stats)
  require(plyr)
  nums<-dframe[which(lapply(dframe,is.numeric) == TRUE)] #make sure the data frame is numeric first...


#1) Plot a pair of blue histograms with a vertical red line at the mean (one using
#counts and the other density) for every numerical variable at each bin size specified
#in the bin sizes input parameter. You can plot individually or as a grid. If you chose
#to plot as a grid, there should be separate grids for each count-bin size combination
#and separate grids for each density-bin size combination. For example, given 5 numeric
#variables and a vector of three bin sizes will generate 30 individual plots or a total
#of 6 grid plots (with each grid plot containing 5 subplots).

#create loop in function for all variables, bin sizes and mean line: 
  
  
#This is similar to Prof G's notes...
  for (i in 1:length(nums)) #loops over each observation in dataframe (nums) 
  { for (j in 1:length(vector)) #loops over each observation in binsize (see below) 
    {

#Calculates the mean and label
col_mean <- mean(nums[[i]]) #calculating the mean of the columns in the dataframe nums 
mean_Label = sprintf("%8.2f ", col_mean) #returns character vector containing a form


#Plot regular count histogram with verticle red line at mean
nums_plot1 <- ggplot(nums, aes_string(x=nums[[i]])) 
nums_plot1<- nums_plot1 + 
            ##Prof G: Need to calcualte the bin width based on the desired
            ##Prof G: number of bins. Take another look at my code.
             geom_histogram(fill = 'blue', binwidth=vector[j] ) + 
            geom_vline(xintercept=col_mean, colour='red') +
        labs(x=names(nums)[[i]]) +
  annotate("text",x=col_mean,y=0,label=mean_Label,hjust=0) #we tell it where to plot the line

      
print(nums_plot1) #printing count histogram
      
#Plot denisty histogram and verticle red line at mean
nums_plot2<-ggplot(nums, aes_string(x=nums[[i]]))
nums_plot2<- nums_plot2 +
        geom_histogram(aes(y=..density..), fill = 'blue', binwidth=vector[j]) +
        labs(x=names(nums)[[i]]) +
        geom_vline(xintercept=col_mean, colour='red')  + 
  annotate("text", x=col_mean, y=0, label=mean_Label, hjust=0) #we tell it where to plot the line

print(nums_plot2) #printing density histogram
  }
}

#2) Plot a gray bar graph for every categorical and binary variable.
    var_cat <- dframe[which(lapply(dframe, is.factor) == TRUE)] #extracting catagorial varialbes 
    var_bi <- dframe[which(lapply(dframe, is.logical) == TRUE)] #extracting binary varialbes 
    
#Plot catagorial variable (loop all observations of catagorial varialbes): 
    if (length(var_cat)>0)
      {
for(i in 1:length(var_cat)) #for each observation in var_cat
  {cat_var<-ggplot(var_cat[i], aes(x=var_cat[[i]])) + geom_bar(fill="gray") 
  print(cat_var)}}

#Plot binary variable (loop all obersations of binary variables):
if (length(var_bi)>0){
    for(i in 1:length(var_bi))
  {   bi_var<-ggplot(var_bi[i], aes(x=var_bi[[i]])) + geom_bar(fill="gray")
  print(bi_var)}
}

#3) Calculate the r2 (r-square) value for every pair of numerical variables.
r_sqr<-c()# creating empty variable for list of r^2 below...
final<- NULL
pearson_cor<-cor(nums, use = "everything", method = c("pearson")) #create matrix to get pearson method

#function to put all variable pairs in a column and have a second column of the pearson correlation coefficents:
var_columns <- function(data) {
  uptri <- upper.tri(data)
  data.frame( var1 = rownames(data)[row(data)[uptri]], var2 = rownames(data)[col(data)[uptri]], pearson =(data)[uptri])}
  #above is creating function(data) using the upper triangle section of the data from correlation (because the correlation data from
  #pearson matrix above will give us a matrix of MxM results for pearson correlation (as explained above, we get repeats)
  #Taking only the upper part of the triangle will eliminate the repeated variables from both the upper and lower part of the triangle. 
  
  a<-var_columns(pearson_cor) #putting the pearson correlations from dataset into the function
  
for (i in 1:(length(nums)-1))# two loops each so each variable in nums calculates r^2 (with no repeats)
{ for (j in ((i+1):length(nums))){
  r_sqr<-cbind(r_sqr,summary(lm(nums[[i]]~nums[[j]]))$r.squared)}#creating regression and extracting r^2 values  
}


#4) Return the following in an R list:
#a. A frequency table for every categorical and binary variable 

#using the binary and catagorial calculations from #2

  if (length(var_bi)>0) 
    { for (i in 1:(length(var_bi)))
  { freq_tbl_bi<-table(var_bi[i])
    }
    print(freq_tbl_bi) }
  

if (length(var_cat)>0)
  { for (i in 1:(length(var_cat)))
      { freq_tbl_cat<-table(var_cat[i])
  }
  print(freq_tbl_cat)
  }


#b. For numerical variables
  #i. A summary statistics table for each numerical variable

df<-nums #defines the data frame (df) as all numeric values
stats_tbl <- do.call(data.frame, #create table from dataframe using do.call function
                     list(mean = apply(df, 2, mean), #makes list of mean 
                    sd = apply(df, 2, sd), #standard deviation
                    median = apply(df, 2, median), #median
                    min = apply(df, 2, min), #min
                    max = apply(df, 2, max), #max
                    n = apply(df, 2, length))) #total number of observations
#print table at end

  #ii. A data frame that contains each pair of variable names and the associated
      #r-square value.

r_sqr<-cbind(as.vector(r_sqr)) #making r_sqr from part 3 into vectors so i can merge it with the pearson 

#To merge the two variabes we use the paste function:
#paste var1 and var2 from x together with separation of "-" 
variable_names<-(paste(a$var1, a$var2, sep= "-", collapse=NULL))  #naming variables
#This will now merge the variable_names of r_sqr and pearson correlation all together
final<-cbind(variable_names, is.numeric(a[3]), is.numeric(r_sqr)) #this will be the table we print for 4iii (though we need to modify a bit)
print(final)
  #iii. A data frame that contains each pair of variable names and correlation 
      #coefficient (Pearson) for all coefficients whose absolute value is greater
      #than the correlation threshold (do not repeat any pairs)

delete=NULL #making empty variable to create for function
pearson<-is.numeric(final[,2])
for (i in 1:length(pearson)) { #for each pearson correlation 
  if (abs(pearson[i]) <= thresh) { #if the abs value of each pearson corr is less than or equal to threshold
      delete<-c(delete,i) #we put it in new variable delete
    final_final<- final[-(delete),]  #we create a new variable with all varialbe pairs but eliminate
    #the ones that are less than/equal to threshold (i.e the ones we put into the delete variable)
  }

print(final_final) #Print the resultant dataframe.
}
print(stats_tbl)
}


#5) Test your function using the diamonds data frame expanded to include a logical
#column. To create a logical column, take the numeric column VS from the mtcars 
#dataset, extend it to the same length as the diamonds data, and convert it to a 
#logical column. Use a vector of bin sizes (5, 20, 50), and a correlation threshold 
#of 0.25. Also test your function using the mtcars data.

###### To test data
require(ggplot2) 
data(diamonds) 
data(mtcars) 
diamonds$vs <- rep((mtcars$vs == 1)[3:22], 2697) #add the logical variable 


myframe <- explore(diamonds,c(5,20,50), 0.25)
explore(mtcars,c(5,20,50), 0.25)

##Prof G: explore should return a list. The list should include frequency
##Prof G: tables for the factor and logical columns, r quare and correlation
##Prof G: tables for the numeric variables, and the stats for the numeric.
for (i in 1:length(myframe)) {print(myframe[i])}
str(myframe)
