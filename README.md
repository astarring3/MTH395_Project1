MTH395_Project1
x1:
===============
\documentclass{article}

\begin{document}
This R program is a sample of how to analyze one of the random vectors from the sample data
\par\vspace{0.6 cm}
When faced with a programming task, it is almost always easier to start with something that resembles the program you are writing and modify it.
\par\vspace{0.6 cm}
The purpose this example is to minimize the programming, possibly at the expense of efficiency setting up and running the program.  You may think of it as a starting point, and you are free to add any enhancements you like.
\par\vspace{0.6 cm}
The scenario is that we are starting with a data set in the form of a comma separated  file named \texttt{Project1\_test.csv}.  There are four columns in the file, and each represents a sample from a different unknown distribution.
\par\vspace{0.6 cm}
The file is stored on github.  The following code uses the RCurl and foreign packages to access it and read it into an R data frame called \texttt{df}:
<<>>=
library(RCurl)
library(foreign)
#d=getURL("https://raw.githubusercontent.com/MTH395-Fall2014/Project1/master/A_Starring.csv")
df=read.csv("/Users/astarring/Downloads/Project1-master/A_Starring.csv")
@
\par\vspace{0.6 cm}
The next step is to examine the data.  First we want to find out what the data frame looks like, so we'll use the \texttt{str()} function:
<<>>=
str(df)
@
\par\vspace{0.6 cm}
From this output, we see that \texttt{df} has 5 columns, named $X$ and $x1-x4$.  The $X$ column is a row number, and the four columns $x1-x4$ represent four random samples from our unknown distributions.
\par\vspace{0.6 cm}
While not mandatory, we will use the \texttt{attach()} function so that we can just reference the columns of \texttt{df} by their names.  Otherwise, if we just reference $x1$, R will not see it:
<<>>=
x1[1:20]
@
\par\vspace{0.6 cm}
We have two options.  We can use a two-level name \texttt{df\$x1},
<<>>=
df$x1[1:20]
@
\par\vspace{0.6 cm}
Alternatively, we can use \texttt{attach}, which makes the internal column names of the data frame visible:
<<>>=
attach(df)
x1[1:20]
@
\par\vspace{0.6 cm}
The first thing we might look at are the summary statistics for this column:
<<>>=
summary(x1)
@
\par\vspace{0.6 cm}
We might also want to see a histogram:
<<>>=
hist(x1)
@
\par\vspace{0.6 cm}
At this point, we might make a judgement that this data could be normal.  The distribution appears to be symmetric (the mean and median are the same), and it is centered at 18.  To fully specify the normal distribution, we need the standard deviation (or variance) in addition to the mean:
<<>>=
mean(x1)
sd(x1)
@
\par\vspace{0.6 cm}
The Method of Moments is trivial in this case because 
\[
E(X)=\mu \quad\mbox{and}\quad V(x)=\sigma^2
\]
\par\vspace{0.6 cm}
We'll use the standard deviation $\sigma$, which is the square root of the variance, because that is what we specify in the \texttt{norm} functions in R.  So our candidate for the distribution of \texttt{x1} might be $N(17.98,2.94).  We can now proceed to the goodness of fit test.
\par\vspace{0.6 cm}
First a bit of guesswork.  We want to partition the sample values into between 10 and 20 ranges, with the endpoints chosen so that there will be at least 8 or so data values in each bin. 
\par\vspace{0.6 cm}
The simplest way to do this is to manually assign the cutoff values to an array, as follows:
<<>>=
z<-c(-1000,5,6,7,8,9,10,11,1000)
@
\par\vspace{0.6 cm}
The first range in the partition will be -1000 to 12, the second will be 12-13, the third 13-14, and so on.  The large values at either end of the ranges are chosen to include any outliers in the data.
\par\vspace{0.6 cm}
It will usually take a couple of iterations to get the cutoff points that produce a partition having enough data values in each cell.  The number of cells is the number of elements in x, minus 1
<<>>=
ncells<-length(z)-1
z                                # To see if it worked, list the array z
@
\par\vspace{0.6 cm}
Now assign the column of interest to the name 'x'.  We don't usually have to worry about whether variables are arrays or not, R will usually do the right thing.
<<>>=
x<-x1
@
\par\vspace{0.6 cm}
We'll allocate an array of 15 values called ct for the counts in each range.  Note that the 16 elements of the z array define 15 ranges because each range is the set of values between an upper and lower limit. Use the rep (repeat) function to create an array with ncells values of zero, and store them in ct:
<<>>=
ct<-rep(0,ncells)
ct
@
\par\vspace{0.6 cm}
Now use a loop (actually a loop inside a loop) to find the counts in each range
<<>>=
for(j in 1:2000){
    for(i in 1:ncells){
        if((z[[i+1]]>=x[[j]]) & (z[[i]]<x[[j]])) ct[[i]]<-ct[[i]]+1
                  }
                }
#
# Display the results
#
ct

# Compute the sample mean and standard deviation 
# 
n<-12
s=mean(x)/n #using s instead of p for prob of successes because we already have a variable p
s

@
\par\vspace{0.6 cm}
Now construct an array p of proabilities for the ranges defined in the x array, using the candidate distribution to compute the probabilities.
<<>>=
p<-rep(0,ncells)
p
@
\par\vspace{0.6 cm}
Use another loop to construct the probabilities.  The value in each element is the probability that a normal random variable with mean mu and standard deviation sigma falls between the lower and upper values.
\par\vspace{0.6 cm}
That probability is computed as:  pnorm(upper\_value,mu,sigma)-pnorm(lower\_value,mu,sigma)
<<>>=
for(i in 1:ncells){
    p[[i]]<-pbinom(z[[i+1]],n,s)-pbinom(z[[i]],n,s)
    }
p
@
\par\vspace{0.6 cm}
Finally we are ready to call the chisq.test() procedure using ct and p as input:
<<>>=
chisq.test(ct,p=p,rescale.p=TRUE)
@
\par\vspace{0.6 cm}
If the p-value is greater than 0.05, you can assume that the distribution used to generate the p array is a reasonable model for this data.
\par\vspace{0.6 cm}
If the p-value is less than 0.05, you should interpret the results to mean it is unlikely that the data are adequately represented by the distribution used to generate the p-values, especially if it is much less than 0.05.
\par\vspace{0.6 cm}
As long as all of your cells had reasonable counts, say 8 or more, you can ignore any warning messages about the approximation.
\par\vspace{0.6 cm}
In this example, the $p$-value is well above 0.05, so we conclude that a N(17.98,2.94) distribution is a reasonable model for this data.  
\end{document}
=================
x2
=================
\documentclass{article}

\begin{document}
This R program is a sample of how to analyze one of the random vectors from the sample data
\par\vspace{0.6 cm}
When faced with a programming task, it is almost always easier to start with something that resembles the program you are writing and modify it.
\par\vspace{0.6 cm}
The purpose this example is to minimize the programming, possibly at the expense of efficiency setting up and running the program.  You may think of it as a starting point, and you are free to add any enhancements you like.
\par\vspace{0.6 cm}
The scenario is that we are starting with a data set in the form of a comma separated  file named \texttt{Project1\_test.csv}.  There are four columns in the file, and each represents a sample from a different unknown distribution.
\par\vspace{0.6 cm}
The file is stored on github.  The following code uses the RCurl and foreign packages to access it and read it into an R data frame called \texttt{df}:
<<>>=
library(RCurl)
library(foreign)
#d=getURL("https://raw.githubusercontent.com/MTH395-Fall2014/Project1/master/Project1_test.csv")
df=read.csv("/Users/astarring/Downloads/Project1-master/A_Starring.csv")
@
\par\vspace{0.6 cm}
The next step is to examine the data.  First we want to find out what the data frame looks like, so we'll use the \texttt{str()} function:
<<>>=
str(df)
@
\par\vspace{0.6 cm}
From this output, we see that \texttt{df} has 5 columns, named $X$ and $x1-x4$.  The $X$ column is a row number, and the four columns $x1-x4$ represent four random samples from our unknown distributions.
\par\vspace{0.6 cm}
While not mandatory, we will use the \texttt{attach()} function so that we can just reference the columns of \texttt{df} by their names.  Otherwise, if we just reference $x1$, R will not see it:
<<>>=
x2[1:20]
@
\par\vspace{0.6 cm}
We have two options.  We can use a two-level name \texttt{df\$x1},
<<>>=
df$x2[1:20]
@
\par\vspace{0.6 cm}
Alternatively, we can use \texttt{attach}, which makes the internal column names of the data frame visible:
<<>>=
attach(df)
x2[1:20]
@
\par\vspace{0.6 cm}
The first thing we might look at are the summary statistics for this column:
<<>>=
summary(x2)
@
\par\vspace{0.6 cm}
We might also want to see a histogram:
<<>>=
hist(x2)
@
\par\vspace{0.6 cm}
At this point, we might make a judgement that this data could be normal.  The distribution appears to be symmetric (the mean and median are the same), and it is centered at 18.  To fully specify the normal distribution, we need the standard deviation (or variance) in addition to the mean:
<<>>=
mean(x2)
sd(x2)
@
\par\vspace{0.6 cm}
The Method of Moments is trivial in this case because 
\[
E(X)=\mu \quad\mbox{and}\quad V(x)=\sigma^2
\]
\par\vspace{0.6 cm}
We'll use the standard deviation $\sigma$, which is the square root of the variance, because that is what we specify in the \texttt{norm} functions in R.  So our candidate for the distribution of \texttt{x1} might be $N(17.98,2.94).  We can now proceed to the goodness of fit test.
\par\vspace{0.6 cm}
First a bit of guesswork.  We want to partition the sample values into between 10 and 20 ranges, with the endpoints chosen so that there will be at least 8 or so data values in each bin. 
\par\vspace{0.6 cm}
The simplest way to do this is to manually assign the cutoff values to an array, as follows:
<<>>=
z<-c(-1000,250,260,270,280,290,300,310,320,330,340,350,1000)
@
\par\vspace{0.6 cm}
The first range in the partition will be -1000 to 12, the second will be 12-13, the third 13-14, and so on.  The large values at either end of the ranges are chosen to include any outliers in the data.
\par\vspace{0.6 cm}
It will usually take a couple of iterations to get the cutoff points that produce a partition having enough data values in each cell.  The number of cells is the number of elements in x, minus 1
<<>>=
ncells<-length(z)-1
z                                # To see if it worked, list the array z
@
\par\vspace{0.6 cm}
Now assign the column of interest to the name 'x'.  We don't usually have to worry about whether variables are arrays or not, R will usually do the right thing.
<<>>=
x<-x2
@
\par\vspace{0.6 cm}
We'll allocate an array of 15 values called ct for the counts in each range.  Note that the 16 elements of the z array define 15 ranges because each range is the set of values between an upper and lower limit. Use the rep (repeat) function to create an array with ncells values of zero, and store them in ct:
<<>>=
ct<-rep(0,ncells)
ct
@
\par\vspace{0.6 cm}
Now use a loop (actually a loop inside a loop) to find the counts in each range
<<>>=
for(j in 1:2000){
    for(i in 1:ncells){
        if((z[[i+1]]>=x[[j]]) & (z[[i]]<x[[j]])) ct[[i]]<-ct[[i]]+1
                  }
                }
#
# Display the results
#
ct

# Compute the sample mean and standard deviation 
# 
mu<-mean(x)
mu
sigma<-sqrt(var(x))
sigma
@
\par\vspace{0.6 cm}
Now construct an array p of proabilities for the ranges defined in the x array, using the candidate distribution to compute the probabilities.
<<>>=
p<-rep(0,ncells)
p
@
\par\vspace{0.6 cm}
Use another loop to construct the probabilities.  The value in each element is the probability that a normal random variable with mean mu and standard deviation sigma falls between the lower and upper values.
\par\vspace{0.6 cm}
That probability is computed as:  pnorm(upper\_value,mu,sigma)-pnorm(lower\_value,mu,sigma)
<<>>=
for(i in 1:ncells){
    p[[i]]<-pnorm(z[[i+1]],mu,sigma)-pnorm(z[[i]],mu,sigma)
    }
p
@
\par\vspace{0.6 cm}
Finally we are ready to call the chisq.test() procedure using ct and p as input:
<<>>=
chisq.test(ct,p=p,rescale.p=TRUE)
@
\par\vspace{0.6 cm}
If the p-value is greater than 0.05, you can assume that the distribution used to generate the p array is a reasonable model for this data.
\par\vspace{0.6 cm}
If the p-value is less than 0.05, you should interpret the results to mean it is unlikely that the data are adequately represented by the distribution used to generate the p-values, especially if it is much less than 0.05.
\par\vspace{0.6 cm}
As long as all of your cells had reasonable counts, say 8 or more, you can ignore any warning messages about the approximation.
\par\vspace{0.6 cm}
In this example, the $p$-value is well above 0.05, so we conclude that a N(17.98,2.94) distribution is a reasonable model for this data.  
\end{document}
======================
x3
=====================
\documentclass{article}

\begin{document}
This R program is a sample of how to analyze one of the random vectors from the sample data
\par\vspace{0.6 cm}
When faced with a programming task, it is almost always easier to start with something that resembles the program you are writing and modify it.
\par\vspace{0.6 cm}
The purpose this example is to minimize the programming, possibly at the expense of efficiency setting up and running the program.  You may think of it as a starting point, and you are free to add any enhancements you like.
\par\vspace{0.6 cm}
The scenario is that we are starting with a data set in the form of a comma separated  file named \texttt{Project1\_test.csv}.  There are four columns in the file, and each represents a sample from a different unknown distribution.
\par\vspace{0.6 cm}
The file is stored on github.  The following code uses the RCurl and foreign packages to access it and read it into an R data frame called \texttt{df}:
<<>>=
library(RCurl)
library(foreign)
#d=getURL("https://raw.githubusercontent.com/MTH395-Fall2014/Project1/master/Project1_test.csv")
df=read.csv("/Users/astarring/Downloads/Project1-master/A_Starring.csv")
@
\par\vspace{0.6 cm}
The next step is to examine the data.  First we want to find out what the data frame looks like, so we'll use the \texttt{str()} function:
<<>>=
str(df)
@
\par\vspace{0.6 cm}
From this output, we see that \texttt{df} has 5 columns, named $X$ and $x1-x4$.  The $X$ column is a row number, and the four columns $x1-x4$ represent four random samples from our unknown distributions.
\par\vspace{0.6 cm}
While not mandatory, we will use the \texttt{attach()} function so that we can just reference the columns of \texttt{df} by their names.  Otherwise, if we just reference $x1$, R will not see it:
<<>>=
x3[1:20]
@
\par\vspace{0.6 cm}
We have two options.  We can use a two-level name \texttt{df\$x1},
<<>>=
df$x3[1:20]
@
\par\vspace{0.6 cm}
Alternatively, we can use \texttt{attach}, which makes the internal column names of the data frame visible:
<<>>=
attach(df)
x3[1:20]
@
\par\vspace{0.6 cm}
The first thing we might look at are the summary statistics for this column:
<<>>=
summary(x3)
@
\par\vspace{0.6 cm}
We might also want to see a histogram:
<<>>=
hist(x3)
@
\par\vspace{0.6 cm}
At this point, we might make a judgement that this data could be normal.  The distribution appears to be symmetric (the mean and median are the same), and it is centered at 18.  To fully specify the normal distribution, we need the standard deviation (or variance) in addition to the mean:
<<>>=
mean(x3)
sd(x3)
@
\par\vspace{0.6 cm}
The Method of Moments is trivial in this case because 
\[
E(X)=\mu \quad\mbox{and}\quad V(x)=\sigma^2
\]
\par\vspace{0.6 cm}
We'll use the standard deviation $\sigma$, which is the square root of the variance, because that is what we specify in the \texttt{norm} functions in R.  So our candidate for the distribution of \texttt{x1} might be $N(17.98,2.94).  We can now proceed to the goodness of fit test.
\par\vspace{0.6 cm}
First a bit of guesswork.  We want to partition the sample values into between 10 and 20 ranges, with the endpoints chosen so that there will be at least 8 or so data values in each bin. 
\par\vspace{0.6 cm}
The simplest way to do this is to manually assign the cutoff values to an array, as follows:
<<>>=
z<-c(-1000,1,2,3,4,5,6,1000)
@
\par\vspace{0.6 cm}
The first range in the partition will be -1000 to 12, the second will be 12-13, the third 13-14, and so on.  The large values at either end of the ranges are chosen to include any outliers in the data.
\par\vspace{0.6 cm}
It will usually take a couple of iterations to get the cutoff points that produce a partition having enough data values in each cell.  The number of cells is the number of elements in x, minus 1
<<>>=
ncells<-length(z)-1
z                                # To see if it worked, list the array z
@
\par\vspace{0.6 cm}
Now assign the column of interest to the name 'x'.  We don't usually have to worry about whether variables are arrays or not, R will usually do the right thing.
<<>>=
x<-log(x3)
@
\par\vspace{0.6 cm}
We'll allocate an array of 15 values called ct for the counts in each range.  Note that the 16 elements of the z array define 15 ranges because each range is the set of values between an upper and lower limit. Use the rep (repeat) function to create an array with ncells values of zero, and store them in ct:
<<>>=
ct<-rep(0,ncells)
ct
@
\par\vspace{0.6 cm}
Now use a loop (actually a loop inside a loop) to find the counts in each range
<<>>=
for(j in 1:2000){
    for(i in 1:ncells){
        if((z[[i+1]]>=x[[j]]) & (z[[i]]<x[[j]])) ct[[i]]<-ct[[i]]+1
                  }
                }
#
# Display the results
#
ct

# Compute the sample mean and standard deviation 
# 
mu<-mean(x)
mu
sigma<-sqrt(var(x))
sigma
@
\par\vspace{0.6 cm}
Now construct an array p of proabilities for the ranges defined in the x array, using the candidate distribution to compute the probabilities.
<<>>=
p<-rep(0,ncells)
p
@
\par\vspace{0.6 cm}
Use another loop to construct the probabilities.  The value in each element is the probability that a normal random variable with mean mu and standard deviation sigma falls between the lower and upper values.
\par\vspace{0.6 cm}
That probability is computed as:  pnorm(upper\_value,mu,sigma)-pnorm(lower\_value,mu,sigma)
<<>>=
for(i in 1:ncells){
    p[[i]]<-plnorm(z[[i+1]],mu,sigma)-plnorm(z[[i]],mu,sigma)
    }
p
@
\par\vspace{0.6 cm}
Finally we are ready to call the chisq.test() procedure using ct and p as input:
<<>>=
chisq.test(ct,p=p,rescale.p=TRUE)
@
\par\vspace{0.6 cm}
If the p-value is greater than 0.05, you can assume that the distribution used to generate the p array is a reasonable model for this data.
\par\vspace{0.6 cm}
If the p-value is less than 0.05, you should interpret the results to mean it is unlikely that the data are adequately represented by the distribution used to generate the p-values, especially if it is much less than 0.05.
\par\vspace{0.6 cm}
As long as all of your cells had reasonable counts, say 8 or more, you can ignore any warning messages about the approximation.
\par\vspace{0.6 cm}
In this example, the $p$-value is well above 0.05, so we conclude that a N(17.98,2.94) distribution is a reasonable model for this data.  
\end{document}
========================
x4
========================
\documentclass{article}

\begin{document}
This R program is a sample of how to analyze one of the random vectors from the sample data
\par\vspace{0.6 cm}
When faced with a programming task, it is almost always easier to start with something that resembles the program you are writing and modify it.
\par\vspace{0.6 cm}
The purpose this example is to minimize the programming, possibly at the expense of efficiency setting up and running the program.  You may think of it as a starting point, and you are free to add any enhancements you like.
\par\vspace{0.6 cm}
The scenario is that we are starting with a data set in the form of a comma separated  file named \texttt{Project1\_test.csv}.  There are four columns in the file, and each represents a sample from a different unknown distribution.
\par\vspace{0.6 cm}
The file is stored on github.  The following code uses the RCurl and foreign packages to access it and read it into an R data frame called \texttt{df}:
<<>>=
library(RCurl)
library(foreign)
#d=getURL("https://raw.githubusercontent.com/MTH395-Fall2014/Project1/master/Project1_test.csv")
df=read.csv("/Users/astarring/Downloads/Project1-master/A_Starring.csv")
@
\par\vspace{0.6 cm}
The next step is to examine the data.  First we want to find out what the data frame looks like, so we'll use the \texttt{str()} function:
<<>>=
str(df)
@
\par\vspace{0.6 cm}
From this output, we see that \texttt{df} has 5 columns, named $X$ and $x1-x4$.  The $X$ column is a row number, and the four columns $x1-x4$ represent four random samples from our unknown distributions.
\par\vspace{0.6 cm}
While not mandatory, we will use the \texttt{attach()} function so that we can just reference the columns of \texttt{df} by their names.  Otherwise, if we just reference $x1$, R will not see it:
<<>>=
x4[1:20]
@
\par\vspace{0.6 cm}
We have two options.  We can use a two-level name \texttt{df\$x1},
<<>>=
df$x4[1:20]
@
\par\vspace{0.6 cm}
Alternatively, we can use \texttt{attach}, which makes the internal column names of the data frame visible:
<<>>=
attach(df)
x4[1:20]
@
\par\vspace{0.6 cm}
The first thing we might look at are the summary statistics for this column:
<<>>=
summary(x4)
@
\par\vspace{0.6 cm}
We might also want to see a histogram:
<<>>=
hist(x4)
@
\par\vspace{0.6 cm}
At this point, we might make a judgement that this data could be normal.  The distribution appears to be symmetric (the mean and median are the same), and it is centered at 18.  To fully specify the normal distribution, we need the standard deviation (or variance) in addition to the mean:
<<>>=
mean(x4)
sd(x4)
@
\par\vspace{0.6 cm}
The Method of Moments is trivial in this case because 
\[
E(X)=\mu \quad\mbox{and}\quad V(x)=\sigma^2
\]
\par\vspace{0.6 cm}
We'll use the standard deviation $\sigma$, which is the square root of the variance, because that is what we specify in the \texttt{norm} functions in R.  So our candidate for the distribution of \texttt{x1} might be $N(17.98,2.94).  We can now proceed to the goodness of fit test.
\par\vspace{0.6 cm}
First a bit of guesswork.  We want to partition the sample values into between 10 and 20 ranges, with the endpoints chosen so that there will be at least 8 or so data values in each bin. 
\par\vspace{0.6 cm}
The simplest way to do this is to manually assign the cutoff values to an array, as follows:
<<>>=
z<-c(-1000,5,10,15,20,25,30,1000)
@
\par\vspace{0.6 cm}
The first range in the partition will be -1000 to 12, the second will be 12-13, the third 13-14, and so on.  The large values at either end of the ranges are chosen to include any outliers in the data.
\par\vspace{0.6 cm}
It will usually take a couple of iterations to get the cutoff points that produce a partition having enough data values in each cell.  The number of cells is the number of elements in x, minus 1
<<>>=
ncells<-length(z)-1
z                                # To see if it worked, list the array z
@
\par\vspace{0.6 cm}
Now assign the column of interest to the name 'x'.  We don't usually have to worry about whether variables are arrays or not, R will usually do the right thing.
<<>>=
x<-x4
@
\par\vspace{0.6 cm}
We'll allocate an array of 15 values called ct for the counts in each range.  Note that the 16 elements of the z array define 15 ranges because each range is the set of values between an upper and lower limit. Use the rep (repeat) function to create an array with ncells values of zero, and store them in ct:
<<>>=
ct<-rep(0,ncells)
ct
@
\par\vspace{0.6 cm}
Now use a loop (actually a loop inside a loop) to find the counts in each range
<<>>=
for(j in 1:2000){
    for(i in 1:ncells){
        if((z[[i+1]]>=x[[j]]) & (z[[i]]<x[[j]])) ct[[i]]<-ct[[i]]+1
                  }
                }
#
# Display the results
#
ct

# Compute the sample mean and standard deviation 
# 
k=mean(x)
k

@
\par\vspace{0.6 cm}
Now construct an array p of proabilities for the ranges defined in the x array, using the candidate distribution to compute the probabilities.
<<>>=
p<-rep(0,ncells)
p
@
\par\vspace{0.6 cm}
Use another loop to construct the probabilities.  The value in each element is the probability that a normal random variable with mean mu and standard deviation sigma falls between the lower and upper values.
\par\vspace{0.6 cm}
That probability is computed as:  pnorm(upper\_value,mu,sigma)-pnorm(lower\_value,mu,sigma)
<<>>=
for(i in 1:ncells){
    p[[i]]<-pchisq(z[[i+1]],k)-pchisq(z[[i]],k)
    }
p
@
\par\vspace{0.6 cm}
Finally we are ready to call the chisq.test() procedure using ct and p as input:
<<>>=
chisq.test(ct,p=p,rescale.p=TRUE)
@
\par\vspace{0.6 cm}
If the p-value is greater than 0.05, you can assume that the distribution used to generate the p array is a reasonable model for this data.
\par\vspace{0.6 cm}
If the p-value is less than 0.05, you should interpret the results to mean it is unlikely that the data are adequately represented by the distribution used to generate the p-values, especially if it is much less than 0.05.
\par\vspace{0.6 cm}
As long as all of your cells had reasonable counts, say 8 or more, you can ignore any warning messages about the approximation.
\par\vspace{0.6 cm}
In this example, the $p$-value is well above 0.05, so we conclude that a N(17.98,2.94) distribution is a reasonable model for this data.  
\end{document}
