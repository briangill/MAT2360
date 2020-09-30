#Helper function to test if x2 > x1 (and flip if not)
#Called in all the functions (except quantile functions) 
check_x2<-function(x1,x2){
  if(!is.null(x2)){if(x2<x1){
    temp<-x2
    x2<-x1
    x1<-temp
  }}
  return(c(x1,x2)) 
}

#Helper function to find min and max values for graph
#Used in all the functions, continuous and discrete
#Determine xmin and xmax for graph using: .0001 and .9999 quantile (for discrete) or .001 and .999 quantiles (for continuous)
#Or extends plot to include x1 and x2 in graph if necessary (if inputted values are extreme for the distribution)
# ... allows for calling function to pass unspecified number and type of parameters, i.e. the distinctive parameters that define the distribution
min_max<-function(FUN,type,x1,x2,...){
  switch(type,
         disc={
           xmin=floor(min(FUN(.0001,...),x1))
           xmax=ceiling(max(FUN(.9999,...),x1,x2,na.rm=T))
         },
         cont={
           xmin=min(FUN(.001,...),x1-(FUN(.999,...)-x1)/5)
           xmax=max(FUN(.999,...),x1+(x1-FUN(.001,...))/5,x2+(x2-FUN(.001,...))/5,na.rm=T)
         }
         )
  return(c(xmin,xmax))
}

#Discrete plot helper function
#Calculates desired probability and constructs the graph (this function does the bulk of the processing)
#Takes all original input and verified input from min_max and check_x2
# ... allows for calling function to pass unspecified number and type of parameters, i.e. the distinctive parameters that define the distribution
discPlot<-function(lims,input,type,sig,col,FUN1,FUN2,...){
  #Calculate probability depending on type desired
  switch(type,
         single = {
           barplot(FUN1(lims[1]:lims[2],...),space=0,names.arg=lims[1]:lims[2],ylab="Probability",xlab="X")
           barplot(c(rep(0,input[1]-lims[1]),FUN1(input[1],...),rep(0,lims[2]-input[1])),space=0,add=TRUE,col=col)
           prob<-signif(FUN1(input[1],...),sig)
           probtext<-paste("P(X = ",input[1],") = ",prob,sep="")
         },
         left = {
           barplot(FUN1(lims[1]:lims[2],...),space=0,names.arg=lims[1]:lims[2],ylab="Probability",xlab="X")
           barplot(c(FUN1(lims[1]:input[1],...),rep(0,lims[2]-input[1])),space=0,add=TRUE,col=col)
           prob<-signif(FUN2(input[1],...),sig)
           probtext<-paste("P(X <= ",input[1],") = ",prob,sep="")
         },
         right = {
           barplot(FUN1(lims[1]:lims[2],...),space=0,names.arg=lims[1]:lims[2],ylab="Probability",xlab="X")
           barplot(c(rep(0,ceiling(input[1])-lims[1]),FUN1(ceiling(input[1]):lims[2],...)),space=0,add=TRUE,col=col)
           prob<-signif(1-FUN2(ceiling(input[1])-1,...),sig)
           probtext<-paste("P(X >= ",input[1],") = ",prob,sep="")
         },
         middle = {
           barplot(FUN1(lims[1]:lims[2],...),space=0,names.arg=lims[1]:lims[2],ylab="Probability",xlab="X")
           barplot(c(rep(0,ceiling(input[1])-lims[1]),FUN1(ceiling(input[1]):floor(input[2]),...),rep(0,lims[2]-floor(input[2]))),space=0,add=TRUE,col=col)
           prob<-signif(FUN2(floor(input[2]),...)-FUN2(ceiling(input[1])-1,...),sig)
           probtext<-paste("P(",input[1]," <= X <= ",input[2],") = ",prob,sep="")
         },
         tails = {
           barplot(FUN1(lims[1]:lims[2],...),space=0,names.arg=lims[1]:lims[2],ylab="Probability",xlab="X")
           barplot(c(FUN1(lims[1]:floor(input[1]),...),rep(0,ceiling(input[2])-floor(input[1])-1),FUN1(ceiling(input[2]):lims[2],...)),space=0,add=TRUE,col="cyan")
           prob<-signif(FUN2(floor(input[1]),...)+1-FUN2(ceiling(input[2])-1,...),sig)
           probtext<-paste("P(X <= ",input[1]," OR X >= ",input[2],") = ",prob,sep="")
         })
  return(c(probtext,prob)) #Return probability and plot
}

#Continuous plot helper function
#Calculates desired probability and constructs the graph (this function does the bulk of the processing)
#Takes all original input and verified input from min_max and check_x2
# ... allows for calling function to pass unspecified number and type of parameters, i.e. the distinctive parameters that define the distribution
contPlot<-function(lims,input,type,sig,col,xlab,FUN1,FUN2,...){
  switch(type,
         #Calculate probability depending on type desired
         left = {
           curve(FUN1(x,...),xlim=c(lims[1],lims[2]),xlab=xlab,ylab="Density") 
           x.vert<-c(lims[1],seq(lims[1],input[1],length.out=100),input[1])
           y.vert<-c(0,FUN1(seq(lims[1],input[1],length.out=100),...),0)
           polygon(x.vert,y.vert,col=col)
           prob<-signif(FUN2(input[1],...),sig)
           probtext<-paste("P(X <= ",input[1],") = ",prob,sep="")
         },
         right = {
           curve(FUN1(x,...),xlim=c(lims[1],lims[2]),xlab=xlab,ylab="Density") 
           x.vert<-c(input[1],seq(input[1],lims[2],length.out=100),lims[2])
           y.vert<-c(0,FUN1(seq(input[1],lims[2],length.out=100),...),0)
           polygon(x.vert,y.vert,col=col)
           prob<-signif(1-FUN2(input[1],...),sig)
           probtext<-paste("P(X >= ",input[1],") = ",prob,sep="")
         },
         middle = {
             curve(FUN1(x,...),xlim=c(lims[1],lims[2]),xlab=xlab,ylab="Density") 
             x.vert<-c(input[1],seq(input[1],input[2],length.out=100),input[2])
             y.vert<-c(0,FUN1(seq(input[1],input[2],length.out=100),...),0)
             polygon(x.vert,y.vert,col=col)
             prob<-signif(FUN2(input[2],...)-FUN2(input[1],...),sig)
             probtext<-paste("P(",input[1]," <= X <= ",input[2],") = ",prob,sep="")
         },
         tails = {
             curve(FUN1(x,...),xlim=c(lims[1],lims[2]),xlab=xlab,ylab="Density") 
             x.vert<-c(lims[1],seq(lims[1],input[1],length.out=100),input[1])
             y.vert<-c(0,FUN1(seq(lims[1],input[1],length.out=100),...),0)
             polygon(x.vert,y.vert,col=col)
             x.vert<-c(input[2],seq(input[2],lims[2],length.out=100),lims[2])
             y.vert<-c(0,FUN1(seq(input[2],lims[2],length.out=100),...),0)
             polygon(x.vert,y.vert,col=col)
             prob<-signif(1-(FUN2(input[2],...)-FUN2(input[1],...)),sig)
             probtext<-paste("P(X <= ",input[1]," OR X >= ",input[2],") = ",prob,sep="")
         })
  return(c(probtext,prob)) #Return probability and plot
}

#***DISCRETE DISTRIBUTIONS***

#Compute hypergeometric probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameters N (population size), M (successes in population), and n (sample size)
#Specify desired probability using type=("single", "left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
ghypgeom<-function(x1,x2=NULL,N=NULL,M=NULL,n=NULL,k=NULL,type=c("single","left","right","middle","tails"),
                   xlab="X",col="cyan",sig=4){
  #If desired probability is not specified and only one x-value is used, set default to "single"
  #If desired probability is not specified and two x-values are used, set default to "middle"
  if(length(type)>1) ifelse(is.null(x2),type<-"single",type<-"middle")
  
  #check which parameters used (use Gill notation in function) (k is R's notation)
  if(is.null(n)) n<-k
  
  type<-match.arg(type)
  
  #QUIT IF WANT MIDDLE OR TAILS W/ NULL x2 HERE
  if(is.null(x2) && (type=="middle" || type=="tails")){
    print("Must specify 2 values of X to use tails or middle")
    return()
  }
  
  #Make sure that x1 < x2
  input<-check_x2(x1,x2)
  
  #Determine xmin and xmax for graph
  #Store in vector (lims) to access/use later
  lims<-min_max(FUN=qhyper,type='disc',input[1],input[2],M,N-M,n)
  
  #Graph distribution and print probability
 returned<-c(discPlot(lims,input,type,sig,col,FUN1=dhyper,FUN2=phyper,M,N-M,n))
 print(returned[1],quote=F)
 title(paste("Hypergeometric Distribution, N = ",N,", M = ",M,", n = ",n,sep=""),sub=returned[1])
 invisible(returned[2])
}  

#Compute binomial probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameters n (sample size) and p (probability of success on each trial)
#Specify desired probability using type=("single", "left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
gbinom<-function(x1,x2=NULL,n=NULL,p=NULL,size=NULL,prob=NULL,type=c("single","left","right","middle","tails"),
                 xlab="X",col="cyan",sig=4){
  #If desired probability is not specified and only one x-value is used, set default to "single"
  #If desired probability is not specified and two x-values are used, set default to "middle"
  if(length(type)>1) ifelse(is.null(x2),type<-"single",type<-"middle")
  
  #check which parameters used (use Gill notation in function) (size and prob are R notation)
  if(is.null(n)) n<-size
  if(is.null(p)) p<-prob
  
  type<-match.arg(type)
  
  #QUIT IF WANT MIDDLE OR TAILS W/ NULL x2 HERE
  if(is.null(x2) && (type=="middle" || type=="tails")){
    print("Must specify 2 values of X to use tails or middle")
    return()
  }
  
  #Make sure that x1 < x2
  #Store results in vector to access later
  input<-check_x2(x1,x2)
  
  #Determine xmin and xmax for graph using .0001 and .9999 quantile 
  #or extend to include x1 and x2 in graph if necessary
  lims<-min_max(FUN=qbinom,type='disc',input[1],input[2],n,p)

  
  #Graph distribution and print probability
  returned<-c(discPlot(lims,input,type,sig,col,FUN1=dbinom,FUN2=pbinom,n,p))
  print(returned[1],quote=F)
  title(paste("Binomial Distribution, n = ",n,", p = ",p,sep=""),sub=returned[1])
  invisible(returned[2])
}  

#Compute negative binomial probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameters s (number of successes) and p (probability of success on each trial)
#Specify desired probability using type=("single", "left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
gnbinom<-function(x1,x2=NULL,s=NULL,size=NULL,p=NULL,prob=NULL,type=c("single","left","right","middle","tails"),
                  xlab="X",col="cyan",sig=4,graph.title=NULL){
  #If desired probability is not specified and only one x-value is used, set default to "single"
  #If desired probability is not specified and two x-values are used, set default to "middle"
  if(length(type)>1) ifelse(is.null(x2),type<-"single",type<-"middle")
  
  #check which parameters used (use Gill notation in function) (prob, size are R notation)
  if(is.null(p)) p<-prob
  if(is.null(s)) s<-size
  
  type<-match.arg(type)
  
  #QUIT IF WANT MIDDLE OR TAILS W/ NULL x2 HERE
  if(is.null(x2) && (type=="middle" || type=="tails")){
    print("Must specify 2 values of X to use tails or middle")
    return()
  }

  #Make sure that x1 < x2
  #Store results in vector to access later
  input<-check_x2(x1,x2)

  #Determine xmin and xmax for graph using .0001 and .9999 quantile 
  #or extend to include x1 and x2 in graph if necessary
  lims<-min_max(FUN=qnbinom,type='disc',input[1],input[2],s,p)
  
  #Graph distribution and print probability
  returned<-c(discPlot(lims,input,type,sig,col,FUN1=dnbinom,FUN2=pnbinom,s,p))
  print(returned[1],quote=F)
  if(is.null(graph.title)) graph.title<-paste("Negative Binomial Distribution, s = ",s,", p = ",p,sep="")
  title(graph.title,sub=returned[1])
  invisible(returned[2])
}  

#Compute geometric probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameter p (probability of success on each trial)
#Specify desired probability using type=("single", "left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
ggeom<-function(x1,x2=NULL,p=NULL,prob=NULL,type=c("single","left","right","middle","tails"),
                xlab="X",col="cyan",sig=4){
  #check which parameters used (use Gill notation in function) (prob is R notation)
  if(is.null(p)) p<-prob
  graph.title<-paste("Geometric Distribution, p = ",p,sep="")
  #Geometric is special case of negative binomial distribution with s=1:
  gnbinom(x1,x2,s=1,p=p,type=type,xlab=xlab,col=col,sig=sig,graph.title=graph.title) 
}

#Compute poisson probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameter lambda
#Specify desired probability using type=("single", "left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
gpois<-function(x1,x2=NULL,lambda=NULL,type=c("single","left","right","middle","tails"),
                   xlab="X",col="cyan",sig=4){
  #If desired probability is not specified and only one x-value is used, set default to "single"
  #If desired probability is not specified and two x-values are used, set default to "middle"
  if(length(type)>1) ifelse(is.null(x2),type<-"single",type<-"middle")
  
  type<-match.arg(type)
  
  #QUIT IF WANT MIDDLE OR TAILS W/ NULL x2 HERE
  if(is.null(x2) && (type=="middle" || type=="tails")){
    print("Must specify 2 values of X to use tails or middle")
    return()
  }
  
  #Make sure that x1 < x2
  #Store in vector for later use
  input<-check_x2(x1,x2)
  
  #Determine xmin and xmax for graph
  #Store in vector (lims) to access/use later
  lims<-min_max(FUN=qpois,type='disc',input[1],input[2],lambda)
  
  #Graph distribution and print probability
  returned<-c(discPlot(lims,input,type,sig,col,FUN1=dpois,FUN2=ppois,lambda))
  print(returned[1],quote=F)
  title(paste("Poisson Distribution, lambda = ",lambda,sep=""),sub=returned[1])
  invisible(returned[2])
}

#***CONTINUOUS DISTRIBUTIONS***

#Compute normal probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameters mean and sd (if not specified, default is standard normal: mean=0, sd=1)
#Specify desired probability using type=("left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
gnorm<-function(x1,x2=NULL,mean=0,sd=1,type=c("left","right","middle","tails"),
                xlab="X",col="cyan",sig=4){
  #If desired probability is not specified and only one x-value is used, set default to "left"
  #If desired probability is not specified and two x-values are used, set default to "middle"
  if(length(type)>1) ifelse(is.null(x2),type<-"left",type<-"middle")
  
  type<-match.arg(type)
  
  #QUIT IF WANT MIDDLE OR TAILS W/ NULL x2 HERE
  if(is.null(x2) && (type=="middle" || type=="tails")){
    print("Must specify 2 values of X to use tails or middle")
    return()
  }
  
  #Make sure that x1 < x2
  #Store in vector for later use
  input<-check_x2(x1,x2)
  
  #Determine xmin and lims[2] for graph using .001 and .999 quantile 
  #or extend to include x1 and x2 in graph if necessary
  lims<-min_max(FUN=qnorm,type='cont',input[1],input[2],mean,sd)

  #Graph distribution and print probability
  returned<-c(contPlot(lims,input,type,sig,col,xlab,FUN1=dnorm,FUN2=pnorm,mean,sd))
  print(returned[1],quote=F)
  title(paste("Normal Distribution, mu = ",mean,", sigma = ",sd,sep=""),sub=returned[1])
  invisible(returned[2])
}  

#Compute gamma probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameters rate and shape (or lambda and alpha)
#Specify desired probability using type=("left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
ggamma<-function(x1,x2=NULL,rate=NULL,shape=NULL,lambda=NULL,alpha=NULL,
                 type=c("left","right","middle","tails"),
                 xlab="X",col="cyan",sig=4,
                 graph.title=NULL){
  #If desired probability is not specified and only one x-value is used, set default to "left"
  #If desired probability is not specified and two x-values are used, set default to "middle"
  if(length(type)>1) ifelse(is.null(x2),type<-"left",type<-"middle")
  
  if(is.null(rate)) rate<-lambda
  if(is.null(shape)) shape<-alpha
  
  type<-match.arg(type)
  
  #QUIT IF WANT MIDDLE OR TAILS W/ NULL x2 HERE
  if(is.null(x2) && (type=="middle" || type=="tails")){
    print("Must specify 2 values of X to use tails or middle")
    return()
  }
  
  #Make sure that x1 < x2
  #Store in vector for later use
  input<-check_x2(x1,x2)

  #Determine xmin and xmax for graph using .00001 and .999 quantile 
  #or extend to include x1 and x2 in graph if necessary
  lims<-min_max(FUN=qgamma,type='cont',input[1],input[2],shape,rate)
  
  #Graph distribution and print probability
  returned<-c(contPlot(lims,input,type,sig,col,xlab,FUN1=dgamma,FUN2=pgamma,shape,rate))
  print(returned[1],quote=F)
  if(is.null(graph.title)) graph.title<-paste("Gamma Distribution, lambda = ",rate,", alpha = ",shape,sep="")
  title(graph.title,sub=returned[1])
  invisible(returned[2])
}  

#Compute exponential probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameter rate (or lambda)
#Specify desired probability using type=("left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
gexp<-function(x1,x2=NULL,rate=NULL,lambda=NULL,
               type=c("left","right","middle","tails"),
               xlab="X",col="cyan",sig=4){
  if(is.null(rate)) rate<-lambda
  graph.title<-paste("Exponential Distribution, lambda = ",rate,sep="")
  #Exponential is a special case of gamma with shape=1:
  ggamma(x1,x2,rate,shape=1,type=type,xlab=xlab,col=col,sig=sig,graph.title=graph.title)
}

#Compute chi-square probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameter df (degrees of freedom)
#Specify desired probability using type=("left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
gchisq<-function(x1,x2=NULL,df=NULL,
                 type=c("left","right","middle","tails"),
                 xlab="X",col="cyan",sig=4){
  graph.title<-paste("Chi-square Distribution, df = ",df,sep="")
  #Chi-square is a special case of gamma with rate=1/2 and shape=df/2
  ggamma(x1,x2,rate=1/2,shape=df/2,type=type,xlab=xlab,col=col,sig=sig,graph.title=graph.title)
}

#Compute uniform probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameters min and max (default are min=0, max=1)
#Specify desired probability using type=("left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
gunif<-function(x1,x2=NULL,min=0,max=1,
                type=c("left","right","middle","tails"),
                xlab="X",col="cyan",sig=4){
  #If desired probability is not specified and only one x-value is used, set default to "left"
  #If desired probability is not specified and two x-values are used, set default to "middle"
  if(length(type)>1) ifelse(is.null(x2),type<-"left",type<-"middle")
  
  type<-match.arg(type)
  
  #QUIT IF WANT MIDDLE OR TAILS W/ NULL x2 HERE
  if(is.null(x2) && (type=="middle" || type=="tails")){
    print("Must specify 2 values of X to use tails or middle")
    return()
  }
  
  #Make sure that x1 < x2
  #Store in vector for later use
  input<-check_x2(x1,x2)
  
  #Determine xmin and xmax (lims) for graph 
  #Simple geometry of uniform distribution allows for easier calculations without quantiles
  lims=c(min-(max-min)/10, max+(max-min)/10)
  
  #Graph distribution and print probability
  returned<-c(contPlot(lims,input,type,sig,col,xlab,FUN1=dunif,FUN2=punif,min,max))
  print(returned[1],quote=F)
  title(paste("Uniform Distribution, min = ",min,", max = ",max,sep=""),sub=returned[1])
  invisible(returned[2])
}  

#Compute t probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameter df (degrees of freedom)
#Specify desired probability using type=("left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
gt<-function(x1,x2=NULL,df=NULL,
             type=c("left","right","middle","tails"),
             xlab="t",col="cyan",sig=4){
  #If desired probability is not specified and only one x-value is used, set default to "left"
  #If desired probability is not specified and two x-values are used, set default to "middle"
  if(length(type)>1) ifelse(is.null(x2),type<-"left",type<-"middle")
  
  type<-match.arg(type)
  
  #QUIT IF WANT MIDDLE OR TAILS W/ NULL x2 HERE
  if(is.null(x2) && (type=="middle" || type=="tails")){
    print("Must specify 2 values of X to use tails or middle")
    return()
  }
  
  #Make sure that x1 < x2
  #Store in vector for later use
  input<-check_x2(x1,x2)
  
  #Determine xmin and xmax for graph using .001 and .999 quantile 
  #or extend to include x1 and x2 in graph if necessary
  lims<-min_max(FUN=qt,type='cont',input[1],input[2],df)

  #Graph distribution and print probability
  returned<-c(contPlot(lims,input,type,sig,col,xlab,FUN1=dt,FUN2=pt,df))
  print(returned[1],quote=F)
  title(paste("T Distribution, df = ",df,sep=""),sub=returned[1])
  invisible(returned[2])
}  

#Compute F probabilities and graph the distribution
#User must specify one or two values of x (x1 and x2)
#Also must specify parameters df1 and df2 (degrees of freedom of each respective chi-square distribution)
#Specify desired probability using type=("left", "right", "middle", or "tails")
#Can also specify label for x-axis and color for shading
gf<-function(x1,x2=NULL,df1=NULL,df2=NULL,
                 type=c("left","right","middle","tails"),
                 xlab="X",col="cyan",sig=4){
  #If desired probability is not specified and only one x-value is used, set default to "left"
  #If desired probability is not specified and two x-values are used, set default to "middle"
  if(length(type)>1) ifelse(is.null(x2),type<-"left",type<-"middle")
  
  type<-match.arg(type)
  
  #QUIT IF WANT MIDDLE OR TAILS W/ NULL x2 HERE
  if(is.null(x2) && (type=="middle" || type=="tails")){
    print("Must specify 2 values of X to use tails or middle")
    return()
  }
  
  #Make sure that x1 < x2
  #Store in vector for later use
  input<-check_x2(x1,x2)
  
  #Determine xmin and xmax for graph using .00001 and .999 quantile 
  #or extend to include x1 and x2 in graph if necessary
  lims<-min_max(FUN=qf,type='cont',input[1],input[2],df1,df2)
  
  #Graph distribution and print probability
  returned<-c(contPlot(lims,input,type,sig,col,xlab,FUN1=df,FUN2=pf,df1,df2))
  print(returned[1],quote=F)
  title(paste("F Distribution, df1 = ",df1,", df2 = ",df2,sep=""),sub=returned[1])
  invisible(returned[2])
}  

#***QUANTILE FUNCTIONS (for continuous distributions only)***

#Compute normal probabilities and graph the distribution
#User must specify tail probability desired
#Also must specify parameters mean and sd (if not specified, default is standard normal: mean=0, sd=1)
#Specify desired probability using type=("left", "right", or "tails")
#Can also specify label for x-axis and color for shading
gnorm.q<-function(q,mean=0,sd=1,type=c("left","right","tails"),
                  xlab="X",col="cyan",sig=4){
  
  if(length(type)>1) type<-"left"
  
  type<-match.arg(type)
  
  input<-c(NULL,NULL)
  if(type=="left") input[1]<-qnorm(q,mean,sd)
  if(type=="right") input[1]<-qnorm(1-q,mean,sd)
  if(type=="tails") {
    input[1]<-qnorm(q/2,mean,sd)
    input[2]<-qnorm(1-q/2,mean,sd)
  }
  
  #Determine xmin and lims[2] for graph using .001 and .999 quantile 
  #or extend to include input[1] and x2 in graph if necessary
  lims<-min_max(FUN=qnorm,type='cont',input[1],input[2],mean,sd)

  #Graph distribution and print probability
  returned<-c(contPlot(lims,input,type,sig,col,xlab,FUN1=dnorm,FUN2=pnorm,mean,sd))
  print(returned[1],quote=F)
  title(paste("Normal Distribution, mu = ",mean,", sigma = ",sd,sep=""),sub=returned[1])
  invisible(returned[2])
}  

#Compute gamma probabilities and graph the distribution
#User must specify tail probability desired
#Also must specify parameters rate and shape (or lambda and alpha)
#Specify desired probability using type=("left", "right", or "tails")
#Can also specify label for x-axis and color for shading
ggamma.q<-function(q,rate=NULL,shape=NULL,lambda=NULL,alpha=NULL,type=c("left","right","tails"),
                  xlab="X",col="cyan",sig=4,graph.title=NULL){
  
  if(length(type)>1) type<-"left"
  if(is.null(rate)) rate<-lambda
  if(is.null(shape)) shape<-alpha
  
  type<-match.arg(type)
  
  input<-c(NULL,NULL)
  if(type=="left") input[1]<-qgamma(q,shape,rate)
  if(type=="right") input[1]<-qgamma(1-q,shape,rate)
  if(type=="tails") {
    input[1]<-qgamma(q/2,shape,rate)
    input[2]<-qgamma(1-q/2,shape,rate)
  }
  
  #Determine xmin and lims[2] for graph using .001 and .999 quantile 
  #or extend to include input[1] and x2 in graph if necessary
  lims<-min_max(FUN=qgamma,type='cont',input[1],input[2],shape,rate)
  
  #Graph distribution and print probability
  returned<-c(contPlot(lims,input,type,sig,col,xlab,FUN1=dgamma,FUN2=pgamma,shape,rate))
  print(returned[1],quote=F)
  if(is.null(graph.title)) graph.title<-paste("Gamma Distribution, lambda = ",rate,", alpha = ",shape,sep="")
  title(graph.title,sub=returned[1])
  invisible(returned[2])
}

#Compute exponential probabilities and graph the distribution
#User must specify tail probability desired
#Also must specify parameter rate (or lambda)
#Specify desired probability using type=("left", "right", or "tails")
#Can also specify label for x-axis and color for shading
gexp.q<-function(q,rate=NULL,lambda=NULL,
               type=c("left","right","tails"),
               xlab="X",col="cyan",sig=4){
  if(is.null(rate)) rate<-lambda
  graph.title<-paste("Exponential Distribution, lambda = ",rate,sep="")
  ggamma.q(q,rate,shape=1,type=type,xlab=xlab,col=col,sig=sig,graph.title=graph.title)
}

#Compute chi-square probabilities and graph the distribution
#User must specify tail probability desired
#Also must specify parameter df (degrees of freedom)
#Specify desired probability using type=("left", "right", or "tails")
#Can also specify label for x-axis and color for shading
gchisq.q<-function(q,df=NULL,
                 type=c("left","right","tails"),
                 xlab="X",col="cyan",sig=4){
  graph.title<-paste("Chi-square Distribution, df = ",df,sep="")
  ggamma.q(q,rate=1/2,shape=df/2,type=type,xlab=xlab,col=col,sig=sig,graph.title=graph.title)
}

#Compute uniform probabilities and graph the distribution
#User must specify tail probability desired
#Also must specify parameters min and max (default are min=0, max=1)
#Specify desired probability using type=("left", "right", or "tails")
#Can also specify label for x-axis and color for shading
gunif.q<-function(q,min=0,max=1,type=c("left","right","tails"),
                   xlab="X",col="cyan",sig=4){
  
  if(length(type)>1) type<-"left"
  
  type<-match.arg(type)
  
  input<-c(NULL,NULL)
  if(type=="left") input[1]<-qunif(q,min,max)
  if(type=="right") input[1]<-qunif(1-q,min,max)
  if(type=="tails") {
    input[1]<-qunif(q/2,min,max)
    input[2]<-qunif(1-q/2,min,max)
  }
  
  #Determine xmin and xmax (lims) for graph 
  lims=c(min-(max-min)/10, max+(max-min)/10)
  
  #Graph distribution and print probability
  returned<-c(contPlot(lims,input,type,sig,col,xlab,FUN1=dunif,FUN2=punif,min,max))
  print(returned[1],quote=F)
  title(paste("Uniform Distribution, min = ",min,", max = ",max,sep=""),sub=returned[1])
  invisible(returned[2])
}

#Compute t probabilities and graph the distribution
#User must specify tail probability desired
#Also must specify parameter df (degrees of freedom)
#Specify desired probability using type=("left", "right", or "tails")
#Can also specify label for x-axis and color for shading
gt.q<-function(q,df=NULL,type=c("left","right","tails"),
               xlab="t",col="cyan",sig=4){
  
  if(length(type)>1) type<-"left"
  
  type<-match.arg(type)
  
  input<-c(NULL,NULL)
  if(type=="left") input[1]<-qt(q,df)
  if(type=="right") input[1]<-qt(1-q,df)
  if(type=="tails") {
    input[1]<-qt(q/2,df)
    input[2]<-qt(1-q/2,df)
  }
  
  #Determine xmin and lims[2] for graph using .001 and .999 quantile 
  #or extend to include input[1] and x2 in graph if necessary
  lims<-min_max(FUN=qt,type='cont',input[1],input[2],df)
  
  #Graph distribution and print probability
  returned<-c(contPlot(lims,input,type,sig,col,xlab,FUN1=dt,FUN2=pt,df))
  print(returned[1],quote=F)
  title(paste("T Distribution, df = ",df,sep=""),sub=returned[1])
  invisible(returned[2])
}

#Compute F probabilities and graph the distribution
#User must specify tail probability desired
#Also must specify parameters df1 and df2 (degrees of freedom of each respective chi-square distribution)
#Specify desired probability using type=("left", "right", or "tails")
#Can also specify label for x-axis and color for shading
gf.q<-function(q,df1=NULL,df2=NULL,type=c("left","right","tails"),
                   xlab="X",col="cyan",sig=4){
  
  if(length(type)>1) type<-"left"
  
  type<-match.arg(type)
  
  input<-c(NULL,NULL)
  if(type=="left") input[1]<-qf(q,df1,df2)
  if(type=="right") input[1]<-qf(1-q,df1,df2)
  if(type=="tails") {
    input[1]<-qf(q/2,df1,df2)
    input[2]<-qf(1-q/2,df1,df2)
  }
  
  #Determine xmin and xmax for graph using .00001 and .999 quantile 
  #or extend to include x1 and x2 in graph if necessary
  lims<-min_max(FUN=qf,type='cont',input[1],input[2],df1,df2)
  
  #Graph distribution and print probability
  returned<-c(contPlot(lims,input,type,sig,col,xlab,FUN1=df,FUN2=pf,df1,df2))
  print(returned[1],quote=F)
  title(paste("F Distribution, df1 = ",df1,", df2 = ",df2,sep=""),sub=returned[1])
  invisible(returned[2])
}