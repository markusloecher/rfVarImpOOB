gini_OOB <- structure(function# compute Gini impurity for binary values only
  ### simple function to compute simple or penalized Gini impurity
  ### The "penalty" compares the class probabilities \code{pOut} with a reference estimate \code{pIn} 
  ### which would typically serve as a prediction (e.g. in a tree node).
  (
    gIn, ##<< Gini Inbag
    gOut, ##<< Gini Outbag
#    nNodeIn, ##<< sample size in node for inbag
#    nNodeOut, ##<< sample size in node for outbag 
    k = 2, ##<< exponent of penalty term: abs(pOut-pIn)^k
    kind = 1, ##<< kind of penalty
    w=2, ##<< weights, default is 2 if you pass just a single probability instead of the vector (p,1-p),
    correctBias = FALSE, ##<< multiply by n/(n-1) for sample variance correction!
    verbose=0 ##<< level of verbosity
  ) {
    
    
   if (kind==0) {#only OOB, i.e. only p1
     g = 2*gOut
   }  else if (kind==1){ 
    g = 2*gOut + abs(pOut-pIn)^k
  } else if (kind==2) {#
    g = gOut + gIn + abs(pOut-pIn)^k
  } else if (kind==3) {#symmetric in p1 and p2
    g = gOut + gIn + 0.5*abs(pOut-pIn)^k
  } else if (kind==4) {#only inbag !
    g = 2*gIn
  }

      
    return(g)
    ### simple or penalized Gini impurity
  }, ex = function(){
    #Test binary case:
    
    gini_OOB(0.5,0.5,kind=1)
    gini_OOB(0.9,0.1,kind=1)
    gini_OOB(0.1,0.9,kind=1)
    
    gini_OOB(0.5,0.5,kind=2)
    gini_OOB(0.9,0.1,kind=2)
    gini_OOB(0.1,0.9,kind=2)
    
    
    gini_OOB(0.5,0.5,kind=3)
    gini_OOB(0.9,0.1,kind=3)
    gini_OOB(0.1,0.9,kind=3)
    
  })
