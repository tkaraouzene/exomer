
strand <- function(call1,call2) {
  
  if (call1 < call2) {
    
    call_tmp <- call1
    call1 <- call2
    call2 <- call_tmp
    
  }
  
  if (call2 == 0) {
    
    if (call1 == 0) {
      strand <- "NS" 
    }
    
    else {
      strand <- "SS"
    }
    
  } else {
    
    strand <- "DS"
  } 
  
  strand
}

call <- function(call1,call2,cov_rate,cov_rate_high = .8, status) {
  
  value <- "0"
  
  if (call1 < call2) {
    
    call_tmp <- call1
    call1 <- call2
    call2 <- call_tmp
    
  }
  
  if (call2 == 0) {
    
    if (call1!=0) {
      
      if (call1==1) {
        value <- "HR"
      }
      else if (call1==2) {
        value <- "LGL"
      }
      else if (call1==3) {
        value <- "LGR"
      }
      else if (call1==4) {
        value <- "HET"
      }
      else if (call1==5) {
        value <- "HGL"
      }
      else if (call1==6) {
        value <- "HGR"
      }
      else if (call1==7) {
        value <- "HV"
      }
      # else {
      #   # die "FIXME: unknown call1 value: call1 returned by callGenoStrand!"
      # }
    }
  }  
  
  else {
    # we have sufficient coverage on both strands
    
    if ((call1 - call2) >= 3) {
      # this may not seem obvious, but trust me: it defines
      # the strong discordant zone
      value <- "discS"
    }
    else if ((call1 - call2) == 2) {
      # similarly this is exactly the weakly discordant zone
      value <- "discW"
    }
    else if ((call2==1) & (call1 <= 2)) {
      value <- "HR"
    }
    else if ((call2 >= 6) & (call1==7)) {
      value <- "HV"
    }
    else if ( ((call2==3) & (call1==4)) |
              ((call2==4) & (call1<=5)) ) {
      value <- "HET"
    }
    else if ((call2==2) & (call1==2)) {
      value <- "NCLGL"
    }
    # all remaining zones should be NoCall, but I define them
    # explicitely for safety
    else if ( ((call2>=2) & (call1==3)) |
              ((call2>=5) & (call1<=6)) ) {
      value <- "NC"
    }
    else {
      # die "FIXME: some zones remain unexplored, this should never happen!"
    }
  }
  
  if ((status != "NS") & (cov_rate < cov_rate_high)) {
    if (value == "HV") {
      value <- "HVPC"
    }
    else {
      value <- "PC"
    }
  }
  
  value
}

strand_call <- function(cov, var, ref,
                        min_cov = 10, max_homo_r = .2, low_grey = .3,
                        min_het = .4, max_het = .75, high_grey = .8,
                        min_homo_v = .85) {
  
  if (cov >= min_cov) {
    
    rat_var <- var / cov
    call <- 1
    if (rat_var <= max_homo_r) {
      call <- 1
    }
    else if (rat_var <= low_grey) {
      call <- 2
    }
    else if (rat_var < min_het) {
      call <- 3
    }
    else if (rat_var <= max_het) {
      call <- 4
    }
    
    else if (rat_var < high_grey) {
      call <- 5
    }
    else if (rat_var < min_homo_v) {
      call <- 6
    }
    else {
      call <- 7
    }
    
  } else {
    call <- 0
  }
  
  call
  
}
