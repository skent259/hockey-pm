#' Set up STAN data (ppool_shots)
#' 
#' @param d Data frame with design matrix. Should be the output of
#'   `build_sog_model_data`
#' @param outcome A character specifying which outcome to use.  Options 
#' are `c("sh-go", "mi-bl")`.
#' @param teams Logical (default: `TRUE`) for whether to include team effects
#' @return A list of data elements to be used with 'ppool.stan' or 'ppool_nt.stan'
make_datalist_ppool_shots <- function(d, outcome = c("sh-go", "mi-bl"), teams = TRUE) {
  outcome <- match.arg(outcome, c("sh-go", "mi-bl"))
  
  ns = nrow(d)/2 #Number of shifts
  y <- d[,1] #Number of shots on goal by a given team in a given shift
  time = d[,2] #Shift lengths
  nt = 31 #Number of teams
  names = d@Dimnames[[2]]
  np = which(names == names[65])[2]-65 #Number of non-goalie players
  ng = ncol(d) - 2*np - 64 #Number of players
  
  if (teams) {
    #Get Stan's sparse representations of offensive team design matrix 
    dTO = d[,seq(3, 63, by=2)]
    spVecsTO = rstan::extract_sparse_parts(dTO)
    wto = spVecsTO$w
    vto = spVecsTO$v
    uto = spVecsTO$u
    nzt = length(wto)
    
    #Get Stan's sparse representations of defensive team design matrix 
    dTD = d[,seq(4, 64, by=2)]
    spVecsTD = rstan::extract_sparse_parts(dTD)
    wtd = spVecsTD$w
    vtd = spVecsTD$v
    utd = spVecsTD$u
  }
  
  #Get Stan's representation of offensive player design matrix
  dPO = d[,64+1:np]
  spVecsPO = rstan::extract_sparse_parts(dPO)
  wpo = spVecsPO$w
  vpo = spVecsPO$v
  upo = spVecsPO$u
  nzpo = length(wpo)
  
  #Get Stan's representation of defensive player design matrix (including goalies)
  dPD = d[,64+np+1:(np+ng)]
  spVecsPD = rstan::extract_sparse_parts(dPD)
  wpd = spVecsPD$w
  vpd = spVecsPD$v
  upd = spVecsPD$u
  nzpd = length(wpd)
  
  meanint = switch( # Based on simulation
    outcome,
    "sh-go" = -5, 
    "mi-bl" = -5.5
  )
  sigmaint = 1
  sigmat <- .5
  s <-  7.5
  r <- 0.5
  
  datalist <- list(
    ns=ns, y=y, time=time, np=np, ng=ng, 
    wpo=wpo, vpo=vpo, upo=upo, nzpo=nzpo, 
    wpd=wpd, vpd=vpd, upd=upd, nzpd=nzpd, 
    meanint=meanint, sigmaint=sigmaint, 
    s=s, r=r
  )  
  if (teams) {
    datalist <- c(datalist, list(
      nt=nt,
      wto=wto, vto=vto, uto=uto, nzt=nzt, 
      wtd = wtd, vtd=vtd, utd=utd, 
      sigmat=sigmat
    ))
  } 
  datalist
}
