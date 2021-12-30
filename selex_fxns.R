# selectivity functions

#' Calculate values for length logistic selectivity
#'
#' @param len A vector of lengths or ages
#' @param a The inflection point
#' @param b The 95% width
#' @return The selectivity curve as a vector
logistic1 <- function(len, a, b) {
  neglog19 <- -1 * log(19)
  denom <- 1. + exp(neglog19 * (len - a) / b)
  sel <- 1 / denom
  return(sel)
}

#' Calculate values for double normal selectivity
#'
#' @param x A vector of lengths or ages
#' @param a The peak
#' @param b The top
#' @param c The ascending width
#' @param d The Descending width
#' @param e The initial value
#' @param f The final value
#' @param use_e_999 Is -999 used for the initial value?
#' @param use_f_999 Is -999 used for the final value?
#' @return The double normal selectivity curve given the parameters as a vector
doubleNorm24 <- function(x, a, b, c, d, e, f, use_e_999, use_f_999) {
  # TODO: check if function not handling f < -1000 correctly (and -999 vals)
  if (use_e_999) {
    e <- -999
  }
  if (use_f_999) {
    f <- -999
  }
  if (e == 0) { # Avoid errors on the bounds
    e <- 1 - 0.999955 # an input that results in approx -10
  }
  if (e == 1) {
    e <- 0.999955 # an input that results in approx 10
  }
  if (e > 0) {
    e <- log(e / (1 - e)) # transform input to logit
  }

  if (f == 0) { # Avoid errors on the bounds
    f <- 1 - 0.999955 # an input that results in approx -10
  }
  if (f == 1) {
    f <- 0.999955 # an input that results in approx 10
  }
  if (f > 0) {
    f <- log(f / (1 - f)) # transform input to logit
  }
  sel <- rep(NA, length(x))
  startbin <- 1
  peak <- a
  upselex <- exp(c)
  downselex <- exp(d)
  final <- f
  if (e < -1000) {
    j1 <- -1001 - round(e)
    sel[1:j1] <- 1e-06
  }
  if (e >= -1000) {
    j1 <- startbin - 1
    if (e > -999) {
      point1 <- 1 / (1 + exp(-e))
      t1min <- exp(-(x[startbin] - peak)^2 / upselex)
    }
  }
  if (f < -1000) {
    j2 <- -1000 - round(f)
  }
  if (f >= -1000) {
    j2 <- length(x)
  }
  bin_width <- x[2] - x[1]
  peak2 <- peak + bin_width + (0.99 * x[j2] - peak - bin_width) / (1 +
    exp(-b))
  if (f > -999) {
    point2 <- 1 / (1 + exp(-final))
    t2min <- exp(-(x[j2] - peak2)^2 / downselex)
  }
  t1 <- x - peak
  t2 <- x - peak2
  join1 <- 1 / (1 + exp(-(20 / (1 + abs(t1))) * t1))
  join2 <- 1 / (1 + exp(-(20 / (1 + abs(t2))) * t2))
  if (e > -999) {
    asc <- point1 + (1 - point1) * (exp(-t1^2 / upselex) -
      t1min) / (1 - t1min)
  }
  if (e <= -999) {
    asc <- exp(-t1^2 / upselex)
  }
  if (f > -999) {
    dsc <- 1 + (point2 - 1) * (exp(-t2^2 / downselex) -
      1) / (t2min - 1)
  }
  if (f <= -999) {
    dsc <- exp(-(t2)^2 / downselex)
  }
  idx.seq <- (j1 + 1):j2
  sel[idx.seq] <- asc[idx.seq] * (1 - join1[idx.seq]) + join1[idx.seq] * (1 -
    join2[idx.seq] + dsc[idx.seq] * join2[idx.seq])
  if (startbin > 1 && e >= -1000) {
    sel[1:startbin] <- (x[1:startbin] / x[startbin])^2 *
      sel[startbin]
  }
  if (j2 < length(x)) {
    sel[(j2 + 1):length(x)] <- sel[j2]
  }
  return(sel)
}

#' Nonparametric size selectivity, pattern 6
#'
#' Note this works for size only and not age. Note that option 43 is similar and
#' could be added onto this function
#'
#' @param n_pts The number of waypoints as specified by the user in the special
#'  category in SS3. Probably not needed.
#' @param len_pt_first The size for the first waypoint
#' @param len_pt_last The size for the last waypoint
#' @param sel_vals_pts A numeric vector containing the selectivity values at
#'  each waypoint. Entered as logistic.
nonparasize6 <- function(len_pt_first, len_pt_last, sel_vals_pts) {
  lastsel-10.0;  // log(selex) for first bin;
  lastSelPoint=len_bins_m(1);    //  first size
  finalSelPoint=value(sp(2+scaling_offset));  // size beyond which selex is constant
  SelPoint=value(sp(1+scaling_offset));   //  first size that will get a parameter.  Value will get incremented by step interval (temp1)
  z=3+scaling_offset;  // parameter counter
  temp1 = (finalSelPoint-SelPoint)/(seltype(f,4)-1.0);  // step interval

  for (j=1;j<=nlength;j++)
  {
    if(len_bins_m(j)<SelPoint)
    {
      tempvec_l(j)=lastsel + (len_bins_m(j)-lastSelPoint)/(SelPoint-lastSelPoint) * (sp(z)-lastsel);
    }
    else if(len_bins_m(j)==SelPoint)
    {
      tempvec_l(j)=sp(z);
      lastsel=sp(z);
      lastSelPoint=SelPoint;
      SelPoint+=temp1;
      if(SelPoint<=finalSelPoint)
      {z++;}
      else
      {SelPoint=finalSelPoint;}
    }
    else if(len_bins_m(j)<=finalSelPoint)
    {
      lastsel=sp(z);
      lastSelPoint=SelPoint;
      SelPoint+=temp1;
      if(SelPoint<=finalSelPoint)
      {z++;}
      else
      {SelPoint=finalSelPoint;}
      tempvec_l(j)=lastsel + (len_bins_m(j)-lastSelPoint)/(SelPoint-lastSelPoint) * (sp(z)-lastsel);
    }
    else
    {tempvec_l(j)=sp(z);}
    #ifdef DO_ONCE
    if(do_once==1)  echoinput<<"selex42  "<<j<<" "<<len_bins_m(j)<<" "<<SelPoint<<" "<<tempvec_l(j)<<endl;
    #endif
  }
  if (scaling_offset == 0)
  {
    temp=max(tempvec_l);
  }
  else
  {
    int low_bin  = int(value(sp(1)));
    int high_bin = int(value(sp(2)));
    if (low_bin < 1)
    {
      low_bin = 1;
      N_warn++;  warning<<N_warn<<" "<<" selex pattern 43; value for low bin is less than 1, so set to 1 "<<endl;
    }
    if (high_bin > nlength)
    {
      high_bin = nlength;
      N_warn++;  warning<<N_warn<<" "<<" selex pattern 43; value for high bin is greater than "<<nlength<<", so set to "<<nlength<<" "<<endl;
    }
    if (high_bin < low_bin) high_bin = low_bin;
    if (low_bin > high_bin) low_bin = high_bin;
    sp(1) = low_bin;
    sp(2) = high_bin;
    temp=mean(tempvec_l(low_bin,high_bin));
    scaling_offset = 0;     // reset scaling offset
  }
  sel = mfexp(tempvec_l-temp);
  break;
}

}
