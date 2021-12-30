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
#' @return The double normal selectivity values given the parameters. This is a
#'  numeric vector of the same length as `x`.
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
  sel
}

#' Nonparametric size selectivity, pattern 6
#'
#' Note this works for size only and not age. Note that option 43 is similar and
#' could be added onto this function, using 1 additional parameter
#'
#' @param x A vector of lengths
#' @param len_pt_first The size for the first waypoint
#' @param len_pt_last The size for the last waypoint
#' @param sel_val_pts A numeric vector containing the log scale selectivity values at each waypoint
#' @param scaling_offset Should be left as 0 except for pattern 43. Defaults to 0.
#'  each waypoint. Entered as logistic.
#' @return The double normal selectivity values given the parameters. This is a
#'  numeric vector of the same length as `x`.
nonparasize6 <- function(x, len_pt_first, len_pt_last, sel_val_pts, scaling_offset = 0) {
  #vector of waypoints length values
  len_val_pts <- seq(from = len_pt_first, to = len_pt_last, length.out = length(sel_val_pts))
  # calculate slope for each line segment
  # line segment before len_pt_first
  slope_before <- (sel_val_pts[1] - (-10))/ # because neg 10 is always first sel in this case
    len_pt_first - x[1]
  # slopes for each line segment
  # length is 1 less than sel_val_pts
  slopes_during <- vector(length(sel_val_pts) - 1, mode = "numeric")
  for(i in seq_along(slopes_during)) {
    slopes_during[i] <- (sel_val_pts[i+1] - sel_val_pts[i])/
      (len_val_pts[i+1] - len_val_pts[i])
  }
  # line segment after
  slope_after <- 0 # because constant
  # based on the slopes, calculate the selectivity vals for each point
  log_selex_x <- vector(length(x), mode = "numeric")
  for(i in seq_along(x)) {
    tmp_len <- x[i]
    #figure out which slope
    if(tmp_len < len_pt_first) {
      tmp_slope <- slope_before
      tmp_b <- -10
      tmp_low_len_waypt <- x[1]
    }
    if(tmp_len >len_pt_last) {
      tmp_slope <- slope_after
      tmp_b <- sel_val_pts[length(sel_val_pts)]
      tmp_low_len_waypt <- len_pt_last
    }
    if (tmp_len >= len_pt_first & tmp_len <= len_pt_last) {
      # finds closest without going over to figure out slope and b index
      num_closest <- len_val_pts - tmp_len
      num_closest <- ifelse(num_closest > 0, NA, num_closest)
      num_ind <- which.min(abs(num_closest))
      tmp_slope <- slopes_during[num_ind]
      if(is.na(tmp_slope)) { # this is the case when tmp_len = len_pt_past
        tmp_slope <- 0
      }
      tmp_b <- sel_val_pts[num_ind]
      tmp_low_len_waypt <- len_val_pts[num_ind]
    }
    # next, figure out the log sel value.
    tmp_sel <- tmp_b + tmp_slope*(tmp_len - tmp_low_len_waypt)
    log_selex_x[i] <- tmp_sel
  }
  # convert values from the log scale
  nominal_selex_x <- exp(log_selex_x)
  nominal_selex_x
}
