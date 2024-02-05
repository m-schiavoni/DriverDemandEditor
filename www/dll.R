calc_freqs <- function(df) {
  if (anyNA(df$pedal) | anyNA(df$load)) {
    duration = df$time[nrow(df)] - df$time[1]
    df = df[,c('pedal', 'load')]
    log_freq_hz = vector('numeric',  2)
    quality = vector('character',  2)
    for (i in 1:2) {
      ii = which(!is.na(df[,i]))
      log_freq_hz[i] = length(ii)/duration
      if (log_freq_hz[i] < 1) {
        quality[i] = 'Low'
      } else if (log_freq_hz[i] < 2) {
        quality[i] = 'Med-Low'
      } else if (log_freq_hz[i] < 4) {
        quality[i] = 'Medium'
      } else if (log_freq_hz[i] < 8) {
        quality[i] = 'Med-High'
      } else if (log_freq_hz[i] < 16) {
        quality[i] = 'High'
      } else {
        quality[i] = 'Very High'
      }
    }
    freqs = data.frame(log_freq_hz, quality, row.names=c('Pedal','Load'))
  } else {
    freqs = data.frame(c(NA,NA), c(NA,NA), row.names=c('Pedal','Load'))
  }
  colnames(freqs) = c('Log Freq Hz', 'Quality') 
  return(freqs)
}

interp_scale_filter <- function(df, log_units) {
  # interpolate data
  for (j in 2:ncol(df)) {  # lapply() is no better than looping in this instance
    if (anyNA(df[,j])) {
      ii = which(!is.na(df[,j]))
      df[,j] = approx(x=df$time[ii], y=df[ii,j], xout=df$time,
                      method='linear', rule=2, ties=list("ordered", mean))$y
    }
  }
  
  # scale load if greater than 100
  if (max(df$load) > 100) df$load = df$load*100/max(df$load)
  
  # scale pedal if min greater than 0
  min_pedal = min(df$pedal)
  max_pedal = max(df$pedal)
  if (min_pedal > 16) {  # 20 - 85
    df$pedal = (df$pedal - min_pedal)*100/(max(85, max_pedal) - min_pedal)
  } else if (min_pedal > 6) {  # 10 - 42
    df$pedal = (df$pedal - min_pedal)*100/(max(42, max_pedal) - min_pedal)
  }
  
  # filter data to only include records with increasing pedal
  df$pedal = round(df$pedal, 1)
  pedal_deltas = c(0, diff(df$pedal))
  df = df[pedal_deltas > 0,]
  
  # drop anomalous data in top-left corner of load-pedal graphs
  df = df[(df$pedal>20) | (df$load<80),]
  df = df[(df$pedal>10) | (df$load<40),]
  
  # drop lowest speeds
  if (log_units == 'MPH') {
    df = df[df$speed > 3.11,]
  } else if (log_units == 'KPH') {
    df = df[df$speed > 5,]
  }
  
  # down-sample to reduce memory usage
  nr = nrow(df)
  if (nr > 40000) {
    df = df[seq(1,nr,8),]
  } else if (nr > 35000) {
    df = df[seq(1,nr,7),]
  } else if (nr > 30000) {
    df = df[seq(1,nr,6),]
  } else if (nr > 25000) {
    df = df[seq(1,nr,5),]
  } else if (nr > 20000) {
    df = df[seq(1,nr,4),]
  } else if (nr > 15000) {
    df = df[seq(1,nr,3),]
  } else if (nr > 10000) {
    df = df[seq(1,nr,2),]
  }
  
  df$gear = round(df$gear)
  
  return(df)
}

bin_speed = function(speed, bins, j) {
  if (j == 1) {
    bin = which(bins == max(bins[bins <= speed]))
  } else if (j == 2) {
    bin = which(bins == min(bins[bins > speed]))
  }
  return(bin)
}

calc_bins_and_weights <- function(df, log_units, dd_units, pedal_breaks, speed_bins) {
  if ((log_units == 'MPH') & (dd_units == 'KPH')) {
    df$speed = df$speed*1.609
  } else if ((log_units == 'KPH') & (dd_units == 'MPH')) {
    df$speed = df$speed/1.609
  }
  
  df$i_pedal = as.integer(cut(df$pedal, pedal_breaks))
  df$i_speed1 = as.integer(lapply(df$speed, 'bin_speed', bins=speed_bins, j=1))
  df$i_speed2 = as.integer(lapply(df$speed, 'bin_speed', bins=speed_bins, j=2))
  
  # add gear weight column
  df$gear_weight = 4/(df$gear + 2)
  df$gear_weight[df$gear == 1] = 4/(3 + 2)  # 1st gear weight equal to 3rd gear weight
  
  return(df)
}

calc_avgs_by_dd_cell <- function(df, n_pedal, speed_bins){
  n_speed = length(speed_bins)
  gear_mat = matrix(nrow=n_pedal, ncol=n_speed)
  load_mat = gear_mat
  for (i_pedal in 1:n_pedal) {
    for (i_speed in 1:n_speed) {
      ii = which((df$i_pedal == i_pedal) & ((df$i_speed1 == i_speed) | (df$i_speed2 == i_speed)))
      
      if (length(ii) > 0) {
        # inverse speed delta weight
        speed_delta = abs(df$speed[ii] - speed_bins[i_speed])
        speed_weight = cos(speed_delta/max(speed_delta)*pi/2)
        
        gear_mat[i_pedal, i_speed] = mean(df$gear[ii])
        load_mat[i_pedal, i_speed] = weighted.mean(df$load[ii], df$gear_weight[ii]*speed_weight)
      }
    }
  }
  return(list(gear=gear_mat, load=load_mat))
}

interp_sort <- function(v, sort_tf=FALSE) {
  ii = which(!is.na(v))
  if (length(ii) < 2) {
    return(v)
  } else {
    if (sort_tf) v[ii] = sort(v[ii])
    
    # interpolate interior missing values when both adjacent values are available
    ii_d1 = c(1, diff(ii))
    if (max(ii_d1) > 1) {
      jj = which(is.na(v))
      for (p in 2:(length(v)-1)) {
        if ((p %in% jj) & ((p-1) %in% ii) & ((p+1) %in% ii)) {
          v[p] = (v[p-1] + v[p+1])/2
        }
      }
    }
    return(v)
  }
}

normalize_load <- function(load_mat, n_speed, n_pedal){
  # scale avg load matrix to have max value of exactly 100
  max_load = max(load_mat, na.rm=TRUE)
  if (max_load > 90) {
    min_load = min(15, min(load_mat, na.rm=TRUE))
    load_mat = load_mat + (100 - max_load)*(load_mat - min_load)/(max_load - min_load)
  }
  
  # interpolate missing values across rows
  load_mat = t(apply(load_mat, 1, interp_sort, sort_tf=FALSE))
  
  # sort and interpolate missing values across columns
  load_mat = apply(load_mat, 2, interp_sort, sort_tf=TRUE)
  
  return(load_mat)
}

calc_new_dd <- function(dd_mat, target_mat, load_mod){
  # generate raw output
  dd_out = dd_mat*target_mat/load_mod
  
  # reset NAs
  nas = which(is.na(dd_out))
  dd_out[nas] = dd_mat[nas]
  
  # reset first cell, negatives, and last row
  dd_out[1,1] = dd_mat[1,1]
  negs = which(dd_mat <= 0)
  dd_out[negs] = dd_mat[negs]
  dd_out[nrow(dd_out),] = dd_mat[nrow(dd_out),]
  
  # ensure max torque is not exceeded
  max_tq = max(dd_mat)
  dd_out[which(dd_out > max_tq)] = max_tq
  
  # ensure all columns are increasing
  dd_out = apply(dd_out, 2, sort)
  
  return(dd_out)
}

whittaker = function(y, lambda=1, d=2) {
  I = diag(length(y))
  D = diff(I, 1, d)
  A = I + lambda * t(D) %*% D
  x = solve(A, y)
  return(x)
}

smooth_dd <- function(dd){
  # smooth columns
  dd_smooth = apply(dd, 2, whittaker, lambda=0.1, d=3)
  
  # reset first and last row
  dd_smooth[1,] = dd[1,]
  dd_smooth[nrow(dd),] = dd[nrow(dd),]
  
  # smooth rows
  dd_smooth = t(apply(dd_smooth, 1, whittaker, lambda=1, d=3))
  
  # don't smooth first cell
  dd_smooth[1,1] = dd[1,1]
  
  # ensure max torque is not exceeded
  max_tq = max(dd)
  dd_smooth[which(dd_smooth > max_tq)] = max_tq
  
  # ensure all columns are increasing
  dd_smooth = apply(dd_smooth, 2, sort)
  
  return(dd_smooth)
}

finalize_dd <- function(dd, decel_mult, max_speed, pedal_bins, speed_bins){
  # increase magnitude of negatives
  negs = which(dd < 0)
  dd[negs] = decel_mult*dd[negs]
  
  # copy max_speed column to the right
  j = which(speed_bins == as.numeric(max_speed))
  dd[,j:length(speed_bins)] = dd[,j]
  
  # final rounded result
  dd = round(dd, 1)
  rownames(dd) = paste0('p_', pedal_bins)
  colnames(dd) = paste0('s_', speed_bins)
  
  return(dd)
}
