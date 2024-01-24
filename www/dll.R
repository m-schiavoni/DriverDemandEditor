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
  for (j in 2:ncol(df)) {
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
  
  # remove extreme outlying RPMs by gear
  df$gear = round(df$gear)
  df = df %>% group_by(gear) %>%
    mutate(iqr=IQR(rpm), q1=quantile(rpm, 0.25), q3=quantile(rpm, 0.75))
  iqr_mult = 2
  df$outlier = (df$rpm < (df$q1 - iqr_mult*df$iqr)) | (df$rpm > (df$q3 + iqr_mult*df$iqr))
  df = df[!df$outlier, c('time', 'gear', 'rpm', 'speed', 'pedal', 'load')]
  
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
  df$gear_weight = 1/(df$gear + 2)
  df$gear_weight[df$gear == 1] = 1/(3 + 2)  # 1st gear weight equal to 3rd gear weight
  
  return(df)
}

calc_avgs_by_dd_cell <- function(df, n_pedal, n_speed, pedal_bins, speed_bins){
  gear_mat = matrix(nrow=n_pedal, ncol=n_speed)
  rownames(gear_mat) = paste0('p_', pedal_bins)
  colnames(gear_mat) = paste0('s_', speed_bins)
  load_mat = gear_mat
  for (i_pedal in 1:n_pedal) {
    for (i_speed in 1:n_speed) {
      temp = df[(df$i_pedal == i_pedal) &
                  ((df$i_speed1 == i_speed) | (df$i_speed2 == i_speed)),]
      
      if (nrow(temp) > 0) {
        # inverse speed delta weight
        speed_delta = abs(temp$speed - speed_bins[i_speed])
        temp$speed_weight = 1/(speed_delta + max(speed_delta)/2)
        
        gear_mat[i_pedal, i_speed] = mean(temp$gear)
        load_mat[i_pedal, i_speed] = weighted.mean(temp$load, temp$gear_weight*temp$speed_weight)
      } else {
        gear_mat[i_pedal, i_speed] = NA
        load_mat[i_pedal, i_speed] = NA
      }
    }
  }
  return(list(gear=gear_mat, load=load_mat))
}

normalize_load <- function(avg_by_dd_cell, n_speed, n_pedal){
  load_mod = avg_by_dd_cell$load
  for (s in 1:n_speed) {
    temp = load_mod[,s]
    
    # if less than two data points, skip to next iteration
    ii = which(!is.na(temp))
    if (length(ii) < 2) next
    
    # sort values
    temp[ii] = sort(temp[ii])
    
    # interpolate interior missing values when adjacent values are available
    ii_d1 = c(1, diff(ii))
    if (max(ii_d1) > 1) {
      jj = which(is.na(temp))
      for (p in 2:(n_pedal-1)) {
        if ((p %in% jj) & ((p-1) %in% ii) & ((p+1) %in% ii)) {
          temp[p] = (temp[p-1] + temp[p+1])/2
        }
      }
    }
    
    # scale load to have max value of exactly 100, and don't let it dip below that
    max_load = max(temp, na.rm=TRUE)
    if (max_load > 90) {
      min_load = min(15, min(temp, na.rm=TRUE))
      temp = temp + (100 - max_load)*(temp - min_load)/(max_load - min_load)
      
      i_max = which.max(temp)
      if (i_max != n_pedal) temp[i_max:n_pedal] = 100
    }
    load_mod[,s] = temp
  }
  return(load_mod)
}

kill_outliers = function(vec) {
  vec_out = vec
  for (i in 2:(length(vec)-1)) {
    peak = (vec[i] > vec[i-1]) & (vec[i] > vec[i+1])
    valley = (vec[i] < vec[i-1]) & (vec[i] < vec[i+1])
    if (peak | valley) {
      pct_delta1 = (vec[i] - vec[i-1]) / vec[i-1]
      pct_delta2 = (vec[i] - vec[i+1]) / vec[i+1]
      if (peak) {
        pct_delta = min(pct_delta1, pct_delta2)
      } else {
        pct_delta = max(pct_delta1, pct_delta2)
      }
      if (abs(pct_delta) > 0.15) vec_out[i] = (vec[i-1] + vec[i+1])/2
    }
  }
  return(vec_out)
}

calc_new_dd <- function(dd_mat, target_mat, load_mod, n_pedal, n_speed){
  # generate raw output
  dd_out = dd_mat*target_mat/load_mod
  
  # reset NAs
  nas = which(is.na(dd_out))
  dd_out[nas] = dd_mat[nas]
  
  # reset first cell and negatives
  dd_out[1,1] = dd_mat[1,1]
  negs = which(dd_mat <= 0)
  dd_out[negs] = dd_mat[negs]
  
  # ensure max torque is not exceeded
  max_tq = max(dd_mat)
  dd_out[which(dd_out > max_tq)] = max_tq
  
  # ensure all columns are increasing
  dd_out = apply(dd_out, 2, sort)
  
  # find and replace outliers
  dd_out = t(apply(dd_out, 1, 'kill_outliers'))
  
  return(dd_out)
}

smooth_dd <- function(dd_out, n_pedal, n_speed){
  # smooth horizontally
  dd_out_smooth = t(apply(dd_out, 1, stats::filter, filter=c(0.5,1,2,1,0.5)/5))
  dd_out_smooth[,1] = dd_out[,1]
  dd_out_smooth[,2] = (dd_out[,1] + 2*dd_out[,2] + dd_out[,3])/4
  dd_out_smooth[,n_speed] = dd_out[,n_speed]
  dd_out_smooth[,n_speed-1] = (dd_out[,n_speed] + 2*dd_out[,n_speed-1] + dd_out[,n_speed-2])/4
  
  # additional step for first and last columns
  dd_out_smooth[,1] = (dd_out_smooth[,1] + 2*dd_out_smooth[,2] - dd_out_smooth[,3])/2
  dd_out_smooth[,n_speed] = (dd_out_smooth[,n_speed] + 2*dd_out_smooth[,n_speed-1] - dd_out_smooth[,n_speed-2])/2
  
  # don't smooth first cell
  dd_out_smooth[1,1] = dd_out[1,1]
  
  # ensure all columns are increasing
  dd_out_smooth = apply(dd_out_smooth, 2, sort)
  
  return(dd_out_smooth)
}

finalize_dd <- function(dd_out_smooth, decel_mult, max_speed, pedal_bins, speed_bins){
  dd_out_final = dd_out_smooth
  
  # increase magnitude of negatives
  negs = which(dd_out_final < 0)
  dd_out_final[negs] = decel_mult*dd_out_final[negs]
  
  # copy max_speed column to the right
  j = which(speed_bins == as.numeric(max_speed))
  dd_out_final[,j:length(speed_bins)] = dd_out_final[,j]
  
  # final rounded result
  dd_out_final = round(dd_out_final, 1)
  rownames(dd_out_final) = paste0('p_', pedal_bins)
  colnames(dd_out_final) = paste0('s_', speed_bins)
  
  return(dd_out_final)
}
