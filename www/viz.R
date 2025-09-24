gear_colors = c('darkorchid1', 'deepskyblue', 'green', 'yellow', 'orange',
                'lightpink1', 'antiquewhite3', 'tan3', 'red', 'darkorchid1')
pch_vec = c(0:6,0:2)

plot_rpm_vs_pedal <- function(df){
  n_row = ceiling(length(unique(df$gear))/2)
  par(mfrow=c(n_row,2), mar=c(4.5, 4.5, 3, 2))
  for (i in 1:10) {
    ii = which(df$gear == i)
    if (length(ii) > 0) {
      plot(df$pedal[ii], df$rpm[ii]/1000, xlab='Pedal %', ylab='Engine RPM (x1000)',
           xlim=c(0,100), main=paste('Gear', i), col=gear_colors[i], pch=pch_vec[i],
           cex.axis=1.15, cex.main=1.3, cex.lab=1.2)
      grid(col='grey60', lty='dotted')
    }
  }
}

plot_avg_gear_by_cell <- function(gear_mat, n_gear, speed_bins, pedal_bins, dd_units){
  n_speed = length(speed_bins)
  n_pedal = length(pedal_bins)
  
  par(mar=c(4,4,3,1))
  min_gear = floor(min(gear_mat, na.rm=TRUE))
  gear_palette = colorRampPalette(gear_colors[min_gear:n_gear])(1+(n_gear-min_gear)*4)
  
  # set background color to grey
  plot(1:n_speed, 1:n_pedal, xaxt='n', yaxt='n', xlab='', ylab='')
  rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col='grey60')
  par(new=TRUE)
  
  # plot matrix
  image(1:n_speed, 1:n_pedal, t(gear_mat)[,n_pedal:1], col=gear_palette,
        xaxt='n', yaxt='n', xlab='', ylab='')
  
  # add cell labels
  gear_vec = as.vector(gear_mat)
  xvec = rep(1:n_speed, each=n_pedal); xvec = xvec[!is.na(gear_vec)]
  yvec = rep(n_pedal:1, times=n_speed); yvec = yvec[!is.na(gear_vec)]
  gear_vec = gear_vec[!is.na(gear_vec)]
  text(xvec, yvec, round(gear_vec, 1))
  
  axis(side=1, at=1:n_speed, labels=speed_bins)
  axis(side=2, at=1:n_pedal, labels=rev(pedal_bins), par(las=1))
  mtext(paste0('Speed (',dd_units,')'), side=1, line=2.5)
  par(las=0); mtext('Pedal %', side=2, line=2.5)
  mtext('Avg. Gear Selected by Driver Demand Table Cell', 3, 1, cex=1.1)
  grid(nx=n_speed, ny=n_pedal, col='white', lty=1)
  graphics::box()
}

profile_plot <- function(pedal_bins, profile_df, profile_vec){
  par(mar=c(4.5,4.5,1.5,1.5))
  plot(c(0:100), c(0:100), xlab='Pedal %', ylab='Engine Load %', las=1, asp=1)
  rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col='grey80')
  grid(col='white', lty=1)
  cols = c('black', 'purple2', 'orange2', 'turquoise3', 'red2')
  for (i in 5:2) lines(profile_df$x, profile_df[,i], lwd=2, col=cols[i])
  if (length(profile_vec) > 1) {
    # if (!is.na(profile_vec)) {
    points(pedal_bins, profile_vec, pch=0, cex=1.2)
    lines(pedal_bins, profile_vec, lty=2)
  }
  legend('bottomright', inset=0.04, col=rev(cols),
         legend=c('Extreme', 'Peppy', 'Default', 'Progressive', 'Selected'),
         lty=c(1,1,1,1,2), lwd=c(2,2,2,2,1), pch=c(NA,NA,NA,NA,0), pt.cex=c(NA,NA,NA,NA,1.1))
}

plot_load_by_speed <- function(df, load_mat, target_mat, speed_breaks, speed_bins, pedal_bins, n_gear, dd_units){
  leg = c('1st gear', '2nd gear', '3rd gear', '4th gear', '5th gear',
          '6th gear', '7th gear', '8th gear', '9th gear', '10th gear')[1:n_gear]
  if (dd_units == 'RPM') {
    i_min_speed = which(speed_breaks == max(speed_breaks[speed_breaks <= min(df$rpm)]))
    i_max_speed = which(speed_breaks == min(speed_breaks[speed_breaks >= max(df$rpm)])) - 1
  } else {
    i_min_speed = which(speed_breaks == max(speed_breaks[speed_breaks <= min(df$speed)]))
    i_max_speed = which(speed_breaks == min(speed_breaks[speed_breaks >= max(df$speed)])) - 1
  }
  speeds = speed_bins[i_min_speed:i_max_speed]
  if (dd_units == 'RPM') speeds = speeds[speeds > 1000]
  if (length(speeds) > 12) speeds = speeds[1:12]
  n_tall = ceiling(length(speeds)/4)
  par(mfrow=c(n_tall, 4), oma=c(0,0,6,0), mar=c(4.5, 4.5, 3, 2))
  for (i in which(speed_bins %in% speeds)) {
    ii = (df$i_speed1 == i) | (df$i_speed2 == i)
    
    # plot scatter data
    plot(df$pedal[ii], df$load[ii], pch=pch_vec[df$gear[ii]], col=gear_colors[df$gear[ii]],
         ylim=c(0,110), asp=1, xaxs='i', yaxs='i', cex.axis=1.15, cex.lab=1.2, cex.main=1.3,
         xlab='Pedal %', ylab='Engine Load %', main=paste0('Speed Column: ', speed_bins[i], dd_units))
    grid(col='grey60', lty='dotted')
    
    # plot avg load curve
    jj = which(!is.na(load_mat[,i]))
    lines(pedal_bins[jj], load_mat[jj,i], lwd=4, lty=1, col='white')
    lines(pedal_bins[jj], load_mat[jj,i], lwd=2, lty=1, col='black')
    points(pedal_bins[jj], load_mat[jj,i], pch=19)
    
    # plot target curve
    lines(pedal_bins, target_mat[,i], lwd=2, lty=1, col='white')
    lines(pedal_bins, target_mat[,i], lwd=2, lty=2, col='red')
  }
  
  mtext('Load % vs. Pedal % by Speed', side=3, outer=TRUE, line=4, cex=1.1)
  par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend('top', horiz=TRUE, inset=0.03, legend=c(leg, 'average', 'target'),
         col=c(gear_colors[1:n_gear], 'black', 'red'), pch=c(pch_vec[1:n_gear], 19, NA),
         lty=c(rep(0, n_gear), 1, 2), lwd=2, bty='n', cex=1.4)
  par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(4.5, 4.5, 3, 2))
}

plot_pedal_1d <- function(dd_out_final, pedal_slider, pedal_bins, speed_bins, dd_units){
  par(mar=c(4,4,3,2))
  y_range = range(dd_out_final[pedal_slider,])
  if ((y_range[2] - y_range[1]) < 50) {
    delta = 50 - y_range[2] + y_range[1]
    y_range[1] = y_range[1] - delta/2
    y_range[2] = y_range[2] + delta/2
  }
  plot(speed_bins, dd_out_final[pedal_slider,], 
       main=paste('Pedal % =', pedal_bins[pedal_slider]), xaxt='n',
       xlab=paste0('Speed (',dd_units,')'), ylab='Demanded Torque', ylim=y_range, las=2)
  rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col='grey80')
  my_by = 20
  if (dd_units == 'RPM') my_by = 500
  abline(v=seq(0, max(speed_bins), by=my_by), col='white')
  abline(h=axTicks(2), col='white')
  axis(1, at = seq(0, max(speed_bins), by=my_by), las=2)
  points(speed_bins, dd_out_final[pedal_slider,], pch=19)
  lines(speed_bins, dd_out_final[pedal_slider,])
  abline(h=0, lty=2)
}

plot_speed_1d <- function(dd_out_final, speed_slider, speed_bins, pedal_bins, dd_units){
  par(mar=c(4,4,3,2))
  plot(pedal_bins, dd_out_final[,speed_slider], 
       main=paste('Speed =', speed_bins[speed_slider], dd_units), xaxt='n',
       xlab='Pedal %', ylab='Demanded Torque', las=2)
  rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col='grey80')
  abline(v=seq(0, 100, by=10), col='white')
  abline(h=axTicks(2), col='white')
  axis(1, at = seq(0, 100, by=10), las=2)
  points(pedal_bins, dd_out_final[,speed_slider], pch=19)
  lines(pedal_bins, dd_out_final[,speed_slider])
  abline(h=0, lty=2)
}

wiz_plot <- function(profile_df, wiz_inc){
  par(mar=c(2.5,2.5,3.5,1.5))
  plot(c(0:100), c(0:100), xaxt='n', yaxt='n', xlab='', ylab='', las=1, asp=1)
  mtext('Pedal %', side=1, line=1)
  mtext('Engine Load %', side=2, line=1)
  mtext('Current vs. New Curve (notional)', side=3, line=1.25, cex=1.2, font=2)
  rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col='grey80')
  grid(col='white', lty=1)
  cols = c('red', 'black')
  wiz_inc = as.numeric(wiz_inc) + 3
  lines(profile_df$x, profile_df[,wiz_inc], lwd=2, col=cols[1])
  lines(profile_df$x, profile_df[,3], lwd=2, lty=2, col=cols[2])
  legend('topleft', inset=0.04, col=rev(cols), legend=c('Current', 'New'), lty=c(2,1), lwd=2)
}
