set datafile separator ","
set title "Meander Centerline"
set xlabel "x (m)"
set ylabel "y (m)"
set grid

plot "data/kinoshita_01.csv" every ::12  using 1:2 with points pt 6 lc rgb "blue" notitle