set terminal _nextfe 
set output '/dev/null'
set noclip points
set clip one
set noclip two
set border
set boxwidth
set dummy x,y
set format x "%g"
set format y "%g"
set format z "%g"
set nogrid
set key 10,50,0
set nolabel
set noarrow
set nologscale
set offsets 0, 0, 0, 0
set nopolar
set angles radians
set noparametric
set view 60, 30, 1, 1
set samples 100, 100
set isosamples 10, 10
set surface
set nocontour
set clabel
set nohidden3d
set cntrparam order 4
set cntrparam linear
set cntrparam levels auto 5
set cntrparam points 5
set size 1.240521,0.814542
set data style points
set function style lines
set noxzeroaxis
set noyzeroaxis
set tics in
set ticslevel 0.5
set xtics 0,20,120
set ytics -20,20,60
set ztics
set title "" 0,0
set notime
set rrange [0 : 10]
set trange [-5 : 5]
set urange [-5 : 5]
set vrange [-5 : 5]
set xlabel "" 0,0
set xrange [-10 : 110]
set ylabel "" 0,0
set yrange [-10 : 60]
set zlabel "" 0,0
set zrange [-10 : 10]
set autoscale r
set autoscale t
set noautoscale   
set autoscale z
set zero 1e-08
plot '/MyDisk/book/code/ch11/simln.dat'  title "robot" with dots, '/MyDisk/book/code/ch11/obst.dat'  title "obstacle" with lines 1, '/MyDisk/book/code/ch11/goals.dat'  title "goals" with points 1 6
