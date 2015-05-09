module plotroutines
  use constants
  use structures
  implicit none
  private
  public :: plot_wavef, close_plot, animate_plot, p_plot

contains
  subroutine animate_plot(Q, P)
    type(plt_par), intent(in)  :: P
    type(modl_par), intent(in) :: Q

    integer  :: ret

    ! creates fifo pipe: plotfifo.dat
    call system("rm -f plotfifo.dat; mkfifo plotfifo.dat", ret)
    
    ! create a gnuplot command file
    open(10,access = 'sequential',file = 'matplot.plt')
      write(10,*) 'set style line 1 lt 1 lc rgb "blue" lw 2 pt 2 ps 0.6'
      write(10,*) 'set style line 2 lt 1 lc rgb "red" lw 2 pt 2 ps 0.6'
      write(10,*) 'set style line 3 lt 1 lc rgb "black" lw 1 pt 2 ps 0.6'
      write(10,*) 'set grid'
      write(10,*) 'set xlabel "x"'
      write(10,*) 'set xrange [0:',Q%L,']'
      if (P%plot_re) then 
        write(10,*) 'set yrange [',P%rng(1),':',P%rng(2),']'
      else
        write(10,*) 'set yrange [0:',P%rng(2),']'
      endif
      write(10,*) 'load "loop.plt"'
    close(10)
    
    ! create plot/animate instruction
    open(10,access = 'sequential', file = 'loop.plt')
      if (P%plot_re) then
        write(10,*) 'plot "< cat plotfifo.dat" u 1:2 w l ls 2 t "Re(Psi)",\'
      else
        write(10,*) 'plot "< cat plotfifo.dat" u 1:3 w l ls 1 t "P",\'
      endif
      write(10,*) '"" using 1:4 with lines ls 3 title "V"'
      write(10,*) 'pause 0.1'
      write(10,*) 'reread'
    close(10)
    
    ! now fork instance of gnuplot to plot/animate the lattice
    call system("gnuplot matplot.plt &",ret)
  end subroutine
  
  subroutine plot_wavef(psi, x, V, Q, auto)
    complex(dp), intent(in)    :: psi(:)
    real(dp), intent(in)       :: x(:), V(:)
    type(modl_par), intent(in) :: Q
    logical, intent(in)        :: auto

    integer :: i
    character(50) :: rfmt, filename

    rfmt = '(F10.5,1X,F10.5,1X,F10.5,1X,F10.5)' 

    if (auto) then
      filename = 'plotfifo.dat'
    else
      filename = 'wf.dat'
    endif
    
    ! write plot data to pipe
    open(11,access = 'sequential',status = 'replace',file = filename)
      do i = 1,Q%M
        write(11,rfmt) x(i), real(psi(i)), abs(psi(i))**2, V(i)/20
      enddo
    close(11)
  end subroutine

  subroutine close_plot()
    call system('pkill gnuplot')
    call system('rm -f plotfifo.dat')
  end subroutine

  subroutine p_plot(psi, x, V, Q, P)
    complex(dp), intent(in)    :: psi(:)
    real(dp) , intent(in)      :: V(:), x(:)
    type(plt_par), intent(in)  :: P
    type(modl_par), intent(in) :: Q

    integer  :: ret

    ! create a gnuplot command file
    open(10,access = 'sequential',file = 'wf.plt')
      write(10,*) 'set term pngcairo'
      write(10,*) 'set output "plot.png"'
      write(10,*) 'set style line 1 lt 1 lc rgb "blue" lw 2 pt 2 ps 0.6'
      write(10,*) 'set style line 2 lt 1 lc rgb "red" lw 2 pt 2 ps 0.6'
      write(10,*) 'set style line 3 lt 1 lc rgb "black" lw 1 pt 2 ps 0.6'
      write(10,*) 'set grid'
      write(10,*) 'set xlabel "x"'
      write(10,*) 'set xrange [0:',Q%L,']'
      if (P%plot_re) then 
        write(10,*) 'set yrange [',P%rng(1),':',P%rng(2),']'
      else
        write(10,*) 'set yrange [0:',P%rng(2),']'
      endif
      if (P%plot_re) then
        write(10,*) 'plot "wf.dat" u 1:2 w l ls 2 t "Re(Psi)",\'
      else
        write(10,*) 'plot "wf.dat" u 1:3 w l ls 1 t "P",\'
      endif
      write(10,*) '"" using 1:4 with lines ls 3 title "V"'
    close(10)

    call plot_wavef(psi, x, V, Q, .false.)
    
    ! now fork instance of gnuplot to plot the wavefunction
    call system("gnuplot wf.plt &",ret)
  end subroutine
end module 
