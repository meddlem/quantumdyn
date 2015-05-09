# 1d quantum dynamics simulation

Time integration of the 1d Schrodinger equation, using the Crank-Nicolson algorithm. 
The resulting systems are solved using the LAPACK routine zgtsv for complex tridiagonal matrices. The program relies on gnuplot for visualization of the wavefunction. 
The result of the simulation is an animation time evolution of the density (default), or the real part of the wavefunction (use flag: -r).
##5 experiments may be selected by the user: 

1. Default: Harmonic potential
2. Use --tun flag for square (tunnel) barrier
3. Use --hsq flag for adiabatic potential variation: Harmonic potential to ISQW
4. Use --hqa flag for adiabatic potential variation: Harmonic potential to quartic potential
5. Use --exp flag for adiabatic potential variation: Harmonic potential to exponential potential

Example: $./main --tun -r
