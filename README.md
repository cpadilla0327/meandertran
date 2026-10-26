# meandertran

A Kinoshita meander centerline generator written in Fortran.

`meandertran` generates meander centerlines using the Kinoshita model. Kinoshita parameter ranges can be configured through an input file, and the resulting centerline can be visualized with Gnuplot.

## Requirements

- A Fortran compiler (e.g., `gfortran`)
- `make`
- [Gnuplot](http://www.gnuplot.info/) (optional, for plotting)

## Building

```bash
git clone https://github.com/cpadilla0327/meandertran.git
cd meandertran
make
```

## Usage

Set the Kinoshita parameters in the input file under `data/`.

Parameters are set in ranges for randomization of values using built-in random number generator.

For constant parameters, ranges should be set to equal values.

Run the generator (see the Makefile for the exact target/executable name).

Plot the results with Gnuplot:

```bash
gnuplot plot.gp
```
Multiple curves can be generated in a single run. Ideal for quick synthetic data generation.
