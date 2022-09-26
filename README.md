# Kongoh

Kongoh (named after the Japanese word “mixture”) is open-source software for DNA evidence interpretation
based on a quantitative continuous model. The software is a graphical user interface and has been released as the R-package Kongoh (>= ver. 3.2.0).

## Getting started

* 1.  Ensure that R software is installed. It is available from the R Development Core Team website (http://www.R-project.org).
* 2.  Begin an R session.
* 3.  Execute the following command in R to install required packages.
```r
install.packages(c("tcltk2", "gtools", "truncnorm", "GenSA"))
```
* 4.  Go to https://github.com/manabe0322/Kongoh/releases.
* 5.  Download ”Kongoh_3.2.0.zip”.
* 6.  Install ”Kongoh_3.2.0.zip” from ”Install package(s) from local files...” in the R session.
* 7.  Execute the following commands in R to start GUI.

```r
library(Kongoh)
Kongoh()
```
## References

Manabe S, Morimoto C, Hamano Y, Fujimoto S, Tamaki K. Development and validation of open-source software for DNA mixture interpretation based on a quantitative continuous model. PLOS ONE 2017;12(11):e0188183.

Manabe S, Hamano Y, Morimoto C, Kawai C, Fujimoto S, Tamaki K. New stutter ratio distribution for DNA mixture interpretation based on a continuous model. Leg Med. 2016;19:16-21.

Manabe S, Fujii K, Fukagawa T, Mizuno N, Sekiguchi K, Inoue K, Hashiyada M, Akane A, Tamaki K. Evaluation of probability distribution models for stutter ratios in the typing system of GlobalFiler and 3500xL Genetic Analyzer. Leg Med. 2021;52:101906.

Manabe S, Fujii K, Fukagawa T, Mizuno N, Sekiguchi K, Inoue K, Hashiyada M, Akane A, Tamaki K. Corrigendum to "Evaluation of probability distribution models for stutter ratios in the typing system of GlobalFiler and 3500xL Genetic Analyzer" [Leg. Med. 52 (2021) 101906]. Leg Med. 2022;54:101984.

Manabe S, Fukagawa T, Fujii K, Mizuno N, Sekiguchi K, Akane A, Tamaki K. Development and validation of Kongoh ver. 3.0.1: Open-source software for
DNA mixture interpretation in the GlobalFiler system based on a quantitative continuous model. Leg Med. 2022;54:101972.

## License

GPL-3
