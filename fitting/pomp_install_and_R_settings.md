To install POMP from its source code

    install.packages(c('devtools','curl'))
    require(devtools)
    install_github("kingaa/pomp")


Some possible problems:

- Dependencies cannot be resolved
aptitude allow to downgrade some programas, use this option.

- Library not found to install `deSolve` dependency
If you have problems with blas, try

`ld -lblas --verbose`

it will show you where ld is looking for. Then you can make a 
symbolic link to one of the path where ld looks for, in my case works

`ln -s /usr/lib/libblas.so.3 /usr/i686-linux-gnu/lib32/libblas.so`

and similar errors
`ln -s /usr/lib/lapack/liblapack.so.3.0 /usr/i686-linux-gnu/lib32/liblapack.so`

Setting R 

If is your first time installing R in your computer, it is very probably that you need this package

        install.packages("ggplot2")
        
