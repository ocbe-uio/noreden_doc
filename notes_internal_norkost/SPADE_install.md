To install the package 

```R
install.packages('~/Desktop/SPADE.RIVM', repos=NULL, type = 'source')
```



Need to reinstall gfortran

```bash
brew install gcc
```



Then moved the entire folder (`/usr/local/opt/gfortran`) to `/opt/gfortran/` since the latter is where SPADE looks for it

 

### Alternative (same outcome)

https://blog.cynkra.com/posts/2021-03-16-gfortran-macos/

create `Makevar` that specifies the path, however the compilation fails.
