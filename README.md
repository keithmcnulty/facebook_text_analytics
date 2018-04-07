# facebook_text_analytics
Performs TF-IDF and Topic Modelling on downloaded Facebook timeline

Installations instructions on MacOS:

1. brew install R
2. brew install pandoc
3. install gsl library according to this quick guide: https://dyerlab.ces.vcu.edu/2015/07/29/compiling-the-gsl-library-for-osx/
4. Edit line 46 to point to your unzipped facebook data folder
5. Run R CMD BATCH facebook_text_analytics.R
6. To see the progress, run from another terminal window tail -f facebook_text_analytics.Rout (same folder as script)


If execution fails on "to must be a finite number", comment out the following 3 lines in the script:

```
jpeg("activity_by_month_scatter.jpeg")
plot(p1)
dev.off()
```
