## IMDb Movies Data Explorer

This Shiny App is for searching through the Movies data set information from year 1893 to 2005.    
The total number of set records is 58,788 and number of variables is 24.  

### How to use this application

In this application you can view the movies' data by filtering the data with following slider bars, combo box and checkboxes:

1. **IMDb Rating** - choose the range of rating from 1 to 10 (a default range is 7 to 10).

2. **Years** - choose the range of years' releases from 1893 to 2005 (a default range is 1990-2005).  

3. **Number of Votes** - choose the range of number of votes (a default range is from 10,000 to max).  

4. **MPAA Rating** - choose one of a values of the rating (a default value is empty).  

5. **Genres** - choose the genres of movies (no one of the genres is checked by default).

You will see appropriate data in *Dataset* tab immediately with doing your choices.


### Movie information and user ratings from IMDB.com.

**Description**

The internet movie database, http://imdb.com/, is a website devoted to collecting movie data supplied by studios and fans. It claims to be the biggest movie database on the web and is run by amazon. More about information imdb.com can be found online, http://imdb.com/help/show_leaf?about, including information about the data collection process, http://imdb.com/help/show_leaf?infosource.

**Details**

Movies were selected for inclusion if they had a known length and had been rated by at least one imdb user. The data set contains the following fields:
  
**title** - Title of the movie.

**year** - Year of release.

**budget** - Total budget (if known) in US dollars

**length** - Length in minutes.

**rating** - Average IMDB user rating.

**votes** - Number of IMDB users who rated this movie.

**r1-10** - Multiplying by ten gives percentile (to nearest 10%) of users who rated this movie a 1. **Note**: I don't use these variables in my application.

**mpaa** - MPAA rating.

**Action, Animation, Comedy, Drama, Documentary, Romance, Short** - Binary variables representing if movie was classified as belonging to that genre.

### References  
  http://had.co.nz/data/movies/
  
