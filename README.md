# us.mapper
Install dependencies
```
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt-get update
sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev
```

### Features
  - `match_city_to_fips` : Checks a city has a FIPs code and appends vector to data.frame, if city can't be found, then a 0 is used.

