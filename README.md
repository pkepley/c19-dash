# c19-dash
A COVID-19 Dashboard that I've been maintaining at [https://www.paulkepley.com/shiny/covid-19/](https://www.paulkepley.com/shiny/covid-19/).

## Notes:

- The script `prep_data.R` pulls data from the [John Hopkins COVID-19 data repository](https://github.com/CSSEGISandData/COVID-19). In order to run this script, you will need to create a configuration file in this directory, called `config.R`, which defines two variables (assuming a *nix operating system):

    1. `jhu_data_dir`: the path of the directory where you have cloned the John Hopkins COVID-19 data repository.
    2. `app_data_dir`:  the path of the directory where you will save the data for the dashboard app.
	
- In my implementation, the script `update.sh` is run in a nightly cron job. Due to RAM limitations on this server, I must use swap to run the update script. Since this cloud provider recommends that you *do not* have swap constantly enabled, I turn swap on and off in this script. If you want to use this script for a similar cron job, you probably would want to comment out the swap file lines.
- The script `prep_data.R` currently generates rates at pull time. This is done so these can be read on page load as opposed to computing them on the fly (since the server I'm running on has limited computational resources too).
