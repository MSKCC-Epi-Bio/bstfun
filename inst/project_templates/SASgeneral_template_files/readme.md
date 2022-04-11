# {{folder_name}}

## Symbolic Link to Secure Data
Not necessary for SAS, but useful in case you want to use R at any point in this project.
{{symbolic_link}}

## Data Version Control
- Save data in a secure folder on a network drive with a subfolder indicating date data was received.
  - e.g. `"H:/ ... /Project Folder/secure_data/{{Sys.Date()}}/data_set.csv"`
- The file `data_date.txt` refers to the date folder in the `"secure_data"` folder. When new data arrives, save it in a new date folder and update `data_date.txt`.
- Use `bstfun::here_data()` to locate current data folder based on `data_date.txt` in R.
- The SAS programs will locate the current data folder based on `data_date.txt`.

## Getting R Template Files
If you want to use R for this project, you can write a single file from any [bstfun](https://www.danieldsjoberg.com/bstfun/index.html) project template to your project folder using the [use_bst_file](https://www.danieldsjoberg.com/bstfun/reference/use_file.html) function. Project template files can be viewed [here](https://github.com/ddsjoberg/bstfun/tree/master/inst/project_templates)

## Project Log
**{{Sys.Date()}}**  
Created project folder
