






# there is still quite a bit that needs to be done for this app
1. finish writing the app
	first and foremost the function to move samples to a new plate needs to be written
	also the app needs to be cleaned and debugged and error messages need to be created
	(upload samples prints completed even when sql encounters errors like not uniq errs)

2. versioning
	the app needs to be version controlled
	this can be accomplished by
	- using docker
	- uploading the app to shinyappsio
	-
	(ask max how he is dealing with versioning errors)

3. hosting
	the app needs to exist somewhere. and that somewhere needs to contain a version
	controlled sampleDB app.
	remote options:
	- AWS
	- google compute cloud
	- mysql
	- shinyappio

	local:
	- as an executable on windows desktop in lab
	- as an app that is sourced on start up and whose port is specified and is reliably accessible via the computer's browser


NOTE: making sure the app is
- version controlled
- accessible
- able to reliably interact with the database
- able to back up the database
- handling/preventing database manipulation by multiple users at once
