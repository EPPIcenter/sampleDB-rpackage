



# Version 0.0.1
1. DELETE PLATE IF IT IS EMPTY AFTER A MOVE
2. MOVE: SHOW US R WHAT TUBES ARE ORPHANED
3. MOVE FORM NEEDS A BUTTON TO CLEAR FORM
4. PLATE NAMES NEED TO BE CON TROLED - NO SPECIAL CHARS
5. FOR UPLOAD & MOVES WE NEED TO PROTECT AGAINST BAD WELL POSITIONS
6. FORMS NEED TO AUTOMATICALLY REFRESH AFTER SUBMISSION
7. PRINT OUT TO USER & HALT OPERATION IF UNI Q BARCODE CONSTRAINT FAILED
8. SHOW ERR & EXIT IF MOVE WAS ACCIDENTLY TO SAME PLATE
9. ALLOW USERS TO SEARCH BY In MULTIPLE VIDS (STUDY-SUBJECT-IDS) * MAKE THE GITHUB REPO PRIVATE
10. STANDARDIZE USER INPUT DATE FORMAT
11. THINK ABOUT HOW DATA CAN BE ARCHIVED - TALK W/ MAX MAX. THERE IS AN "EXHAUSED" COL IN THE DB THAT MIGHT SERVE THIS FUN
12. DO A CHECK THAT USES ARE NOT TRYING TO MOVE TUBES THAT ARE NOT YET IN THE DATABASE
13. THINK ABOUT HOW A DATASET CAN BE BACKED UP (AFTER EVERY TBL ADD (INCLUDING REF TABLES)
14. FOR BARCODES SEARCHING PRINT THE NUMBER OF TUBES THAT WERE AND WERE NOT FOUND
15. TEST THE REFERENCE FORMS THEY SHOULD PRODUCE MSGS WHEN UPDATE IS COMPLETE

# Version 0.0.0
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
