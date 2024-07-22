--- Add shipped status ---

INSERT OR ROLLBACK INTO "status" (name)
VALUES
	("Shipped"); --! COMMAND_END !--

INSERT OR ROLLBACK INTO "state_status_relationship" ("status_id", "state_id", "default") 
VALUES 
	(5, 2, FALSE); --! COMMAND_END !--

INSERT OR ROLLBACK INTO version (name) VALUES ('1.4.4'); --! COMMAND_END !--
