import os
import json
import logging
import sqlite3
import paramiko
from flask import Flask, request, jsonify
from apscheduler.schedulers.background import BackgroundScheduler

app = Flask(__name__)

host_key_file_path = "/home/backupuser/host_keys"
current_config = None
scheduler = BackgroundScheduler()

class Config:
    def __init__(self, database_path, sftp_host, sftp_port, sftp_user, sftp_destination, schedule):
        self.database_path = database_path
        self.sftp_host = sftp_host
        self.sftp_port = sftp_port
        self.sftp_user = sftp_user
        self.sftp_destination = sftp_destination
        self.schedule = schedule

@app.route('/update-config', methods=['POST'])
def update_config():
    global current_config
    try:
        data = request.json
        logging.info(f"Received data: {data}")
        sftp_password = request.headers.get('X-SFTP-Password')

        current_config = Config(**data)

        # Parse the schedule and check if it's valid
        schedule_dict = parse_schedule(current_config.schedule)
        if not schedule_dict:
            raise ValueError("Invalid schedule format")

        # Restart backup schedule with new config
        scheduler.remove_all_jobs()
        scheduler.add_job(func=perform_backup, trigger='cron', **schedule_dict, args=[sftp_password])
        return jsonify(data)

    except Exception as e:
        logging.error(f"Error in update_config: {e}")
        return jsonify({"error": str(e)}), 500


@app.route('/get-status', methods=['GET'])
def get_status():
    jobs = scheduler.get_jobs()
    status = {
        "running": len(jobs) > 0,
        "message": "Service is running" if jobs else "No backup scheduled"
    }
    return jsonify(status)

@app.route('/update-action', methods=['POST'])
def update_action():
    data = request.json
    action = data.get('action')
    sftp_password = request.headers.get('X-SFTP-Password')

    if action == 'start':
        scheduler.start()
    elif action == 'stop':
        scheduler.shutdown()
    elif action == 'restart':
        scheduler.shutdown()
        scheduler.start()
    else:
        return "Invalid action", 400

    return "Action performed successfully"

def perform_backup(sftp_password):
    logging.info("Performing backup...")
    try:
        with tempfile.NamedTemporaryFile(delete=False) as temp_file:
            backup_database(current_config.database_path, temp_file.name)
            upload_to_sftp(temp_file.name, current_config.sftp_destination, sftp_password)
            logging.info("Backup completed successfully")
        os.remove(temp_file.name)  # Delete the temporary file after successful transfer
    except Exception as e:
        logging.error("Backup failed: %s", str(e))


def backup_database(db_path, backup_path):
    con = sqlite3.connect(db_path)
    bck = sqlite3.connect(f'file:{backup_path}?mode=memory&cache=shared', uri=True)
    with bck:
        con.backup(bck)
    bck.close()
    con.close()

def upload_to_sftp(local_file, remote_path, password):
    host_key = get_stored_host_key(current_config.sftp_host)
    transport = paramiko.Transport((current_config.sftp_host, current_config.sftp_port))
    transport.connect(username=current_config.sftp_user, password=password, hostkey=host_key)
    sftp = paramiko.SFTPClient.from_transport(transport)

    with open(local_file, 'rb') as file:
        sftp.putfo(file, remote_path)

    sftp.close()
    transport.close()

def get_stored_host_key(hostname):
    with open(host_key_file_path, 'r') as file:
        for line in file:
            host, key_type, key = line.strip().split()
            if host == hostname:
                return paramiko.RSAKey(data=decodebytes(key.encode()))

def parse_schedule(cron_str):
    """
    Parses a cron schedule string into a dictionary.
    Example cron string: "0 15 * * *"
    """
    if not cron_str:
        return None

    try:
        parts = cron_str.split()
        return {
            'minute': parts[0],
            'hour': parts[1],
            'day': parts[2],
            'month': parts[3],
            'day_of_week': parts[4]
        }
    except Exception as e:
        logging.error(f"Error parsing cron string: {cron_str} - {e}")
        return None


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    scheduler.start()
    app.run(host='0.0.0.0', port=8081)
