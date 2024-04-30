# Use an official Python runtime as a parent image
FROM python:3.10-slim

# Set the working directory in the container
WORKDIR /usr/src/app

# Copy the current directory contents into the container at /usr/src/app
COPY sdb-backup-service.py ./

# Install any needed packages specified in requirements.txt
RUN pip install --no-cache-dir flask paramiko APScheduler

# Make port 8081 available to the world outside this container
EXPOSE 8081

# Define environment variable
ENV NAME BackupService

# Run sdb-backup-service.py when the container launches
CMD ["python", "./sdb-backup-service.py"]
