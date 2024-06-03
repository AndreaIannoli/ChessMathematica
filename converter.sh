#!/bin/bash

# Check if Python is installed
if ! command -v python &> /dev/null; then # If Python is not installed then install it
    echo "Python is not installed. Installing Python..." # Display message for installing Python
    # Install Python
    sudo apt-get update # Update the package list
    sudo apt-get install python -y # Install Python
fi

# Install pandas
echo "Installing pandas..."
pip install pandas

# Install pychess
echo "Installing pychess..."
pip install pychess

# Execute movesConverter.py
echo "Executing movesConverter.py..."
python movesConverter.py