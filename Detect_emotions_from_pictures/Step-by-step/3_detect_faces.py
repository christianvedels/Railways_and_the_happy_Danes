# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 11:11:33 2024

@author: Win7ADM

Purpose: Sort out persons shown from behind.
"""

# load libraries
import pandas as pd
from PIL import Image
import os
from huggingface_hub import hf_hub_download
from ultralytics import YOLO

# Set up base directory and model
script_dir = os.path.dirname(os.path.abspath(__file__))
model_path = hf_hub_download(repo_id="arnabdhar/YOLOv8-Face-Detection", filename="model.pt")
face_model = YOLO(model_path)  # Assuming the model is trained to detect faces

# Load the CSV
df = pd.read_csv(os.path.join(script_dir, 'detection_and_emotion_results.csv'))

# Filter for persons
persons = df[df['Detected'] == 'person'].copy()

# Define a function to process each person entry
def detect_face(row):
    image_path = os.path.join(script_dir, "..", 'Example images', row['Image'])
    image = Image.open(image_path)

    # Extract coordinates and apply bounding box
    coords = eval(row['Location'])
    cropped_image = image.crop((coords[0], coords[1], coords[2], coords[3]))

    # Run face detection model
    output = face_model(cropped_image)

    # Check if any faces are detected by accessing the correct attribute
    if len(output) > 0 and hasattr(output[0], 'boxes') and len(output[0].boxes) > 0:
        return 1
    return 0

# Apply the detection function
persons['Face Detected'] = persons.apply(detect_face, axis=1)

# If 'Image' isn't unique, consider adding index as an auxiliary merging column
df['index_col'] = df.index
persons['index_col'] = persons.index

# Merge results back into the original DataFrame using an auxiliary index if needed
df = df.merge(persons[['index_col', 'Face Detected']], on='index_col', how='left')

# Clean up after merge
df.drop('index_col', axis=1, inplace=True)

# Save the updated DataFrame
df.to_csv(os.path.join(script_dir, 'detection_and_emotion_and_faces.csv'), index=False)

