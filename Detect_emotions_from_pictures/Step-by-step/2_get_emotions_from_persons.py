# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 07:56:18 2024

@author: Win7ADM
"""
import os
import pandas as pd
from PIL import Image
import torch
from transformers import AutoImageProcessor, AutoModelForImageClassification

# Set up base directory relative to the script file
script_dir = os.path.dirname(os.path.abspath(__file__))

# Load the CSV file using the relative path
csv_path = os.path.join(script_dir, "detection_results.csv")
df = pd.read_csv(csv_path)

# Initialize the model and processor
processor = AutoImageProcessor.from_pretrained("dima806/facial_emotions_image_detection")
model = AutoModelForImageClassification.from_pretrained("dima806/facial_emotions_image_detection")

# Function to process and predict emotion on a person's bounding box
def predict_emotion(image_path, bbox):
    image = Image.open(image_path).convert('RGB')
    # Convert string bbox to tuple and apply
    bbox = tuple(map(float, bbox.strip('[]').split(',')))
    cropped_image = image.crop(bbox)
    inputs = processor(images=cropped_image, return_tensors="pt")
    outputs = model(**inputs)
    predictions = torch.nn.functional.softmax(outputs.logits, dim=-1)
    predicted_class_idx = predictions.argmax().item()
    return model.config.id2label[predicted_class_idx]

# Apply the model to detected persons and store results
df['Emotion'] = None  # Initialize the column with None
for index, row in df.iterrows():
    if row['Detected'] == 'person':
        # Use the relative path for the image
        image_path = os.path.join(script_dir, "..", 'Example images', row["Image"])
        emotion = predict_emotion(image_path, row['Location'])
        df.at[index, 'Emotion'] = emotion


# Save the modified dataframe to a new CSV file using a relative path
df.to_csv(os.path.join(script_dir, 'detection_and_emotion_results.csv'), index=False)


print(df.head())
