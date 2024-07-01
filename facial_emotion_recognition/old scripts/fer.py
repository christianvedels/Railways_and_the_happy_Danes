# -*- coding: utf-8 -*-
"""
Created on Mon May 11 2024
Facial Emotion Recognition (FER) Script
Author: Tom Görges
Purpose: Detect objects (persons, ties, table, chair, etc.), their locations in the image, detect faces and finally
recognize emotions of identified persons.
"""

from PIL import Image, ImageDraw, ImageFont
import os
import csv
import torch
import numpy as np
from transformers import DetrImageProcessor, DetrForObjectDetection
from hsemotion.facial_emotions import HSEmotionRecognizer
import face_recognition

# Load fonts for drawing text on images, use default if not found
try:
    font = ImageFont.truetype("arial.ttf", 30)
except IOError:
    font = ImageFont.load_default()

# Setup directories
script_dir = os.path.dirname(os.path.abspath(__file__))
folder_path = os.path.join(script_dir, os.pardir, os.pardir, "Data not redistributable", "Arkiv.dk", "Images")

# Move up two levels and navigate to the specified folder
annotated_images_dir = os.path.join(script_dir, os.pardir, os.pardir, "Data not redistributable", "Arkiv.dk", "Annotated Images")

# Ensure the annotated images directory exists
if not os.path.exists(annotated_images_dir):
    os.makedirs(annotated_images_dir)

# Load exclusion list from placeholder.csv
placeholder_csv = os.path.join(script_dir, os.pardir, os.pardir, "Data not redistributable", "Arkiv.dk", "placeholder.csv")
exclusion_list = []
with open(placeholder_csv, mode='r', newline='', encoding='utf-8') as file:
    reader = csv.DictReader(file)
    for row in reader:
        exclusion_list.append(row['id'])

# Initialize models and processors for object detection
processor_det = DetrImageProcessor.from_pretrained("facebook/detr-resnet-50")
model_det = DetrForObjectDetection.from_pretrained("facebook/detr-resnet-50")

# Initialize the facial emotion recognition model
fer = HSEmotionRecognizer(model_name='enet_b0_8_best_afew', device='cpu')

# Define the emotions to be recognized
emotions = ['Anger', 'Contempt', 'Disgust', 'Fear', 'Happiness', 'Neutral', 'Sadness', 'Surprise']

# Setup CSV for results logging
csv_file = os.path.join(script_dir, 'fer_results.csv')
fieldnames = ['Image', 'Detected', 'Object Location', 'Confidence', 'Face Detected', 'Face Location', 'Emotion'] + emotions

with open(csv_file, mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=fieldnames)
    writer.writeheader()

    # Process each image which is not a placeholder in the folder
    for filename in os.listdir(folder_path):
        if filename.lower().endswith(('.png', '.jpg', '.jpeg')) and filename not in exclusion_list: # exclusion of placeholder images
            image_path = os.path.join(folder_path, filename)
            image = Image.open(image_path).convert("RGB")  # Convert image to RGB
            draw = ImageDraw.Draw(image)

            # Process image through object detection model
            inputs = processor_det(images=image, return_tensors="pt")
            outputs = model_det(**inputs)
            target_sizes = torch.tensor([image.size[::-1]])
            results = processor_det.post_process_object_detection(outputs, target_sizes=target_sizes, threshold=0.9)[0]

            for score, label, box in zip(results["scores"], results["labels"], results["boxes"]):
                detected_item = model_det.config.id2label[label.item()]
                object_box = [round(i, 2) for i in box.tolist()]
                draw.rectangle(object_box, outline='red', width=2)
                draw.text((object_box[0], object_box[1]-10), detected_item, fill='red', font=font)

                # Default values for face and emotion detection
                face_detected = "No"
                face_location = "NA"
                emotion = "NA"
                emotion_scores = {em: "NA" for em in emotions}

                # Check for persons and recognize emotions
                if detected_item == 'person':
                    cropped_image = image.crop(object_box)
                    cropped_image_array = np.array(cropped_image)
                    face_locations = face_recognition.face_locations(cropped_image_array)
                    if face_locations:
                        face_detected = "Yes"
                        top, right, bottom, left = face_locations[0]
                        face_box = [left + object_box[0], top + object_box[1], right + object_box[0], bottom + object_box[1]]
                        draw.rectangle(face_box, outline='blue', width=2)
                        face_image = Image.fromarray(cropped_image_array[top:bottom, left:right])

                        emotion, scores = fer.predict_emotions(np.array(face_image), logits=False)
                        emotion_scores = dict(zip(emotions, scores))
                        max_emotion = max(emotion_scores, key=emotion_scores.get)
                        draw.text((face_box[0], face_box[1]-10), f"{max_emotion}: {round(emotion_scores[max_emotion], 2)}", fill='blue', font=font)

                        face_location = face_box
                        emotion = max_emotion

                writer.writerow({
                    'Image': filename,
                    'Detected': detected_item,
                    'Object Location': str(object_box),
                    'Confidence': round(score.item(), 3),
                    'Face Detected': face_detected,
                    'Face Location': str(face_location),
                    'Emotion': emotion,
                    **emotion_scores
                })

            # Save annotated image
            annotated_image_path = os.path.join(annotated_images_dir, f"annotated_{filename}")
            image.save(annotated_image_path)

print("Processing complete. Results are saved in the CSV file and annotated images are saved in the Annotated Images folder.")
