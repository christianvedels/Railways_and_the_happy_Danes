# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 2024
Image Processing Script
Author: Win7ADM
Purpose: Detect objects (persons, ties, table, chair, etc.), their locations in the image,
recognize emotions of identified persons, and lastly sort out persons without faces (e.g., persons shown from behind).
"""

import os
import csv
#import pandas as pd
import torch
from transformers import DetrImageProcessor, DetrForObjectDetection, AutoImageProcessor, AutoModelForImageClassification
from PIL import Image
from huggingface_hub import hf_hub_download
from ultralytics import YOLO

# Base setup
script_dir = os.path.dirname(os.path.abspath(__file__))
folder_path = os.path.join(script_dir, "Example images")

# Load object detection model and processor
processor_det = DetrImageProcessor.from_pretrained("facebook/detr-resnet-50", revision="no_timm")
model_det = DetrForObjectDetection.from_pretrained("facebook/detr-resnet-50", revision="no_timm")

# Load emotion recognition model and processor
processor_emo = AutoImageProcessor.from_pretrained("dima806/facial_emotions_image_detection")
model_emo = AutoModelForImageClassification.from_pretrained("dima806/facial_emotions_image_detection")

# Load face detection model
model_path = hf_hub_download(repo_id="arnabdhar/YOLOv8-Face-Detection", filename="model.pt")
model_face = YOLO(model_path)

# CSV file setup
csv_file = os.path.join(script_dir, 'results_example_images.csv')
fieldnames = ['Detected', 'Confidence', 'Location', 'Image', 'Emotion', 'Face Detected']
with open(csv_file, mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=fieldnames)
    writer.writeheader()
    
    # Loop over all images in the directory
    for filename in os.listdir(folder_path):
        if filename.lower().endswith(('.png', '.jpg', '.jpeg', '.bmp', '.gif')):
            image_path = os.path.join(folder_path, filename)
            image = Image.open(image_path)
            
            # Object detection
            inputs = processor_det(images=image, return_tensors="pt")
            outputs = model_det(**inputs)
            target_sizes = torch.tensor([image.size[::-1]])
            results = processor_det.post_process_object_detection(outputs, target_sizes=target_sizes, threshold=0.9)[0]
            
            for score, label, box in zip(results["scores"], results["labels"], results["boxes"]):
                detected_item = model_det.config.id2label[label.item()]
                box = [round(i, 2) for i in box.tolist()]
                
                # Emotion recognition
                emotion = "N/A"
                if detected_item == 'person':
                    cropped_image = image.crop(box)
                    inputs_emo = processor_emo(images=cropped_image, return_tensors="pt")
                    outputs_emo = model_emo(**inputs_emo)
                    predictions_emo = torch.nn.functional.softmax(outputs_emo.logits, dim=-1)
                    emotion = model_emo.config.id2label[predictions_emo.argmax().item()]
                
                # Face detection
                face_detected = "N/A"
                if detected_item == 'person':
                    output_face = model_face(image.crop(box))
                    if len(output_face) > 0 and hasattr(output_face[0], 'boxes') and len(output_face[0].boxes) > 0:
                        face_detected = 1
                    else:
                        face_detected = 0 # No face detected, but the detected item is a person
                
                # Write results to CSV
                writer.writerow({
                    'Detected': detected_item,
                    'Confidence': round(score.item(), 3),
                    'Location': str(box),
                    'Image': filename,
                    'Emotion': emotion,
                    'Face Detected': face_detected
                })

print("Processing complete. Results are saved in:", csv_file)
