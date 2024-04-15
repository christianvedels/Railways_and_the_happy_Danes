# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 07:44:29 2024

@author: Win7ADM
"""

import os
import csv
from transformers import DetrImageProcessor, DetrForObjectDetection
import torch
from PIL import Image

# Path to the directory containing this script
script_dir = os.path.dirname(__file__)

# Path to the folder containing the images, relative to the script's directory
folder_path = os.path.join(script_dir, "Example images")

# Load the image processor and model
processor = DetrImageProcessor.from_pretrained("facebook/detr-resnet-50", revision="no_timm")
model = DetrForObjectDetection.from_pretrained("facebook/detr-resnet-50", revision="no_timm")

# CSV file setup
csv_file = 'detection_results.csv'
fieldnames = ['Detected', 'Confidence', 'Location', 'Image']

# Open the CSV file for writing
with open(csv_file, mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=fieldnames)
    writer.writeheader()

    # Loop over all images in the directory
    for filename in os.listdir(folder_path):
        if filename.lower().endswith(('.png', '.jpg', '.jpeg', '.bmp', '.gif')):
            image_path = os.path.join(folder_path, filename)
            image = Image.open(image_path)

            inputs = processor(images=image, return_tensors="pt")
            outputs = model(**inputs)

            # Convert outputs (bounding boxes and class logits) to COCO API
            # Let's only keep detections with score > 0.9
            target_sizes = torch.tensor([image.size[::-1]])
            results = processor.post_process_object_detection(outputs, target_sizes=target_sizes, threshold=0.9)[0]

            for score, label, box in zip(results["scores"], results["labels"], results["boxes"]):
                box = [round(i, 2) for i in box.tolist()]
                # Write each result to the CSV
                writer.writerow({
                    'Detected': model.config.id2label[label.item()],
                    'Confidence': round(score.item(), 3),
                    'Location': str(box),
                    'Image': filename
                })

