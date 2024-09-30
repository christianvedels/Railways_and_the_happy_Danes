# -*- coding: utf-8 -*-
"""
Created on Mon May 11 2024
Archival Picture Sentiments (APS)
Author: Tom Görges, Christian Vedel
Purpose: Detect objects (persons, ties, table, chair, etc.), their bounding box 
in the image, detect faces and finally recognize emotions of identified persons.
"""

from PIL import Image, ImageDraw, ImageFont
import os
import csv
import torch
import numpy as np
from transformers import DetrImageProcessor, DetrForObjectDetection
import face_recognition
import matplotlib.pyplot as plt
from fer import FER # Importing the FER library
import time
from yoloface import face_analysis
import random

# Load fonts for drawing text on images, use default if not found
try:
    font = ImageFont.truetype("arial.ttf", 30)
except IOError:
    font = ImageFont.load_default()
    
def draw_image(image):
    """
    Draws and displays an image using Matplotlib.
    
    Args:
        image (PIL.Image): The image to be displayed.
    """
    plt.figure(figsize=(10, 10))
    plt.imshow(image)
    plt.axis('off')  # Hide axes
    plt.show()

def dhms_string(x):
    # Convert x_seconds to days, hours, minutes, and seconds
    x_days = int(x // 86400)  # 1 day = 86400 seconds
    x_hours = int((x % 86400) // 3600)
    x_minutes = int((x % 3600) // 60)
    x_seconds = int(x % 60)
    x_str = f"{x_days}d {x_hours}h {x_minutes}m {x_seconds}s"
    
    return x_str

# Function to generate ETA string
def eta(i, start_time: float, cap_n: int) -> str:
    """
    Generates an ETA string based on the current progress.

    Parameters:
    i (int): Current iteration index.
    start_time (float): The start time of the process.
    capN (int): Total number of iterations.

    Returns:
    str: The ETA string.
    """
    elapsed_time = time.time() - start_time
    average_time_per_n = elapsed_time / (i+1)
    remaining_n = cap_n - (i+1)
    eta_seconds = remaining_n * average_time_per_n
        
    # Elapsed string
    elapsed_str = dhms_string(elapsed_time)

    # Convert eta_seconds to hours, minutes, and seconds
    eta_str = dhms_string(eta_seconds)

    # Convert total_seconds to hours, minutes, and seconds
    total_seconds = cap_n * average_time_per_n
    total_str = dhms_string(total_seconds)

    full_str = f"{i} of {cap_n}: Elapsed {elapsed_str}; ETA: {eta_str} of {total_str}"

    return full_str

class ImageDetector:
    """
    Class to handle image detection.
    """
    
    def __init__(self, image_path, annotated_image_path = None, batch_size=16, results_file_name='fer_results.csv',
                 verbose=True,
                 object_detection_thr = 0.9
                 ):
        """
        Initialize the ImageDetector class.
        
        Args:
            image_path (str): Path to the directory containing images to process.
            annotated_image_path (str): Path to the directory where annotated images will be saved. (Defaults to None)
            batch_size (int): Number of images to process in parallel. Default is 16.
            results_file_name (str): Filename for the results. Default is 'fer_results.csv'
            verbose (bool): Should updates be printed?
            object_detection_thr (float): Threshold to use for object detection
        """
                
        self.image_path = image_path
        self.annotated_image_path = annotated_image_path
        self.batch_size = batch_size
        self.csv_file = results_file_name
        self.verbose = verbose
        self.object_detection_thr = object_detection_thr
        self.face = face_analysis()
        
        self.device = 'cuda' if torch.cuda.is_available() else 'cpu'  # Autoloads cuda if available
        print(f"--> Using {self.device}")
        
        self.processor_det = DetrImageProcessor.from_pretrained("facebook/detr-resnet-50")
        self.model_det = DetrForObjectDetection.from_pretrained("facebook/detr-resnet-50").to(self.device)
        
        self.fer = FER(mtcnn=True)  # Initialize the FER library
        
        if self.annotated_image_path:
            self._ensure_directory_exists(self.annotated_image_path)
        
    def _ensure_directory_exists(self, path):
        """
        Ensure that a directory exists. If not, create it.
        
        Args:
            path (str): Path to the directory.
        """
        if not os.path.exists(path):
            os.makedirs(path)
            
        
    def _load_images_batch(self, image_files):
        """
        Load a batch of images from the specified files.
        
        Args:
            image_files (list): List of image filenames to load.
            
        Returns:
            tuple: A tuple containing lists of image objects, filenames, and draw objects.
        """
        images = []
        filenames = []
        draws = []
        
        for filename in image_files:
            image_path = os.path.join(self.image_path, filename)
            image = Image.open(image_path).convert("RGB")
            images.append(image)
            filenames.append(filename)
            draws.append(ImageDraw.Draw(image))
        
        return images, filenames, draws
    
    def _chunked_iterable(self, iterable, size):
        """
        Yield successive n-sized chunks from the iterable.
        
        Args:
            iterable (iterable): An iterable to chunk.
            size (int): The size of each chunk.
            
        Yields:
            iterable: A chunk of the original iterable.
        """
        for i in range(0, len(iterable), size):
            yield iterable[i:i + size]
    
    def _detect_objects_in_batch(self, images, filenames):
        """
        Process a batch of images.
        
        Args:
            images (list): A list of image objects to process.
            filenames (list): A list of image filenames.
        """
        inputs = self.processor_det(images=images, return_tensors="pt").to(self.device)
        outputs = self.model_det(**inputs)
        target_sizes = torch.tensor([image.size[::-1] for image in images]).to(self.device)
        results = self.processor_det.post_process_object_detection(outputs, target_sizes=target_sizes, threshold=self.object_detection_thr)
        
        objects_detected = []
        for i, result in enumerate(results):
            objects = self._process_object_detection(images[i], filenames[i], result)
            objects_detected.extend(objects)
               
        return objects_detected
    
            
    def _process_object_detection(self, image, filename, result):
        """
        Cleans the results for a single image
        
        Args:
            image (PIL.Image): The image to process.
            filename (str): The filename of the image.
            result (dict): The result from the object detection model.
        """
        
        clean_results = []
        
        for score, label, box in zip(result["scores"], result["labels"], result["boxes"]):
            detected_item = self.model_det.config.id2label[label.item()]
            object_box = [round(i, 2) for i in box.tolist()]
            
            res_i = {
                'Image': filename,
                'Detected': detected_item,
                'Object Location': str(object_box),
                'Confidence': score.item(),
            }
            
            clean_results.append(res_i)
        
        return clean_results
    
    def _detect_faces(self, image, object_box, detected_item):
        """
        Prepare face images for emotion detection if the detected item is a person.
        
        Args:
            image (PIL.Image): The original image.
            object_box (list): The bounding box of the detected object.
            detected_item (str): The detected item label.
        
        Returns:
            dict: A dictionary containing face detection and the cropped face image.
        """
        if detected_item != 'person':
            return {
                'Face Detected': "No",
                'Face Location': "NA",
                'Face Image': None
            }
    
        cropped_image = image.crop(object_box)
        # cropped_image.save("tmp.png")
        # img, box, conf = self.face.face_detection("tmp.png",model='full')
        face_locations = face_recognition.face_locations(np.array(cropped_image))
    
        if not face_locations:
            return {
                'Face Detected': "No",
                'Face Location': "NA",
                'Face Image': None
            }
    
        top, right, bottom, left = face_locations[0]
        face_box = [left + object_box[0], top + object_box[1], right + object_box[0], bottom + object_box[1]]
        face_image = cropped_image.crop((left, top, right, bottom))
    
        return {
            'Face Detected': "Yes",
            'Face Location': face_box,
            'Face Image': np.array(face_image)
        }
    
    def _fer_in_detected_objects(self, objects_detected, images, filenames, draws):
        """
        Process detected objects for facial emotion detection, annotation, and saving results.
        
        Args:
            objects_detected (list): A list of detected objects.
            images (list): A list of image objects.
            filenames (list): A list of image filenames.
            draws (list): A list of drawing contexts for the images.
        """
        face_images = []
        face_details = []
        
        fer_start_time = time.time()
        
        # Collect face images
        for obj in objects_detected:
            filename = obj['Image']
            detected_item = obj['Detected']
            object_box = eval(obj['Object Location'])  # Convert string back to list
            index = filenames.index(filename)
            image, draw = images[index], draws[index]
    
            face_details_dict = self._detect_faces(image, object_box, detected_item)
            obj.update({
                'Face Detected': face_details_dict['Face Detected'],
                'Face Location': face_details_dict['Face Location'],
                'Dominant Emotion': "NA",
                'Emotion Scores': {}
            })
            
            if face_details_dict['Face Detected'] == "Yes":
                face_images.append(face_details_dict['Face Image'])
                face_details.append((obj, image, draw, face_details_dict['Face Location']))
        
        if self.verbose:
            print(f"Face detection took: {dhms_string(time.time() - fer_start_time)}")
    
        # Process all face images in batch
        if face_images:
            emotions_batch = [self.fer.detect_emotions(x) for x in face_images]
            
            for i, face_emotion_results in enumerate(emotions_batch):
                if face_emotion_results:
                    emotion_scores = face_emotion_results[0]["emotions"]
                    dominant_emotion = max(emotion_scores, key=emotion_scores.get)
                    
                    obj, image, draw, face_box = face_details[i]
                    obj.update({
                        'Dominant Emotion': dominant_emotion,
                        'Emotion Scores': emotion_scores
                    })
    
                    # Draw bounding boxes and labels on the image
                    draw.rectangle(face_box, outline='blue', width=2)
                    draw.text((face_box[0], face_box[1] - 10), f"{dominant_emotion}: {round(emotion_scores[dominant_emotion], 2)}", fill='blue')
        
        if self.verbose:
            print(f"Emotion detection took: {dhms_string(time.time() - fer_start_time)}")
    
        # Draw object bounding boxes and labels
        for obj in objects_detected:
            filename = obj['Image']
            detected_item = obj['Detected']
            object_box = eval(obj['Object Location'])  # Convert string back to list
            index = filenames.index(filename)
            image, draw = images[index], draws[index]
            
            # Draw bounding boxes and labels on the image
            draw.rectangle(object_box, outline='red', width=2)
            draw.text((object_box[0], object_box[1] - 10), detected_item, fill='red')
            
            # Save the annotated image
            if self.annotated_image_path:
                annotated_image_path = os.path.join(self.annotated_image_path, f"annotated_{filename}")
                image.save(annotated_image_path)
    
        return objects_detected
    
    def _update_csv(self, results_batch):
        """
        Update CSV file with the results of the batch processing.
        
        Args:
            results_batch (list): List of dictionaries containing results to be written to CSV.
        """
        if not results_batch:
            return  # If results_batch is empty, do nothing
    
        file_exists = os.path.isfile(self.csv_file)
        
        with open(self.csv_file, mode='a', newline='') as file:
            # Assuming all dictionaries in results_batch have the same keys
            writer = csv.DictWriter(file, fieldnames=results_batch[0].keys())
            
            if not file_exists:
                writer.writeheader()  # Write header only if the file doesn't exist
            
            for result in results_batch:
                writer.writerow(result)
    
    def _already_processed(self):
        """
        Checks self.results_file_name and returns a list of filenames that are already processed.
        """
        processed_files = set()
        
        if os.path.exists(self.csv_file):
            with open(self.csv_file, mode='r') as file:
                reader = csv.DictReader(file)
                for row in reader:
                    processed_files.add(row['Image'])
                    
        return processed_files
    
    def run_aps(self, skip_already_analysed = True):
        """
        Main method of the class. Runs APS on images in 'image_path'.
        """
        image_files = [f for f in os.listdir(self.image_path) if f.lower().endswith(('.png', '.jpg', '.jpeg'))]
        
        if skip_already_analysed:
            processed_files = self._already_processed()
            image_files = [f for f in image_files if f not in processed_files]
        
        # Shuffle the order of images
        random.shuffle(image_files)
        
        start_time = time.time()
        counter = 0
                    
        for image_batch in self._chunked_iterable(image_files, self.batch_size):
            
            start_time_batch = time.time()
            # Step 1: Load files in batch
            images, filenames, draws = self._load_images_batch(image_batch)
            
            if self.verbose:
                print(f"Processing batch: {filenames}, Batch time: {dhms_string(time.time() - start_time_batch)}")
            
            # Step 2: Detect objects and process images
            objects_detected = self._detect_objects_in_batch(images, filenames)
            if self.verbose:
                print(f"--> Finished object detection in batch, Batch time: {dhms_string(time.time() - start_time_batch)}")
            
            # Step 3: Process detected objects
            results_batch = self._fer_in_detected_objects(objects_detected, images, filenames, draws)
            if self.verbose:
                print(f"--> Finished facial emotional recognition in batch, Batch time: {dhms_string(time.time() - start_time_batch)}")
            
            # Step 4
            self._update_csv(results_batch)
            if self.verbose:
                print(f"--> Writing results to csv, Batch time: {dhms_string(time.time() - start_time_batch)}")
                
            # Print ETA string
            if self.verbose:
                counter += self.batch_size
                print(eta(i = counter, start_time=start_time, cap_n = len(image_files)))
                

def run_aps_wrapper(image_path = "Example_images", annotated_image_path = None, batch_size = 2, results_file_name = 'fer_results.csv'):
    """
    image_path, 
    annotated_image_path
    Wrapper function to load class and excecute archical picture sentiments
    """
    detector = ImageDetector(
        image_path = image_path, 
        annotated_image_path = annotated_image_path, 
        batch_size = batch_size, 
        results_file_name = results_file_name
        )
    detector.run_aps()
    
if __name__ == '__main__':
    run_aps_wrapper(
        image_path = "D:/Dropbox/Research_projects/Railways/Data not redistributable/Arkiv.dk/Images",
        results_file_name = 'fer_results.csv'
        )