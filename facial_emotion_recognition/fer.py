# -*- coding: utf-8 -*-
"""
Created on Mon May 11 2024
Facial Emotion Recognition (FER) Script
Author: Tom Görges, Christian Vedel
Purpose: Detect objects (persons, ties, table, chair, etc.), their locations in the image, detect faces and finally
recognize emotions of identified persons.
"""

from PIL import Image, ImageDraw, ImageFont
import os
import csv
import torch
import numpy as np
from transformers import DetrImageProcessor, DetrForObjectDetection
# from hsemotion.facial_emotions import HSEmotionRecognizer
import face_recognition
import matplotlib.pyplot as plt
from fer import FER # Importing the FER library


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

class ImageDetector:
    """
    Class to handle image detection.
    """
    
    def __init__(self, image_path, annotated_image_path, batch_size=16, results_file_name='fer_results.csv',
                 emotions=['Anger', 'Contempt', 'Disgust', 'Fear', 'Happiness', 'Neutral', 'Sadness', 'Surprise'],
                 verbose=True,
                 object_detection_thr = 0.9
                 ):
        """
        Initialize the ImageDetector class.
        
        Args:
            image_path (str): Path to the directory containing images to process.
            annotated_image_path (str): Path to the directory where annotated images will be saved.
            batch_size (int): Number of images to process in parallel. Default is 16.
            results_file_name (str): Filename for the results. Default is 'fer_results.csv'
            emotions (list of str): Emotions to detect. Default is ['Anger', 'Contempt', 'Disgust', 'Fear', 'Happiness', 'Neutral', 'Sadness', 'Surprise']
            verbose (bool): Should updates be printed?
            object_detection_thr (float): Threshold to use for object detection
        """
        self.image_path = image_path
        self.annotated_image_path = annotated_image_path
        self.batch_size = batch_size
        self.csv_file = results_file_name
        self.emotions = emotions
        self.verbose = verbose
        self.object_detection_thr = object_detection_thr
        
        self.device = 'cuda' if torch.cuda.is_available() else 'cpu'  # Autoloads cuda if available
        
        self.processor_det = DetrImageProcessor.from_pretrained("facebook/detr-resnet-50")
        self.model_det = DetrForObjectDetection.from_pretrained("facebook/detr-resnet-50").to(self.device)
        
        self.fer = FER(mtcnn=True)  # Initialize the FER library
        self.fieldnames = ['Image', 'Detected', 'Object Location', 'Confidence', 'Face Detected', 'Face Location', 'Emotion'] + self.emotions
        
        # self.fer = HSEmotionRecognizer(model_name='enet_b0_8_best_afew', device=self.device)
        # self.fieldnames = ['Image', 'Detected', 'Object Location', 'Confidence', 'Face Detected', 'Face Location', 'Emotion'] + self.emotions
        
        self._ensure_directory_exists(self.annotated_image_path)
        self._setup_csv()
        
    def _ensure_directory_exists(self, path):
        """
        Ensure that a directory exists. If not, create it.
        
        Args:
            path (str): Path to the directory.
        """
        if not os.path.exists(path):
            os.makedirs(path)
            
    def _setup_csv(self):
        """
        Set up the CSV file for logging results.
        """
        with open(self.csv_file, mode='w', newline='') as file:
            writer = csv.DictWriter(file, fieldnames=self.fieldnames)
            writer.writeheader()
    
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
            writer (csv.DictWriter): The CSV writer to log results.
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
    
    def _detect_faces_and_emotions(self, image, object_box, detected_item):
        """
        Detect faces and recognize emotions in the given object box if the detected item is a person.
        
        Args:
            image (PIL.Image): The original image.
            object_box (list): The bounding box of the detected object.
            detected_item (str): The detected item label.
        
        Returns:
            dict: A dictionary containing face detection and emotion recognition results.
        """
        face_detected = "No"
        face_location = "NA"
        emotion = "NA"
        emotion_scores = {em: "NA" for em in self.emotions}

        if detected_item == 'person':
            cropped_image = image.crop(object_box)
            face_locations = face_recognition.face_locations(np.array(cropped_image), model = "cnn")
            if face_locations:
                face_detected = "Yes"
                top, right, bottom, left = face_locations[0]
                face_box = [left + object_box[0], top + object_box[1], right + object_box[0], bottom + object_box[1]]
                face_image = cropped_image.crop((left, top, right, bottom))
                emotion_analysis = self.fer.detect_emotions(face_image)
                if emotion_analysis:
                    emotion = max(emotion_analysis[0]["emotions"], key=emotion_analysis[0]["emotions"].get)
                    emotion_scores.update(emotion_analysis[0]["emotions"])
                
                return {
                    'Face Detected': face_detected,
                    'Face Location': face_box,
                    'Emotion': emotion,
                    **emotion_scores
                }
        
        return {
            'Face Detected': face_detected,
            'Face Location': face_location,
            'Emotion': emotion,
            **emotion_scores
        }
    
    # def _detect_faces_and_emotions(self, image, object_box, detected_item):
    #     """
    #     Detect faces and recognize emotions in the given object box if the detected item is a person.
        
    #     Args:
    #         image (PIL.Image): The original image.
    #         object_box (list): The bounding box of the detected object.
    #         detected_item (str): The detected item label.
        
    #     Returns:
    #         dict: A dictionary containing face detection and emotion recognition results.
    #     """
    #     face_detected = "No"
    #     face_location = "NA"
    #     emotion = "NA"
    #     emotion_scores = {em: "NA" for em in self.emotions}

    #     if detected_item == 'person':
    #         cropped_image = image.crop(object_box)
    #         cropped_image_array = np.array(cropped_image)
    #         face_locations = face_recognition.face_locations(cropped_image_array)
    #         if face_locations:
    #             face_detected = "Yes"
    #             top, right, bottom, left = face_locations[0]
    #             face_box = [left + object_box[0], top + object_box[1], right + object_box[0], bottom + object_box[1]]
    #             face_image = Image.fromarray(cropped_image_array[top:bottom, left:right])

    #             emotion, scores = self.fer.predict_emotions(np.array(face_image), logits=False)
    #             emotion_scores = dict(zip(self.emotions, scores))
    #             max_emotion = max(emotion_scores, key=emotion_scores.get)

    #             return {
    #                 'Face Detected': face_detected,
    #                 'Face Location': face_box,
    #                 'Emotion': max_emotion,
    #                 **emotion_scores
    #             }
        
    #     return {
    #         'Face Detected': face_detected,
    #         'Face Location': face_location,
    #         'Emotion': emotion,
    #         **emotion_scores
    #     }

    def _fer_in_detected_objects(self, objects_detected, images, filenames, draws, writer):
        """
        Process detected objects for facial emotion detection, annotation, and saving results.
        
        Args:
            objects_detected (list): A list of detected objects.
            images (list): A list of image objects.
            filenames (list): A list of image filenames.
            draws (list): A list of drawing contexts for the images.
            writer (csv.DictWriter): The CSV writer to log results.
        """
        for obj in objects_detected:
            filename = obj['Image']
            detected_item = obj['Detected']
            object_box = eval(obj['Object Location'])  # Convert string back to list
            image = images[filenames.index(filename)]
            draw = draws[filenames.index(filename)]
            
            face_emotion_results = self._detect_faces_and_emotions(image, object_box, detected_item)
            obj.update(face_emotion_results)
            
            # Draw bounding boxes and labels on the image
            draw.rectangle(object_box, outline='red', width=2)
            draw.text((object_box[0], object_box[1] - 10), detected_item, fill='red')
            
            if face_emotion_results['Face Detected'] == "Yes":
                face_box = face_emotion_results['Face Location']
                draw.rectangle(face_box, outline='blue', width=2)
                draw.text((face_box[0], face_box[1] - 10), f"{face_emotion_results['Emotion']}: {round(face_emotion_results[face_emotion_results['Emotion']], 2)}", fill='blue')
            
            writer.writerow(obj)
            
            # Save the annotated image
            annotated_image_path = os.path.join(self.annotated_image_path, f"annotated_{filename}")
            image.save(annotated_image_path)
    
    def run_fer(self):
        """
        Main method of the class. Runs FER on images in 'image_path'.
        """
        image_files = [f for f in os.listdir(self.image_path) if f.lower().endswith(('.png', '.jpg', '.jpeg'))]
        
        with open(self.csv_file, mode='a', newline='') as file:
            writer = csv.DictWriter(file, fieldnames=self.fieldnames)
            
            for image_batch in self._chunked_iterable(image_files, self.batch_size):
                # Step 1: Load files in batch
                images, filenames, draws = self._load_images_batch(image_batch)
                
                # Step 2: Detect objects and process images
                if self.verbose:
                    print(f"Processing batch: {filenames}")
                objects_detected = self._detect_objects_in_batch(images, filenames)
                
                # Step 3: Process detected objects
                self._fer_in_detected_objects(objects_detected, images, filenames, draws, writer)


    

if __name__ == '__main__': # This bit ensures that the script runs from its directory
    detector = ImageDetector("Example images", "Annotated Images", batch_size=2)
    detector.run_fer()

    # folder_path = "Example images"
    # annotated_images_dir = "Annotated Images"    
    # # Ensure the annotated images directory exists
    # if not os.path.exists(annotated_images_dir):
    #     os.makedirs(annotated_images_dir)
    
    # # Initialize models and processors for object detection
    # processor_det = DetrImageProcessor.from_pretrained("facebook/detr-resnet-50")
    # model_det = DetrForObjectDetection.from_pretrained("facebook/detr-resnet-50")
    
    # # Initialize the facial emotion recognition model
    # fer = HSEmotionRecognizer(model_name='enet_b0_8_best_afew', device='cpu')
    
    # # Define the emotions to be recognized
    # emotions = ['Anger', 'Contempt', 'Disgust', 'Fear', 'Happiness', 'Neutral', 'Sadness', 'Surprise']
    
    # # Setup CSV for results logging
    # csv_file = 'fer_results.csv'
    # fieldnames = ['Image', 'Detected', 'Object Location', 'Confidence', 'Face Detected', 'Face Location', 'Emotion'] + emotions
    
    # with open(csv_file, mode='w', newline='') as file:
    #     writer = csv.DictWriter(file, fieldnames=fieldnames)
    #     writer.writeheader()
    
    #     # Process each image in the folder
    #     for filename in os.listdir(folder_path):
    #         if filename.lower().endswith(('.png', '.jpg', '.jpeg')):
    #             image_path = os.path.join(folder_path, filename)
    #             image = Image.open(image_path)
    #             draw = ImageDraw.Draw(image)
    
    #             # Process image through object detection model
    #             inputs = processor_det(images=image, return_tensors="pt")
    #             outputs = model_det(**inputs)
    #             target_sizes = torch.tensor([image.size[::-1]])
    #             results = processor_det.post_process_object_detection(outputs, target_sizes=target_sizes, threshold=0.9)[0]
    
    #             for score, label, box in zip(results["scores"], results["labels"], results["boxes"]):
    #                 detected_item = model_det.config.id2label[label.item()]
    #                 object_box = [round(i, 2) for i in box.tolist()]
    #                 draw.rectangle(object_box, outline='red', width=2)
    #                 draw.text((object_box[0], object_box[1]-10), detected_item, fill='red', font=font)
    
    #                 # Default values for face and emotion detection
    #                 face_detected = "No"
    #                 face_location = "NA"
    #                 emotion = "NA"
    #                 emotion_scores = {em: "NA" for em in emotions}
    
    #                 # Check for persons and recognize emotions
    #                 if detected_item == 'person':
    #                     cropped_image = image.crop(object_box)
    #                     cropped_image_array = np.array(cropped_image)
    #                     face_locations = face_recognition.face_locations(cropped_image_array)
    #                     if face_locations:
    #                         face_detected = "Yes"
    #                         top, right, bottom, left = face_locations[0]
    #                         face_box = [left + object_box[0], top + object_box[1], right + object_box[0], bottom + object_box[1]]
    #                         draw.rectangle(face_box, outline='blue', width=2)
    #                         face_image = Image.fromarray(cropped_image_array[top:bottom, left:right])
    
    #                         emotion, scores = fer.predict_emotions(np.array(face_image), logits=False)
    #                         emotion_scores = dict(zip(emotions, scores))
    #                         max_emotion = max(emotion_scores, key=emotion_scores.get)
    #                         draw.text((face_box[0], face_box[1]-10), f"{max_emotion}: {round(emotion_scores[max_emotion], 2)}", fill='blue', font=font)
    
    #                         face_location = face_box
    #                         emotion = max_emotion
    
    #                 writer.writerow({
    #                     'Image': filename,
    #                     'Detected': detected_item,
    #                     'Object Location': str(object_box),
    #                     'Confidence': round(score.item(), 3),
    #                     'Face Detected': face_detected,
    #                     'Face Location': str(face_location),
    #                     'Emotion': emotion,
    #                     **emotion_scores
    #                 })
    
    #             # Save annotated image
    #             annotated_image_path = os.path.join(annotated_images_dir, f"annotated_{filename}")
    #             image.save(annotated_image_path)
    
    # print("Processing complete. Results are saved in the CSV file and annotated images are saved in the Annotated Images folder.")
