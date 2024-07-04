# Setting up the environment of 'facial emotion recognition'

### Create environment
```
conda create --name happy_danes python=3.8
conda activate happy_danes
```

### PyTorch install:
*CPU version:*
```
conda install pytorch torchvision torchaudio cpuonly -c pytorch
```

*GPU version:*
```
conda install pytorch torchvision torchaudio pytorch-cuda=11.8 -c pytorch -c nvidia
```

### Other libraries
```
conda install pillow
conda install -c huggingface transformers
pip install hsemotion # Might not use
conda install dlib
pip install face-recognition
conda install spyder
conda install matplotlib
pip install fer
pip install tensorflow
```



### Misc
```
conda config --add channels conda-forge
conda config --set channel_priority strict
```