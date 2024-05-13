# How to set up the environment

```
conda create -name happy_danes python=3.8
```

```
conda activate happy_danes
```

```
conda install pillow
```

## PyTorch

#### CPU install
```
conda install pytorch torchvision torchaudio cpuonly -c pytorch
```

#### GPU install 
```
conda install pytorch torchvision torchaudio pytorch-cuda=11.8 -c pytorch -c nvidia
```

## transformers

```
conda install -c huggingface transformers
```

## Emotion detection

```
pip install hsemotion
```

## Misc

```
conda config --add channels conda-forge
```

```
conda config --set channel_priority strict
```

```
conda install dlib
```

## Face recognition

```
pip install face-recognition
```

