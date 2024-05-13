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

### CPU install
```
conda install pytorch torchvision torchaudio cpuonly -c pytorch
```

### GPU install 
```
conda install pytorch torchvision torchaudio pytorch-cuda=11.8 -c pytorch -c nvidia
```

```
conda install -c huggingface transformers
```

```
pip install hsemotion
```

```
conda config --add channels conda-forge
```

```
conda config --set channel_priority strict
```

```
conda install dlib
```

```
pip install face-recognition
```

