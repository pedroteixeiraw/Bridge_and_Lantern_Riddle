# The Bridge and Lantern Riddle

This project focuses on developing a **Variational Quantum Circuit** capable of performing **Binary Classification** between two classes: **red wine** and **white wine**, based on their characteristics using machine learning. Our system consists of both quantum and classical components.  

- **Quantum Component (Quantum Circuit):**  
  A block responsible for encoding/embedding, a block responsible for the variational algorithm, and measurement blocks.  

- **Classical Component:**  
  Post-processing (associating measurements with labels), loss function computation, and optimization of variational parameters (θ). In some cases, special classical preprocessing may also be necessary, such as dimensionality reduction of the dataset using PCA techniques.  

**Python Stack:**  qiskit, json, numpy, pandas, sklearn, itertools, matplotlib, qiskit_machine_learning, sys
