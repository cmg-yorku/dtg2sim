# RL Gen
Title: _Tool and Reproducibility Package for: Model-driven Design and Generation of Training Simulators for Reinforcement Learning_



## Overview
This repository contains the scripts and examples that accompany the conference submission "Model-driven Design and Generation of Domain Simulators for Reinforcement Learning". 

The python scripts implementing GMEnv and and Query Interface can be found in `/scripts`
The example goal models and various tests and experiments against them can be found in `/examples`


## Installation and Execution Instructions

The scripts have been tested using the following versions of the python interpreter and key libraries:

* `python version 3.9.12`
* `stable-baselines3 1.5.0`
* `pyswip 0.2.11`

Directions to run the test and experiments:

* Clone the repository locally.
* Acquire the DT-Golog code from [its creator's page](https://www.cs.ryerson.ca/~mes/publications/appendix/appendixC/dtgolog), and place it in a file called `DT-Golog.pl` under `/scripts/QE/`
* Make the following changes so that it runs on SWI-Prolog
  
  - Comment out the following:
    ```
    /* :- pragma(debug).  */
    ```
  - Add:
    ```
    :- op(900, fy, [not]).
    (not X) := (\+ X).
    cputime(5).
    ```
* Run the test and trial scripts in the `/examples` folder.

## The Models

* Several models have been developed for tests and experiments. They can be reviewed in GoalModels.drawio which can be opened using https://app.diagrams.net/
* The corresponding specifications can be found in the `.pl` file in the `/examples` folder. The same model may have both a discrete and a continuous implementation (which are different only in one line of code whereby the state space is specified).
* Files named `[XXX]_Tests.py` contain simple python `unittest` tests.
* Files named `[XXX]_Trials.py` contain simulation and learning experiments. 
  * For running simulations or learning be sure to give meaningful iteration numbers to `simRandomIter`, `simOptimalRandomIter`, `trainingIter` (number of training steps) `testingIter` (number of testing episodes). `10,000` is a good number to start with.
  * `learningAlgorithm` can be one of `A2C`, `PPO`, or `DQN` implemented as part of [stable-baselines3](https://stable-baselines3.readthedocs.io/en/master/guide/algos.html)


# Contact

Please send questions, issues, bugs and recommendations to [liaskos@yorku.ca](mailto:liaskos@yorku.ca?Subject=RLGen).

  
    

    

