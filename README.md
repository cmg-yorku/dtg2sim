# dtg2Sim: Enabling simulation of and reinforcement learning with DT-Golog domains

`dtg2Sim` is a tool that allows simulations of domains specified in DT-Golog for the purpose of training stable_baselines2 Reinforcement Learning (RL) agents. The tool reads a DT-Golog specification, minimally augmented with configuration information, and avails it for simulation and training.
## Prerequisites
- Python 3.9 (required) - This project has been tested with Python 3.9 and may not work with other versions
- SWI-Prolog (for running the DT-Golog code)
- Git
## Installation and Setup Instructions

To ensure a reproducible environment, it is recommended to use a Python virtual environment. Follow these steps:

1. **Clone the repository locally.**
   ```bash
   git clone <this-repository-url>
   cd dtg2sim
   ```

2. **Verify Python version and create virtual environment:**
   ```bash
   # Check Python version
   python3.9 --version  # Should show Python 3.9.x
   
   # Create virtual environment with Python 3.9
   python3.9 -m venv venv
   
   # Activate virtual environment
   source venv/bin/activate  # On Linux/Mac
   # OR
   venv\Scripts\activate     # On Windows
   ```

3. **Install the required dependencies:**
   ```bash
   # First, install specific versions of pip, setuptools, and wheel
   python3.9 -m pip install pip==21.0.0 setuptools==65.5.0 wheel==0.38.0
   
   # Then install project dependencies
   python3.9 -m pip install -r requirements.txt
   ```

   This will install all necessary packages with the correct versions for this project.

4. **Acquire the DT-Golog code** from [its creator's page](https://www.cs.ryerson.ca/~mes/publications/appendix/appendixC/dtgolog), and place it in a file called `DT-Golog.pl` under `/scripts/QE/`

5. **Make the following changes so that it runs on SWI-Prolog**
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

# Usage

Load the environment as follows:

```
python -i main.py [DT-Golog Spec]
```

for example:

```
python -i main.py ./src/Order.pl
```

You will be greeted with:

```
Loaded Specification: ./src/Order.pl
Access dtg2sim via varibale c, GMEnv via g, and QueryEngine via e.

To simulate..: c.simulate()
To train.....: c.train()
To debug.....: g.reset() followed by repeated g.step(agentAction,[stochasticAction]).

For parameter info: help([dtg2sim or GMEnv].[method])
```

Follow these directions to simulate or train the spec.
# See also

The (extended) DT-Golog will typically be the result of a translation from iStarDT-X, an XML based language for representing iStar-DT domains. Please visit the dtx2dtg repository for more information.
# Contact

Please send questions, issues, bugs, and recommendations to [liaskos@yorku.ca](mailto:liaskos@yorku.ca?Subject=RLGen).


