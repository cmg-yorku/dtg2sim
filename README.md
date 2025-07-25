# dtg2Sim: Enabling simulation of and reinforcement learning with DT-Golog domains

`dtg2Sim` is a tool that allows simulations of domains specified in DT-Golog for the purpose of training stable_baselines2 Reinforcement Learning (RL) agents. The tool reads a DT-Golog specification, minimally augmented with configuration information, and avails it for simulation and training.
## Prerequisites
- Python 3.13 (latest)
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
python --version  # Should show Python 3.13.x
   
# Create virtual environment with Python 3.9
python -m venv venv
   
# Activate virtual environment
source venv/bin/activate  # On Linux/Mac
# OR
venv\Scripts\activate     # On Windows
```

On Windows, you may need to open in PowerShell as administrator and give:

```
Set-ExecutionPolicy RemoteSigned 
```

Watch for `(venv)` to appear at the beginning of your command prompt to verify that the virtual environment is active.

3. **Install the required dependencies:**
```bash
# Install project dependencies
python -m pip install -r requirements.txt
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
Access dtg2sim via variable c, GMEnv via g, and QueryEngine via e.

To simulate..: c.simulate()
To train.....: c.train()
To test......: c.test()
To debug.....: g.reset() followed by repeated g.step(agentAction,[stochasticAction]).

For parameter info: help([dtg2sim or GMEnv].[method])
```

Follow these directions to simulate or train and test the spec.
# See also

The (extended) DT-Golog will typically be the result of a translation from iStarDT-X, an XML based language for representing iStar-DT domains. Translation is accomplished via the [`dtx2X`](https://github.com/cmg-yorku/dtx2X) deserializer/validator and the accompanying `dtx2dtg` translator, both currently found in the [same repository](https://github.com/cmg-yorku/dtx2X).

# Contact

Please send questions, issues, bugs, and recommendations to [liaskos@yorku.ca](mailto:liaskos@yorku.ca?Subject=RLGen).


