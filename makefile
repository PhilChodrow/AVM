# C++ parameters
CC = g++
CFlags = -O3 -Wall -o

# needed for ims
voter_files = C/main.cpp C/Dynamic_Voter.cpp C/Node.cpp C/Edge.cpp C/Random1.cpp 
py_files    = py/avm/avm.py py/run.py
model_peds  = throughput/model_preds.csv
approx_files = r/models.R r/compute_arch.R


# directories and file paths
C_DATA_DIR = data/C
BIN_DIR = C/bin
bin_loc = C/bin/DynamicVoter

PY_DATA_DIR = data/py

# compile bin
$(bin_loc): $(voter_files)
	if [ ! -d "$(BIN_DIR)" ]; then \
	  mkdir $(BIN_DIR);            \
	fi
	$(CC) $(voter_files) $(CFlags) $(bin_loc)

# run simulation (C)
c_data: $(bin_loc) C/run.py
	if [ ! -d "$(C_DATA_DIR)" ]; then \
	  mkdir -p $(C_DATA_DIR);           \
	fi
	python C/run.py

# run simulation (py)

py_data: $(py_files) 
	if [ ! -d "$(PY_DATA_DIR)" ]; then \
	  mkdir -p $(PY_DATA_DIR);           \
	fi
	python py/run.py

approx: $(approx_files)
    Rscript r/compute_arch.r


# all: $(bin_loc) c_data py_data
all: $(bin_loc) c_data approx

clean: 
	rm -rf C/bin
	rm -rf data
	rm -rf fig