# C++ parameters
CC = g++
CFlags = -O3 -Wall -o

# needed for bin
voter_files = C/main.cpp C/Dynamic_Voter.cpp C/Node.cpp C/Edge.cpp C/Random1.cpp 

# directories and file paths
DATA_DIR = data/C
BIN_DIR = C/bin
bin_loc = C/bin/DynamicVoter

# run simulation
data: $(bin_loc) C/run.py
	if [ ! -d "$(DATA_DIR)" ]; then \
	  mkdir -p $(DATA_DIR);           \
	fi
	python C/run.py

# compile bin
$(bin_loc): $(voter_files)
	if [ ! -d "$(BIN_DIR)" ]; then \
	  mkdir $(BIN_DIR);            \
	fi
	$(CC) $(voter_files) $(CFlags) $(bin_loc)

all: $(bin_loc) data

clean: 
	rm -rf bin
	rm -rf data