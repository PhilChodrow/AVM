#include <cstring>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <time.h>
#include <vector>
#include "Dynamic_Voter.h"
#include <algorithm>
#include <math.h>
#include <cstdlib>
#include <string>
using namespace std;

int main(int argc, char* argv[]) {
// Arguments to the function:
// -n number_of_nodes -m number_of_edges -u number_of_opinions initial_densities -a alpha -l lambda -t dt -T max_steps -o output_filename
	long int number_of_nodes, number_of_edges;
	double steps, max_steps;
	float alpha, lambda;
	int dt, i, number_of_opinions, mode;
	vector<float> initial_density;
	clock_t t_start;
	string s,process, filename="evolving_voter";
	size_t start_pos;

//	Initialize the parameters
	number_of_nodes=1000;
	number_of_edges=2000;
	number_of_opinions=2;
	initial_density.push_back(0.5);
	initial_density.push_back(0.5);
	alpha=0.3;
    lambda=0.1;
    dt=1;
    max_steps=-1;
    mode = 0;
	if(argc>1){
		for (i = 1; i < argc; i++){
			if (!strcmp(argv[i],"-n")){
				i++;
				number_of_nodes=atol(argv[i]);}
			else if (!strcmp(argv[i],"-m")){
				i++;
				number_of_edges=atol(argv[i]);}
			else if (!strcmp(argv[i],"-u")){
				i++;
                initial_density.clear();
                initial_density.push_back(atof(argv[i]));
                initial_density.push_back(1.0-atof(argv[i]));}                 
//				number_of_opinions=atoi(argv[i]);
//				initial_density.clear();
//				for (j=0; j<number_of_opinions; j++){
//					i++;
//					initial_density.push_back(atof(argv[i]));}}
			else if (!strcmp(argv[i],"-a")){
				i++;
				alpha=atof(argv[i]);}
			else if (!strcmp(argv[i],"-t")){
				i++;
				dt=atoi(argv[i]);}
			else if (!strcmp(argv[i],"-T")){
				i++;
				max_steps=atof(argv[i]);}
			else if (!strcmp(argv[i],"-o")){
				i++;
				filename=argv[i];}
			else if (!strcmp(argv[i],"-l")){
				i++;
				lambda=atof(argv[i]);}
			else if (!strcmp(argv[i],"-M")){
				i++;
				mode=atoi(argv[i]);}
			else {
				cout<<"unknown parameter: "<<argv[i]<<endl;
				exit(1);}
		}
	}
    


	// summary of the simulation
	filename = filename + ".summary";

	ofstream outfile(filename.c_str());
	outfile << "nodes " << "edges " << "alpha "<<"lambda ";
	for (i=0; i<number_of_opinions; i++)
		outfile<<"u"<<i<<" ";
	outfile<< "steps " << "time " << endl;

    // create file to save the frequencies of opinions throughout the process
	s="summary";
	process=filename;
	start_pos = process.find(s);
	process.replace(start_pos, s.length(), "process");

	t_start=clock();
	Dynamic_Voter *dv = new Dynamic_Voter();
    dv->ER_network(number_of_nodes,number_of_edges,number_of_opinions);
	dv->assign_states(initial_density);
	steps = dv->simulate(mode, alpha, lambda, dt, max_steps, process);
	outfile <<number_of_nodes<<" "<<number_of_edges<<" "<< alpha <<" " << lambda <<" ";
	for (i=0; i<number_of_opinions; i++)
		outfile << dv->sites[i].size() <<" ";
    outfile << steps <<" "<< (float)(clock()-t_start)/CLOCKS_PER_SEC << endl;
	delete dv;

	outfile.close();
	return 0;
}
