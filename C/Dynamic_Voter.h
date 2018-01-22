#ifndef DYNAMIC_VOTER_H_
#define DYNAMIC_VOTER_H_

#include "Node.h"
#include "Edge.h"
#include <fstream>
#include <string>

class Dynamic_Voter{
public:
	vector<Node> population; // list of nodes
	vector<Edge> edges; // list of edges
	vector<vector<vector<Node>::iterator> > sites; // list of pointers to i-nodes
	vector<vector<Edge>::iterator> edge_boundary; // list of discordant edges

public:
	Dynamic_Voter();
	virtual ~Dynamic_Voter();
    int ER_network(long int number_of_nodes, long int number_of_edges, int number_of_opinions); // Generate ER getwork
	bool is_neighbor(long int i1, long int i2); // test if nodes i1 and i2 and neighbors
	vector<Edge>::iterator add_edge(long int i1, long int i2); // add an edge between node i1 and i2
	int delete_edge(vector<Edge>::iterator edge_it); // delete a single edge
	bool swap_delete(vector<Node>::iterator person_it, vector<vector<Node>::iterator> & sites);
	bool swap_delete(vector<Edge>::iterator edge_it, vector<vector<Edge>::iterator> & sites);
	int assign_states(vector<float> &initial_density);
	double simulate(int mode, float alpha, float lambda, int dt, double max_steps, string process);
	int mutate_state(long int pid); // mutate node state
	int adopt_state(vector<Edge>::iterator edge_it);
	int rand_rewire(vector<Edge>::iterator edge_it); // rewire to random
	bool pref_rewire(vector<Edge>::iterator edge_it);
	void print_statistics_simple(ofstream &pFile_process); //print simulation statistics at given time point
	void print_statistics_triple(ofstream &pFile_process); //print simulation statistics at given time point
};

#endif /* DYNAMIC_VOTER_H_ */
