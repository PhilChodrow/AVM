#include "Dynamic_Voter.h"
#include "global_variables.h"
#include <sstream>
#include <iostream>
#include <iomanip>
#include <queue>
#include <math.h>
#include <algorithm>

using namespace std;

Dynamic_Voter::Dynamic_Voter() {
	// TODO Auto-generated constructor stub
}

//Generate a ER random graph with given number of nodes and edges
int Dynamic_Voter::ER_network(long int number_of_nodes, long int number_of_edges, int number_of_opinions) {
	bool need_new_neigh;
	long int j,i1,i2;
	int i;
	vector<Node>::iterator person_it;

    population.resize(number_of_nodes);
    sites.resize(number_of_opinions);
	for(i=0;i<number_of_opinions;i++)
		sites[i].reserve(number_of_nodes);
    edges.reserve(number_of_edges);
	edge_boundary.reserve(number_of_edges);

	for(person_it=population.begin(),j=0; person_it!=population.end(); person_it++,j++) {
		person_it->ID=j;
		person_it->state=0;
		person_it->myself=person_it;
    }
	for(j=0; j<number_of_edges; j++) {
		// choose two nodes
		need_new_neigh=true;
		while (need_new_neigh){
			i1=random_number.integer(number_of_nodes);
			i2 = i1;
			while (i2 == i1) {
				i2=random_number.integer(number_of_nodes);
            }
			need_new_neigh=is_neighbor(i1,i2);
        }
		add_edge(i1, i2); // add this edge
    }
	return 0;
}

Dynamic_Voter::~Dynamic_Voter() {
	// TODO Auto-generated destructor stub
}

bool Dynamic_Voter::is_neighbor(long int i1, long int i2) {
	list<vector<Edge>::iterator>::iterator neigh_edge_it;

	for (neigh_edge_it=population[i1].edge_list.begin(); neigh_edge_it!=population[i1].edge_list.end(); neigh_edge_it++) {
		if ((*neigh_edge_it)->person1->ID == i2)
			return true;
		if ((*neigh_edge_it)->person2->ID == i2)
			return true;
        }
	return false;
}

vector<Edge>::iterator Dynamic_Voter::add_edge(long int i1, long int i2) {
	Edge new_edge;
	vector<Edge>::iterator edge_it;

	edge_it = edges.insert(edges.end(), new_edge);
	edge_it->person1 = population[i1].myself;
	edge_it->person2 = population[i2].myself;
	edge_it->link1 = population[i1].edge_list.insert(population[i1].edge_list.begin(), edge_it);
	edge_it->link2 = population[i2].edge_list.insert(population[i2].edge_list.begin(), edge_it);
	edge_it->myself=edge_it;

	if (population[i1].state != population[i2].state) {
		edge_it->state = 1;
		edge_it->boundary_place = edge_boundary.insert(edge_boundary.end(),	edge_it);
    }
	else {
		edge_it->state=0;
		edge_it->boundary_place = NULL_EDGE_BOUNDARY_ITERATOR;
    }
	return edge_it;
}

int Dynamic_Voter::assign_states(vector<float> &initial_density) {
    vector<Node>::iterator person_it;
    vector<Edge>::iterator edge_it;
    double u;
    int i;
    vector<float> cmf;
    cmf.reserve(initial_density.size()+1);
    cmf.push_back(0);

    for (i=0;i<(int)initial_density.size();i++)
    	cmf.push_back(initial_density[i]+cmf[i]);

    for (person_it = population.begin(); person_it != population.end(); person_it++) {
    	u=random_number.real();
    	for (i=0;i<(int)cmf.size()-1;i++)
    		if (u > cmf[i] && u<=cmf[i+1]) {
    			person_it->state = i;
    			person_it->sites_place = (sites[i]).insert((sites[i]).end(),person_it);
    			break;
            }
    }
    for (edge_it = edges.begin(); edge_it != edges.end(); edge_it++) {
    	if (edge_it->person1->state != edge_it->person2->state) {
    		edge_it->state=1;
    		edge_it->boundary_place=edge_boundary.insert(edge_boundary.end(),edge_it);
        }
    }
    return 0;
}

// swap the edge with the last edge in the edgelist and delete it
int Dynamic_Voter::delete_edge(vector<Edge>::iterator edge_it) {
	if (edge_it->state == 1) {
		swap_delete(edge_it, edge_boundary);
	}
	edge_it->person1->edge_list.erase(edge_it->link1);
	edge_it->person2->edge_list.erase(edge_it->link2);
	if (edge_it != (edges.back()).myself && edges.size()!=1) {
		*edge_it=edges.back();
		*(edge_it->link1) = edge_it;
		*(edge_it->link2) = edge_it;
		edge_it->myself = edge_it;
		if (edge_it->state==1)
			*(edge_it->boundary_place) = edge_it;
    }
	edges.pop_back();

	return 0;
}

// swap the edge pointer with the last edge pointer in the list and remove it
bool Dynamic_Voter::swap_delete(vector<Edge>::iterator edge_it, vector<vector<Edge>::iterator> & site) {
	if (edge_it->boundary_place == NULL_EDGE_BOUNDARY_ITERATOR)
		return false;
	if (site.size()==1 || edge_it == site.back())
		;
	else {
		*(edge_it->boundary_place) = site.back();
		(*site.back()).boundary_place = edge_it->boundary_place;
    }
	site.pop_back();
	edge_it->boundary_place=NULL_EDGE_BOUNDARY_ITERATOR;
	return true;
}

// swap the node pointer with the last node pointer in the list and remove it
bool Dynamic_Voter::swap_delete(vector<Node>::iterator person_it, vector<vector<Node>::iterator> & site) {
	if(person_it->sites_place == NULL_SITES_ITERATOR)
		return false;
	if (site.size()==1 || person_it == site.back())
		;
	else {
		*(person_it->sites_place) = site.back();
		(*site.back()).sites_place = person_it->sites_place;
    }
	site.pop_back();
	person_it->sites_place = NULL_SITES_ITERATOR;
	return true;
}

double Dynamic_Voter::simulate(int mode, float alpha, float lambda, int dt, double max_steps, string process) {
	long int e1,e0;
	double step=0;
	// int i,j,k;
	int i,j;
	vector<Edge>::iterator edge_it;
	int action=-1; //0:adapt, 1:rewire
	ofstream pFile_process;

	
	if (!process.empty()) {
		pFile_process.open(process.c_str());
		pFile_process<<"step action alpha lambda N01_prev ";
		for (j=0; j< (int)sites.size(); j++)
			pFile_process<<"N"<<j<<" ";
		for (i=0;i<(int)sites.size();i++)
			for (j=i;j<(int)sites.size();j++)
				pFile_process<<"N"<<i<<j<<" ";
		// for (i=0;i<(int)sites.size();i++)
		// 	for (j=0;j<(int)sites.size();j++)
		// 		for (k=i;k<(int)sites.size();k++)
		// 			pFile_process<<"N"<<i<<j<<k<<" ";
		pFile_process<<endl;
        
        e0 = edge_boundary.size();
		pFile_process<<step<<" "<<action<<" "<<alpha<<" "<<lambda<< " "<<e0<<" ";
		print_statistics_triple(pFile_process);
    }

	while (  (max_steps<0 || step<max_steps) ) {
	    action=-1; // default is pass
	    e0 = edge_boundary.size();
        if (random_number.real()<lambda) {
            //mutation model
            e1 = random_number.integer(population.size());
            action=2;
            mutate_state(e1);
        }
        else { //evolving voter model
        	if (edge_boundary.size() > 0){
	    		e1 = random_number.integer(edge_boundary.size());
	    		edge_it=edge_boundary[e1];
	    		if (random_number.real()<alpha) {
	                action=1;
                    if (mode == 0){
                        rand_rewire(edge_it);
                    }
                    else if (mode == 1){
                        pref_rewire(edge_it);
                    }
	            }
	    		else {
	                action=0;
	    			adopt_state(edge_it);
	            }
	        }
	    }
		step++;
        if ((long int)step%dt==0) {
            if (pFile_process.is_open()) {
                pFile_process<<step<<" "<<action<<" "<<alpha<<" "<<lambda<<" "<<e0<<" ";
                print_statistics_triple(pFile_process);
            }
        }
	}

    if ((long int)step%dt!=0) {
        if (pFile_process.is_open()) {
    		pFile_process<<step<<" "<<action<<" "<<alpha<<" "<<lambda<<" "<<e0<<" ";
    		print_statistics_triple(pFile_process);
    		pFile_process.close();
        }
    }

	return step;
}

int Dynamic_Voter::mutate_state(long int pid){
    int state_after;
    vector<Node>::iterator person3_it;
    vector<Node>::iterator person1_it=population[pid].myself;
    vector<Edge>::iterator edge_it;
	list<vector<Edge>::iterator>::iterator neigh_edge_it;

    // flip this person's state
    if (person1_it->state==0)
        state_after=1;
    else
        state_after=0;
	swap_delete(person1_it, sites[person1_it->state]);
	person1_it->sites_place = sites[state_after].insert(sites[state_after].end(),person1_it);
	person1_it->state = state_after;

	// need to go through all of person 1's neighbors:
	// each edge that was concordant is now discordant and vice versa
	for (neigh_edge_it = person1_it->edge_list.begin(); neigh_edge_it!=person1_it->edge_list.end(); neigh_edge_it++) {
		edge_it = *neigh_edge_it;
		if (edge_it->state==0) { // "0" means that this edge was concordant
			edge_it->state=1;
			edge_it->boundary_place=edge_boundary.insert(edge_boundary.end(),edge_it);
		}
		else { // edge was discordant, it may be concordant now
			if (edge_it->person1 == person1_it)
				person3_it=edge_it->person2;
			else
				person3_it=edge_it->person1;
			if (person1_it->state == person3_it->state){
				edge_it->state=0;
				swap_delete(edge_it, edge_boundary);
            }
        }
    }
	return 0;
}


int Dynamic_Voter::adopt_state(vector<Edge>::iterator edge_it) {
	vector<Node>::iterator person1_it, person2_it, person3_it;
	list<vector<Edge>::iterator>::iterator neigh_edge_it;

	person1_it = edge_it->person1;
	person2_it = edge_it->person2;
	if (random_number.real() < 0.5) {
		person1_it = edge_it->person2;
		person2_it = edge_it->person1;
    }
	// person 1 adopts the state of person 2.
	swap_delete(person1_it, sites[person1_it->state]);
	person1_it->sites_place = sites[person2_it->state].insert(sites[person2_it->state].end(),person1_it);
	person1_it->state = person2_it->state;

	// need to go through all of person 1's neighbors:
	// each edge that was concordant is now discordant and vice versa
	for (neigh_edge_it = person1_it->edge_list.begin(); neigh_edge_it!=person1_it->edge_list.end(); neigh_edge_it++) {
		edge_it = *neigh_edge_it;
		if (edge_it->state==0) { // "0" means that this edge was concordant
			edge_it->state=1;
			edge_it->boundary_place=edge_boundary.insert(edge_boundary.end(),edge_it);
		}
		else { // edge was discordant, it may be concordant now
			if (edge_it->person1 == person1_it)
				person3_it=edge_it->person2;
			else
				person3_it=edge_it->person1;
			if (person1_it->state == person3_it->state){
				edge_it->state=0;
				swap_delete(edge_it, edge_boundary);
            }
        }
    }
	return 0;
}

int Dynamic_Voter::rand_rewire(vector<Edge>::iterator edge_it){
	vector<Node>::iterator person1_it, person2_it;
	long int i1, i2;
	bool need_new_neigh=true;

	person1_it = edge_it->person1;
	person2_it = edge_it->person2;
	// swap roles of the two people, but need to note this!
	if (random_number.real() < 0.5) {
		person1_it = edge_it->person2;
		person2_it = edge_it->person1;}

	i1 = person1_it->ID;
	while (need_new_neigh){
		i2 = random_number.integer(population.size());
		need_new_neigh = (i1==i2) || is_neighbor(i1, i2);
    }
	delete_edge(edge_it);
	add_edge(i1, i2);

	return 0;
}

bool Dynamic_Voter::pref_rewire(vector<Edge>::iterator edge_it){
	vector<Node>::iterator person1_it, person2_it;
	long int i1, i2;
	int counter=0;
	bool need_new_neigh=true;

	person1_it = edge_it->person1;
	person2_it = edge_it->person2;
	// swap roles of the two people, but need to note this!
	if (random_number.real() < 0.5) {
		person1_it = edge_it->person2;
		person2_it = edge_it->person1;}

	// remove edge between person 1 and person 2
	// then form list between person 1 and person x
	// x chosen at random from the same group as person 1
	i1 = person1_it->ID;
	while (need_new_neigh && counter<10){
		i2 = random_number.integer(sites[person1_it->state].size());
		i2 = (sites[person1_it->state][i2])->ID;
		need_new_neigh = (i1==i2) || is_neighbor(i1, i2);
		counter++;}

	if (need_new_neigh == false){
		delete_edge(edge_it);
		add_edge(i1, i2);
		return true;}
	else
		return false;
}

void Dynamic_Voter::print_statistics_simple(ofstream &pFile_process) {
	int j;
	for (j=0; j<(int)sites.size(); j++)
		pFile_process<<sites[j].size()<<" ";
	pFile_process<<edge_boundary.size()<<endl;
}

void Dynamic_Voter::print_statistics_triple(ofstream &pFile_process) {
    int i,j;
	vector<Node>::iterator person_it,person_it1,person_it2;
	vector<Edge>::iterator edge_it;
	list<vector<Edge>::iterator>::iterator neigh_edge_it1, neigh_edge_it2;
    long int *d=new long int [sites.size()];

    long int **N_edges=new long int* [sites.size()];
    for (i=0;i<(int)sites.size();i++) {
        N_edges[i]=new long int [sites.size()];
        for (j=0;j<(int)sites.size();j++)
            N_edges[i][j]=0;}

    // long int ***N_triples=new long int** [sites.size()];
    // for (i=0;i<(int)sites.size();i++) {
    //     N_triples[i]=new long int* [sites.size()];
    //     for (j=0;j<(int)sites.size();j++) {
    //         N_triples[i][j]=new long int [sites.size()];
    //         for (k=0;k<(int)sites.size();k++)
    //             N_triples[i][j][k]=0;}}

    for (person_it = population.begin(); person_it != population.end(); person_it++) {
        for (i=0;i<(int)sites.size();i++)
            d[i]=0;
        for (neigh_edge_it1 = person_it->edge_list.begin(); neigh_edge_it1!=person_it->edge_list.end(); neigh_edge_it1++) {
            if ((*neigh_edge_it1)->person1==person_it)
                person_it1=(*neigh_edge_it1)->person2;
            else
                person_it1=(*neigh_edge_it1)->person1;
            d[person_it1->state]++;}
        for (i=person_it->state;i<(int)sites.size();i++)
            N_edges[person_it->state][i]+=d[i];
        // for (i=0;i<(int)sites.size();i++)
        //     N_triples[i][person_it->state][i]+=d[i]*(d[i]-1);
        // for (i=0;i<(int)sites.size();i++)
        //     for (j=i+1;j<(int)sites.size();j++)
        //         N_triples[i][person_it->state][j]+=(d[i])*(d[j]); }
    }

    for (j=0; j<(int)sites.size(); j++)
		pFile_process<<sites[j].size()<<" ";

	for (i=0; i<(int)sites.size(); i++)
		for (j=i; j<(int)sites.size(); j++)
			pFile_process<<N_edges[i][j]<<" ";

	// for (i=0; i<(int)sites.size(); i++)
	// 	for (j=0; j<(int)sites.size(); j++)
	// 		for (k=i; k<(int)sites.size(); k++)
	// 			pFile_process<<N_triples[i][j][k]<<" ";

    pFile_process<<endl;

	delete[] d;
	for (i=0;i<(int)sites.size();i++) {
		delete[] N_edges[i];
		// for (j=0;j<(int)sites.size();j++) {
		// 	delete[] N_triples[i][j]; }
		// delete[] N_triples[i]; }
	}
	delete[] N_edges;
	// delete[] N_triples;
}
