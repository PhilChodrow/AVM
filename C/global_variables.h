#ifndef GLOBAL_VARIABLES_H_
#define GLOBAL_VARIABLES_H_

#include "Random1.h"
#include <vector>
#include "Edge.h"

Random1 random_number;
vector<vector<Edge>::iterator> NULL_EDGE_BOUNDARY;
vector<vector<Edge>::iterator>::iterator NULL_EDGE_BOUNDARY_ITERATOR=NULL_EDGE_BOUNDARY.end();
vector<vector<Node>::iterator> NULL_SITES;
vector<vector<Node>::iterator>::iterator NULL_SITES_ITERATOR=NULL_SITES.end();
int lastadopt=0;

#endif /* GLOBAL_VARIABLES_H_ */
